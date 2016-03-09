;;; -*- syntax: common-lisp; package: keim; base: 10; mode: lisp -*-
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

(IN-PACKAGE "KEIM")





(mod~defmod RES 
            :uses (cl data delta env just keim lit node pds pos post pp prob subst term termix th type)
            :documentation "Datastructures, I/O functions and basic operations for resolution proofs."
            :exports (
                      res+factoring
                      res+flip
                      res+hyper-resolution
                      res+initial
                      res+instance
                      res+justification
                      res+negative-hyper-resolution
                      res+paramodulation
                      res+positive-hyper-resolution
                      res+proof
                      res+reflex
                      res+resolution
		      res+equality-factoring
                      res+ur-resolution
                      
                      res~add-proof-in-hash-table
		      res~binary-demodulation
		      res~binary-demodulation-ii
                      res~binary-factoring
                      res~binary-paramodulation
                      res~binary-resolution
		      res~equality-factoring
                      res~check-electrons-p
                      res~factoring-clause
                      res~factoring-create
                      res~factoring-p
                      res~factoring-positions
                      res~find-proof
                      res~flip-create
                      res~flip-p
                      res~hyper-resolution
                      res~hyper-resolution-create
                      res~hyper-resolution-p
                      res~initial-create
                      res~initial-p
                      res~instance-clause
                      res~instance-create
                      res~instance-p
                      res~just-renamings
                      res~just-unifier
                      res~justification-parents
                      res~justification-positions
                      res~justification-renamings
                      res~justification-unifier
                      res~literal-equal-p
		      res~equality-factoring-create
		      res~equality-factoring-just-remaining-literal-position
		      res~equality-factoring-p 
                      res~paramod-create
                      res~paramod-direction
                      res~paramod-father
                      res~paramod-father-position
                      res~paramod-just-direction
                      res~paramod-mother
                      res~paramod-mother-position
                      res~paramodulation-p
		      res~paramodulation-mother-father
                      res~proof-assumptions
                      res~proof-clauses
                      res~proof-conclusion
                      res~proof-create
                      res~proof-create-from-problem
                      res~proof-delta-relation
                      res~proof-empty-clause
                      res~proof-environment
                      res~proof-initial-clauses
                      res~proof-p
                      res~proof-read
                      res~proof-skolem-functions
                      res~proof-step-clauses
                      res~proof-upper-object
                      res~read-resolution-proof-from-file
                      res~read-step
                      res~reflex-create
                      res~reflex-p
                      res~resolution-clauses
                      res~resolution-create
                      res~resolution-p
                      res~resolution-positions
                      res~separate-clauses
                      res~separate-literals
                      res~set-initial-justification!
                      res~step-p
                      res~tautology-p
                      res~ur-resolution
                      res~ur-resolution-create
                      res~ur-resolution-p
                      res~write-resolution-proof-in-file
		      res~check-resolution-proof
		      res~check-step
                      
                      res*current-resolution-proof
                      res*justification-counter
                      res*proof-hash-table))





;; res*paramod-counter                           -->  ersetzt durch res*justification-counter
;; res*res-justification-counter                 -->  ersetzt durch res*justification-counter
;; res*fact-justification-counter                -->  ersetzt durch res*justification-counter
;; res*initial-res-justification-counter         -->  ersetzt durch res*justification-counter
;; res*hyper-resolution-justification-counter    -->  ersetzt durch res*justification-counter
;; res*ur-resolution-justification-counter       -->  ersetzt durch res*justification-counter
;; res*flip-counter                              -->  ersetzt durch res*justification-counter
;; res*reflex-counter                            -->  ersetzt durch res*justification-counter





(defvar res*current-resolution-proof nil)
;; Variable to store the current resolution proof in

(defvar res*proof-hash-table (make-hash-table :test #'equal)
  "Hash table, indexed by proof name, that holds all existing resolution proofs.
This way we can refer to proofs by name.")

(defvar res*justification-counter 0)
;; Variable to store a counter for the produced resolution proofs in

#{\section{Resolution}
\label{mod:res}

Resolution is a well-known concept, and this module defines resolution
proofs and steps in a straightforward manner. The definitions are based on
the proof module and the justification
module (sections \ref{mod:proof} and \ref{mod:just} in chapter \ref{sys:problem}).


Here is the POST input form for
resolution proofs:
\begin{postsyntax}
\syntax{

\nt{proof-step}        ::=  \nt{resolution-step} | \nt{factoring-step} | \nt{paramodulation-step}
                               | \nt{instance-step}.

%\bigskip

\nt{resolution-step}   ::=   (resolution \nt{name} \nt{resolvent}
                                (parents \nt{resolution-parent} \nt{resolution-parent})
                                \nt{unifier}).

%\medskip

\nt{resolvent}         ::=   \nt{clause}.
\nt{resolution-parent} ::=   (\nt{clause-name} \nt{position} \nt{renaming}).
\nt{renaming}          ::=   (renaming \nt{varlist} \nt{varlist}).
\nt{unifier}           ::=   \nt{substitution}.


%\bigskip

\nt{factoring-step}    ::=   (factoring \nt{name} \nt{resolvent} \nt{factoring-parent} \nt{unifier}).
\nt{factoring-parent}  ::=   (parents (\nt{clause-name} \nt{position} \nt{position}  \nt{renaming})).

%\bigskip

\nt{instance-step}     ::=   (instance \nt{name} \nt{resolvent} \nt{parent} \nt{substitution}).
\nt{instance-parent}   ::=   (\nt{clause-name}).

%\bigskip

\nt{paramodulation-step}   ::=    (paramodulation \nt{name} \nt{resolvent}
                                     \nt{resolution-parent} \nt{equation} \nt{unifier}).

%\medskip

\nt{equation}   ::=   (\nt{clause-name} \nt{position} \nt{renaming} \nt{direction}).
\nt{direction}  ::=   LR | RL.

}

\end{postsyntax}
#}

#{\subsection{Resolution proofs}

Resolution proofs are a subclass of {\vb proof+abstract}. From this
class and from its superclass {\vb prob+problem} they inherit some
slots and some generic functions (see section \ref{mod:prob} and \ref{mod:proof}).
A resolution proof object needs not to be a complete proofs. Also the
startup for a resolution proof and the proving itself can be managed with this class.

The class {\vb res+proof} defines the following slots: a list of the
initial clauses, the empty clause, and the delta relation.
The initial clauses are the conjunctive normal form of the assumptions
and the conclusion slot (inherited from {\vb prob+problem}). To store
the origins of the literals in these clauses the delta relation slot
can contain a mapping between literals and positions in formulae. 
Normalization is provided by the cnf module (section \ref{mod:cnf})
and delta relations are defined in the delta module (section \ref{mod:delta}).

Since the empty clause in a resolution proof is the root of the proof
tree, the resolution proof class provides a slot for it.
The steps of a resolution proof are stored in the steps slot which is inherited
from {\vb proof+abstract}.

#}

(eval-when (load compile eval) 
  (defclass res+proof (prob+proof) 
    ((problem :initarg :upper-object
	      :accessor res~proof-upper-object
	      :documentation "The problem or the PDS, to that the res+proof accords")
     (environment :initarg :environment
		  :accessor res~proof-environment
		  :documentation "The environment of the resolution proof, it inherits from the environment of the corresponding upper-object")
     (conclusion :initarg :conclusion
		 :accessor res~proof-conclusion
		 :documentation "The named formula to be proved by the resolution proof")
     (assumptions :initarg :assumptions
		  :accessor res~proof-assumptions
		  :documentation "The named formulae that can be used as hypothesis for the resolution-proof") 
     (skolem-functions :initarg :skolem-functions
		       :accessor res~proof-skolem-functions
		       :documentation "The list of alle skolem-functions in the resolution-proof")
     (steps :initarg :clauses
	    :accessor res~proof-clauses
	    :documentation "The set of all clauses"
	    ;; new accessor for the steps slot of prob+problem
	    )
     (delta-relation :initarg :delta-relation
		     :accessor res~proof-delta-relation
		     :documentation "The delta-relation of the resolution proof.")
     (root :initarg :empty-clause
	   :accessor res~proof-empty-clause
	   :documentation "The empty clause which is a member of the step-clauses."
	   ;; new accessor for the root slot of prob+proof
	   ))
    (:documentation "The datastructure for resolution proofs.")))

(defun res~proof-p (thing)
  (declare
   (authors nesmith)
   (input   "Any lisp object")
   (effect  "None.")
   (value   "T if THING is a resolution proof, otherwise nil."))
  (typep thing 'res+proof))


(defmethod print-object ((resolution res+proof) stream)
    (format stream "(Resolution-proof ~A)"
	    (keim~name resolution)))

(defun res~proof-create (name upper-object theory conclusion assumptions &key (env nil))
  (declare (edited  "20-JUN-1997")
	   (authors Ameier)
	   (input   "A name, a upper-object, a theory, a cnclusion (a line in the"
		    "upper-object), the assumption (a list of lines in the"
		    "upper-object) and as keyword a possible environment.") 
	   (effect  "None.")
	   (value   "A resolution proof."))
  (let* ((new-res-proof (make-instance 'res+proof
				       :name (if (stringp name)
						 (intern (string-upcase name) (find-package :keim))
					       name)
				       :upper-object upper-object
				       :environment (if env
							env
						      (env~create (if (prob~p upper-object)
								      (prob~environment upper-object)
								    (pds~environment upper-object))))
				       :conclusion conclusion
				       :assumptions assumptions
				       :theory theory
				       :empty-clause nil
				       :clauses nil
				       :delta-relation (delta~create-relation)
				       :skolem-functions nil)))
    (res~add-proof-in-hash-table new-res-proof)
    new-res-proof))

(defun res~proof-create-from-problem (problem)
  (declare (edited  "20-JUN-1997")
	   (authors Ameier)
	   (input   "A problem.")
	   (effect  "None.")
	   (value   "A new resolution proof with the problem as upper-object, the same"
		    "theory as the problem and the conclusion and the assumptions of the problem"
		    "as concluiosn and assumptions."))
  (let* ((problem-name (keim~name problem))
	 (string-name (if (stringp problem-name)
			  problem-name
			(string problem-name)))
	 (new-res-proof (res~proof-create
			  (format nil "RES-PROOF-TO-PROBLEM-~A" string-name) 
			  problem
			  (prob~theory problem)
			  (make-instance 'termix+named-term
					 :term (data~copy (node~formula (prob~conclusion problem))
							  :downto '(data+primitive))
					 :name (keim~name (prob~conclusion problem)))
			  (mapcar #'(lambda (assumption-line)
				      (make-instance 'termix+named-term
						     :term (data~copy (node~formula assumption-line)
								      :downto '(data+primitive))
						     :name (keim~name assumption-line)))
				  (prob~assumptions problem)))))
    new-res-proof))
;; TO-DO: Vorsicht:Name nicht eindeutig !


(defun res~add-proof-in-hash-table (resolution-proof)
  (declare (edited  "5-DEC-1996")
	   (authors Ameier)
	   (input  "A resolution proof.")
	   (effect "The proof is added into the res*proof-hash-table with its name as index.")
	   (value  "Undefined."))
  (let* ((name (keim~name resolution-proof))
	 (symbol (if (stringp name)
		     (intern (string-upcase name) (find-package :keim))
		   name)))
    (setf (gethash (symbol-name symbol) res*proof-hash-table)
	  resolution-proof)))


(defgeneric res~proof-initial-clauses (res-proof)
  (declare (edited  "20-JUN-1997")
	   (authors Ameier)
	   (input   "A resolution proof.")
	   (effect  "None.")
	   (value   "The list of all initial clauses of the resolution proof."))
  (:method (res-proof)
	   (error "~A must be of type RES+PROOF" res-proof))
  (:method ((res-proof res+proof))
	   (remove-if-not #'res~initial-p (res~proof-clauses res-proof))))

(defgeneric res~proof-step-clauses (res-proof)
  (declare (edited  "20-JUN-1997")
	   (authors Ameier)
	   (input   "A resolution proof.")
	   (effect  "None.")
	   (value   "The list of all none initial clauses of the resolution proof."))
  (:method (res-proof)
	   (error "~A must be of type RES+PROOF" res-proof))
  (:method ((res-proof res+proof))
	   (remove-if #'res~initial-p (res~proof-clauses res-proof))))

#| -----------------------------------------        ----------------------------------------------------------- |#

(defun res~proof-read (post-expr)
  (declare (edited  "26-NOV-1996")
	   (authors Ameier)
	   (input  "A Post expression for a resolution proof.")
	   (effect "None.")
	   (value  "The POST expression will be parsed as a Resolution proof object"
		   "and returned."))
  (when  (not (string= (string (first post-expr)) (string 'resolution-proof)))
    (error "Awaiting keyword resolution-proof instead of ~A" (first post-expr)))
 
  (when (not (or (string= (string (second (fourth post-expr))) 'problem)
		 (string= (string (second (fourth post-expr))) 'pds)))
    (error "Awaiting keyword problem or pds instead of ~A" (second (fourth post-expr))))
  
  (let* ((from-upper-object (if (string= (string (second (fourth post-expr))) (string 'problem))
				(prob~find-problem (third (fourth post-expr)))
			      (pds~find-proof-plan (third (fourth post-expr))))))
    (when (null from-upper-object)
      (error "Please load first the corresponding problem or pds ~A" (third (fourth post-expr))))
    (post~read-object (cdr post-expr) (env~create (if (prob~p from-upper-object)
						      (prob~environment from-upper-object)
						    (pds~environment from-upper-object))) :resolution-proof)))

      
(defun res~read-resolution-proof-from-file (file)
  (declare (edited  "26-NOV-1996")
	   (authors Ameier)
	   (input   "A file and optional.")
	   (effect  "None.")
	   (value   "The file is tried to read as resolution-proof."
		    "The proof is returned."))
  (with-open-file (stream file :direction :input)
		  (res~proof-read (read stream))))

(defun res~write-resolution-proof-in-file (resolution-proof file &key (supersede t))
  (declare (edited  "26-NOV-1996")
	   (authors Ameier)
	   (input   "A file and a resolution proof.")
	   (effect  "The resolution proof is written into the file, if the file exits it is"
		    "supersided.")
	   (value   "Undefined." )) 
    (with-open-file (proof-stream file :direction :output
				:if-exists (if supersede :supersede :append)
				:if-does-not-exist :create)	       
		    (let ((*standard-output* proof-stream))
		      (post~print resolution-proof *standard-output*))
		    (format *standard-output* ";;; Proof ~S written into file ~A"
			    resolution-proof (merge-pathnames file))))

(defun res~find-proof (name)
  (declare
   (authors NESMITH)
   (input   "A name of a resolution proof (symbol or string).")
   (effect  "None.")
   (value   "The resolution proof with this name, or NIL if none exists."))
  (gethash 
   (etypecase name
     (symbol (symbol-name name))
     (string name))
   res*proof-hash-table))

;; make sure the new proof is in the hash table
(defmethod shared-initialize :after ((obj res+proof) slot-names &rest initargs)
  (declare (ignore initargs slot-names))
  (when (eq (find-class 'res+proof) (class-of obj))
    (setf (gethash (symbol-name (keim~name obj)) res*proof-hash-table)
	  obj))
  obj)


#{\subsection{\post\ syntax for resolution proofs}

Since resolution proofs are a subclass of {\vb prob+problem} in this
module we only define the syntax for {\tt \nt{proof-result}} which
contains rules for the new slots and for the steps.  Resolution proofs
can be read in with {\vb prob~read} if the problem declaration is added.
There is an example at the end of this section.

\begin{postsyntax}
\syntax{
\nt{proof-result}    ::=   (resolution-proof \nt{cnf} \nt{proof-step}*).
\nt{cnf}             ::=   (cnf (\nt{clause}*) \nt{delta-relation}).}
\end{postsyntax}

If the \post\ syntax of a resolution proof is read in via {\vb
prob~read}, a resolution proof is created and the clauses slot and the
delta relation slot is set to the objects read from the {\tt \nt{cnf}} form.
The steps slot is set to the {\tt \nt{proof-steps}}. If an empty clause
is found amoung the steps it is stored in the empty clause slot.
#}

#|(defmethod post~print ((proof res+proof) stream)
  (declare (edited  "23-JUN-1997")
	   (authors Ameier)
	   (input   "A resolution proof and a stream.")
	   (effect  "Because the declarations of the problem are already printed by the"
		    "around-method for prob+problem this primary-method only has print the new slots.")
	   (value   "Undefined."))
  (format stream "(resolution-proof ")
  (format stream "~A" (keim~name proof))
  (format stream "~%(in ~A)" (keim~name (prob~proof-theory proof)))
  (format stream "~%(upper-object ~A ~A)" (if (prob~p (res~proof-upper-object proof))
					      'problem
					    'pds)
	  (keim~name (res~proof-upper-object proof)))
  (let* ((env (res~proof-environment proof))
	 (type-vars  (env~class-keys env 'type+variable nil))
	 (type-constants (env~class-keys env 'type+constant nil))
	 (constants (env~class-keys env 'term+constant nil))
	 (variables (env~class-keys env 'term+variable nil)))
    (format stream "~%(declarations " stream)
    (mapc #'(lambda (x) (env~post-print x 
					(env~lookup-object x env)
					stream))
	  (append type-vars type-constants constants variables))
    (format stream "~%(assumptions " stream)
    (mapc #'(lambda (x)
	      (post~print x stream)
	      (terpri stream))
	  (res~proof-assumptions proof))
    (princ ")" stream)
    (format stream "~%(conclusion " stream)
    (post~print (res~proof-conclusion proof) stream)
    (terpri stream)
    (princ ")" stream)
    (princ ")" stream)
    )
  (format stream "~%(cnf (")
  (post~print (res~proof-initial-clauses proof) stream)
  (format stream ") ~%")

  (format stream "(skolem-functions ")
  (mapc #'(lambda (skolem)
	    (format stream "~A " (keim~name skolem)))
	(res~proof-skolem-functions proof))
  (princ ")" stream)

  (post~print (res~proof-delta-relation proof) stream)
  (format stream ")")

  (format stream "~%(steps ") 
  (node~post-print-steps (res~proof-step-clauses proof) stream)
  (format stream "))"))
OLD: look at the end of the file |#


(defmethod post~read-object ((thing list) (env env+environment)
			     (indicator (eql :namedterm)))
  (let* ((name (post~read-symbol (car thing) env))
	 (term (post~read-object (cadr thing) env :existing-term))
	 (named-term (make-instance 'termix+named-term
				    :term term
				    :name name)))
    (env~enter name named-term env)
    named-term))
    
	       
(defmethod post~read-object ((cmd list) (env env+environment) 
			     (indicator (eql :resolution-proof)))
  (when (not (string= (string (first (second cmd))) (string 'in)))
    (error "Awaiting keyword in instead of ~A" (first (second cmd))))
  (when (not (string= (string (first (third cmd))) (string 'upper-object)))
    (error "Awaiting keyword upper-object instead of ~A" (first (third cmd))))
  (when (not (string= (string (first (fourth cmd))) (string 'declarations)))
    (error "Awaiting keyword declarations instead of ~A" (first (fourth cmd))))
  (when (not (string= (string (first (fifth cmd))) (string 'cnf)))
    (error "Awaiting keyword cnf instead of ~A" (first (fifth cmd))))
  (when (not (string= (string (first (sixth cmd))) (string 'steps)))
    (error "Awaiting keyword steps instead of ~A" (first (sixth cmd))))
  (when (not (string= (string (first (first (reverse (fourth cmd))))) (string 'conclusion)))
    (error "Awaiting keyword conclusion instead of ~A" (first (first (reverse (fourth cmd))))))
  (when (not (string= (string (first (second (reverse (fourth cmd))))) (string 'assumptions)))
    (error "Awaiting keyword assumptions instead of ~A" (first (second (reverse (fourth cmd))))))

  (let* ((name (post~read-symbol (first cmd) env))
	 (theory (th~find-theory (second (second cmd))))
	 (upper-object (if (string= (string (second (third cmd))) (string 'problem))
			   (prob~find-problem (third (third cmd)))
			 (pds~find-proof-plan (third (third cmd)))))

	 (declarations (rest (fourth cmd)))
	 (decls (post~read-object-list (reverse (rest (rest (reverse declarations)))) env))
	 (conclusion (post~read-object (second (first (reverse declarations))) env :namedterm))
	 (assumptions (mapcar #'(lambda (ass)
				  (post~read-object ass env :namedterm))
			      (rest (second (reverse declarations)))))	 
	 
	 (clauses-skolems-delta-relation (post~read-object (rest (fifth cmd)) env :cnf))	 
	 (clauses (first clauses-skolems-delta-relation))
	 (skolem-functions (second clauses-skolems-delta-relation))
	 (delta-relation (third clauses-skolems-delta-relation))
	 
	 (steps (mapcar #'(lambda (step) (res~read-step step env))
			(rest (sixth cmd))))

	 (empty-clause (find-if #'cl~empty-p steps))
	 
	 (proof (res~proof-create name upper-object theory conclusion assumptions :env env)))

    (declare (ignore decls))
    (mapcar #'(lambda (clause)
		(setf (node~justification clause) (res~initial-create))) 
	    clauses)
    
    (mapcar #'(lambda (element)
		(env~remove (keim~name element) env))
	    (cons conclusion (append assumptions clauses steps)))
    
    (setf (res~proof-delta-relation proof) delta-relation)
    (setf (res~proof-empty-clause proof) empty-clause)
    (setf (res~proof-skolem-functions proof) skolem-functions)
    (setf (res~proof-clauses proof) (append clauses steps))
    proof))


    
(defmethod post~read-object (cmd (env env+environment) 
				 (indicator (eql :cnf)))
  (declare (edited  "23-JUN-1997")
	   (authors Ameier)
	   (input   "A list representing the input of the clause normal form of a"
		    "resolution proof.")
	   (effect  "None.")
	   (value   "A list of three values:"
		    "first being the clauses, the second being the skolem-functions and"
		    "the third beeing the delta-relation."))  
  (unless (and (listp cmd)
	       (= 3 (length cmd))
	       (listp (first cmd)))
    (post~error "~A does not match specification for a ~A declaration"
		cmd indicator))
  (when (not (string= (string (first (second cmd))) (string 'skolem-functions)))
    (error "Awaiting keyword skolem-functions instead of ~A" (first (sixth cmd))))
  (when (not (string= (string (first (third cmd))) (string 'delta-relation)))
    (error "Awaiting keyword delta-relation instead of ~A" (first (sixth cmd))))
  
  (let ((clauses (mapcar #'(lambda (clause)
			     (cl~read clause env))
			 (first cmd)))
	(skolem-functions (mapcar #'(lambda (skolem)
				      (let* ((keybe (post~read-symbol (first skolem) env))
					     (skolem-function (env~lookup-object keybe env))
					     (number (second skolem)))
					(change-class skolem-function 'sksym+sk-constant)
					(setf (sksym~arity skolem-function) number)
					skolem-function))
				  (rest (second cmd))))
	(delta-relation (delta~read (third cmd) env)))
    (list clauses skolem-functions delta-relation)))



#{\subsection{Steps for resolution proofs}

A step in a resolution proof consists of a clause and a
justification for this clause. The clause is the result of the step and the
justification contains all information how the clause was
constructed. To avoid a new datastructure which
would make the handling of resolution proofs more difficult, a
step of a resolution proof in \keim\ is simply a clause with a
justification in its justification slot, i.e. a clause together with
its justification forms a step. All generic function defined for
justifications are also defined for instances of {\vb node+node}
(including clauses) so that clauses really can be viewed as steps.

This module provides datastructures and algorithms for the following
steps used in a resolution proof: resolution, factoring,
paramodulation, instantiation and giving an initial clause. For this purpose some
justification classes are defined.

\subsubsection{Abstract steps in resolution proofs}

To make the handling of steps of different kinds easier, they are as
uniformly represented as much as possible. Therefore an abstract
superclass {\vb res+justification} is defined which provides slots for
almost all information needed by the subclasses. This class is a
subclass of {\vb just+justification} from which it inherits the premises
slot. This slot is used to store all the parent clauses, i.e. the set of
clauses from which the new clause was inferred.

The new slots in {\vb res+justification} are:
\begin{description}
\item[positions] determine the literal or term in a parent clause
which was used in the step. To which clause a position belongs is
defined in the subclasses of {\vb res+justification}.
\item[unifier] is a substitution which unifies two terms or literals
of the parent clauses.
\item[renamings] is a list of substitution which rename the variables of the respective 
parent clauses in order to enforce variable disjointness. Note that it does not suffice to rename the resolvent only in the case of self resolution.
\end{description}

{\vb res+justification} is an abstract class, i.e. it has no
instances. Instances can be created for one of its subclasses. These
subclasses define the meaning of the slots of {\vb res+justification}.

For slots with a general meaning there are functions for selecting and
setting these slots defined on {\vb res+justification}: for the parent
clauses the accessor {\vb just~premises} is inherited from {\vb
just+justification} and {\vb node+node}; {\vb res~just-unifier} and
{\vb res~just-renamings} are defined.

There is also a common structure for the \post\ syntax of resolution steps:

{\vb \nt{proof-step}  ::= ({\rm step name} \nt{name} \nt{resolvent}
				{\rm ... parent information ...} \nt{unifier}).}

The resolvent is the resulting clause of the step, the unifier is a
substitution which unifies two terms in the (renamed) parent clauses.
\begin{postsyntax}
\syntax{
\nt{resolvent}         ::=   \nt{clause}.
\nt{unifier}           ::=   \nt{substitution}.
}
\end{postsyntax}
#}



(eval-when (load compile eval)
(defclass res+justification (just+justification keim+name)
  ((premises :initarg :parents
	     :accessor res~justification-parents 
	     :initform nil
	     :documentation "The two parent clauses that were resolved.")
   (renamings :initarg :renamings
	      :accessor res~justification-renamings
	      :initform nil
	      :documentation "The renamings for the parent clauses.")
   (positions :initarg :positions
	      :accessor res~justification-positions 
	      :initform nil
	      :documentation "The two literal positions in the parent clauses that were resolved.")
   (unifier :initarg :unifier
	    :accessor res~justification-unifier 
	    :initform nil
	    :documentation "The unifier.")
   )
  (:documentation "This is the superclass to all justifications used in resolution proofs.")))

(defmethod keim~copy ((res-just res+justification) &key (explode :all-classes) share preserve downto)
  (if (or (eq downto :all-classes) (some #'(lambda (x) (typep res-just x)) downto))
      res-just
    (make-instance (type-of res-just)
		   :name (gensym)
		   :method (just~method res-just)
		   :parents (res~justification-parents res-just)
		   :renamings (keim~copy (res~justification-renamings res-just)
					 :explode explode
					 :share share
					 :preserve preserve
					 :downto downto)
		   :positions (keim~copy (res~justification-positions res-just)
					 :explode explode
					 :share share
					 :preserve preserve
					 :downto downto)
		   :unifier (keim~copy (res~justification-unifier res-just)
				       :explode explode
				       :share share
				       :preserve preserve
				       :downto downto))))
    
(defgeneric res~step-p (object)
  (declare (edited  "11-NOV-1992 12:07")
	   (authors RICHTS)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "T iff object is a justification for resolution proofs or a node with such a justification."))
  (:method ((node node+node))
   (res~step-p (node~justification node)))
  (:method ((res-just res+justification))
   t)
  (:method ((object t))
   nil))

(defgeneric res~just-unifier (justification)
  (declare (edited  "11-NOV-1992 12:07")
	   (authors RICHTS)
	   (input   "A justification for resolution proofs or a node with such a justification.")
	   (effect  "None.")
	   (value   "The unifier of this step (a substitution)"))
  (:method ((node node+node))
   (res~just-unifier (node~justification node)))
  (:method ((res-just res+justification))
   (res~justification-unifier res-just)))

(defgeneric res~just-renamings (justification)
  (declare (edited  "11-NOV-1992 12:07")
	   (authors RICHTS)
	   (input   "A justification for resolution proofs or a node with such a justification.")
	   (effect  "None.")
	   (value   "The renamings of this step (a list of substitutions)"))
  (:method ((node node+node))
   (res~just-renamings (node~justification node)))
  (:method ((res-just res+justification))
   (res~justification-renamings res-just)))

(defmethod node~post-print-step ((just res+justification) node stream)
  (format stream "(~A " (keim~name just))
  (post~print node stream)
  (format stream " () () ")
  (post~print (res~just-unifier just) stream)
  (post~print (res~just-renamings just) stream)
  (format stream ")"))

(defun res~read-step (step env)
  (declare
   (input "a POST representation of a resolution step and an environment")
   (effect "The resolution step is parsed in using the environment"))
  (unless (and (listp step)
	       (symbolp (car step)))
    (post~error "~A does not match specification for a RESOLUTION STEP."
		step))
  (post~read-object (cdr step) env 
		    (intern (string (car step)) (find-package :keyword))))




#{\subsubsection{Initial clauses}

For the initial clause, the clauses that are returned by the
normalization, here we define a justification. All slots are {\vb
NIL}, because these clauses are the leave nodes of a proof graph.
To get the origins of the literals in a clause one can use the delta
relation.

This justification has no \post\ syntax since it is created for the
clauses in the cnf form of a \post\ resolution proof.

#}

(eval-when (load compile eval)
(defclass res+initial (res+justification)
    ()
  (:documentation 
  "Justification for initial clauses."))) 

(defmethod print-object ((initial-just res+initial) stream)
  (format stream "~A" (keim~name initial-just)))

(defun res~initial-create (&optional (name (intern (format nil "INITIAL-just-~A"
							   (incf res*justification-counter))
						   (find-package :keim))))
  (declare (authors nesmith)
	   (input "A name")
	   (value "A new initial justification."))
  (make-instance 'res+initial
		 :name name
		 :method 'initial))

(defmethod keim~copy ((just res+initial) &key (explode :all-classes) share preserve downto)
  (res~initial-create))

(defgeneric res~initial-p (obj)
  (:method ((node t))
      nil)
  (:method ((node node+node))
      (res~initial-p (node~justification node)))
  (:method ((res-initial res+initial))
      t))

#{\subsubsection{Resolution}

This justification is the representation of binary resolution on two literals.
The premises slot contains the two parent clauses and the
positions slot contains exactly two positions. Each position identifies
a literal in the corresponding clause in the premises slot. The literals
have different polarity and the unifier slot contains a substitution which
unifies the atoms of these literals.
\begin{postsyntax}
\syntax{
\nt{proof-step}  ::=  (resolution \nt{name} \nt{resolvent} \nt{parents} \nt{unifier}).
\nt{parents}     ::= (parents (\nt{name} \nt{literal-position} \nt{renaming})
			      (\nt{name} \nt{literal-position} \nt{renaming})).
}
\end{postsyntax}
Note that the \post\ representation differs from the representation of
the \keim\ objects: In \keim\ a clause with a justification in its
justification slot is a step and there is no step object which
contains a clause. Furthermore the representation of the parent
information is different. A \keim\ jsutification has a list of parent
clauses and a list of positions and not two lists with a clause and
position. Also the \keim\ justification contains the clause object and
not just its name.
#}


(eval-when (load compile eval)
(defclass res+resolution (res+justification)
    ()
  (:documentation 
  "Justification for resolution steps: UNIFIER is a unifier for the literals at POSITIONS in the clauses in PREMISES.")))

(defun res~resolution-create (parents positions renamings unifier &optional (name (intern (format nil "RES-just-~A"
												  (incf res*justification-counter))
											  (find-package :keim))))
  (declare (edited  "03-NOV-1992 15:09")
	   (authors RICHTS)
	   (input   "A list of two clauses, a list of two positions of two literals in the clauses and of two renamings"
		    "and a unifier for these literals.")
	   (effect  "None.")
	   (value   "A justification object for the resolution of the two literals."))
  (make-instance 'res+resolution
		 :parents parents
		 :positions positions
		 :unifier unifier
		 :renamings renamings
		 :name name
		 :method 'resolution))

(defgeneric res~resolution-p (object)
  (declare (edited  "11-NOV-1992 12:07")
	   (authors RICHTS)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "T iff object is a justification for a resolution step or a node with such a justification."))
  (:method ((node node+node))
   (res~resolution-p (node~justification node)))
  (:method ((res-just res+resolution))
   t)
  (:method ((object t))
   nil))


(defgeneric res~resolution-clauses (resolution-just)
  (declare (edited  "11-NOV-1992 12:07")
	   (authors RICHTS)
	   (input   "A justification for a resolution step or a node with such a justification.")
	   (effect  "None.")
	   (value   "The parents of this step (a list of clauses)"))
  (:method ((node node+node))
   (res~resolution-clauses (node~justification node)))
  (:method ((res-just res+resolution))
   (res~justification-parents res-just)))

(defgeneric res~resolution-positions (resolution-just)
  (declare (edited  "11-NOV-1992 12:07")
	   (authors RICHTS)
	   (input   "A justification for a resolution step or a node with such a justification.")
	   (effect  "None.")
	   (value   "The positions of this step (two positions identifying
literals in clauses)."))
  (:method ((node node+node))
   (res~resolution-positions (node~justification node)))
  (:method ((res-just res+resolution))
   (res~justification-positions res-just)))

(defmethod print-object ((resolution-just res+resolution) stream)
  (let ((parents (res~resolution-clauses resolution-just))
	(positions (res~resolution-positions resolution-just))
	(unifier (res~just-unifier resolution-just))
	(renamings (res~just-renamings resolution-just)))
    (format stream "Resolution of (~S ~S) renamed by ~S  and (~S ~S) renamed by ~S and unifier ~S"
	    (keim~name (first parents)) 
	    (first positions)
	    (first renamings)
	    (keim~name (second parents)) 
	    (second positions)
	    (second renamings)
	    unifier)))

(defmethod node~post-print-step ((just res+resolution) node stream)
  (format stream "~%(resolution ~A " (keim~name just))
  (keim~put node :cl=extra-variables
	    (remove-duplicates (append (data~free-variables (res~just-unifier just))
				       (mapcan #'data~free-variables (res~just-renamings just)))
			       :test #'data~equal))
  (post~print node stream)
  (keim~remprop node :cl=extra-variables)
  (format stream "(parents ")
  (mapc #'(lambda (clause pos renaming)
	    (format stream "(~A " (keim~name clause))
	    (post~print pos stream)
	    (subst~post-print-renaming renaming stream)
	    (format stream ") "))
	(res~resolution-clauses just)
	(res~resolution-positions just)
	(res~just-renamings just))
  (format stream ")")
  (post~print (res~just-unifier just) stream)
  (format stream ")"))

(defmethod post~read-object ((step list) (env env+environment)
			     (indicator (eql :resolution)))
  (let* ((name (post~read-symbol (first step) env))
	 (resolvent (cl~read (second step) env))
	 (fts (intern (first (third step)) (find-package "KEYWORD")))
	 (parents (if (eq fts :PARENTS)
		      (rest (third step))
		    (error "parent information in ~S should begin with PARENTS" step)))
	 (clause1 (cl~env-lookup (first (first parents)) env))
	 (pos1 (pos~read (second (first parents)) env))
	 (renaming1 (cl~with-context-list (list resolvent clause1)
					  env
					  (subst~read-renaming (third (first parents)) env)))
					; the variables of resolvent must be considered while reading the
					; renaming, since the resolvent is read before the unifier is read and
					; when reading the unifier the same variables as in the renamings must
					; be used. Only the codomain variables of the renaming which do not
					; occur in the resulting clause are read globally.
	 (clause2 (cl~env-lookup (first (second parents)) env))
	 (pos2 (pos~read (second (second parents)) env))
	 (renaming2 (cl~with-context-list (list resolvent clause2)
					  env
					  (subst~read-renaming (third (second parents)) env)))
					; Same as for renaming1.
	 (unifier (cl~with-context-list (list clause1 clause2 resolvent)
					env
					(subst~read (fourth step) env)))
	 )
    (setf (node~justification resolvent) 
	  (res~resolution-create (list clause1 clause2)
				 (list pos1 pos2)
				 (list renaming1 renaming2) unifier name))
    resolvent))

				
#{\subsubsection{Factoring}

This justification represents the factoring of two literals of a clause.
The premises slot contains a list of the parent clause and the
positions slot contains exactly two positions. Each position identifies
a literal in the clause in the premises slot. The literals
have the same polarity and the unifier slot contains a substitution which
unifies the atoms of these literals.
\begin{postsyntax}
\syntax{

\nt{factoring-step}    ::=   (factoring \nt{name} \nt{resolvent} \nt{factoring-parent} \nt{unifier}).
\nt{factoring-parent}  ::=   (parents (\nt{clause-name} \nt{position} \nt{position}  \nt{renaming})).
}
\end{postsyntax}
Note the differences between \post\ representation and the representation of
\keim\ objects mentioned at resolution steps.

#}

(eval-when (load compile eval)
(defclass res+factoring (res+justification)
    ()
  (:documentation 
  "Justification for factoring steps: UNIFIER is a unifier for the two literals at POSITIONS in the clause in PREMISES.")))

(defun res~factoring-create (parent positions renaming unifier &optional (name (intern (format nil "FAC-just-~A"
											       (incf res*justification-counter))
										       (find-package :keim))))
  (declare (edited  "03-NOV-1992 15:09")
	   (authors RICHTS)
	   (input   "A clause, a list of two positions of two literals in the clause"
		    "and a unifier for these.")
	   (effect  "None.")
	   (value   "A justification object for the factoring of the two literals."))
  (make-instance 'res+factoring
		 :parents (list parent)
		 :positions positions
		 :unifier unifier
		 :renamings (list renaming)
		 :name name
		 :method 'factoring))

(defgeneric res~factoring-p (object)
  (declare (edited  "11-NOV-1992 12:07")
	   (authors RICHTS)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "T iff object is a justification for a factoring step or a node with such a justification."))
  (:method ((node node+node))
   (res~factoring-p (node~justification node)))
  (:method ((fact-just res+factoring))
   t)
  (:method ((object t))
   nil))


(defgeneric res~factoring-clause (factoring-just)
  (declare (edited  "11-NOV-1992 12:07")
	   (authors RICHTS)
	   (input   "A justification for a factoring step or a node with such a justification.")
	   (effect  "None.")
	   (value   "The parent of this step (a clause)"))
  (:method ((node node+node))
   (res~factoring-clause (node~justification node)))
  (:method ((res-just res+factoring))
   (first (res~justification-parents res-just))))


(defgeneric res~factoring-positions (factoring-just)
  (declare (edited  "11-NOV-1992 12:07")
	   (authors RICHTS)
	   (input   "A justification for a factoring step or a node with such a justification.")
	   (effect  "None.")
	   (value   "The positions of this step"))
  (:method ((node node+node))
   (res~factoring-positions (node~justification node)))
  (:method ((res-just res+factoring))
   (res~justification-positions res-just)))




(defmethod print-object ((factoring-just res+factoring) stream)
  (let ((parent (res~factoring-clause factoring-just))
	(positions (res~factoring-positions factoring-just))
	(unifier (res~just-unifier factoring-just)))
    (format stream "Factoring of ~S at ~S and ~S with unifier ~S"
	    (keim~name parent) (first positions) (second positions)
	    unifier)))



(defmethod node~post-print-step ((just res+factoring) node stream)
  (format stream "~%(factoring ~A " (keim~name just))
  (keim~put node :cl=extra-variables
	    (remove-duplicates (append (data~free-variables (res~just-unifier just))
				      (mapcan #'data~free-variables (res~just-renamings just)))
			       :test #'data~equal))
  (post~print node stream)
  (keim~remprop node :cl=extra-variables)
  (format stream "(parents (~A " (keim~name (res~factoring-clause just)))
  (mapcar #'(lambda (x) (post~print x stream))
	  (res~factoring-positions just))
  (format stream ") ")
  (post~print (first (res~just-renamings just)) stream)
  (format stream " )")
  (post~print (res~just-unifier just) stream)
  (format stream ")"))



(defmethod post~read-object ((step list) (env env+environment)
			     (indicator (eql :factoring)))
  (let* ((name (post~read-symbol (first step) env))
	 (resolvent (cl~read (second step) env))
	 (fts (intern (first (third step)) (find-package "KEYWORD")))
	 (parents (if (eq fts :parents)
		      (rest (third step))
		    (error "parent information in ~S should begin with PARENTS" step)))
	 (clause (cl~env-lookup (first (first parents)) env))
	 (pos1 (pos~read (second (first parents)) env))
	 (pos2 (pos~read (third (first parents)) env))
	 (renaming (cl~with-context-list (list resolvent clause)
					 env
					 (subst~read-renaming (fourth (first parents)) env)))
	 (unifier (cl~with-context-list (list clause resolvent) env
					(subst~read (fourth step) env))))
    (setf (node~justification resolvent) 
	  (res~factoring-create clause (list pos1 pos2) renaming unifier name))
    resolvent))



#{\subsubsection{Paramodulation}

This justification represents the paramodulation of a clause with a
clause containing an equation.
The premises slot contains a the two parent clauses and the
positions slot contains exactly two positions. The first position identifies
a term in the first clause in the premises slot. The second position
identifies a literal in the second clause. This literal
has positive polarity and an atom which is an equation (that is an
term with the predefined predicate {\vb =} as function symbol). The class {\vb
res+paramodulation} provides an additional slot containing the
direction in which the equation is to the term in the first clause.
The possible values are {\vb LR} for left-to-right and {\vb RL} for right-to-left.
If the direction is {\vb LR}, the substitution in the unifier slot
unifies the term in the first clause and the first argument of the
equational atom of the literal in the second clause; if the direction
is {\vb RL} it unifies the term with the second argument.
\begin{postsyntax}
\syntax{
\nt{paramodulation-step}   ::=    (paramodulation \nt{name} \nt{resolvent}
                                     \nt{resolution-parent} \nt{equation} \nt{unifier}).
\nt{equation}   ::=   (\nt{clause-name} \nt{position} \nt{direction} \nt{renaming}).
\nt{direction}  ::=   LR | RL.
}
\end{postsyntax}
Note the differences between \post\ representation and the representation of
\keim\ objects mentioned at resolution steps.
#}

(eval-when (load compile eval)
  (defclass res+paramodulation (res+justification)
    ((direction :initarg :direction
		:accessor res~paramod-just-direction
	        :documentation "The direction in which the second literal is applied: 'LR or 'RL."))
    (:documentation 
     "Justification for paramodulation steps: UNIFIER is a unifier for the position (first POSITIONS) in (first PREMISES) ~
   and one side of the equality literal at position (second POSITIONS) in the clause (second PREMISES).")))




(defgeneric res~paramodulation-p (object)
  (declare (edited  "11-NOV-1992 12:07")
	   (authors RICHTS)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "T iff object is a justification for a paramodulation step or a node with such a justification."))
  (:method ((node node+node))
   (res~paramodulation-p (node~justification node)))
  (:method ((paramod-just res+paramodulation))
   t)
  (:method ((object t))
   nil))

(defun res~paramod-create (mother mother-position mother-renaming father father-position father-renaming direction unifier
				  &optional (name (intern (format nil "PARAMOD-just-~A" (incf res*justification-counter))
							  (find-package :keim))))
  (declare (edited  "03-NOV-1992 15:09")
	   (authors RICHTS)
	   (input   "A clause and a position of a term in a literal in this clause, and a renaming for it,"
		    "another clause, position and renaming of an equality literal in this clause and the symbol 'LR or RL,"
		    "a unifier for the term in the first clause and one side of the equation in the second clause.")
	   (effect  "None.")
	   (value   "A justification object for the paramodulation of the" 
		    "first clause with the equality literals in the second clause."))
  (make-instance 'res+paramodulation 
		 :parents (list mother father)
		 :positions (list mother-position father-position)
		 :direction direction
		 :unifier unifier
		 :renamings (list mother-renaming father-renaming)
		 :name name
		 :method 'paramodulation))

(defmethod keim~copy ((paramod-just res+paramodulation) &key (explode :all-classes) share preserve downto)
  (if (or (eq downto :all-classes) (some #'(lambda (x) (typep paramod-just x)) downto))
      paramod-just
    (res~paramod-create (res~paramod-mother paramod-just)
			(keim~copy (res~paramod-mother-position paramod-just)
				   :explode explode
				   :share share
				   :preserve preserve
				   :downto downto)
			(keim~copy (first (res~justification-renamings paramod-just))
				   :explode explode
				   :share share
				   :preserve preserve
				   :downto downto)
			(res~paramod-father paramod-just)
			(keim~copy (res~paramod-father-position paramod-just)
				   :explode explode
				   :share share
				   :preserve preserve
				   :downto downto)			
			(keim~copy (second (res~justification-renamings paramod-just))
				   :explode explode
				   :share share
				   :preserve preserve
				   :downto downto)
			(res~paramod-direction paramod-just)
			(keim~copy (res~justification-unifier paramod-just)
				   :explode explode
				   :share share
				   :preserve preserve
				   :downto downto))))


(defgeneric res~paramod-mother (paramod-just)
  (declare (edited  "11-NOV-1992 12:07")
	   (authors RICHTS)
	   (input   "A justification for a paramodulation step or a node with such a justification.")
	   (effect  "None.")
	   (value   "The clause in which the replacement was done."))
  (:method ((node node+node))
   (res~paramod-mother (node~justification node)))
  (:method ((res-just res+paramodulation))
   (first (res~justification-parents res-just))))

(defgeneric res~paramod-mother-position (paramod-just)
  (declare (edited  "11-NOV-1992 12:07")
	   (authors RICHTS)
	   (input   "A justification for a paramodulation step or a node with such a justification.")
	   (effect  "None.")
	   (value   "The position at which the replacement was done."))
  (:method ((node node+node))
   (res~paramod-mother-position (node~justification node)))
  (:method ((res-just res+paramodulation))
   (first (res~justification-positions res-just))))

(defgeneric res~paramod-father (paramod-just)
  (declare (edited  "11-NOV-1992 12:07")
	   (authors RICHTS)
	   (input   "A justification for a paramodulation step or a node with such a justification.")
	   (effect  "None.")
	   (value   "The clause containing the equality literal with which the replacement was done."))
  (:method ((node node+node))
   (res~paramod-father (node~justification node)))
  (:method ((res-just res+paramodulation))
   (second (res~justification-parents res-just))))

(defgeneric res~paramod-father-position (paramod-just)
  (declare (edited  "11-NOV-1992 12:07")
	   (authors RICHTS)
	   (input   "A justification for a paramodulation step or a node with such a justification.")
	   (effect  "None.")
	   (value   "The positions of the equality literal with which the replacement was done."))
  (:method ((node node+node))
   (res~paramod-father-position (node~justification node)))
  (:method ((res-just res+paramodulation))
   (second (res~justification-positions res-just))))

(defgeneric res~paramod-direction (paramod-just)
  (declare (edited  "11-NOV-1992 12:07")
	   (authors RICHTS)
	   (input   "A justification for a paramodulation step or a node with such a justification.")
	   (effect  "None.")
	   (value   "The direction in which the equality literal is applied: 'LR or 'RL."))
  (:method ((node node+node))
	   (res~paramod-direction (node~justification node)))
  (:method ((res-just res+paramodulation))
	   (res~paramod-just-direction res-just)))

(defmethod print-object ((paramod-just res+paramodulation) stream)
  (let ((mother (res~paramod-mother paramod-just))
	(mother-position (res~paramod-mother-position paramod-just))
	(father (res~paramod-father paramod-just))
	(father-position (res~paramod-father-position paramod-just))
	(direction (res~paramod-direction paramod-just))
	(unifier (res~just-unifier paramod-just)))
    (format stream "Paramodulation of (~S ~S) and (~S ~S ~S) both possibly renamed, with unifier ~S"
	    (keim~name mother) mother-position
	    (keim~name father) father-position direction
	    unifier)))

(defmethod node~post-print-step ((just res+paramodulation) node stream)
  (format stream "~%(paramodulation ~A " (keim~name just))
  (keim~put node :cl=extra-variables
	    (remove-duplicates (append (data~free-variables (res~just-unifier just))
				       (mapcan #'data~free-variables (res~just-renamings just)))
			       :test #'data~equal))
  (post~print node stream)
  (keim~remprop node :cl=extra-variables)
  (format stream "(parents (~A " (keim~name (res~paramod-mother just)))
  (post~print (res~paramod-mother-position just) stream)
  (subst~post-print-renaming (first (res~just-renamings just)) stream)
  (format stream ") ")
  (format stream "(~A " (keim~name (res~paramod-father just)))
  (post~print (res~paramod-father-position just) stream)
  (subst~post-print-renaming (second (res~just-renamings just)) stream)
  (post~print (res~paramod-direction just) stream)
  (format stream ")) ")
  (post~print (res~just-unifier just) stream)
  (format stream ")"))


(defmethod post~read-object ((step list) (env env+environment)
			     (indicator (eql :paramodulation)))
  (let* ((name (post~read-symbol (first step) env))
	 (resolvent (cl~read (second step) env))
	 (fts (intern (first (third step)) (find-package "KEYWORD")))
	 (parents (if (eq fts :parents)
		      (rest (third step))
		    (error "parent information in ~S should begin with PARENTS" step)))
	 (mother (cl~read (first (first parents)) env))
	 (mother-position (pos~read (second (first parents)) env))
	 (mother-renaming (cl~with-context-list (list resolvent mother)
						env
						(subst~read-renaming (third (first parents)) env)))
	 (father (cl~read (first (second parents)) env))
	 (father-position (pos~read (second (second parents)) env))
	 (father-renaming (cl~with-context-list (list resolvent father)
						env
						(subst~read-renaming (third (second parents)) env)))
	 (direction (fourth (second parents)))
	 (unifier (cl~with-context-list (list mother father resolvent) env
					(subst~read (fourth step) env))))
    (setf (node~justification resolvent) 
	  (res~paramod-create mother mother-position mother-renaming
			      father father-position father-renaming direction unifier name))
    resolvent))




#{\subsubsection{Instantiation}


This justification is the representation of an instantiation of a clause.
Though this step is not explicitly stated in most resolution calculi
it is usefull during programming for protocolling renamings etc.
The premises slot contains one parent clause and the
positions slot is {\vb NIL}. The unifier slot contains a substitution which
instantiates the parent clause.
\begin{postsyntax}
\syntax{
\nt{proof-step}  ::=  (instance  \nt{name} \nt{resolvent} (\nt{parent}) \nt{unifier}).
}
\end{postsyntax}
Note the differences between \post\ representation and the representation of
\keim\ objects mentioned at resolution steps.

#}

(eval-when (load compile eval)
(defclass res+instance (res+justification)
    ()
  (:documentation 
  "Justification for instance steps: UNIFIER is a instanciates the clause in PREMISES.")))


(defun res~instance-create (parent unifier name)
  (declare (edited  "03-NOV-1992 15:09")
	   (authors RICHTS)
	   (input   "A clause, and substitution.")
	   (effect  "None.")
	   (value   "A justification object for the instance of parent."))
  (make-instance 'res+factoring
		 :parents (list parent)
                 :positions nil
		 :unifier unifier
		 :renamings (subst~create nil nil)
		 :name name
		 :method 'instance))

(defgeneric res~instance-p (object)
  (declare (edited  "11-NOV-1992 12:07")
	   (authors RICHTS)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "T iff object is a justification for a instance step or a node with such a justification."))
  (:method ((node node+node))
   (res~instance-p (node~justification node)))
  (:method ((fact-just res+instance))
   t)
  (:method ((object t))
   nil))

(defgeneric res~instance-clause (instance-just)
  (declare (edited  "11-NOV-1992 12:07")
	   (authors RICHTS)
	   (input   "A justification for a instance step or a node with such a justification.")
	   (effect  "None.")
	   (value   "The parent of this step (a clause)"))
  (:method ((node node+node))
   (res~instance-clause (node~justification node)))
  (:method ((res-just res+instance))
   (first (res~justification-parents res-just))))

(defmethod print-object ((instance-just res+instance) stream)
  (let ((parent (res~instance-clause instance-just))
	(unifier (res~just-unifier instance-just)))
    (format stream "Instance of ~S with substitution ~S:"
	    (keim~name parent) unifier)))

(defmethod node~post-print-step ((just res+instance) node stream)
  (format stream "(instance ~A " (keim~name just))
  (post~print node stream)
  (format stream "(~A) " (keim~name (res~factoring-clause just)))
  (post~print (res~just-unifier just) stream)
  (format stream ")"))

(defmethod post~read-object ((step list) (env env+environment)
			     (indicator (eql :instance)))
  (let* ((name (post~read-symbol (first step) env))
	 (resolvent (cl~read  (second step) env))
	 (parent (cl~read (first (third step)) env))
	 (unifier (cl~with-context resolvent env
				   (cl~with-context parent env
						    (subst~read (fifth step) env)))))
    (setf (node~justification resolvent)
	  (res~instance-create parent unifier name))
    resolvent))






(defun res~literal-equal-p (lit1 lit2)
  (declare 
   (authors nesmith)
   (input "Two literals.")
   (value "T if the literals have the same polarity and their
atoms are data~equal, otherwise nil")
   (effect "None"))
  (if (lit~positive-p lit1)
      (and (lit~positive-p lit2)
	   (data~equal (lit~atom lit1) (lit~atom lit2)))
      (and (not (lit~positive-p lit2))
	   (data~equal (lit~atom lit1) (lit~atom lit2)))))


(defun res~tautology-p (cl)
  (declare
   (authors nesmith)
   (input "A clause")
   (effect "None")
   (value "T if clause is a tautology, otherwise nil"))
  ;; a clause is a tautology if it contains two literals of form
  ;; -P and +P (the atoms must match exactly, that is they are unifiable
  ;; by the empty substitution).
  (let ((pos-lits (cl~pos-literals cl))
	(neg-lits (cl~neg-literals cl)))
    (dolist (lit pos-lits nil)
      (let ((lit-atom (lit~atom lit)))
	(when (find-if #'(lambda (x)
			   (let* ((mgu (term~alpha-unify lit-atom (lit~atom x))))
			     (and mgu
				  (subst~empty-p mgu))))
		       neg-lits)
	  (return-from res~tautology-p t))))))

#| Unknown use of the follwing (could be used for a future proof-checker) VS


(defmethod just~check-p ((resolvent cl+clause) (just res+resolution))
  (declare (edited  "26-JUL-1995")
	   (authors Acsehn)
	   (input "A RESOLVENT and its justification JUST.")
	   (effect "None.")
	   (value "T, if RESOLVENT is correctly justified by JUST, i.e., when applying the"
		  "renamings to the parent clauses and removing the literals resolved on" 
		  "from the parent literals and applying the unifier, then the resulting literals"
		  "must be keim~equal to the literals in RESOLVENT."))
  (let* ((parents (res~resolution-clauses just))
	 (parent-1 (first parents))
	 (parent-2 (second parents))
	 (positions (res~resolution-positions just))
	 (position-1 (first positions))
	 (position-2 (second positions))
	 (renamings (res~just-renamings just))
	 (renaming-1 (first renamings))
	 (renaming-2 (second renamings))
	 (unifier (res~just-unifier just))
	 (literals-from-1 (remove (nth (res=position2number position-1) 
				       (cl~literals parent-1))
				  (cl~literals parent-1)))
	 (renamed-literals-from-1
	  (subst~apply renaming-1 literals-from-1))
	 (literals-from-2 (remove (nth (res=position2number position-2)
				       (cl~literals parent-2))
				  (cl~literals parent-2)))
	 (renamed-literals-from-2
	  (subst~apply renaming-2 literals-from-2))
	 (renamed-literals (append renamed-literals-from-1 renamed-literals-from-2))
	 (result-literals (subst~apply unifier renamed-literals))
	 (literals-result-clause (cl~literals resolvent))
	 )
    (every #'(lambda (literal-1 literal-2) (keim~equal literal-1 literal-2))
	   result-literals literals-result-clause)))

(defun res=position2number (position)
  (declare (edited  "07-MAR-1995")
	   (authors Acsehn)
	   (input "A POSITION."  )
	   (effect "None." )
	   (value "POSITION as a number, if it is single, otherwise an error." ))
  (let ((pos-list (pos~number-list position)))
    (if (= (length pos-list) 1)
	(first pos-list)
      (error "The position ~A cannot be converted into a single number!" position))))


(defmethod proof~check-p ((proof res+proof))
  (declare (edited  "27-JUL-1995")
	   (authors Acsehn)
	   (input "A resolution proof, PROOF." )
	   (effect "None.")
	   (value "T, if PROOF is a correctly justified resolution proof, otherwise NIL."))
  (let ((steps (proof~steps proof)))
    (every #'(lambda (clause) (just~check-p clause (node~justification clause))) steps)))

|#



;; ------------------------------------------------------------> Equality Factoring


(eval-when (load compile eval)
  (defclass res+equality-factoring (res+justification)
    ((remaining-literal-position :initarg :remaining-literal-position
				 :accessor res~equality-factoring-just-remaining-literal-position
				 :documentation "The position of the remaining literal of the equality factoring."))
    (:documentation "Justification for equality factoring steps.")))

(defgeneric res~equality-factoring-p (obj)
  (declare (edited  "25-OCT-1999")
	   (authors Ameier)
	   (input   "An object.")
	   (effect  "NOne.")
	   (value   "T if the object is a justification for a equality factoring step or a node with such a justification."))
  (:method ((node node+node))
	   (res~equality-factoring-p (node~justification node)))
  (:method ((equality-factoring-just res+equality-factoring))
	   t)
  (:method ((object t))
	   nil))

(defun res~equality-factoring-create (parent eq-side-positions remaining-literal-position unifier renaming
					     &optional (name (intern (format nil "EQFAC-just-~A" (incf res*justification-counter))
								     (find-package :keim))))
  (declare (edited  "25-OCT-1999")
	   (authors Ameier)
	   (input   "A clause and the positions of equality-sides which are unified, the position"
		    "of the remaining factored literal and the unifier of the both sides.")
	   (effect  "None.")
	   (value   "A justification object for the equality factoring of the clause on the"
		    "given positions."))
  (make-instance 'res+equality-factoring
		 :parents (list parent)
		 :renamings (list renaming)
		 :positions eq-side-positions
		 :unifier unifier
		 :remaining-literal-position remaining-literal-position
		 :name name
		 :method 'equality-factoring))
  

(defmethod  print-object ((eqfac-just res+equality-factoring) stream)
  (format stream "Equality factoring of ~A at the positions ~A with unifier ~A and remaining literal ~A"
	  (first (res~justification-parents eqfac-just))
	  (res~justification-positions eqfac-just)
	  (res~justification-unifier eqfac-just)
	  (res~equality-factoring-just-remaining-literal-position eqfac-just)))
	  

(defmethod keim~copy ((eqfac-just res+equality-factoring) &key (explode :all-classes) share preserve downto)
  (if (or (eq downto :all-classes) (some #'(lambda (x) (typep eqfac-just x)) downto))
      eqfac-just
    (res~equality-factoring-create (first (res~justification-parents eqfac-just))
				   (res~justification-positions eqfac-just)
				   (res~equality-factoring-just-remaining-literal-position eqfac-just)
				   (res~justification-unifier eqfac-just)
				   (first (res~justification-renamings eqfac-just)))))

;; Remark:
;; In the positions slot of the justification are the positions of the two equality sides which are unified (the unifier is
;; in the unifier slot). The sides are given as full positions (e.g., (0 1 1) is the left side of the literal at position 0!)
;; The first literal is as first in the list! The position in slot remaining-literal-position is the position of the literal
;; of the factorized equality literals in the original clause which remains! The remaining literal is always placed at the
;; position of the first literal.

(defmethod node~post-print-step ((just res+equality-factoring) node stream)
  (format stream "~%(equality-factoring ~A " (keim~name just))
  (keim~put node :cl=extra-variables
	    (remove-duplicates (append (data~free-variables (res~just-unifier just))
				       (mapcan #'data~free-variables (res~just-renamings just)))
			       :test #'data~equal))
  (post~print node stream)
  (keim~remprop node :cl=extra-variables)
  (format stream "(parents (~A " (keim~name (first (res~justifications-parents just))))
  (post~print (first (res~justifications-positions just)) stream)
  (post~print (second (res~justifications-positions just)) stream)
  (post~print (res~equality-factoring-just-remaining-literal-position just) stream)
  (subst~post-print-renaming (first (res~just-renamings just)) stream)
  (format stream "))")
  (post~print (res~just-unifier just) stream)
  (format stream ")"))

(defmethod post~read-object ((step list) (env env+environment)
			     (indicator (eql :equality-factoring)))
  (let* ((name (post~read-symbol (first step) env))
	 (resolvent (cl~read (second step) env))
	 (fts (intern (first (third step)) (find-package "KEYWORD")))
	 (parents (if (eq fts :parents)
		      (rest (third step))
		    (error "parent information in ~S should begin with PARENTS" step)))
	 (clause (cl~env-lookup (first (first parents)) env))
	 (pos1 (pos~read (second (first parents)) env))
	 (pos2 (pos~read (third (first parents)) env))
	 (remaining-pos (pos~read (fourth (first parents)) env))
	 (renaming (cl~with-context-list (list resolvent clause)
					 env
					 (subst~read-renaming (fifth (first parents)) env)))
	 (unifier (cl~with-context-list (list clause resolvent) env
					(subst~read (fourth step) env))))
    (setf (node~justification resolvent) 
	  (res~equality-factoring-create clause (list pos1 pos2) remaining-pos unifier renaming name))
    resolvent))


;; ------------------------------------------------------------> End Equality Factoring



#{  Auxiliary Functions #}

(defun res~separate-clauses (clause1 clause2)
  (declare (edited  "05-JUL-1995")
	   (authors Ameier)
	   (input   "Two clauses: clause1 clause2.")
	   (effect  "None.")
	   (value   "A Multiple-value of clause2' (= clause2 separated from clause1) "
		    "and the corresponding renaming of clause2."))
  (let* ((free-variables-of-clause1 (remove-duplicates (data~free-variables clause1)))
	 (free-variables-of-clause2-also-in-clause1
	  (remove-if-not #'(lambda (variable) (member variable free-variables-of-clause1 :test #'(lambda (it1 it2)
												   (equal (keim~name it1)
													  (keim~name it2)))))
			 (remove-duplicates (data~free-variables clause2)))) 
	 (new-renaming-variables-for-clause2 (mapcar #'(lambda (variable)
							 (term~variable-create (gensym "dc") (term~type variable)))
						     free-variables-of-clause2-also-in-clause1))
	 (substitution (subst~create free-variables-of-clause2-also-in-clause1 new-renaming-variables-for-clause2)))
    (values (subst~apply substitution clause2 :downto '(data+primitive))
	    substitution)))

(defun res=tautology-p (literallist)
  (declare (edited  "18-AUG-1995")
	   (authors Ameier)
	   (input   "A list of literals.")
	   (effect  "None.")
	   (value   "t ,if there is an atom ,so that +atom and -atom "
		    "is element of the literallist"))
  (if literallist
      (let ((head (car literallist))
	    (tail (rest literallist)))
	(if (member head tail :test 'res=complementary-literals-p)
	    t
	  (res=tautology-p tail)))
    nil))


(defun res=complementary-literals-p (literal1 literal2)
  (declare (edited  "01-AUG-1995")
	   (authors Ameier)
	   (input   "Two literals.")
	   (effect  "None.")
	   (value   "t if the literals have equal atoms (keim~equal) "
		    "and different polarities, nil else."))
  (if (eql (lit~positive-p literal1) (lit~positive-p literal2))
      nil
    (if (keim~equal (lit~atom literal1) (lit~atom literal2))
	t
      nil)))

(defun res=literal-position (literal clause)
  (declare (edited  "21-MAY-1996")
	   (authors Ameier)
	   (input   "A literal and a clause.")
	   (effect  "None.")
	   (value   "The position of the literal in the clause-list."))
  (let ((pos-number (position literal (cl~literals clause))))
    (pos~list-position (list pos-number))))

#| --------------------------------------- CREATING initial/bin_res/bin_fac Justifications ----------------------------- |#


(defun res=create-clauses (list-of-literallists parents list-of-positions renamings list-of-unifiers &key (kind-of-justification 'resolution))
  (declare (edited  "06-JUL-1995")
	   (authors Ameier)
	   (input   "A list of literallists ,which should become the "
		    "literallists of the new clauses, a list of one or two "
		    "parentclauses (resolution -> 2, factoring -> 1) , a list "
		    "of the corresponding positions (literals) ,a list of renamings "
		    "for the parentclauses ,a list of corresponding unifiers and "
		    "a keyword kind-of-justification : "
		    "resolution/factoring, standart is resolution."
		    "REMARK: The list-of-literallists, list-of-positions and "
		    "list-of-unifiers contain respectively corresponding "
		    "elements, for example the first literallist, the first "
		    "positions and the first unifier -> first new clause."
		    "But the parents and the renamings are forall arising "
		    "clauses the same.")
	   (effect  "None.")
	   (value   "The list of new clauses. From every literallist is made "
		    "a corresponding clause, the other informations :"
		    "parents, positions, renamings, unifiers are needed to "
		    "produce the correct justification."))
  (if (eql kind-of-justification 'resolution)
      (mapcar #'(lambda (litli positions unifier)
		  (let ((justification (res~resolution-create
					(list (first parents) (second parents))
					(list (res=literal-position (first positions) (first parents)) 
					      (res=literal-position (rest positions) (third parents)))
					renamings
					unifier
					)))
		    (cl~create litli :justification justification)))
	      list-of-literallists list-of-positions list-of-unifiers)
    (mapcar #'(lambda (litli positions unifier)
		(let ((justification (res~factoring-create
				      (car parents)
				      (list (res=literal-position (first positions) (first parents))
					    (res=literal-position (rest positions) (first parents)))
				      (subst~create nil nil)
				      unifier)))
		  (cl~create litli :justification justification)))
	    list-of-literallists list-of-positions list-of-unifiers)))

(defun res~set-initial-justification! (clauselist)
  (declare (edited  "10-JUL-1995")
	   (authors Ameier)
	   (input   "A list of clauses.")
	   (effect  "Set the justification-slot of the clauses to new produced initial-res-justifications.")
	   (value   "Nil."))
  (mapcar #'(lambda (x) (setf (node~justification x) (res~initial-create)))
	  clauselist))

#| ------------------------------------------- HANDLING BINARY RESOLUTION --------------------------------------------- |#

(defun res~binary-resolution (clause1 clause2 &key (eliminate-tautologies nil))
  (declare (edited  "06-JUL-1995")
	   (authors Ameier)
	   (input   "Two clauses ,a keyword eliminate-tautologies (t/nil) to decide whether"
		    "arising tautologies should be returned or should be eliminated directly."
		    "(default nil)")
	   (effect  "None.")
	   (value   "The list of all binary resolvent clauses of the two input clauses."))
  (if (and (cl~clause-p clause1) (cl~clause-p clause2))
      (multiple-value-bind
	  (newclause2 renamingclause2)
	  (res~separate-clauses clause1 clause2)
      	(multiple-value-bind
	    (resolventslists unifiers position-literals)
	    (res=clause-clause-resolution clause1 newclause2)
	  (if eliminate-tautologies
	      (multiple-value-bind
		  (checked-literal-list-list checked-unifier-list checked-position-list)
		  (res=delete-tautologies resolventslists unifiers position-literals)
		(res=create-clauses checked-literal-list-list (list clause1 clause2 newclause2) checked-position-list
				    (list (subst~create nil nil) renamingclause2) checked-unifier-list
				    :kind-of-justification 'resolution))
	    (res=create-clauses resolventslists (list clause1 clause2 newclause2) position-literals (list (subst~create nil nil) renamingclause2) unifiers :kind-of-justification 'resolution))))
    (post~error "Only clauses are accepted ~A ~A" clause1 clause2)))

(defun res=delete-tautologies (literal-list-list unifier-list position-literal-lists)
  (declare (edited  "21-MAY-1996")
	   (authors Ameier)
	   (input   "The list of literal-lists the according unifer list and the"
		    "according position-literals list.")
	   (effect  "None.")
	   (value   "Multiple-value:"
		    "First: The list of all literal-lists who aren't tautologies."
		    "Second: The list of the according unifiers."
		    "Third: The list of the according position-literals."))
  (let ((checked-lists (apply 'append (mapcar #'(lambda (literal-list unifier position-literals)
						  (if (res=tautology-p literal-list)
						      nil
						    (list (list literal-list unifier position-literals))))
					      literal-list-list unifier-list position-literal-lists))))
    (values (mapcar #'first checked-lists)
	    (mapcar #'second checked-lists)
	    (mapcar #'third checked-lists))))

(defun res=clause-clause-resolution (clause1 clause2)
  (declare (edited  "21-MAY-1996")
	   (authors Ameier)
	   (input   "Two clauses.")
	   (effect  "None.")
	   (value   "A multiple-value:"
		    "First: A list of all literallists arising by binary resolution"
		    "       of the two clauses."
		    "Second: The list of the according most general unifiers."
		    "Third: The list of the according pairs of parent literals"
		    "       who has been resolved."))
  (let ((lit-list1 (cl~literals clause1))
	(lit-list2 (cl~literals clause2)))
    (do* ((rest-lit-list1 lit-list1 (rest rest-lit-list1))
	  (used-lit-list1 nil)
	  (resol-lit-list-list nil)
	  (unifier-list nil)
	  (position-literal-list nil))
	((null rest-lit-list1) (values resol-lit-list-list unifier-list position-literal-list))
      (let ((head-literal1 (first rest-lit-list1)))
	(do* ((rest-lit-list2 lit-list2 (rest rest-lit-list2))
	      (used-lit-list2 nil))
	    ((null rest-lit-list2) nil)
	  (let* ((head-literal2 (first rest-lit-list2))
		 (pol1 (lit~positive-p head-literal1))
		 (pol2 (lit~positive-p head-literal2)))
	    (if (or (and (null pol1) pol2)
		    (and (null pol2) pol1))
		(let* ((mgu (term~alpha-unify head-literal1 head-literal2)))
		  (if mgu
		      (progn
			(setq resol-lit-list-list (cons ;;(subst~apply-separate mgu
							;;		      (append used-lit-list1 (rest rest-lit-list1)
							;;			      used-lit-list2 (rest rest-lit-list2))
							;;		      :downto '(data+primitive))
							(mapcar #'(lambda (literal)
							 	    (subst~apply mgu literal
										 :downto '(data+primitive)))
								(append used-lit-list1 (rest rest-lit-list1)
									used-lit-list2 (rest rest-lit-list2)))
							resol-lit-list-list))
			(setq unifier-list (cons mgu unifier-list))
			(setq position-literal-list (cons (cons head-literal1 head-literal2) position-literal-list))))))
	    (setq used-lit-list2 (append used-lit-list2 (list head-literal2)))))
	(setq used-lit-list1 (append used-lit-list1 (list head-literal1)))))))

#| -------------------------------------------- HANDLING BINARY FACTORING ---------------------------------------------- |#

(defun res~binary-factoring (clause)
  (declare (edited  "06-JUL-1995")
	   (authors Ameier)
	   (input  "A clause.")
	   (effect "None.")
	   (value  "The list of all binary factoring clauses."))
  (if (cl~clause-p clause)
      (multiple-value-bind
	  (factlists unifiers literal-positions)
	  (res=clause-factoring clause)
	(res=create-clauses factlists (list clause) literal-positions nil unifiers :kind-of-justification 'factoring))
    (post~error "~A is not a clause" clause)))   

(defun res=clause-factoring (clause)
  (declare (edited  "21-MAY-1996")
	   (authors Ameier)
	   (input   "A clauses.")
	   (effect  "None.")
	   (value   "A multiple-value:"
		    "First: A list of all literallists arising by binray factoring"
		    "       with the clause."
		    "Second: The list of the according most general unifiers."
		    "Third: The list of the according pairs of parent literals"
		    "       who has been factored."
		    "Remark that the factored-literal stand at the place of the"
		    "first of its parent-literals."))
  (let ((literals (cl~literals clause)))
    (do* ((rest-literals literals (rest rest-literals))
	  (used-literals nil)
	  (literal-lists nil)
	  (fact-clause-pairs nil)
	  (unifier-list nil))
	((null rest-literals) (values literal-lists unifier-list fact-clause-pairs))
      (let* ((head-literal (first rest-literals))
	     (head-polarity (lit~positive-p head-literal)))
	(do* ((pot-fact-literals (rest rest-literals) (rest pot-fact-literals))
	      (used-fact-literals nil))
	    ((null pot-fact-literals) nil)
	  (let* ((head-pot-literal (first pot-fact-literals))
		 (mgu (if (not (equal (lit~positive-p head-pot-literal) head-polarity))
			  nil
			(let ((mguni (term~alpha-unify head-pot-literal head-literal)))
			  mguni))))
	    (if mgu
		(progn
		  (setq literal-lists (cons ;;(subst~apply-separate mgu
					    ;;			  (append used-literals (list head-literal) used-fact-literals
					    ;;				  (rest pot-fact-literals))
					    ;;			  :downto '(data+primitive))
					    ;; 
				            (mapcar #'(lambda (literal)
							(subst~apply mgu literal
					    			     :downto '(data+primitive)))
					    	    (append used-literals (list head-literal) used-fact-literals
					    		    (rest pot-fact-literals)))
					    literal-lists))
		  (setq fact-clause-pairs (cons (cons head-literal head-pot-literal) fact-clause-pairs))
		  (setq unifier-list (cons mgu unifier-list))))
	    (setq used-fact-literals (append used-fact-literals (list head-pot-literal)))))
	(setq used-literals (append used-literals (list head-literal)))))))

#| ------------------------------------------------ Handling Paramodulation ---------------------------------------- |#

(defun res~binary-paramodulation (clause1 clause2 &key (eliminate-tautologies nil)) 
  (declare (edited  "20-FEB-1997")
	   (authors Ameier)
	   (input   "Two clauses and a keyword eliminate-tautologies (t/nil) to deceide, whether"
		    "arising tautologies should be returned or not.")
	   (effect  "None.")
	   (value   "The set of all clauses, that arise by paramodulation of a literal of one input"
		    "clause with a term of the other input clause."))
  (if (and (cl~clause-p clause1) (cl~clause-p clause2))
      (multiple-value-bind
	  (separated-clause2 renaming-clause2)
	  (res~separate-clauses clause1 clause2)
	(let* ((resulting-clauses (append (res=clause-clause-paramod clause1 (subst~create () ()) clause1
								     separated-clause2 renaming-clause2 clause2)
					  (res=clause-clause-paramod separated-clause2 renaming-clause2 clause2
								     clause1 (subst~create () ()) clause1))))
	  (if (not eliminate-tautologies)
	      resulting-clauses
	    (remove-if #'(lambda (clause)
			   (let ((literals (cl~literals clause)))
			     (res=tautology-p literals)))
		       resulting-clauses))))
    (error "The input for res~binary-paramodulation should be clauses."))) 


(defun res~paramodulation-mother-father (mother-clause father-clause &key (eliminate-tautologies nil))
  (declare (edited  "08-SEP-1998")
	   (authors Ameier)
	   (input   "Two clauses and a keyword eliminate-tautologies (t/nil) to deceide, whether"
		    "arising tautologies should be returned or not.")
	   (effect  "None.")
	   (value   "The set of all clauses, that arise by paramodulation of the two input clauses."
		    "clause1 is used as mother clause, clause2 is used as father clause."))
  (if (and (cl~clause-p mother-clause) (cl~clause-p father-clause))
      (multiple-value-bind
	  (separated-father-clause father-renaming)
	  (res~separate-clauses mother-clause father-clause)
	(let* ((resulting-clauses (res=clause-clause-paramod separated-father-clause father-renaming father-clause
							     mother-clause (subst~create () ()) mother-clause)))
	  (if (not eliminate-tautologies)
	      resulting-clauses
	    (remove-if #'(lambda (clause)
			   (let ((literals (cl~literals clause)))
			     (res=tautology-p literals)))
		       resulting-clauses))))
    (error "The input for res~paramodulation-mother-father should be clauses.")))

;; Diese Demodulation treibt im Genegensatz zur Paramodulation zwei Sachen anders:
;; 1.) Es wird immer die linke Seite durch die Rechte ersetzt !
;; 2.) Im mother term wird niemals eine Variable mit der linken Seite unifiziert !
;;     Z.B. Sei der Demodulator (+ x 0) = x
;;     Und (P Y Z) der mother term, so sind die beiden Anwendungen mit den Substitutionen Y bzw. Z -> (+ x 0)
;;     verboten !!, da dies zu einem Aufblaehen der ganzen Sache fuehrt und nicht zu einer Vereinfachung !!
;;     Das umgekehrte ist erlaubt, sei X = (+ X 0) der Demodulator
;;     dann ist es erlaubt x -> y bzw. x -> z zu machen  (allerdings ist die Gefahr, dass solche allgemeingueltigen
;;     Demodulatoren vorhanden sind verschwindend gering !!)
;;     hat man symbol Level im Term erreicht, wird daher ein matching der linken Seite auf das Symbol gemacht, kein unify !!!

(defun res~binary-demodulation (mother-clause father-clause &key (eliminate-tautologies nil))
  (declare (edited  "08-SEP-1998")
	   (authors Ameier)
	   (input   "Two clauses and a keyword eliminate-tautologies (t/nil) to deceide, whether"
		    "arising tautologies should be returned or not.")
	   (effect  "None.")
	   (value   "The set of all clauses, that arise by demodulation of the first clause as mother clause"
		    "and the second clause as father clause."))
  (if (and (cl~clause-p mother-clause) (cl~clause-p father-clause))
      (multiple-value-bind
	  (separated-father-clause father-renaming)
	  (res~separate-clauses mother-clause father-clause)
	(let* ((resulting-clauses (res=clause-clause-paramod separated-father-clause father-renaming father-clause
							     mother-clause (subst~create () ()) mother-clause
							     :demod-only 't)))
	  (if (not eliminate-tautologies)
	      resulting-clauses
	    (remove-if #'(lambda (clause)
			   (let ((literals (cl~literals clause)))
			     (res=tautology-p literals)))
		       resulting-clauses))))
    (error "The input for res~binary-demodulation should be clauses.")))   


(defun res=clause-clause-paramod (equation-clause renaming-of-eqcl father-clause
						  renamed-mother-clause renaming-of-mocl mother-clause
						  &key (demod-only nil))
  (declare (edited  "20-FEB-1997")
	   (authors Ameier)
	   (input   "The clause, that should contain equations, its renaming and its original"
		    "the clause, that should be used as mother-clause, its renaming and its original"
		    "(original = the unrenamed clause), a keyword, demod-only (default is nil).")
	   (effect  "None.")
	   (value   "The set of all resulting clauses, by paramodulation of a equation-literal"
		    "of the first input clause with a term of a literal in the second input clause."
		    "If demod-only is 't only paramods in one direction are allowed: left-side"
		    "replaced by right-side and it is not allowed to unify a mother-variable with"
		    "the left side !."))
  (multiple-value-bind
      (equation-literals positions)
      (res=get-equation-literals equation-clause)
    (apply 'append (mapcar #'(lambda (equation-literal position)
			       (let* ((equation-args (data~appl-arguments (lit~atom equation-literal)))
				      (equation-left (first equation-args))
				      (equation-right (second equation-args)))
				 (append (res=unify-equation-part equation-left equation-right
								  father-clause position renaming-of-eqcl
								  mother-clause renaming-of-mocl 'lr
								  renamed-mother-clause (pos~empty) :demod-only demod-only)
					 (if demod-only
					     nil
					   (res=unify-equation-part equation-right equation-left
								    father-clause position renaming-of-eqcl
								    mother-clause renaming-of-mocl 'rl
								    renamed-mother-clause (pos~empty) :demod-only demod-only)))))
			   equation-literals positions))))

(defgeneric res=unify-equation-part (part-to-unify new-part
						   father-clause equation-literal-position father-renaming
						   mother-clause mother-renaming direction
						   object position &key (demod-only nil))
  (declare (edited  "20-FEB-1997")
	   (authors Ameier)
	   (input   "The part of a equation to unify and the part of a equation to replace."
		    "The father clause and the position of the equation literal in it and the"
		    "renaming of the father-clause. The mother-clause and the renaming of the mother"
		    "clause and the direction, the current looked at object in the mother-clause"
		    "and its position.")
	   (effect  "None.")
	   (value   "The resulting clauses."))
  (:method (part-to-unify new-part
			  father-clause equation-literal-position father-renaming
			  mother-clause mother-renaming direction
			  (clause cl+clause) position &key (demod-only nil))
	   (do* ((rest-literals (cl~literals clause) (rest rest-literals))
		 (number 0 (+ number 1))
		 (results nil))
	       ((null rest-literals) results)
	     (let* ((head-literal (first rest-literals)))
	       (setq results (append results 
				     (res=unify-equation-part part-to-unify new-part
							      father-clause equation-literal-position father-renaming
							      mother-clause mother-renaming direction
							      head-literal (pos~add-end number position)
							      :demod-only demod-only))))))
  (:method (part-to-unify new-part
			  father-clause equation-literal-position father-renaming
			  mother-clause mother-renaming direction
			  (literal lit+literal) position &key (demod-only nil))
	   (res=unify-equation-part part-to-unify new-part
				    father-clause equation-literal-position father-renaming
				    mother-clause mother-renaming direction
				    (lit~atom literal) (pos~add-end '1 position)
				    :demod-only demod-only))
  (:method (part-to-unify new-part
			  father-clause equation-literal-position father-renaming
			  mother-clause mother-renaming direction
			  (application term+appl) position &key (demod-only nil))
	   (let* ((mgu (term~alpha-unify application part-to-unify))) 
	     (let* ((current-result-list
		     (if mgu
			 (list (res=get-new-clause new-part father-clause equation-literal-position father-renaming
						   mother-clause position mother-renaming mgu direction))
		       nil)))
	       (do* ((rest-args (data~appl-arguments application) (rest rest-args))
		     (number 1 (+ number 1))
		     (results nil))
		   ((null rest-args) (append results current-result-list))
		 (let* ((head-arg (first rest-args)))
		   (setq results (append results
					 (res=unify-equation-part part-to-unify new-part
								  father-clause equation-literal-position father-renaming
								  mother-clause mother-renaming direction
								  head-arg (pos~add-end number position)
								  :demod-only demod-only))))))))
  (:method (part-to-unify new-part
			  father-clause equation-literal-position father-renaming
			  mother-clause mother-renaming direction
			  symbol position &key (demod-only nil))
	   (let* ((mgu (if demod-only
			   (term~alpha-match part-to-unify symbol)
			 (term~alpha-unify symbol part-to-unify))))
	     (if mgu
		 (list (res=get-new-clause new-part father-clause equation-literal-position father-renaming
					   mother-clause position mother-renaming mgu direction))
	       nil))))

(defun res=get-new-clause (new-term father-clause father-position father-renaming
				    mother-clause mother-position mother-renaming
				    unifier direction)
  (declare (edited  "20-FEB-1997")
	   (authors Ameier)
	   (input   "The new part of the equation that is used, the father-clause,"
		    "father-position and father-renaming, the mother-clause, mother-position"
		    "and mother-renaming, the unifier and the direction.")
	   (effect  "None.")
	   (value   "A new clause, consisting of the literals of the mother-clause,"
		    "but the term to mother-position is replaced by the new-term, and the"
		    "literals of the father-clause, without the used equation-literal"
		    "at position father-position. At all this literal the unifier is applied."
		    "The just of the clause is a paramodulation with the corresponding"
		    "settings."))
  (let* ((father-literals (cl~literals father-clause))
	 (renamed-father-literals (subst~apply father-renaming father-literals))
	 (mother-literals (cl~literals mother-clause))
	 (renamed-mother-literals (subst~apply mother-renaming mother-literals))
	 (new-mother-literals (data~replace-at-position renamed-mother-literals mother-position new-term
							:destructive nil))
	 
	 (new-literals (mapcar #'(lambda (literal)
				   (subst~apply unifier literal
						:downto '(data+primitive)))
			       (append new-mother-literals
				       (remove (data~struct-at-position renamed-father-literals father-position)
					       renamed-father-literals))))
	 ;; first the mother literals, second the rest father literals
	 
	 (new-justification (res~paramod-create mother-clause mother-position mother-renaming
						father-clause father-position father-renaming
						direction unifier)))
    (cl~create new-literals :justification new-justification)))

(defun res=get-equation-literals (clause)
  (declare (edited  "20-FEB-1997")
	   (authors Ameier)
	   (input   "A clause.")
	   (effect  "None.")
	   (value   "Multiple-Value:"
		    "First a list of all positive equation literals of clause."
		    "Second a list of their positions in clause."))
  (do* ((rest-literals (cl~literals clause) (rest rest-literals))
	(number 0 (+ number 1))
	(equation-literals nil)
	(positions nil))
      ((null rest-literals) (values equation-literals positions))
    (let* ((head-literal (first rest-literals)))
      (if (res=equation-p head-literal)
	  (progn
	    (setq equation-literals (cons head-literal equation-literals))
	    (setq positions (cons (pos~list-position (list number)) positions)))))))

(defgeneric res=equation-p (object)
  (declare (edited  "20-FEB-1997")
	   (authors Ameier)
	   (input   "A literal or a term.")
	   (effect  "None.")
	   (value   "If the input is a literal: T if the polarity of the literal is + and"
		    "the atom of the literal is a equation. If the input is a term: t if"
		    "the term is a equation."))	  
  (:method ((literal lit+literal))
	   (res=equation-p (lit~atom literal)))
  (:method ((term term+term))
	   (if (data~appl-p term)
	       (let* ((appl-name (keim~name (data~appl-function term)))
		      (appl-name-string (if (stringp appl-name)
					    appl-name
					  (string (intern appl-name (find-package 'keim))))))
		 (if (string= appl-name-string "=")
		     't
		   nil))
	     nil)))


#| -------------------------------------------------- Handling Equality Factoring ------------------------------------------- |#


(defun res~equality-factoring (clause)
  (declare (edited  "22-OCT-1999")
	   (authors Ameier)
	   (input   "A clause.")
	   (effect  "Nil.")
	   (value   "A list of all clauses which are resulting from the input clause by an"
		    "equality factoring:"
		    "From { t1 = t2, t1 = t3, R } derive"
		    "{ ! t2 = t3, t1 = t3, R }."))

  (let* ((literals (cl~literals clause))
	 (literal-number (length literals)))
    
    (do* ((counter1 0 (+ counter1 1))
	  (back-clauses nil))
	((= counter1 (- literal-number 1))
	 back-clauses)
      
      (let* ((lit1 (data~struct-at-position literals (pos~list-position (list counter1)))))
	
	(do* ((counter2 (+ counter1 1) (+ counter2 1)))
	    ((= counter2 literal-number)
	     nil)
	  (let* ((lit2 (data~struct-at-position literals (pos~list-position (list counter2))))
		 (eqfac-clauses (res=literal-literal-equality-factoring lit1 lit2 counter1 counter2 clause)))
	    
	    (when eqfac-clauses
	      ;; falls equality factoring moeglich war, fuege neue Klauseln ein
	      (setq back-clauses (append eqfac-clauses back-clauses)))))))))


(defun res=literal-literal-equality-factoring (lit1 lit2 pos1 pos2 clause)
  (declare (edited  "22-OCT-1999")
	   (authors Ameier)
	   (input   "Two literals and the positions of the literals in the original clause and"
		    "the original calsue.")
	   (effect  "None.")
	   (value   "A list of all possible clauses resulting from equality factoring between the"
		    "two selected literals."))
  
  (if (null (and (and (lit~positive-p lit1)
		      (string= (keim~name (data~appl-function (lit~atom lit1))) "="))
		 (and (lit~positive-p lit2)
		      (string= (keim~name (data~appl-function (lit~atom lit2))) "="))))
      
      ;; literals are not both positive equaltions!
      nil
    
    ;; literlals are both positive equations! -> try to unify sides!
    (let* ((lit1-sides (data~appl-arguments (lit~atom lit1)))
	   (lit2-sides (data~appl-arguments (lit~atom lit2)))
	   (lit1-l (first lit1-sides))
	   (lit1-r (second lit1-sides))
	   (lit2-l (first lit2-sides))
	   (lit2-r (second lit2-sides))
	   (back-list nil))
      
      (let* ((mgu (term~alpha-unify lit1-l lit2-l)))
		  
	(when mgu
	  (setq back-list (append (res=make-eqfac-clauses mgu pos1 pos2 1 1 lit1-r lit2-r clause) back-list))))
      
      (let* ((mgu (term~alpha-unify lit1-l lit2-r)))

	(when mgu
	  (setq back-list (append (res=make-eqfac-clauses mgu pos1 pos2 1 2 lit1-r lit2-l clause) back-list))))
      
      (let* ((mgu (term~alpha-unify lit1-r lit2-l)))
	
	(when mgu
	  (setq back-list (append (res=make-eqfac-clauses mgu pos1 pos2 2 1 lit1-l lit2-r clause) back-list))))
      
      (let* ((mgu (term~alpha-unify lit1-r lit2-r)))
	
	(when mgu
	  (setq back-list (append (res=make-eqfac-clauses mgu pos1 pos2 2 2 lit1-l lit2-l clause) back-list))))
      
      back-list)))

(defun res=make-eqfac-clauses (unifier pos1 pos2 side1 side2 remaining-side1 remaining-side2 clause)
  (declare (edited  "22-OCT-1999")
	   (authors Ameier)
	   (input   "The unifer, the positions of two equation literals, the two numbers (1 for left side"
		    "wasunified, 2 for right side was unified), the two remaining sides,"
		    "and the original clause (Notice, the first position is always smaller as the"
		    "second, by construction from function res\~equality-factoring.).")
	   (effect  "None.")
	   (value   "The two new clauses are produced, justified by an equality factoring justification."
		    "(Two new clauses, since there are two possibilities which equation literal can"
		    " remain."))

  (let* ((positions1 (list (pos~list-position (list pos1 1 side1))
			   (pos~list-position (list pos2 1 side2))))
	 (positions2 (keim~copy positions1 :downto nil))
	 (new-inequality-literal1 (lit~literal-create (term~appl-create (env~lookup-object '= (th~env 'base))
									(list (data~copy remaining-side1 :downto '(data+primitive))
									      (data~copy remaining-side2 :downto '(data+primitive))))
						      nil))
	 (new-inequality-literal2 (data~copy new-inequality-literal1 :downto '(data+primitive)))
	 (remaining-position1 pos1) ;; welches equation literal
	 (remaining-position2 pos2) ;; jeweils uebrig bleibt
	 (remaining-literals1 (do* ((counter 0 (+ counter 1))
				    (rest-lits (cl~literals clause) (rest rest-lits))
				    (back-list nil))
				  ((null rest-lits)
				   back-list)
				(let* ((head-literal (first rest-lits)))
				  (cond ((< counter pos1)
					 (setq back-list (append back-list (list head-literal))))
					((= counter pos1)
					 ;; ertses Literal bleibt uebrig -> pos1
					 (setq back-list (append back-list (list (data~struct-at-position (cl~literals clause)
													  (pos~list-position (list pos1)))))))
					((and (> counter pos1)
					      (not (= counter pos2)))
					 (setq back-list (append back-list (list head-literal))))
					((= counter pos2)
					 ;; zweites Literal entfaellt
					 nil)
					(t ;; counter > pos2
					 (setq back-list (append back-list (list head-literal))))))))
	 (remaining-literals2 (do* ((counter 0 (+ counter 1))
				    (rest-lits (cl~literals clause) (rest rest-lits))
				    (back-list nil))
				  ((null rest-lits)
				   back-list)
				(let* ((head-literal (first rest-lits)))
				  (cond ((< counter pos1)
					 (setq back-list (append back-list (list head-literal))))
					((= counter pos1)
					 ;; zweites Literal bleibt uebrig -> pos2
					 (setq back-list (append back-list (list (data~struct-at-position (cl~literals clause)
													  (pos~list-position (list pos2)))))))
					((and (> counter pos1)
					      (not (= counter pos2)))
					 (setq back-list (append back-list (list head-literal))))
					((= counter pos2)
					 ;; zweites Literal entfaellt
					 nil)
					(t ;; counter > pos2
					 (setq back-list (append back-list (list head-literal))))))))
	 (unifier1 unifier)
	 (unifier2 (keim~copy unifier :downto '(data+primitive)))
	 (just1 (res~equality-factoring-create clause positions1 (pos~list-position (list pos1)) unifier1 (subst~create () ())))
	 (just2 (res~equality-factoring-create clause positions2 (pos~list-position (list pos2)) unifier2 (subst~create () ())))
	 (clause1 (cl~create (mapcar #'(lambda (lit)
					 (subst~apply unifier1 lit))
				     (append remaining-literals1 (list new-inequality-literal1)))
			     :justification just1))
	 (clause2 (cl~create (mapcar #'(lambda (lit)
					 (subst~apply unifier2 lit))
				     (append remaining-literals2 (list new-inequality-literal2)))
			     :justification just2)))

    (list clause1 clause2)
    ))
  
;; Zur Bemerkung: Beim Equality Factoring gelten fuer die neuen entstehenden Klauseln folgende Regeln:
;; 1.) Das neu enstehende ungleichungs literal steht ganz am Schluss!
;; 2.) Das zweite (hoehere position), weiter hinten stehende Equality Literal entfaellt!
;; 3.) Das erste (niedrigere position) bleibt erhalten!
;;     Dieses erste kann nun allerdings t1=t2 sein oder t1=t3 (d.h. es kommen jeweils immer zwei Sachen raus!)
;; 4.) Die entsthende Ungleichung enthaelt als linke Seite die uebrige Seite des ersten Lietrals und als rechte Seite
;;     die uebrige Seite des zweiten Literals!
;; 5.) Die marks l,r fuer die Seiten, die unifiziert werden, werden in die Positionen miteinberechnet
;;     1 fuer linke Seite, 2 fuer rechte Seite
;; 6.) Zusaeztlicher Eintrag in Position gibt das equations Literal an, das letztlich uebrig bleibt (aber immer vorne!)
;;
;;
;; Dies entspricht folgender Idee, wie man das ganze auf Paramodulation + Factorisierung zurueckfuehren kann:
;; 1.) Fall: l,l unifiziert + erstes equation Literal bleibt uebrig!
;;      C1:[R1,t1=t2,R2,t1=t3,R3]    C2:[x=y,x!=y]
;;               Paramodulation mit C2 als Father mit {y->t3}, und Ersetzung t3 durch x in mother C1 (RL Ersetzung)
;;      C3:[R1,t1=t2,R2,t1=x,R3,x!=t3]
;;               Factorisierung von C3 mittels {x -> t2} -> hinteres equation Literal entfaellt
;;      C4:[R1,t1=t2,R2,R3,t2!=t3]
;; 2.) Fall: l,l unifiziert + zweites equation Literal bleibt uebrig!
;;     C1:[R1,t1=t2,R2,t1=t3,R3]    C2:[x=y,x!=y]
;;               Paramodulation mit C2 als Father mit {x->t2}, und Ersetzung t2 durch y in mother C1 (LR Ersetzung)
;;     C3:[R1,t1=y,R2,t1=t3,R3,t2!=y]  
;;               Factorisierung von C3 mittels {y -> t3} -> hinteres equation Literal entfaellt
;;     C4:[R1,t1=t3,R2,R3,t2!=t3]     
;; 3.) Fall: l,r unifiziert + erstes equation Literal bleibt uebrig!
;;      C1:[R1,t1=t2,R2,t3=t1,R3]    C2:[x=y,x!=y]
;;               Paramodulation mit C2 als Father mit {y->t3}, und Ersetzung t3 durch x in mother C1 (RL Ersetzung)
;;      C3:[R1,t1=t2,R2,x=t1,R3,x!=t3]
;;               Flip von x=t1 zu t1=x in C3
;;      C4:[R1,t1=t2,R2,t1=x,R3,x!=t3]
;;               Factorisierung von C4 mittels {x -> t2} -> hinteres equation Literal entfaellt
;;      C5:[R1,t1=t2,R2,R3,t2!=t3]
;; 4.) Fall: l,r unifiziert + zweites equation Literal bleibt uebrig!
;;      C1:[R1,t1=t2,R2,t3=t1,R3]    C2:[x=y,x!=y]
;;               Paramodulation mit C2 als Father mit {x->t2}, und Ersetzung t2 durch y in mother C1 (RL Ersetzung)
;;      C3:[R1,t1=y,R2,t3=t1,R3,t2!=y]
;;               Flip von t3=t1 zu t1=t3 in C3
;;      C4:[R1,t1=y,R2,t1=t3,R3,t2!=y]
;;               Factorisierung von C4 mittels {y -> t3} -> hinteres equation Literal entfaellt
;;      C5:[R1,t1=t3,R2,R3,t2!=t3]
;;
;; Faelle 5.),6.),7.),8.) funktionieren analog


#| -------------------------------------------------- Handling demodulation ------------------------------------------- |#

;; Um ehrlich zu sein, so spontan weiss ich nicht was das hier genau treibt und worin
;; genau die Unterschiede zu obigem res~binary-paramodulation liegen,
;; jedenfalls ist das hier die Version aus dem OTTER-File, die jahrelang gute Dienste geleistet hat !!
;; Allerdings ist bei obiger Version leichter zu sagen, was genau passiert und was nicht passiert !!

(defun res~binary-demodulation-ii (mother-clause demod-clause)
  (declare (edited  "11-SEP-1996")
	   (authors Ameier)
	   (input   "Two clauses, in first should be paramodulated something, by using a"
		    "Equality literal from the second.")
	   (effect  "None.")
	   (value   "A list of all clauses who could be generated by paramodulation of"
		    "one term in clause1 by using a equation-literal of clause2."
		    "Remark: The equation literals are only used in one direction:"
		    "        left part replaced by rigth."))
  (multiple-value-bind
      (renamed-demod-clause renaming)
      (res~separate-clauses mother-clause demod-clause)
    (let* ((equation-triples (res=get-equation-partition (cl~literals renamed-demod-clause))))
      (apply 'append (mapcar #'(lambda (equation-triple)
				 (res=demod-with-literal mother-clause
							 demod-clause
							 renaming
							 (first equation-triple)
							 (second equation-triple)
							 (third equation-triple)))
			     equation-triples)))))

(defun res=demod-with-literal (mother-clause demod-clause renaming equation-literal literal-position rest-renamed-literals)
  (declare (edited  "11-SEP-1996")
	   (authors Ameier)
	   (input   "Two clauses, a renamig for the second clause, a positive equation literal"
		    "who is (back renamed) contained in the second clause,"
		    "the position of this literal in second clause"
		    "and the list of all other renamed literals of second clause in correct order.")
	   (effect  "None.")
	   (value   "A list of clauses, produced by using paramodulation of one subterm of motherclause"
		    "with the equation literal."
		    "Remark: equation is only used from left to right (left side is replaced by rigth side."))
  (let* ((equation-sides (data~appl-arguments (lit~atom equation-literal)))
	 (left-side (first equation-sides))
	 (rigth-side (second equation-sides))
	 (mother-literals (cl~literals mother-clause)))
    (do* ((mother-rest-literals mother-literals (rest mother-rest-literals))
	  (checked-literals nil)
	  (current-position 0 (+ current-position 1))
	  (return-clauses nil))
	((null mother-rest-literals) return-clauses)
      (let* ((head (first mother-rest-literals))
	     (result-literal-position-mgus (res=demod-literal head left-side rigth-side
							      (pos~list-position (list current-position 1)))))
	;; die 1 braucht man um vom literal auf das atom zu referenzieren
	(setq return-clauses
	      (append return-clauses
		      (mapcar #'(lambda (result-literal-position-mgu)
				  (let* ((result-literal (first result-literal-position-mgu))
					 (mother-position (second result-literal-position-mgu))
					 (mgu (third result-literal-position-mgu))
					 (just (res~paramod-create mother-clause mother-position (subst~create () ())
								   demod-clause literal-position renaming
								   'LR mgu (gensym))))
				    (cl~create (append
						(mapcar #'(lambda (literal)
							    (lit~literal-create
							     (subst~apply mgu (lit~atom literal))
							     (lit~positive-p literal)))
							checked-literals)
						(list result-literal)
						(mapcar #'(lambda (literal)
							    (lit~literal-create
							     (subst~apply mgu (lit~atom literal))
							     (lit~positive-p literal)))
							(append (rest mother-rest-literals) rest-renamed-literals)))
					       :justification just)))
			      result-literal-position-mgus)))
	(setq checked-literals (append checked-literals (list head)))))))


(defun res=demod-literal (mother-literal left-side rigth-side literal-position)
  (declare (edited  "11-SEP-1996")
	   (authors Ameier)
	   (input   "A literal ,two terms (representing the left and the rigth side of an"
		    "equation) and a position.")
	   (effect  "None.")
	   (value   "A list of all Triples created by replacing in a subterm of mother-literal"
		    "the left-side by the right side of the equation."
		    "Each triple consists of a new literal, the complete position of subterm"
		    "in literal who is replaced, and the according most general unifier."))
  (let* ((polarity (lit~positive-p mother-literal))
	 (result-triples (res=demod-term (lit~atom mother-literal) left-side rigth-side literal-position)))
    (mapcar #'(lambda (result-triple)
		(let* ((result-term (first result-triple))
		       (result-position (second result-triple))
		       (result-mgu (third result-triple)))
		  (list (lit~literal-create result-term polarity) result-position result-mgu)))
	    result-triples)))

(defun res=demod-term (term left-side rigth-side current-position)
  (declare (edited  "17-SEP-1996")
	   (authors Ameier)
	   (input   "A term ,and two terms representing the left side and the right side of"
		    "an equation and a position.")
	   (effect  "None.")
	   (value   "A list of all Triples created by replacing in a subterm of the term"
		    "the left-side by the right side of the equation."
		    "Each triple consists of a new literal, the complete position of subterm"
		    "in literal who is replaced, and the according most general unifier."
		    "REMARK: There is no unifiing between the left side and a variable."))
  (if (term~variable-p term)
      nil
    (let* ((mgu (term~unify term left-side))
	   (new-triple-list (if mgu
				(list (list (subst~apply mgu rigth-side) current-position mgu))
			      nil)))
      (append new-triple-list
	      (if (not (term~appl-p term))
		  nil
		(let* ((sub-terms (data~appl-arguments term))
		       (function (data~appl-function term)))
		  (do* ((rest-terms sub-terms (rest rest-terms))
			(checked-terms nil)
			(current-number 1 (+ current-number 1))
			(back-triples nil))
		      ((null rest-terms) back-triples)
		    (let* ((head-term (first rest-terms))
			   (result-triples (res=demod-term head-term left-side rigth-side (pos~add-end current-number
													 current-position))))
		      (setq back-triples
			    (append
			     (mapcar #'(lambda (result-triple)
					 (let* ((new-subterm (first result-triple))
						(position (second result-triple))
						(mgu (third result-triple))
						(new-term (term~appl-create function
									    (append 
									     (mapcar #'(lambda (subterm)
											 (subst~apply mgu subterm))
										     checked-terms)
									     (list new-subterm)
									     (mapcar #'(lambda (subterm)
											 (subst~apply mgu subterm))
										     (rest rest-terms)))
									    )))
					   (list new-term position mgu)))
				     result-triples)
			     back-triples))
		      (setq checked-terms (append checked-terms (list head-term)))))))))))

;; REMARK: ABOVE VERSION OF res=DEMOD-TERM is possibly not sound ;; ?????
;; Verglichen mit der Version von oben (auskommentiert, ein Abschnitt oben drueber) wird hier nur der Subterm in der Mother
;; unifieziert, nicht der linke Teil im Demodulator !!!!!!!

(defun res=get-equation-partition (literals)
  (declare (edited  "11-SEP-1996")
	   (authors Ameier)
	   (input   "A list of literals.")
	   (effect  "None.")
	   (value   "A list of triples:"
		    "First part of a triple: A positive equation literal from input list."
		    "Second part of a triple: The position of this literal in the input list."
		    "Third part a list of all other literals in right order."))
  (do* ((rest-literals literals (rest rest-literals))
	(checked-literals nil) 
	(current-position 0 (+ current-position 1))
	(back-triples nil))
      ((null rest-literals) back-triples)
    (let* ((head (first rest-literals)))
      (if (and (lit~positive-p head)
	       (res=equation-p head))
	  (setq back-triples
		(cons (list head
			    (pos~list-position (list current-position))
			    (append checked-literals (rest rest-literals)))
		      back-triples)))
      (setq checked-literals (append checked-literals (list head)))))) 


#| ------------------------------------------------ Hyper Resolution -------------------------------------------------- |#

;; Remark: By a NEGATIV hyper-resolution you have a clause containing at least one positive literal, this
;;         clause is named NUKLEUS, and a set of clauses that contain only negative literals named ELECTRONS.
;;         You need for every positive Literal of the nukleus an electron, so the resulting clause is again
;;         a complet negativ one and can be used as electron.
;;         The first of the parents-slot is the nukleus, than the electrons.
;;         The order of the electrons is ,that the first of the electrons is resolved with the first of the
;;         positiv literals of the nukleus, the second of the electrons with the second of the pos. literals and so on ...
;;         The positions of the resolved positiv literals in the nukleus aren't stored in the positions slot.
;;         In the positions-slot are stored the positions of the literals in the elctrons.
;;         First position is the position of the literal in the first electron who is resolved with the first positiv
;;         literal of nukleus and so on ...
;;         So the position-list is 1 lesser than the renaming and the parent list.
;;         In a POSITIV hyper-resolution things are the same, only the + and - are changed.


#| --------------------------------------------------- Classes --------------------------------------------------------- |#

(eval-when (load compile eval)
  (defclass res+hyper-resolution (res+justification)
    ())
  )

(eval-when (load compile eval)
  (defclass res+negative-hyper-resolution (res+hyper-resolution)
    ())
  )

(eval-when (load compile eval)
  (defclass res+positive-hyper-resolution (res+hyper-resolution)
    ())
  )

(defun res~hyper-resolution-create (parents positions renamings unifier
					    &optional (name (intern (format nil "Hyper-just-~A"
									    (incf res*justification-counter))
								    (find-package :keim))))
  (declare (edited  "21-MAY-1996")
	   (authors Ameier)
	   (input   "The parents-list, the positions-list, the renamings-list ,the unifier"
		    "and optional the name of a hyper-resolution-justification.")
	   (effect  "None.")
	   (value   "The created justifiation."))
  (make-instance (if (lit~positive-p (first (cl~literals (second parents))))
		     'res+positive-hyper-resolution
		   'res+negative-hyper-resolution)
		 :parents parents
		 :positions positions
		 :unifier unifier
		 :renamings renamings
		 :name name
		 :method 'hyper-resolution))

(defgeneric res~hyper-resolution-p (object)
  (declare (edited  "21-MAY-1996")
	   (authors Ameier)
	   (input   "A clause or a justification.")
	   (effect  "None.")
	   (value   "T iff object is a justification for a hyper-resolution-step "
		    "or a node with such a justification."))
  (:method ((node node+node))
	   (res~hyper-resolution-p (node~justification node)))
  (:method ((res-just res+hyper-resolution))
	   t)
  (:method ((object t))
	   nil))

(defmethod res~resolution-positions ((res-just res+hyper-resolution))
  (res~justification-positions res-just))

(defmethod res~resolution-clauses ((res-just res+hyper-resolution))
  (res~justification-parents res-just))

(defmethod print-object ((resolution-just res+hyper-resolution) stream)
  (let* ((parents (res~resolution-clauses resolution-just))
	 (positions (res~resolution-positions resolution-just))
	 (unifier (res~just-unifier resolution-just))
	 (renamings (res~just-renamings resolution-just)))
    (if (lit~positive-p (first (cl~literals (second parents))))
	(format stream "POSITIVE-Hyper-resolution of nucleus ~S renamed by ~S and electrons :"
		(car parents)
		(car renamings))
      (format stream "NEGATIVE-Hyper-resolution of nukleus ~S renamed by ~S and electrons :"
	      (car parents)
	      (car renamings)))
    (mapcar #'(lambda (x y z)
		(format stream "~%~S renamed by ~S at position ~S" x y z))
	    (rest parents) (rest renamings) positions)
    (format stream "~%and unifier ~S" unifier)))

(defmethod node~post-print-step ((just res+hyper-resolution) node stream)
  (format stream "~%(hyper-resolution ~A " (keim~name just))
  (keim~put node :cl=extra-variables
	    (remove-duplicates (append (data~free-variables (res~just-unifier just))
				       (mapcan #'data~free-variables (res~just-renamings just))) 
			       :test #'data~equal))
  (post~print node stream)
  (keim~remprop node :cl=extra-variables)
  (let* ((parents (res~resolution-clauses just))
	 (positions (res~resolution-positions just))
	 (renamings (res~just-renamings just)))
    (format stream "(nucleus (~A " (keim~name (first parents)))
    (subst~post-print-renaming (first renamings) stream)
    (format stream ")) ")
    
    (format stream "(electrons ")
    (mapc #'(lambda (clause pos renaming)
	      (format stream "(~A " (keim~name clause))
	      (post~print pos stream)
	      (subst~post-print-renaming renaming stream)
	      (format stream ") "))
	  (rest parents)
	  positions
	  (rest renamings))
    (format stream ")"))
  (post~print (res~just-unifier just) stream)
  (format stream ")"))

(defmethod post~read-object ((step list) (env env+environment)
			     (indicator (eql :hyper-resolution)))
  (let* ((name (post~read-symbol (first step) env))
	 (resolvent (cl~read (second step) env))
	 (nucleus-information (if (eq :NUCLEUS (intern (first (third step)) (find-package "KEYWORD")))
				  (second (third step))
				(error "nucleus information in ~S should begin with NUCLEUS" step)))
	 (electrons-information (if (eq :ELECTRONS (intern (first (fourth step)) (find-package "KEYWORD")))
				    (rest (fourth step))
				  (error "electrons information in ~S should begin with electrons" step)))
	 (nucleus-clause (cl~env-lookup (first nucleus-information) env))
	 (nucleus-renaming (cl~with-context-list (list nucleus-clause)
						 env
						 (subst~read-renaming (second nucleus-information) env)))
	 (electron-clauses (mapcar #'(lambda (electron)
				       (cl~env-lookup (first electron) env))
				   electrons-information))
	 (electron-positions (mapcar #'(lambda (electron)
					 (pos~read (second electron) env))
				     electrons-information))
	 (electron-renamings (mapcar #'(lambda (electron clause)
					 (cl~with-context-list (list clause)
							       env
							       (subst~read-renaming (third electron) env)))
				     electrons-information electron-clauses))
	 (unifier (cl~with-context-list (cons nucleus-clause electron-clauses)
					env
					(subst~read (fifth step) env)))
	 )
    (setf (node~justification resolvent) 
	  (res~hyper-resolution-create (cons nucleus-clause electron-clauses) 
				       electron-positions
				       (cons nucleus-renaming electron-renamings)
				       unifier
				       name))
    resolvent))

#| ----------------------------------------------- Auxiliaries ------------------------------------------------------- |#

(defun res=pure-polarity-clause-p (clause)
  (declare (edited  "21-MAY-1996")
	   (authors Ameier)
	   (input   "A clause.")
	   (effect  "None.")
	   (value   "Multiple-value:"
		    "First: t whether all literals of clause have the same polarity,"
		    "       nil otherwise."
		    "Second: if first value is true , the polarity of all literals,"
		    "        otherwise undefined."))
  (let* ((literals (cl~literals clause))
	 (pol (lit~positive-p (first literals))))
    (do* ((rest-literals (rest literals) (rest rest-literals))
	  (flag 't))
	((or (null flag) (null rest-literals)) (values flag pol))
      (let* ((head-literal (first rest-literals))
	     (head-pol (lit~positive-p head-literal)))
	(if (not (equal head-pol pol))
	    (setq flag nil))))))

(defun res~check-electrons-p (list-of-clauses)
  (declare (edited  "21-MAY-1996")
	   (authors Ameier)
	   (input   "A list of clauses to use as electrons.")
	   (effect  "None.")
	   (value   "Multiple-value:"
		    "First: t whether all literals of all clauses have same polarity,"
		    "       nil otherwise."
		    "Second: if first value is true, the polarity of all literals,"
		    "        otherwise undefined."))
  (multiple-value-bind
      (flag pol)
      (res=pure-polarity-clause-p (first list-of-clauses))
    (if flag
	(do* ((rest-clauses (rest list-of-clauses) (rest rest-clauses))
	      (flag 't))
	    ((or (null flag) (null rest-clauses)) (values flag pol))
	  (multiple-value-bind
	      (next-flag next-pol)
	      (res=pure-polarity-clause-p (first rest-clauses))
	    (if (or (not next-flag) (not (equal next-pol pol)))
		(setq flag nil))))
      (values nil nil))))

(defun res=separate-electrons (nucleus electrons)
  (declare (edited  "21-MAY-1996")
	   (authors Ameier)
	   (input   "A nucleus clause and a list of clauses (electrons).")
	   (effect  "None.")
	   (value   "Multiple-value:"
		    "First: The list of the renamed electrons."
		    "Second: The list of the renamings for the electrons, so that"
		    "        they are renamed from each other and from the nucleus."))
  (do* ((rest-electrons electrons (rest rest-electrons))
	(used-variables (remove-duplicates (data~free-variables nucleus)))
	(renamed-electrons nil)
	(renamings nil))
      ((null rest-electrons) (values renamed-electrons renamings))
    (let* ((head-electron (first rest-electrons))
	   (head-free-variables (remove-duplicates (data~free-variables head-electron)))
	   (intersection-variables (intersection head-free-variables used-variables :test 'data~equal-p))
	   (renaming (subst~create intersection-variables (mapcar #'(lambda (x)
								      (term~variable-create (gensym "dc") (term~type x)))
								  intersection-variables))))
      (setq renamed-electrons (append renamed-electrons (list (subst~apply renaming head-electron :downto '(data+primitive)))))
      (setq renamings (append renamings (list renaming)))
      (setq used-variables (append used-variables head-free-variables)))))
    
#| ---------------------------------------------- Hyper-resolution --------------------------------------------------- |#

(defun res~hyper-resolution (nucleus list-of-electrons)
  (declare (edited  "28-AUG-1995")
	   (authors Ameier)
	   (input   "A nukleus (clause) ,a list of electrons."
		    "The electrons must all be pure positive clauses"
		    "(-> positive hyperresolution) or pure negative clauses"
		    "(-> negative hyperresolution). According to this it is tried"
		    "to resolve the first of the electrons with the first of"
		    "the contra-polarity-literals of the nucleus and so on..."
		    "the electrons must be as same as the conta-polarity literals"
		    "of the nucleus.")
	   (effect  "None.")
	   (value   "The list of all hyper-resolution clauses of the nucleus with "
		    "the electrons." ))
  (multiple-value-bind
      (flag polarity)                          ;; polarity is the polarity of the literals in electrons
      (res~check-electrons-p list-of-electrons)
    (if (not flag)
	(progn (format *error-output* "Sorry, but ~A isn't geeignet for electrons, since not all literals have the same polarity."
		       list-of-electrons)
	       nil)
      (let ((nucleus-contra-literals (remove-if-not #'(lambda (literal)
							(not (equal polarity (lit~positive-p literal))))
						    (cl~literals nucleus))))
	(if (not (= (length nucleus-contra-literals) (length list-of-electrons)))
	    (progn
	      (format *error-output*  "Sorry, but ~A (Electrons) doesn't have same length as ~A (nucleus-contra-literals)."
		      list-of-electrons
		      nucleus-contra-literals)
	      nil)
	  (multiple-value-bind
	      (renamed-electrons renamings)
	      (res=separate-electrons nucleus list-of-electrons)
	    (multiple-value-bind
		(literal-list-list position-list unifier-list)
		(res=hyper-resolution nucleus renamed-electrons polarity)
	      (mapcar #'(lambda (literal-list unifier position-list)
			  (let ((just (res~hyper-resolution-create (cons nucleus list-of-electrons)   ;; parents
								   position-list                   ;; electron-positions
								   (cons (subst~create () ()) renamings) ;; renamings
								   unifier                               ;; unifier
								   )))
			    (cl~create literal-list :justification just)))
		      literal-list-list unifier-list position-list))))))))

(defun res=hyper-resolution (nucleus electrons electron-polarity)
  (let* ((nucl-rest-literals (remove-if-not #'(lambda (lit)
						(equal electron-polarity (lit~positive-p lit)))
					    (cl~literals nucleus)))
	 (nucl-resolve-literals (remove-if #'(lambda (lit)
					       (equal electron-polarity (lit~positive-p lit)))
					   (cl~literals nucleus))))
    
    (do* ((rest-nucl-res-lits nucl-resolve-literals (rest rest-nucl-res-lits))
	  (rest-electrons electrons (rest rest-electrons))
	  (literal-list-list '(nil))
	  (position-list-list '(nil))
	  (mgu-list (list (subst~create nil nil))))
	((null rest-nucl-res-lits)
	 (values (mapcar #'(lambda (literal-list mgu)
			     (append (mapcar #'(lambda (lit)
						 (subst~apply mgu lit :downto '(data+primitive)))
					     nucl-rest-literals)
				     literal-list))
			 literal-list-list mgu-list)
		 position-list-list
		 mgu-list))
      (let ((head-rest-literal (first rest-nucl-res-lits))
	    (head-electron (first rest-electrons)))
	
	;; -> resolution
	(multiple-value-bind
	    (new-literal-list-list new-mgu-list new-position-list-list)
	    (res=add-next-electron-at-list literal-list-list position-list-list mgu-list head-rest-literal head-electron)
	  (setq literal-list-list new-literal-list-list)
	  (setq mgu-list new-mgu-list)
	  (setq position-list-list new-position-list-list))))))


(defun res=add-next-electron-at-list (literal-list-list position-list-list mgu-list nucleus-literal electron)
  (do* ((current-literal-list-list literal-list-list (rest current-literal-list-list))
	(current-mgu-list mgu-list (rest mgu-list))
	(current-position-list-list position-list-list (rest position-list-list))
	(new-literal-list-list nil)
	(new-mgu-list nil)
	(new-position-list-list nil))
      ((null current-literal-list-list) (values new-literal-list-list new-mgu-list new-position-list-list))
    (let ((literal-list (first current-literal-list-list))
	  (mgu (first current-mgu-list))
	  (position-list (first current-position-list-list)))
      (multiple-value-bind
	  (get-lit-lists get-mgus get-positions)
	  (res=add-next-electron literal-list position-list mgu nucleus-literal electron)
	(setq new-literal-list-list (append new-literal-list-list get-lit-lists))
	(setq new-mgu-list (append new-mgu-list get-mgus))
	(setq new-position-list-list (append new-position-list-list get-positions))))))

(defun res=add-next-electron (current-literal-list current-position-list current-mgu nucleus-literal electron)
  (multiple-value-bind
      (add-literal-list-list add-mgu-list  add-position-list)
      (res=literal-clause-resolution (subst~apply current-mgu nucleus-literal :downto '(data+primitive)) electron)
    (if (not add-literal-list-list)
	(values nil nil nil)
      (do* ((rest-add-literals add-literal-list-list (rest rest-add-literals))
	    (rest-add-mgus add-mgu-list (rest rest-add-mgus))
	    (rest-add-positions add-position-list (rest rest-add-positions))
	    (return-literal-list-list nil)
	    (return-mgu-list nil)
	    (return-position-list))
	  ((null rest-add-literals) (values return-literal-list-list return-mgu-list return-position-list))
	(let ((add-lit-list (first rest-add-literals))
	      (add-mgu (first rest-add-mgus))
	      (add-position (first rest-add-positions)))
	  (setq return-literal-list-list (cons (append (mapcar #'(lambda (literal)
								   (subst~apply add-mgu literal :downto '(data+primitive)))
							       current-literal-list)
						       add-lit-list)
					       return-literal-list-list))
	  (setq return-mgu-list (cons (subst~compose-substitution add-mgu current-mgu)
				      return-mgu-list))
	  (setq return-position-list (cons (append current-position-list (list add-position))
					   return-position-list)))))))




(defun res=literal-clause-resolution (literal clause)
  (declare (edited  "22-MAY-1996")
	   (authors Ameier)
	   (input   "A literal and a clause.")
	   (effect  "None.")
	   (value   "A multiple-value:"
		    "First: A list of resulting literallists from clause, if literal is"
		    "       resolved with one of the literals in the clause."
		    "Second: The according list of mgu's."
		    "Third: The according list of positions of partner-literals in clause."))
  (let ((new-clause (cl~create (list literal))))
    (multiple-value-bind
	(literal-lists unifiers position-literals)
	(res=clause-clause-resolution clause new-clause)
      (values
       literal-lists
       unifiers
       (mapcar #'(lambda (position-literals-pair)
		   (res=literal-position (first position-literals-pair) clause))
	       position-literals)))))

;; This is the modul for handling UR-resolution (unit-resulting resolution)
;; The non-unit-clause in resolution is named nucleus, the start unit-clauses are named electrons.
;; In the justification holds:
;; parent-slot: list ,nucleus + electrons, and the electrons in the rigth order, that means first electron deletes
;;              the first of the nucleus-literals who must be deleted and so on ...
;; positions-slot: list ,the position of the nucleus-literal who represents the unit-result
;; renamings-slot: list ,renamings for nucleus and electrons according to the parent-slot

#| ------------------------------------------------------- CLASSES -------------------------------------------------- |#

(eval-when (load compile eval)
  (defclass res+ur-resolution (res+justification)
    ())
)
(defun res~ur-resolution-create (parents positions renamings unifier
					 &optional (name (intern (format nil "UR-just-~A"
									 (incf res*justification-counter))
								 (find-package :keim))))
  (declare (edited  "21-MAY-1996")
	   (authors Ameier)
	   (input   "The parents-list, the positions-list, the renamings-list ,the unifier"
		    "and optional the name for a ur-resolution-justification.")
	   (effect  "None.")
	   (value   "The created justifiation."))
  (make-instance 'res+ur-resolution
		 :parents parents
		 :positions positions
		 :unifier unifier
		 :renamings renamings
		 :name name
		 :method 'ur-resolution))

(defgeneric res~ur-resolution-p (object)
  (declare (edited  "21-MAY-1996")
	   (authors Ameier)
	   (input   "A clause or a justification.")
	   (effect  "None.")
	   (value   "T iff object is a justification for a ur-resolution-step "
		    "or a node with such a justification."))
  (:method ((node node+node))
	   (res~ur-resolution-p (node~justification node)))
  (:method ((res-just res+ur-resolution))
	   t)
  (:method ((object t))
	   nil))

(defmethod res~resolution-positions ((res-just res+ur-resolution))
  (res~justification-positions res-just))

(defmethod res~resolution-clauses ((res-just res+ur-resolution))
  (res~justification-parents res-just))

(defmethod print-object ((resolution-just res+ur-resolution) stream)
  (let* ((parents (res~resolution-clauses resolution-just))
	 (positions (res~resolution-positions resolution-just))
	 (unifier (res~just-unifier resolution-just))
	 (renamings (res~just-renamings resolution-just)))
    (format stream "UR-resolution of nucleus ~S renamed by ~S and electrons :"
	    (car parents)
	    (car renamings))
    (mapcar #'(lambda (x y)
		(format stream "~%~S renamed by ~S" x y))
	    (rest parents) (rest renamings))
    (format stream "~%unit-result from nucleus-position ~A" (first positions))
    (format stream "~%and unifier ~S" unifier)))

(defmethod node~post-print-step ((just res+ur-resolution) node stream)
  (format stream "~%(ur-resolution ~A " (keim~name just))
  (keim~put node :cl=extra-variables
	    (remove-duplicates (append (data~free-variables (res~just-unifier just))
				       (mapcan #'data~free-variables (res~just-renamings just)))
			       :test #'data~equal))
  (post~print node stream)
  (keim~remprop node :cl=extra-variables)
  (let* ((parents (res~resolution-clauses just))
	 (positions (res~resolution-positions just))
	 (renamings (res~just-renamings just)))
    (format stream "(non-unit (~A " (keim~name (first parents)))
    (post~print (first positions) stream)
    (subst~post-print-renaming (first renamings) stream)
    (format stream ")) ")
    
    (format stream "(units ")
    (mapc #'(lambda (clause renaming)
	      (format stream "(~A " (keim~name clause))
	      (subst~post-print-renaming renaming stream)
	      (format stream ") "))
	  (rest parents)
	  (rest renamings))
    (format stream ")"))
  (post~print (res~just-unifier just) stream)
  (format stream ")"))

(defmethod post~read-object ((step list) (env env+environment)
			     (indicator (eql :ur-resolution)))
  (let* ((name (post~read-symbol (first step) env))
	 (resolvent (cl~read (second step) env))
	 (non-unit-information (if (eq :NON-UNIT (intern (first (third step)) (find-package "KEYWORD")))
				   (second (third step))
				 (error "Non-unit clause information in ~S should begin with NON-UNIT" step)))
	 (units-information (if (eq :UNITS (intern (first (fourth step)) (find-package "KEYWORD")))
				(rest (fourth step))
			      (error "Unit clause information in ~S should begin with UNITS" step)))
	 (non-unit-clause (cl~env-lookup (first non-unit-information) env))
	 (non-unit-position (pos~read (second non-unit-information) env))
	 (non-unit-renaming (cl~with-context-list (list non-unit-clause)
						  env
						  (subst~read-renaming (third non-unit-information) env)))
	 (unit-clauses (mapcar #'(lambda (unit)
				   (cl~env-lookup (first unit) env))
			       units-information))
	 (unit-renamings (mapcar #'(lambda (unit clause)
				     (cl~with-context-list (list clause)
							   env
							   (subst~read-renaming (second unit) env)))
				 units-information unit-clauses))
	 (unifier (cl~with-context-list (cons non-unit-clause unit-clauses)
					env
					(subst~read (fifth step) env)))
	 )
    (setf (node~justification resolvent) 
	  (res~ur-resolution-create (cons non-unit-clause unit-clauses) 
				    (list non-unit-position)
				    (cons non-unit-renaming unit-renamings)
				    unifier
				    name))
    resolvent))



#| ------------------------------------------------- AUXILIARIES ------------------------------------------------------- |#

(defun res=incf-position (position)
  (declare (edited  "23-MAY-1996")
	   (authors Ameier)
	   (input   "A position.")
	   (effect  "None.")
	   (value   "A position with increased first number by 1."))
  (pos~list-position (list (+ 1 (first (pos~number-list position))))))

#| ------------------------------------------------ UR-RESOLUTION ------------------------------------------------------ |#


#|(defun res~ur-resolution (nucleus electrons unit-result-clause) 
  (declare (edited  "23-MAY-1996")
	   (authors Ameier)
	   (input   "A nucleus, electrons and the unit-result to produce."
		    "The electrons must be in right order to use.")
	   (effect  "None.")
	   (value   "A list of all UR-resolutions with unit-clauses, equal till"
		    "renaming to unit-result."))
  (let ((nucleus-literals (cl~literals nucleus))
	(result-literal (first (cl~literals unit-result-clause))))
    (if (not (= (- (length nucleus-literals) 1) (length electrons)))
	(progn
	  (format *error-output* "Sorry, but ~A (electrons) has to be exactly one less than literals in nucleus ~A"
		  electrons nucleus)
	  nil)
      (multiple-value-bind
	  (possible-result-literals other-literals-list-list possible-result-positions)
	  (res=get-possible-resulting-literals-and-positions nucleus-literals result-literal)
	(multiple-value-bind
	    (renamed-electrons renamings)
	    (res=separate-electrons nucleus electrons)
	  (multiple-value-bind
	      (accepted-flags mgus)
	      (res=ur-resolution other-literals-list-list renamed-electrons)
	    (let ((parents (cons nucleus electrons))
		  (renamings (cons (subst~create () ()) renamings)))
	      (do* ((rest-flags accepted-flags (rest rest-flags))
		    (rest-pos-literals possible-result-literals (rest rest-pos-literals))
		    (rest-pos-positions possible-result-positions (rest rest-pos-positions))
		    (rest-mgus mgus (rest rest-mgus))
		    (produced-unit-results nil))
		  ((null rest-flags) produced-unit-results)
		(if (first rest-flags)
		    (let ((result-clause (cl~create (list (subst~apply (first rest-mgus) (first rest-pos-literals))))))
		      (if (res=clauses-equal-till-renaming-p result-clause unit-result-clause)
			  (let ((justification (res~ur-resolution-create parents
									 (list (first rest-pos-positions))
									 renamings
									 (first rest-mgus))))
			    (setf (node~justification result-clause) justification)
			    (setq produced-unit-results (cons result-clause produced-unit-results))))))))))))))|#

(defun res~ur-resolution (nucleus electrons) 
  (declare (edited  "23-MAY-1996")
	   (authors Ameier)
	   (input   "A nucleus, electrons."
		    "The electrons must be in right order to use and exactly one"
		    "less than the nucleus has literals.")
	   (effect  "None.")
	   (value   "A list of all UR-resolutions."))
  (let ((nucleus-literals (cl~literals nucleus)))
    (if (not (= (- (length nucleus-literals) 1) (length electrons)))
	(progn
	  (format *error-output* "Sorry, but ~A (electrons) has to be exactly one less than literals in nucleus ~A"
		  electrons nucleus)
	  nil)
      (multiple-value-bind
	  (possible-result-literals other-literals-list-list possible-result-positions)
	  (res=get-possible-resulting-literals-and-positions nucleus-literals)
	(multiple-value-bind
	    (renamed-electrons renamings)
	    (res=separate-electrons nucleus electrons)
	  (multiple-value-bind
	      (accepted-flags mgus)
	      (res=ur-resolution other-literals-list-list renamed-electrons)
	    (let ((parents (cons nucleus electrons))
		  (renamings (cons (subst~create () ()) renamings)))
	      (do* ((rest-flags accepted-flags (rest rest-flags))
		    (rest-pos-literals possible-result-literals (rest rest-pos-literals))
		    (rest-pos-positions possible-result-positions (rest rest-pos-positions))
		    (rest-mgus mgus (rest rest-mgus))
		    (produced-unit-results nil))
		  ((null rest-flags) produced-unit-results)
		(if (first rest-flags)
		    (let ((result-clause (cl~create (list (subst~apply (first rest-mgus) (first rest-pos-literals)
								       :downto '(data+primitive)))))
			  (justification (res~ur-resolution-create parents
								   (list (first rest-pos-positions))
								   renamings
								   (first rest-mgus))))
		      (setf (node~justification result-clause) justification)
		      (setq produced-unit-results (cons result-clause produced-unit-results))))))))))))


(defun res=ur-resolution (literal-list-list electrons)
  (do* ((rest-literal-list-list literal-list-list (rest rest-literal-list-list))
	(return-flags nil)
	(return-mgus nil))
      ((null rest-literal-list-list) (values return-flags return-mgus))
    (let ((head-literal-list (first rest-literal-list-list)))
      (do* ((rest-literal-list head-literal-list (rest rest-literal-list))
	    (rest-electrons electrons (rest rest-electrons))
	    (current-mgu (subst~create () ()))
	    (current-flag t))
	  ((or (null current-flag) (null rest-literal-list)) (progn
							       (setq return-flags (append return-flags (list current-flag)))
							       (setq return-mgus (append return-mgus (list  current-mgu)))))
	(let ((current-literal (subst~apply current-mgu (first rest-literal-list)
					    :downto '(data+primitive)))
	      (electron-literal (subst~apply current-mgu (first (cl~literals (first rest-electrons)))
					     :downto '(data+primitive)))) 
	  (if (not (equal (lit~positive-p current-literal) (lit~positive-p electron-literal)))
	      (let* ((unifier (term~alpha-unify (lit~atom current-literal) (lit~atom electron-literal))))
		(if unifier
		    (setq current-mgu (subst~compose-substitution unifier current-mgu))
		  (setq current-flag nil)))
	    (setq current-flag nil)))))))

#|(defun res=get-possible-resulting-literals-and-positions (nucleus-literals result-literal)
  (declare (edited  "23-MAY-1996")
	   (authors Ameier)
	   (input   "The list of nucleus-literals and the unit-result to produce,"
		    "by UR-resolution.")
	   (effect  "None.")
	   (value   "Multiple-value-bind:"
		    "First: The list of all literals in nucleus, who are unifiable"
		    "       and of the same polarity as unit-result. This are the"
		    "       possible literals to produce this unit-result."
		    "Second: The according list of all other literals in right order."
		    "Third: The list of the according positions in nucleus."))
  (let ((result-pol (lit~positive-p result-literal))
	(result-atom (lit~atom result-literal)))
    (do* ((rest-literals nucleus-literals (rest rest-literals))
	  (used-literals nil)
	  (position (pos~list-position '(0)) (res=incf-position position))
	  (pos-literals nil)
	  (other-literals-list-list nil)
	  (pos-positions nil))
	((null rest-literals) (values pos-literals other-literals-list-list pos-positions))
      (let* ((head-literal (first rest-literals))
	     (sep-head-literal (res~separate-literals result-literal head-literal)))
	(if (and (equal (lit~positive-p sep-head-literal) result-pol)
		 (term~alpha-unify (lit~atom sep-head-literal) result-atom))
	    (progn
	      (setq pos-literals (cons head-literal pos-literals))
	      (setq other-literals-list-list (cons (append used-literals (rest rest-literals)) other-literals-list-list))
	      (setq pos-positions (cons position pos-positions))))
	(setq used-literals (append used-literals (list head-literal)))))))|#

(defun res=get-possible-resulting-literals-and-positions (nucleus-literals)
  (declare (edited  "23-MAY-1996")
	   (authors Ameier)
	   (input   "The list of nucleus-literals.")
	   (effect  "None.")
	   (value   "Multiple-value-bind:"
		    "First: The list of all literals in nucleus."
		    "Second: The according list of all other literals in right order."
		    "Third: The list of the according positions in nucleus."))
  (do* ((rest-literals nucleus-literals (rest rest-literals))
	(position-number 0 (+ position-number 1))
	(position-list nil)
	(other-literals-list-list nil))
      ((null rest-literals) (values nucleus-literals
				    other-literals-list-list
				    position-list))
    (let* ((head-literal (first rest-literals)))
      (setq position-list (append position-list (list (pos~list-position (list position-number)))))
      (setq other-literals-list-list (append other-literals-list-list (list (remove head-literal nucleus-literals)))))))

(defun res~separate-literals (literal1 literal2)
  (let* ((vars-in-literal2 (remove-duplicates (data~free-variables literal2)))
	 (double-vars (intersection vars-in-literal2 (remove-duplicates (data~free-variables literal1)) :test 'data~equal-p))
	 (renaming-for-literal2 (subst~create double-vars (mapcar #'(lambda (var)
								      (term~variable-create (gensym "dc") (term~type var)))
								  double-vars))))
    (values (subst~apply renaming-for-literal2 literal2 :downto '(data+primitive)) renaming-for-literal2)))

#| ------------------------------------------------------ flip justification ------------------------------------ |#

(eval-when (load compile eval)
  (defclass res+flip (res+justification)
    ()))

;; in parents stands the parent-clause
;; in positions the position of the literal to flip
;; in renaming and unifier {} {}

(defun res~flip-create (parent position &optional (name (intern (format nil "FLIP-just-~A"
									(incf res*justification-counter))
								(find-package :keim))))
  (make-instance 'res+flip
		 :parents (list parent)
		 :positions (list position)
		 :unifier (subst~create () ())
		 :renamings (list (subst~create () ()))
		 :name name
		 :method 'flip))

(defmethod print-object ((resolution-just res+flip) stream)
  (format stream "FLIP of ~A at position ~A"
	  (first (res~justification-parents resolution-just))
	  (first (res~justification-positions resolution-just))))

(defgeneric res~flip-p (object)
  (:method ((object cl+clause))
	   (res~flip-p (node~justification object)))
  (:method (object)
	   (typep object 'res+flip)))

(defmethod res~resolution-positions ((res-just res+flip))
  (res~justification-positions res-just))

(defmethod res~resolution-clauses ((res-just res+flip))
  (res~justification-parents res-just))

(defmethod node~post-print-step ((just res+flip) node stream)
  (format stream "~%(flip ~A " (keim~name just))
  (keim~put node :cl=extra-variables
	    (remove-duplicates (append (data~free-variables (res~just-unifier just))
				       (mapcan #'data~free-variables (res~just-renamings just)))
			       :test #'data~equal))
  (post~print node stream)
  (keim~remprop node :cl=extra-variables)
  (let* ((parents (res~resolution-clauses just))
	 (positions (res~resolution-positions just)))
    (format stream "(parents (~A " (keim~name (first parents)))
    (post~print (first positions) stream)
    (format stream ")) ")
    (format stream ")")))

(defmethod post~read-object ((step list) (env env+environment)
			     (indicator (eql :flip)))
  (let* ((name (post~read-symbol (first step) env))
	 (resolvent (cl~read (second step) env))
	 (flip-information (if (eq :PARENTS (intern (first (third step)) (find-package "KEYWORD")))
			       (second (third step))
			     (error "Parents information in ~S should begin with PARENTS" step)))
	 (parent-clause (cl~env-lookup (first flip-information) env))
	 (position (pos~read (second flip-information) env)))
    (setf (node~justification resolvent) 
	  (res~flip-create parent-clause
			   position
			   name))
    resolvent))

#| --------------------------------------------------- Reflex Justification ---------------------------------------- |#

(eval-when (load compile eval)
  (defclass res+reflex (res+justification)
    ()))

(defun res~reflex-create (&optional (name (intern (format nil "REFLEX-just-~A"
							  (incf res*justification-counter))
						  (find-package :keim))))
  (make-instance 'res+reflex
		 :parents nil
		 :positions nil
		 :unifier (subst~create () ())
		 :renamings nil
		 :name name
		 :method 'reflex))

(defgeneric res~reflex-p (object)
  (:method ((object cl+clause))
	   (res~reflex-p (node~justification object)))
  (:method (object)
	   (typep object 'res+reflex)))

(defmethod print-object ((resolution-just res+reflex) stream)
  (format stream "REFLEXIVITY-Clause"))

(defmethod res~resolution-positions ((res-just res+reflex))
  (res~justification-positions res-just))

(defmethod res~resolution-clauses ((res-just res+reflex))
  (res~justification-parents res-just))

(defmethod node~post-print-step ((just res+reflex) node stream)
  (format stream "~%(reflex ~A " (keim~name just))
  (keim~put node :cl=extra-variables
	    (remove-duplicates (append (data~free-variables (res~just-unifier just))
				       (mapcan #'data~free-variables (res~just-renamings just)))
			       :test #'data~equal))
  (post~print node stream)
  (keim~remprop node :cl=extra-variables)
  (format stream ")"))

(defmethod post~read-object ((step list) (env env+environment)
			     (indicator (eql :reflex)))
  (let* ((name (post~read-symbol (first step) env))
	 (resolvent (cl~read (second step) env)))
    (setf (node~justification resolvent) 
	  (res~reflex-create name))
    resolvent))

#| --------------------------------------------------- |#

(defun res=clauses-equal-till-renaming-p (clause1 clause2)
  (declare (edited  "17-SEP-1996")
	   (authors Ameier)
	   (input   "Two clauses.")
	   (effect  "None.")
	   (value   "Multiple-value-bind:"
		    "First: T if the two clauses are equal till renaming, that means"
		    "       there exists a renaming, that if applied on a clause will make the"
		    "       clauses equal. otherwise nil."
		    "Second: If first is nil,nil too. Otherwise a list of variable pairs"
		    "        that are representing are renaming, to make clause1 equal"
		    "        clause2."))
  (if (= (length (cl~literals clause1)) (length (cl~literals clause2)))
      (multiple-value-bind
	  (renamed-clause2 renaming)
	  (res~separate-clauses clause1 clause2)
	(declare (ignore renaming))
	(do ((rest-literals1 (cl~literals clause1) (rest rest-literals1))
	     (rest-literals2 (cl~literals renamed-clause2) (rest rest-literals2))
	     (flag 't)
	     (var-matching-list nil))
	    ((or (null flag) (null rest-literals1)) (values flag var-matching-list))
	  (multiple-value-bind
	      (literals-equal-flag new-var-matchings) 
	      (res=literals-equal-till-renaming-p (first rest-literals1) (first rest-literals2))
	    (if (null literals-equal-flag)
		(setq flag nil)
	      (multiple-value-bind
		  (matching-union-flag union-var-matching-list)
		  (res=matching-union var-matching-list new-var-matchings)
		(if (null matching-union-flag)
		    (setq flag nil)
		  (setq var-matching-list union-var-matching-list)))))))
    (values nil nil)))

;; matching-list consists of pairs var1/var2 from vars in clause1 and clause2 at the same position

(defun res=matching-union (matching1 matching2)
  (do ((rest-add-matching matching2 (rest rest-add-matching))
       (union-matching matching1)
       (flag 't))
      ((or (null flag) (null rest-add-matching)) (values flag union-matching))
    (let* ((current-pair (first rest-add-matching))
	   (conter-pair-of-var1 (first (member (first current-pair) matching1 :test #'(lambda (var pair)
											(keim~equal var (first pair))))))
	   (conter-pair-of-var2 (first (member (second current-pair) matching1 :test #'(lambda (var pair)
											 (keim~equal var (second pair)))))))
      (if (or conter-pair-of-var1 conter-pair-of-var2)
	  (if (not (and (keim~equal current-pair conter-pair-of-var1) (keim~equal current-pair conter-pair-of-var2)))
	      (setq flag nil))
	(setq union-matching (cons current-pair union-matching))))))

(defun res=literals-equal-till-renaming-p (literal1 literal2)
  (declare (edited  "17-SEP-1996")
	   (authors Ameier)
	   (input   "Two literals.")
	   (effect  "None.")
	   (value   "Multiple-value-bind:"
		    "First: T if the two literals are equal till renaming, that means"
		    "       there exists a renaming, that if applied on one literal will make the"
		    "       literals equal. otherwise nil."
		    "Second: If first is nil,nil too. Otherwise a list of variable pairs"
		    "        that are representing are renaming, to make literal1 equal"
		    "        literal2."))
  (if (equal (lit~positive-p literal1) (lit~positive-p literal2))
      (res=terms-equal-till-renaming-p (lit~atom literal1) (lit~atom literal2))
    (values nil nil)))

(defgeneric res=terms-equal-till-renaming-p (term1 term2)
  (declare (edited  "17-SEP-1996")
	   (authors Ameier)
	   (input   "Two terms.")
	   (effect  "None.")
	   (value   "Multiple-value-bind:"
		    "First: T if the two terms are equal till renaming, that means"
		    "       there exists a renaming, that if applied on a term will make the"
		    "       terms equal. otherwise nil."
		    "Second: If first is nil,nil too. Otherwise a list of variable pairs"
		    "        that are representing are renaming, to make term1 equal"
		    "        term2."))
  (:method ((term1 term+appl) (term2 term+appl))
	   (if (keim~equal (data~appl-function term1) (data~appl-function term2))
	       (do ((rest-terms1 (data~appl-arguments term1) (rest rest-terms1))
		    (rest-terms2 (data~appl-arguments term2) (rest rest-terms2))
		    (flag 't)
		    (var-matching-list nil))
		   ((or (null flag) (null rest-terms1)) (values flag var-matching-list))
		 (multiple-value-bind
		     (terms-equal-flag new-var-matchings) 
		     (res=terms-equal-till-renaming-p (first rest-terms1) (first rest-terms2))
		   (if (null terms-equal-flag)
		       (setq flag nil)
		     (multiple-value-bind
			 (matching-union-flag union-var-matching-list)
			 (res=matching-union var-matching-list new-var-matchings)
		       (if (null matching-union-flag)
			   (setq flag nil)
			 (setq var-matching-list union-var-matching-list))))))
	     (values nil nil)))
  (:method ((term1 term+variable) (term2 term+variable))
	   (values 't (list (list term1 term2))))
  (:method ((term1 term+constant) (term2 term+constant))
	   (if (keim~equal term1 term2)
	       (values 't nil)
	     (values nil nil)))
  (:method (term1 term2)
	   (declare (ignore term1 term2))
	   (values nil nil)))

#| ----------------- res-proof-checker -------------------- |#

(defun res~check-resolution-proof (res-proof)
  (let* ((empty-clause (res~proof-empty-clause res-proof)))
    (res=check-steps-recursive empty-clause)
    (mapcar #'(lambda (clause)
		(keim~remprop clause 'rec-check))
	    (res~proof-clauses res-proof))
    (mapcar #'(lambda (pair)
		(res=check-delta-pair pair))
	    (delta~relation-pairs (res~proof-delta-relation res-proof)))
    (let* ((clauses-in-delta-pairs (remove-duplicates (mapcar #'delta~delta-clause
							      (delta~relation-pairs (res~proof-delta-relation res-proof)))))
	   (initial-clauses-in-res-proof (res~proof-initial-clauses res-proof))
	   (initial-clauses-used-in-res-proof (res=get-initial-clauses res-proof))
	   (dummy-delta-relation (delta~create-relation))
	   (dummy-environment (env~create))
	   (clauses-from-cnf-of-formulas
	    (apply 'append
		   (cons (hocnf=normalize-named-formula (res~proof-conclusion res-proof)
							dummy-environment
							:delta-relation dummy-delta-relation
							:pol nil
							:tautology-elim nil)
			 (mapcar #'(lambda (assumption)
				     (hocnf=normalize-named-formula assumption
								    dummy-environment
								    :delta-relation dummy-delta-relation
								    :pol t
								    :tautology-elim nil))
				 (res~proof-assumptions res-proof))))))
      (res=check-original-delta-relation-new-delta-relation (res~proof-delta-relation res-proof) dummy-delta-relation)
      (res=check-clause-sets initial-clauses-in-res-proof
			     initial-clauses-used-in-res-proof
			     clauses-in-delta-pairs))
    
    ))

(defun res=check-clause-sets (clauses-in-initial used-initial-clauses clauses-in-delta)
  (mapcar #'(lambda (clause-in-initial)
	      (when (null (find clause-in-initial clauses-in-delta))
		(format t "~%~%INITIAL Clause ~A is not in delta-relation." clause-in-initial)))
	  clauses-in-initial)
  (mapcar #'(lambda (used-initial-clause)
	      (when (null (find used-initial-clause clauses-in-initial))
		(format t "~%~%FOLLOWING CLAUSE IS USED AS INITIAL BUT IS NOT IN THE STEPS SLOT: ~A." used-initial-clause)))
	  used-initial-clauses ))

(defun res=get-initial-clauses (res-proof)
  (remove-duplicates
   (res=get-initial-clauses-recursive (res~proof-empty-clause res-proof) (node~justification (res~proof-empty-clause res-proof)))))

(defgeneric res=get-initial-clauses-recursive (clause just)
  (:method (clause (just res+initial))
	   (list clause))
  (:method (clause just)
	   (apply 'append (mapcar #'(lambda (parent)
				      (res=get-initial-clauses-recursive parent (node~justification parent)))
				  (res~justification-parents just)))))
				  
  
	      
(defun res=check-original-delta-relation-new-delta-relation (orig-delta new-delta)
  (let* ((orig-pairs (delta~relation-pairs orig-delta))
	 (new-pairs (delta~relation-pairs new-delta)))
    (mapcar #'(lambda (orig-pair)
		(when (null (find orig-pair new-pairs
				  :test #'(lambda (o-pair n-pair)
					    (and (data~equal (delta~delta-formula o-pair)
							     (delta~delta-formula n-pair))
						 (keim~equal (delta~delta-position-in-formula o-pair)
							     (delta~delta-position-in-formula n-pair))
						 (keim~equal (delta~delta-position-in-clause o-pair)
							     (delta~delta-position-in-clause n-pair))
						 (res=clauses-equal-till-renaming-p (delta~delta-clause o-pair)
										    (delta~delta-clause n-pair))))))
		  (format t "~%~%DELTA-RELATION-PAIR not found in CNF OF FORMULAS: ~A." orig-pair)))
	    orig-pairs)))
	 
  
(defun res=check-delta-pair (pair)
  (let* ((formula (delta~delta-formula pair))
	 (formula-position (delta~delta-position-in-formula pair))
	 (clause (delta~delta-clause pair))
	 (lit-position (delta~delta-position-in-clause pair))
	 (term-in-form (data~struct-at-position formula formula-position))
	 (term-in-lit (lit~atom (data~struct-at-position clause lit-position)))
	 (eq-flag (res=terms-equal-till-renaming-p term-in-form term-in-lit)))
    (when (null eq-flag)
      (format t "~%~%IN PAIR: ~A" pair)
      (format t "~%Term in formula ~A is not equal till renaming to term in lit ~A~%" term-in-form term-in-lit))
    eq-flag))

(defun res=clauses-equal-till-renaming-p (clause1 clause2)
  (declare (edited  "17-SEP-1996")
	   (authors Ameier)
	   (input   "Two clauses.")
	   (effect  "None.")
	   (value   "Multiple-value-bind:"
		    "First: T if the two clauses are equal till renaming, that means"
		    "       there exists a renaming, that if applied on a clause will make the"
		    "       clauses equal. otherwise nil."
		    "Second: If first is nil,nil too. Otherwise a list of variable pairs"
		    "        that are representing are renaming, to make clause1 equal"
		    "        clause2."))
  (if (= (length (cl~literals clause1)) (length (cl~literals clause2)))
      (multiple-value-bind
	  (renamed-clause2 renaming)
	  (res~separate-clauses clause1 clause2)
	(declare (ignore renaming))
	(do ((rest-literals1 (cl~literals clause1) (rest rest-literals1))
	     (rest-literals2 (cl~literals renamed-clause2) (rest rest-literals2))
	     (flag 't)
	     (var-matching-list nil))
	    ((or (null flag) (null rest-literals1))
	     (values flag (if (null flag)
			      nil
			    (let* ((reverse-renaming (subst~create (subst~codomain renaming) (subst~domain renaming))))
			      (mapcar #'(lambda (var-pair)
					  (list (first var-pair)
						(subst~apply reverse-renaming (second var-pair)))) 
				      var-matching-list)))))
	  (multiple-value-bind
	      (literals-equal-flag new-var-matchings) 
	      (res=literals-equal-till-renaming-p (first rest-literals1) (first rest-literals2))
	    (if (null literals-equal-flag)
		(setq flag nil)
	      (multiple-value-bind
		  (matching-union-flag union-var-matching-list)
		  (res=matching-union var-matching-list new-var-matchings)
		(if (null matching-union-flag)
		    (setq flag nil)
		  (setq var-matching-list union-var-matching-list)))))))
    (values nil nil)))



(defun res=literals-equal-till-renaming-p (literal1 literal2)
  (declare (edited  "17-SEP-1996")
	   (authors Ameier)
	   (input   "Two literals.")
	   (effect  "None.")
	   (value   "Multiple-value-bind:"
		    "First: T if the two literals are equal till renaming, that means"
		    "       there exists a renaming, that if applied on one literal will make the"
		    "       literals equal. otherwise nil."
		    "Second: If first is nil,nil too. Otherwise a list of variable pairs"
		    "        that are representing are renaming, to make literal1 equal"
		    "        literal2."))
  (if (equal (lit~positive-p literal1) (lit~positive-p literal2))
      (res=terms-equal-till-renaming-p (lit~atom literal1) (lit~atom literal2))
    (values nil nil)))



(defgeneric res=terms-equal-till-renaming-p (term1 term2)
  (declare (edited  "17-SEP-1996")
	   (authors Ameier)
	   (input   "Two terms.")
	   (effect  "None.")
	   (value   "Multiple-value-bind:"
		    "First: T if the two terms are equal till renaming, that means"
		    "       there exists a renaming, that if applied on a term will make the"
		    "       terms equal. otherwise nil."
		    "Second: If first is nil,nil too. Otherwise a list of variable pairs"
		    "        that are representing are renaming, to make term1 equal"
		    "        term2."))
  (:method ((term1 term+appl) (term2 term+appl))
	   (if (keim~equal (data~appl-function term1) (data~appl-function term2))
	       (do ((rest-terms1 (data~appl-arguments term1) (rest rest-terms1))
		    (rest-terms2 (data~appl-arguments term2) (rest rest-terms2))
		    (flag 't)
		    (var-matching-list nil))
		   ((or (null flag) (null rest-terms1)) (values flag var-matching-list))
		 (multiple-value-bind
		     (terms-equal-flag new-var-matchings) 
		     (res=terms-equal-till-renaming-p (first rest-terms1) (first rest-terms2))
		   (if (null terms-equal-flag)
		       (setq flag nil)
		     (multiple-value-bind
			 (matching-union-flag union-var-matching-list)
			 (res=matching-union var-matching-list new-var-matchings)
		       (if (null matching-union-flag)
			   (setq flag nil)
			 (setq var-matching-list union-var-matching-list))))))
	     (values nil nil)))
  (:method ((term1 term+variable) (term2 term+variable))
	   (values 't (list (list term1 term2))))
  (:method ((term1 term+constant) (term2 term+constant))
	   (if (keim~equal term1 term2)
	       (values 't nil)
	     (values nil nil)))
  (:method (term1 term2)
	   (declare (ignore term1 term2))
	   (values nil nil)))


(defun res=matching-union (matching1 matching2)
  (do ((rest-add-matching matching2 (rest rest-add-matching))
       (union-matching matching1)
       (flag 't))
      ((or (null flag) (null rest-add-matching)) (values flag union-matching))
    (let* ((current-pair (first rest-add-matching))
	   (conter-pair-of-var1 (first (member (first current-pair) matching1 :test #'(lambda (var pair)
											(keim~equal var (first pair))))))
	   (conter-pair-of-var2 (first (member (second current-pair) matching1 :test #'(lambda (var pair)
											 (keim~equal var (second pair)))))))
      (if (or conter-pair-of-var1 conter-pair-of-var2)
	  (if (not (and (keim~equal current-pair conter-pair-of-var1) (keim~equal current-pair conter-pair-of-var2)))
	      (setq flag nil))
	(setq union-matching (cons current-pair union-matching))))))




(defun res=check-steps-recursive (clause)
  (let* ((already-done (keim~get clause 'rec-check)))
    (if already-done
	nil
      (let* ((result (res~check-step clause (node~justification clause))))
	(keim~put clause 'rec-check 't)
	(when (null result)
	  (format t "~%The following clause isn't correct justified:")
	  (format t "~%Clause: ~A" clause)
	  (format t "~%Justification: ~A" (node~justification clause))
	  (res~check-step clause (node~justification clause) :verbose 't))
	(mapcar #'res=check-steps-recursive (res~justification-parents (node~justification clause)))))))

(defgeneric res~check-step (clause just &key (verbose nil))
  (:method (clause (just res+initial) &key (verbose nil))
	   (when verbose
	     (format t "~%Clause is a input-clause -> always correct"))
	   't)
  (:method (clause (just res+reflex) &key (verbose nil))
	   (let* ((literals (cl~literals clause))

		  (length-flag (= (length literals) 1))
		  (pol-flag (lit~positive-p (first literals)))
		  (equation-flag (string= (keim~name (data~appl-function (lit~atom (first literals)))) "="))
		  (equal-sides-flag (keim~equal (first (data~appl-arguments (lit~atom (first literals))))
						(second (data~appl-arguments (lit~atom (first literals)))))))
	     (when verbose
	       (format t "~%Reflex Clause: ~A with justification: ~A" clause just)
	       (format t "~%Length: ~A" length-flag)
	       (format t "~%Polarity: ~A" pol-flag)
	       (format t "~%Equation: ~A" equation-flag)
	       (format t "~%Equal-sides: ~A" equal-sides-flag))
	     (and length-flag
		  pol-flag
		  equation-flag
		  equal-sides-flag)))
  (:method (clause (just res+flip) &key (verbose nil))
	   (let* ((literals (cl~literals clause))
		  (parent (first (res~justification-parents just)))
		  (renaming (first (res~justification-renamings just)))
		  (unifier (res~justification-unifier just))
		  (position (first (res~justification-positions just)))
		  (parent-literals (cl~literals parent))
		  (substed-parent-literals (mapcar #'(lambda (literal)
						       (subst~apply unifier
								    (subst~apply renaming literal)))
						   parent-literals))
		  (unflipped-literal (data~struct-at-position substed-parent-literals position))
		  (flipped-literal (data~struct-at-position literals position))
		  (unflipped-atom (lit~atom unflipped-literal))
		  (flipped-atom (lit~atom flipped-literal))

		  (length-flag (= (length literals)
				  (length substed-parent-literals)))
		  (other-clauses-equal-flag (every #'(lambda (pair)
						       (keim~equal (first pair) (second pair)))
						   (mapcar #'(lambda (old-lit new-lit)
							       (list old-lit new-lit))
							   (remove unflipped-literal substed-parent-literals)
							   (remove flipped-literal literals))))
		  (equation-unflipped-flag (and (data~appl-p (lit~atom unflipped-literal))
						(string= (keim~name (data~appl-function unflipped-atom)) "=")))
		  (equation-flipped-flag (and (data~appl-p (lit~atom flipped-literal))
						(string= (keim~name (data~appl-function flipped-atom)) "=")))
		  (flipped-equal-flag (and (equal (lit~positive-p unflipped-literal) (lit~positive-p flipped-literal))
					    (keim~equal (first (data~appl-arguments unflipped-atom))
							(second (data~appl-arguments flipped-atom)))
					    (keim~equal (first (data~appl-arguments flipped-atom))
							(second (data~appl-arguments unflipped-atom))))))
	     (when verbose
	       (format t "~%Flip clause ~A with justification ~A" clause just)
	       (format t "~%Clauses have same length: ~A" length-flag)
	       (format t "~%Other-clauses-equal: ~A" other-clauses-equal-flag)
	       (format t "~%Unflipped is equation: ~A" equation-unflipped-flag)
	       (format t "~%Flipped is equation: ~A" equation-flipped-flag)
	       (format t "~%Literals are flipped: ~A" flipped-equal-flag))
	     (and length-flag
		  other-clauses-equal-flag
		  equation-unflipped-flag
		  equation-flipped-flag
		  flipped-equal-flag)))
  (:method (clause (just res+resolution) &key (verbose nil))
	   (let* ((literals (cl~literals clause))
		  (parents (res~justification-parents just))
		  (positions (res~justification-positions just))
		  (renamings (res~justification-renamings just))
		  (unifier (res~justification-unifier just))
		  (renaming1 (first renamings))
		  (renaming2 (second renamings))
		  (parent1-substed-literals (mapcar #'(lambda (lit)
							(subst~apply unifier
								     (subst~apply renaming1 lit)))
						    (cl~literals (first parents))))
		  (parent2-substed-literals (mapcar #'(lambda (lit)
							(subst~apply unifier
								     (subst~apply renaming2 lit)))
						    (cl~literals (second parents))))
		  (res-lit1 (data~struct-at-position parent1-substed-literals (first positions)))
		  (res-lit2 (data~struct-at-position parent2-substed-literals (second positions)))

		  (length-flag (= (length literals)
				  (length (append (remove res-lit1 parent1-substed-literals)
						  (remove res-lit2 parent2-substed-literals)))))
		  (equal-literals-flag (every #'(lambda (pair)
						  (keim~equal (first pair) (second pair)))
					      (mapcar #'(lambda (old-lit new-lit)
							  (list old-lit new-lit))
						      (append (remove res-lit1 parent1-substed-literals)
							      (remove res-lit2 parent2-substed-literals))
						      literals)))
		  (resolve-flag (and (null (equal (lit~positive-p res-lit1) (lit~positive-p res-lit2)))
				     (keim~equal (lit~atom res-lit1) (lit~atom res-lit2)))))
	     (when verbose
	       (format t "~%Resolution Clause ~A with justification ~A" clause just)
	       (format t "~%Equal length: ~A" length-flag)
	       (format t "~%Literals are equal: ~A" equal-literals-flag)
	       (format t "~%Resolve literals are contradictional: ~A" resolve-flag))

	     (and length-flag
		  equal-literals-flag
		  resolve-flag)))
  (:method (clause (just res+factoring) &key (verbose nil))
	   (let* ((literals (cl~literals clause))
		  (parent (first (res~justification-parents just)))
		  (renaming (first (res~justification-renamings just)))
		  (positions (res~justification-positions just))
		  (unifier (res~justification-unifier just))
		  (substed-parent-literals (mapcar #'(lambda (lit)
						       (subst~apply unifier
								    (subst~apply renaming lit)))
						   (cl~literals parent)))
		  (remaining-lit (data~struct-at-position substed-parent-literals (first positions)))
		  (removed-lit (data~struct-at-position substed-parent-literals (second positions)))

		  (length-flag (= (length literals)
				  (length (remove removed-lit substed-parent-literals))))
		  (equal-literals-flag (every #'(lambda (pair)
						  (keim~equal (first pair) (second pair)))
					      (mapcar #'(lambda (old-lit new-lit)
							  (list old-lit new-lit))
						      (remove removed-lit substed-parent-literals)
						      literals)))
		  (fac-flag (and (equal (lit~positive-p removed-lit) (lit~positive-p remaining-lit))
				 (keim~equal (lit~atom removed-lit) (lit~atom remaining-lit)))))
	     
	     (when verbose
	       (format t "~%Factoring clause ~A with justification ~A" clause just)
	       (format t "~%Length: ~A" length-flag)
	       (format t "~%Equal literals: ~A" equal-literals-flag)
	       (format t "~%Facable literals: ~A" fac-flag))

	     (and length-flag
		  equal-literals-flag
		  fac-flag)))
  (:method (clause (just res+paramodulation) &key (verbose nil))
	   (let* ((literals (cl~literals clause))
		  (parents (res~justification-parents just))
		  (positions (res~justification-positions just))
		  (renamings (res~justification-renamings just))
		  (unifier (res~justification-unifier just))
		  (direction (string (res~paramod-direction just)))
		  (substed-mother-literals (mapcar #'(lambda (lit)
						       (subst~apply unifier
								    (subst~apply (first renamings) lit)))
						   (cl~literals (first parents))))
		  (substed-father-literals (mapcar #'(lambda (lit)
						       (subst~apply unifier
								    (subst~apply (second renamings) lit)))
						   (cl~literals (second parents))))
		  (mother-position (first positions))
		  (father-position (second positions))
		  (father-lit (data~struct-at-position substed-father-literals father-position))
		  (new-mother-literals (data~replace-at-position substed-mother-literals 
								 mother-position
								 (if (or (string= direction "lr")
									 (string= direction "LR"))
								     (second (data~appl-arguments (lit~atom father-lit)))
								   (first (data~appl-arguments (lit~atom father-lit))))))
		  
		  (father-equality-flag (and (lit~positive-p father-lit)
					     (string= (keim~name (data~appl-function (lit~atom father-lit))) "=")))
		  (mother-father-eq-flag (keim~equal (data~struct-at-position substed-mother-literals mother-position)
						     (if (or (string= direction "LR")
							     (string= direction "lr"))
							 (first (data~appl-arguments (lit~atom father-lit)))
						       (second (data~appl-arguments (lit~atom father-lit))))))
		  (length-flag (= (length literals)
				  (length (append new-mother-literals
						  (remove father-lit substed-father-literals)))))
		  (equality-flag (every #'(lambda (pair)
					    (keim~equal (first pair) (second pair)))
					(mapcar #'(lambda (old-lit new-lit)
						    (list old-lit new-lit))
						(append new-mother-literals
							(remove father-lit substed-father-literals))
						literals))))
	     (when verbose
	       (format t "~%Paramodulation clause ~A with just ~A" clause just)
	       (format t "~%EQual length: ~A" length-flag)
	       (format t "~%Father-lit-is-pos-equation: ~A" father-equality-flag)
	       (format t "~%Mother-term-at-pos is equal to father side: ~A" mother-father-eq-flag)
	       (format t "~%Literals are equal: ~A" equality-flag))
	     
	     (and length-flag
		  father-equality-flag
		  mother-father-eq-flag
		  equality-flag)))
  (:method (clause (just res+hyper-resolution) &key (verbose nil))
	   (let* ((literals (cl~literals clause))
		  (parents (res~justification-parents just))
		  (positions (res~justification-positions just))
		  (renamings (res~justification-renamings just))
		  (unifier (res~justification-unifier just))
		  (kind (if (typep just 'res+negative-hyper-resolution)
			    'neg
			  'pos))
		  (nucleus-substed-literals (mapcar #'(lambda (lit)
							(subst~apply unifier
								     (subst~apply (first renamings) lit)))
						    (cl~literals (first parents))))
		  (electrons-substed-literals-list (mapcar #'(lambda (electron renaming)
							       (mapcar #'(lambda (lit)
									   (subst~apply unifier
											(subst~apply renaming lit)))
								       (cl~literals electron)))
							   (rest parents)
							   (rest renamings)))
		  (resolved-electron-literals (mapcar #'(lambda (electron-literals position)
							  (data~struct-at-position electron-literals position))
						      electrons-substed-literals-list positions))
		  (rest-electron-lits (apply 'append (mapcar #'(lambda (electron-lits resolved-lit)
								 (remove resolved-lit electron-lits))
							     electrons-substed-literals-list resolved-electron-literals)))
		  (resolved-nucleus-literals (remove-if #'(lambda (lit)
							    (if (equal kind 'neg)
								(null (lit~positive-p lit))
							      (lit~positive-p lit)))
							nucleus-substed-literals))
		  (rest-nucleus-literals (remove-if-not #'(lambda (lit)
							    (if (equal kind 'neg)
								(null (lit~positive-p lit))
							      (lit~positive-p lit)))
							nucleus-substed-literals))
		  
		  (electron-pol-flag (every #'(lambda (lit)
						(if (equal kind 'neg)
						    (null (lit~positive-p lit))
						  (lit~positive-p lit)))
					    (apply 'append electrons-substed-literals-list)))
		  (length-resolved-flag (= (length resolved-electron-literals)
					   (length resolved-nucleus-literals)))
		  (length-remaining-flag (= (length literals)
					    (length (append rest-nucleus-literals rest-electron-lits))))
		  (resolvable-flag  (every #'(lambda (pair)
					       (and (null (equal (lit~positive-p (first pair)) (lit~positive-p (second pair))))
						    (keim~equal (lit~atom (first pair)) (lit~atom (second pair)))))
					   (mapcar #'(lambda (old-lit new-lit)
						       (list old-lit new-lit))
						   resolved-electron-literals 
						   resolved-nucleus-literals)))
		  (equal-flag (every #'(lambda (pair)
					 (keim~equal (first pair) (second pair)))
				     (mapcar #'(lambda (old-lit new-lit)
						 (list old-lit new-lit))
					     (append rest-nucleus-literals rest-electron-lits)
					     literals))))
	     (when verbose
	       (format t "~%Hyper-resolution clause ~A with just ~A" clause just)
	       (format t "~%Electron-polarity: ~A" electron-pol-flag)
	       (format t "~%Length of resolved literals: ~A" length-resolved-flag)
	       (format t "~%Length of remaining literals: ~A" length-remaining-flag)
	       (format t "~%Resolvable: ~A" resolvable-flag)
	       (format t "~%Equal literals: ~A" equal-flag))

	     (and electron-pol-flag
		  length-resolved-flag
		  length-remaining-flag
		  resolvable-flag
		  equal-flag)))
  (:method (clause (just res+ur-resolution) &key (verbose nil))
	   (let* ((literals (cl~literals clause))
		  (parents (res~justification-parents just))
		  (positions (res~justification-positions just))
		  (renamings (res~justification-renamings just))
		  (unifier (res~justification-unifier just))
		  (nucleus-substed-literals (mapcar #'(lambda (lit)
							(subst~apply unifier
								     (subst~apply (first renamings) lit)))
						    (cl~literals (first parents))))
		  (electrons-substed-literals-list (mapcar #'(lambda (electron renaming)
							       (mapcar #'(lambda (lit)
									   (subst~apply unifier
											(subst~apply renaming lit)))
								       (cl~literals electron)))
							   (rest parents)
							   (rest renamings)))
		  (resolved-electron-literals (apply 'append electrons-substed-literals-list))
		  (remaining-nucleus-literal (data~struct-at-position (first positions) nucleus-substed-literals))
		  (resolved-nucleus-literals (remove remaining-nucleus-literal nucleus-substed-literals))
		  
		  (electrons-flag (every #'(lambda (lit-list)
					     (= (length lit-list) 1))
					 electrons-substed-literals-list))
		  (length-of-resolved-flag (= (length resolved-nucleus-literals)
					      (length resolved-electron-literals)))
		  (length-of-remaining-flag (= (length literals) 1))
		  (resolvable-flag (every #'(lambda (pair)
					      (and (null (equal (lit~positive-p (first pair)) (lit~positive-p (second pair))))
						   (keim~equal (lit~atom (first pair)) (lit~atom (second pair)))))
					  (mapcar #'(lambda (old-lit new-lit)
						      (list old-lit new-lit))
						  resolved-electron-literals 
						  resolved-nucleus-literals)))
		  (equal-flag (keim~equal remaining-nucleus-literal (first literals))))

	     (when verbose
	       (format t "~%Ur-Resolution clause ~A justification ~A" clause just)
	       (format t "~%Electrons-length-flag: ~A" electrons-flag)
	       (format t "~%Length of resolved: ~A" length-of-resolved-flag)
	       (format t "~%Length of remaining: ~A" length-of-remaining-flag)
	       (format t "~%Resolvable: ~A" resolvable-flag)
	       (format t "~%Equal literals: ~A" equal-flag))

	     (and electrons-flag
		  length-of-resolved-flag
		  length-of-remaining-flag
		  resolvable-flag
		   equal-flag))))

	 
#| --------------- pprint-style --------------------------- |#

(defmethod post~print ((proof res+proof) stream)
  (let ((*standard-output* stream))
    (pp~pprint proof 'res-post)))

(pp~defstyle res-post :parent pp+top
 :help "A style which will print RESOLUTION-PROOFS and related things in POST format."
 :pprint-methods
 ((res+proof	   
   (lambda (stream proof)
     (let* ((*standard-output* stream)
	    (env (res~proof-environment proof))
	    (type-vars (env~class-keys env 'type+variable nil))
	    (type-constants (env~class-keys env 'type+constant nil))
	    (constants (env~class-keys env 'term+constant nil))
	    (variables (env~class-keys env 'term+variable nil)))
				     
       (pprint-logical-block 
	(nil nil :prefix "(" :suffix ")")
	(write-string "resolution-proof ")
	(pprint-logical-block
	 (nil nil :prefix "" :suffix "")
	 (write-string (princ-to-string (keim~name proof)))
	 (pprint-newline :mandatory)
	 (write-string "(in ")
	 (write-string (princ-to-string (keim~name (prob~proof-theory proof))))
	 (write-string ")")
	 (pprint-newline :mandatory)
	 (write-string "(upper-object ")
	 (write-string (princ-to-string (if (prob~p (res~proof-upper-object proof))
					    'problem
					  'pds)))
	 (write-char #\space)
	 (write-string (princ-to-string (keim~name (res~proof-upper-object proof))))
	 (write-string ")")
	 (pprint-newline :mandatory)
	 (pprint-logical-block
	  (nil  nil :prefix "(" :suffix ")")
	  (write-string "declarations ")
	  (pprint-logical-block
	   (nil (list type-vars type-constants constants variables) :prefix "" :suffix "")	   
	   (pprint-indent :block 0)
	   (pprint-logical-block 
	    (nil (pprint-pop) :prefix "("
		 :suffix ")")
	    (pprint-indent :block 1)
	    (write-string "type-variables ")
	    (pprint-exit-if-list-exhausted)
	    (loop
	     (let* ((type (pprint-pop)))
	       (when type
		 (post~print type stream)
		 (pprint-exit-if-list-exhausted)
		 (pprint-newline :mandatory))))
	    (pprint-newline :linear))
	   (pprint-newline :mandatory)
	   (pprint-logical-block 
	    (nil (pprint-pop) :prefix "("
		 :suffix ")")
	    (pprint-indent :block 1)
	    (write-string "type-constants ")
	    (pprint-exit-if-list-exhausted)
	    (loop
	     (let* ((type (pprint-pop)))
	       (when type
		 (post~print type stream)
		 (pprint-exit-if-list-exhausted)
		 (pprint-newline :mandatory))))
	    (pprint-newline :linear))
	   (pprint-newline :mandatory)
	   (pprint-logical-block 
	    (nil (mapcar #'(lambda (sym) (post~read-object sym env :existing-term))
			 (pprint-pop))
		 :prefix "("
		 :suffix ")")
	    (pprint-indent :block 1)
	    (write-string "constants ")
	    (pprint-exit-if-list-exhausted)
	    (loop 
	     (let* ((term (pprint-pop)))
	       (when term
		 (write-char #\()
		 (write term)
		 (write-char #\space)
		 (write (term~type term))
		 (write-char #\))))
	     (pprint-exit-if-list-exhausted)
	     (pprint-newline :linear)))
	   (pprint-newline :mandatory)
	   (pprint-logical-block 
	    (nil (mapcar #'(lambda (sym) (post~read-object sym env :existing-term))
			 (pprint-pop))
		 :prefix "("
		 :suffix ")")
	    (pprint-indent :block 1)
	    (write-string "variables ")
	    (pprint-exit-if-list-exhausted)
	    (loop 
	     (let* ((term (pprint-pop)))
	       (when term
		 (write-char #\()
		 (write term)
		 (write-char #\space)
		 (write (term~type term))
		 (write-char #\))))
	     (pprint-exit-if-list-exhausted)
	     (pprint-newline :linear)))
	   (pprint-newline :mandatory)
	   (pprint-logical-block
	    (nil nil :prefix "(" :suffix ")")
	    (write-string "assumptions ")
	    (pprint-logical-block
	     (nil (res~proof-assumptions proof) :prefix "" :suffix "")
	     (pprint-exit-if-list-exhausted)
	     (loop
	      (let* ((assumption (pprint-pop)))
		(when assumption
		  (write assumption)
		  (pprint-exit-if-list-exhausted)
		  (pprint-newline :mandatory)))))
	    (pprint-exit-if-list-exhausted))
	   (pprint-newline :mandatory)
	   (write-string "(conclusion ")
	   (write (res~proof-conclusion proof))
	   (write-string ")")
	   (pprint-exit-if-list-exhausted)) ;; exists internal logical block of declarations 
	  (pprint-exit-if-list-exhausted)) ;; exists outer logical block of declarations
	 (pprint-newline :mandatory)

	 ;; cnf
	 
	 (pprint-logical-block
	  (nil nil :prefix "(" :suffix ")")
	  (write-string "cnf ")
	  (pprint-logical-block
	   (nil nil :prefix "" :suffix "")

	   ;; clauses
	   
	   (pprint-logical-block
	    (nil (res~proof-initial-clauses proof) :prefix "(" :suffix ")")
	    (pprint-exit-if-list-exhausted)
	    (loop
	     (let* ((clause (pprint-pop)))
	       (when clause
		 (write clause)
		 (pprint-exit-if-list-exhausted)
		 (pprint-newline :mandatory)))))
	   (pprint-newline :mandatory)

	   ;; skolem-constants

	   (pprint-logical-block
	    (nil (res~proof-skolem-functions proof) :prefix "(" :suffix ")")
	    (write-string "skolem-functions ")
	    (pprint-exit-if-list-exhausted)
	    (loop
	     (let* ((skf (pprint-pop)))
	       (when skf
		 (write-string (format nil "(~A ~A)" (keim~name skf) (sksym~arity skf)))
		 
		 ;; (princ-to-string (format nil "(~A ~A)" (keim~name skf) (sksym~arity skf))))
		 (pprint-exit-if-list-exhausted)
		 (write-char #\space)))))
	   (pprint-newline :mandatory)
	   
	   ;; delta-relation

	   (pprint-logical-block
	    (nil nil :prefix "(" :suffix ")")
	    (write-string "delta-relation ")
	    (pprint-logical-block
	     (nil (delta~relation-pairs (res~proof-delta-relation proof)) :prefix "" :suffix "")
	     (pprint-exit-if-list-exhausted)
	     (loop
	      (let* ((delta-pair (pprint-pop)))
		(when delta-pair
		  (write delta-pair)
		  (pprint-exit-if-list-exhausted)
		  (pprint-newline :mandatory)))))
	    (pprint-exit-if-list-exhausted))

	   (pprint-exit-if-list-exhausted)) ;; exists internal cnf block
	  (pprint-exit-if-list-exhausted)) ;; exists external cnf block
	 (pprint-newline :mandatory)

	 
	 ;; steps

	 (pprint-logical-block
	  (nil nil :prefix "(" :suffix ")")
	  (write-string "steps ")
	  (pprint-logical-block
	   (nil (res~proof-step-clauses proof) :prefix "" :suffix "")
	   (pprint-exit-if-list-exhausted)
	   (loop
	    (let* ((step (pprint-pop)))
	      (when step
		(let* ((just (node~justification step)))
		  (keim~put just 'clause step)
		  (write just))                       ;; a small hack to print clause with just
		(pprint-exit-if-list-exhausted)
		(pprint-newline :mandatory)))))
	  (pprint-exit-if-list-exhausted))
	 
	 (pprint-newline :mandatory)
	 
	 (pprint-exit-if-list-exhausted)) ;; exists internal big block
	(pprint-exit-if-list-exhausted))))) ;; exists outer big block
  (res+resolution
   (lambda (stream just)
     (let* ((clause (keim~get just 'clause))
	    (parents (res~resolution-clauses just))
	    (positions (res~resolution-positions just))
	    (renamings (res~just-renamings just)))
       (keim~remprop just 'clause)
       (keim~put clause :cl=extra-variables
		 (remove-if #'type~variable-p
			    (remove-duplicates (append (data~free-variables (res~just-unifier just))
						       (mapcan #'data~free-variables (res~just-renamings just)))
					       :test #'keim~equal)))
       
       (pprint-logical-block
	(nil nil :prefix "(" :suffix ")")
	(write-string "resolution ")
	(pprint-logical-block
	 (nil nil :prefix "" :suffix "")
	 (write-string (princ-to-string (keim~name just)))
	 (write-char #\space)
	 (write clause)
	 (pprint-newline :mandatory)
	 (pprint-logical-block
	  (nil nil :prefix "(" :suffix ")")
	  (write-string "parents ")
	  (pprint-logical-block
	   (nil nil :prefix "" :suffix "")
	   (write-char #\()
	   (write-string (princ-to-string (keim~name (first parents))))
	   (write-char #\space)
	   (write (first positions))
	   (write-char #\space)
	   (write (first renamings))
	   (write-char #\))
	   (pprint-newline :mandatory)
	   (write-char #\()
	   (write-string (princ-to-string (keim~name (second parents))))
	   (write-char #\space)
	   (write (second positions))
	   (write-char #\space)
	   (write (second renamings))
	   (write-char #\))
	   (pprint-exit-if-list-exhausted)) ;; internal parents block
	  (pprint-exit-if-list-exhausted)) ;; external parents block
	 (pprint-newline :mandatory)
	 (write (res~just-unifier just))
	 (keim~remprop clause :cl=extra-variables)
	 (pprint-exit-if-list-exhausted))
	(pprint-exit-if-list-exhausted)))))
  (res+factoring
   (lambda (stream just)
     (let* ((clause (keim~get just 'clause))
	    (parent (res~factoring-clause just))
	    (positions (res~factoring-positions just))
	    (renaming (first (res~just-renamings just))))
       (keim~remprop just 'clause)
       (keim~put clause :cl=extra-variables
		 (remove-duplicates (append (data~free-variables (res~just-unifier just))
					    (mapcan #'data~free-variables (res~just-renamings just)))
				    :test #'keim~equal))
       
       (pprint-logical-block
	(nil nil :prefix "(" :suffix ")")
	(write-string "factoring ")
	(pprint-logical-block
	 (nil nil :prefix "" :suffix "")
	 (write-string (princ-to-string (keim~name just)))
	 (write-char #\space)
	 (write clause)
	 (pprint-newline :mandatory)
	 (write-string "(parents (")
	 (write-string (princ-to-string (keim~name parent)))
	 (write-char #\space)
	 (write (first positions))
	 (write-char #\space)
	 (write (second positions))
	 (write-char #\))
	 (write-char #\space)
	 (write renaming)
	 (write-char #\))
	 (pprint-newline :mandatory)
	 (write (res~just-unifier just))
	 (keim~remprop clause :cl=extra-variables)
	 (pprint-exit-if-list-exhausted))
	(pprint-exit-if-list-exhausted)))))
  (res+equality-factoring
   (lambda (stream just)
     (let* ((clause (keim~get just 'clause))
	    (parent (first (res~justification-parents just)))
	    (positions (res~justification-positions just))
	    (renaming (first (res~justification-renamings just)))
	    (remaining-pos (res~equality-factoring-just-remaining-literal-position just))
	    (unifier (res~justification-unifier just)))

       (keim~remprop just 'clause)
       (keim~put clause :cl=extra-variables
		 (remove-duplicates (append (data~free-variables unifier)
					    (mapcan #'data~free-variables (res~justification-renamings just)))
				    :test #'keim~equal))
       
       (pprint-logical-block
	(nil nil :prefix "(" :suffix ")")
	(write-string "equality-factoring ")
	(pprint-logical-block
	 (nil nil :prefix "" :suffix "")
	 (write-string (princ-to-string (keim~name just)))
	 (write-char #\space)
	 (write clause)
	 (pprint-newline :mandatory)
	 (write-string "(parents (")
	 (write-string (princ-to-string (keim~name parent)))
	 (write-char #\space)
	 (write (first positions))
	 (write-char #\space)
	 (write (second positions))
	 (write-char #\space)
	 (write remaining-pos)
	 (pprint-newline :mandatory)
	 (write renaming)
	 (write-char #\))
	 (write-char #\))
	 (pprint-newline :mandatory)
	 (write unifier)
	 (keim~remprop clause :cl=extra-variables)
	 (pprint-exit-if-list-exhausted))
	(pprint-exit-if-list-exhausted)))))
  (res+paramodulation
   (lambda (stream just)
     (let* ((clause (keim~get just 'clause))
	    (renamings (res~just-renamings just))
	    (mother (res~paramod-mother just))
	    (mother-position (res~paramod-mother-position just))
	    (mother-renaming (first renamings))
	    (father (res~paramod-father just))
	    (father-position (res~paramod-father-position just))
	    (father-renaming (second renamings))
	    (direction (res~paramod-direction just)))
       (keim~remprop just 'clause)
       (keim~put clause :cl=extra-variables
		 (remove-duplicates (append (data~free-variables (res~just-unifier just))
					    (mapcan #'data~free-variables (res~just-renamings just)))
				    :test #'keim~equal))
       
       (pprint-logical-block
	(nil nil :prefix "(" :suffix ")")
	(write-string "paramodulation ")
	(pprint-logical-block
	 (nil nil :prefix "" :suffix "")
	 (write-string (princ-to-string (keim~name just)))
	 (write-char #\space)
	 (write clause)
	 (pprint-newline :mandatory)
	 (pprint-logical-block
	  (nil nil :prefix "(" :suffix ")")
	  (write-string "parents ")
	  (pprint-logical-block
	   (nil nil :prefix "" :suffix "")
	   (write-char #\()
	   (write-string (princ-to-string (keim~name mother)))
	   (write-char #\space)
	   (write mother-position)
	   (write-char #\space)
	   (write mother-renaming)
	   (write-char #\))
	   (pprint-newline :mandatory)
	   (write-char #\()
	   (write-string (princ-to-string (keim~name father)))
	   (write-char #\space)
	   (write father-position)
	   (write-char #\space)
	   (write father-renaming)
	   (write-char #\space)
	   (write-string (princ-to-string direction))
	   (write-char #\))
	   (pprint-exit-if-list-exhausted)) ;; internal parents block
	  (pprint-exit-if-list-exhausted)) ;; external parents block
	 (pprint-newline :mandatory)
	 (write (res~just-unifier just))
	 (keim~remprop clause :cl=extra-variables)
	 (pprint-exit-if-list-exhausted))
	(pprint-exit-if-list-exhausted)))))
  (res+flip
   (lambda (stream just)
     (let* ((clause (keim~get just 'clause))
	    (parents (res~resolution-clauses just))
	    (positions (res~resolution-positions just)))
       (keim~remprop just 'clause)
       (keim~put clause :cl=extra-variables
		 (remove-duplicates (append (data~free-variables (res~just-unifier just))
					    (mapcan #'data~free-variables (res~just-renamings just)))
				    :test #'keim~equal))
       
       (pprint-logical-block
	(nil nil :prefix "(" :suffix ")")
	(write-string "flip ")
	(pprint-logical-block
	 (nil nil :prefix "" :suffix "")
	 (write-string (princ-to-string (keim~name just)))
	 (write-char #\space)
	 (write clause)
	 (pprint-newline :mandatory)
	 (write-string "(parents (")
	 (write-string (princ-to-string (keim~name (first parents))))
	 (write-char #\space)
	 (write (first positions))
	 (write-char #\))
	 (write-char #\))
	 (keim~remprop clause :cl=extra-variables)
	 (pprint-exit-if-list-exhausted))
	(pprint-exit-if-list-exhausted)))))
  (res+reflex
   (lambda (stream just)
     (let* ((clause (keim~get just 'clause)))
       (keim~remprop just 'clause)
       (pprint-logical-block
	(nil nil :prefix "(" :suffix ")")
	(write-string "reflex ")
	(write-string (princ-to-string (keim~name just)))
	(write-char #\space)
	(write clause)
	(pprint-exit-if-list-exhausted)))))
  (res+hyper-resolution
   (lambda (stream just)
     (let* ((clause (keim~get just 'clause))
	    (parents (res~resolution-clauses just))
	    (positions (res~resolution-positions just))
	    (renamings (res~just-renamings just))
	    (par-pos-ren-triples (mapcar #'list (rest parents) positions (rest renamings))))
       (keim~remprop just 'clause)
       (keim~put clause :cl=extra-variables
		 (remove-duplicates (append (data~free-variables (res~just-unifier just))
					    (mapcan #'data~free-variables (res~just-renamings just)))
				    :test #'keim~equal))
       
       (pprint-logical-block
	(nil nil :prefix "(" :suffix ")")
	(write-string "hyper-resolution ")
	(pprint-logical-block
	 (nil nil :prefix "" :suffix "")
	 (write-string (princ-to-string (keim~name just)))
	 (write-char #\space)
	 (write clause)
	 (pprint-newline :mandatory)
	 (write-string "(nucleus (")
	 (write-string (princ-to-string (keim~name (first parents))))
	 (write-char #\space)
	 (write (first renamings))
	 (write-char #\))
	 (write-char #\))
	 (pprint-newline :mandatory)
	 (pprint-logical-block
	  (nil nil :prefix "(" :suffix ")")
	  (write-string "electrons ")
	  (pprint-logical-block
	   (nil par-pos-ren-triples :prefix "" :suffix "")
	   (pprint-exit-if-list-exhausted)
	   (loop
	    (let* ((next-triple (pprint-pop)))
	      (when next-triple
		(let* ((electron (first next-triple))
		       (position (second next-triple))
		       (renaming (third next-triple)))
		  (write-char #\()
		  (write-string (princ-to-string (keim~name electron)))
		  (write-char #\space)
		  (write position)
		  (write-char #\space)
		  (write renaming)
		  (write-char #\)))))
	    (pprint-exit-if-list-exhausted)
	    (pprint-newline :mandatory)))
	  (pprint-exit-if-list-exhausted)) ;; external electrons block
	 (pprint-newline :mandatory)
	 (write (res~just-unifier just))
	 (keim~remprop clause :cl=extra-variables)
	 (pprint-exit-if-list-exhausted))
	(pprint-exit-if-list-exhausted)))))
  (res+ur-resolution
   (lambda (stream just)
     (let* ((clause (keim~get just 'clause))
	    (parents (res~resolution-clauses just))
	    (positions (res~resolution-positions just))
	    (renamings (res~just-renamings just))
	    (par-ren-pairs (mapcar #'list (rest parents) (rest renamings))))
       (keim~remprop just 'clause)
       (keim~put clause :cl=extra-variables
		 (remove-duplicates (append (data~free-variables (res~just-unifier just))
					    (mapcan #'data~free-variables (res~just-renamings just)))
				    :test #'keim~equal))
       
       (pprint-logical-block
	(nil nil :prefix "(" :suffix ")")
	(write-string "ur-resolution ")
	(pprint-logical-block
	 (nil nil :prefix "" :suffix "")
	 (write-string (princ-to-string (keim~name just)))
	 (write-char #\space)
	 (write clause)
	 (pprint-newline :mandatory)
	 (write-string "(non-unit (")
	 (write-string (princ-to-string (keim~name (first parents))))
	 (write-char #\space)
	 (write (first positions))
	 (write-char #\space)
	 (write (first renamings))
	 (write-char #\))
	 (write-char #\))
	 (pprint-newline :mandatory)
	 (pprint-logical-block
	  (nil nil :prefix "(" :suffix ")")
	  (write-string "units ")
	  (pprint-logical-block
	   (nil par-ren-pairs :prefix "" :suffix "")
	   (pprint-exit-if-list-exhausted)
	   (loop
	    (let* ((next-pair (pprint-pop)))
	      (when next-pair
		(let* ((clause (first next-pair))
		       (renaming (second next-pair)))
		  (write-char #\()
		  (write-string (princ-to-string (keim~name clause)))
		  (write-char #\space)
		  (write renaming)
		  (write-char #\)))))
	    (pprint-exit-if-list-exhausted)
	    (pprint-newline :mandatory)))
	  (pprint-exit-if-list-exhausted)) ;; external electrons block
	 (pprint-newline :mandatory)
	 (write (res~just-unifier just))
	 (keim~remprop clause :cl=extra-variables)
	 (pprint-exit-if-list-exhausted))
	(pprint-exit-if-list-exhausted)))))
  (pos+position
   (lambda (stream pos)
     (post~print pos stream)))
  (subst+substitution
   (lambda (stream subst)
     (post~print subst stream)))
  (delta+relation-pair
   (lambda (stream type)
     (post~print type stream)))
  (cl+clause
   (lambda (stream type)
     (post~print type stream)))
  (type+type
   (lambda (stream type)
     (post~print type stream)))
  (term+term
   (lambda (stream term)
     (post~print term stream)))
  (termix+named-term
   (lambda (stream term)
     (post~print term stream)))))

