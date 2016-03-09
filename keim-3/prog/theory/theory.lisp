;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: KEIM -*-
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Theory Module
;; -----------------
;; This nice little module contains (should contain) all (most of) the
;; stuff that's needed to define compact little theories.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; On May the 22nd 1997 I decided to do some major changes...   VS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

     
(in-package "KEIM")

(mod~defmod th :uses (node env keim mod post inter serv sort rpc)
	    :documentation "Datastructures and functions to deal with theories"
	    :exports (th+theory
		      th~create
		      th~p
		      th~find-theory
		      th~find-assumption
		      th~find-problem
		      th~find-problem&assumption
		      th~all-theories
		      th~defined-in
		      th~imports
		      th~imports-recursed
		      th~env
		      th~senv
		      th~connectives
		      ;;; th~assertions --> th~assumptions   VS
		      th~assumptions
		      th~assumptions-recursed
		      th~axioms
		      th~definitions
                      th~definitions-recursed
		      th~theorems
		      th~problems
		      th~problems-recursed
		      th~prove
		      th~methods
		      th~deftheory
                      th~defsimplifier
                      th~simplifiers
		      th~simplifiers-recursed
		      th~agent-default

		      ;;; th+assertion --> th+assumption  VS
		      th+assumption
		      th+axiom
                      th+simplifier
		      th+def

		      th~assumption-p
		      th~axiom-p
		      th~definition-p
		      th~problem-p
                      th~simplifier-p

		      th~ass-node
		      th~definition-constant
		      th~ass-formula
		      th~ass-theory
                      th~simplifier-antecedent
                      th~simplifier-consequent
		      th~simplifier-assumption
		      th~simplifier-bindable-type-vars
                      
		      th~problem2theorem
		      th~defaxiom
		      th~defdef
		      th~defconstant
		      th~deftype
		      th~defproblem
		      th~deftheorem
		      th~defagentdefault
		      ;;; th~read-file --> th~load-file    VS
		      th~load-file
		      th~require
		      th~require-only             
		      th~require-completely
		      th~input
		      th~load-theorems
		      th~load-problems
		      th~load-rules
		      th~load-methods
		      th~load-tactics
		      th~load-crules
		      th~load-metaprds
		      th~load-strategies
		      th~load-agents
		      th~load-lingu
		      th~load-constraintsolver
		      th~append2file
		      th~write2theory
		      th~delete-from-file
		      th~delete-from-theory
		      th~problem2theorem-file
		      th~file-integrity

		      th~proof-storage-slot
		      th~new-proof-name
		      th~load-theorem-proofs
		      th~write-theorem-proofs
		      th~write-theorem-last-proof
		      th~theorem-proofs?
		      th~theorem-proof-number

		      th~all-ancestors

		      th*current-connective-types

		      th~execute-theory-replay
		      th~execute-all-theory-replay
		      th~lml
		      
		      th*current-db
		      th*output

		      )
	    )

(defvar th*current-db nil) ;; MP's switch: nil for old theory, anything for mbase, affected functions:
                           ;; th~require..., th~load-problems/theorems/rules/tactics/methods/crules/metaprds
                           ;; th=all-theories


(defvar th*output '(theory)
  "affects the output during loading a theory; it is a list with following recognized keywords:
   'theory 'rules 'tactics 'methods  'crules
   'problems (the last one will work only with 'theory)")

(defvar th*current-connective-types
  (list "EXISTENTIAL-QUANTOR"
	"UNIVERSAL-QUANTOR"
	"TRUTH"
	"FALSITY"
	"NEGATION"
	"EQUALITY"
	"EQUIVALENCE"
	"DISJUNCTION"
	"CONJUNCTION"
	"IMPLICATION"))

#{\section{Theories}\label{mod:theory}

Databases of deduction systems could be structured by using theories. Therefore \keim provides the
necessary facilities to distinguish certain groups of theorems. \newline
Theories can depend on each other, i.e. a theory can inherit from one parent theory. A theory itself
consists of an environment containing basic definitions of {\em type-constants}, {\em type-variables}
and {\em constants}. Furthermore a theory contains {\em axioms}, {\em definitions}, {\em theorems} and
{\em lemmata}, all usable as {\em assumptions}, unproved {\em problems} and {\em methods}.
Although a defined theory can be extended by {\em definitions} - using {\vb TH~DEFDEF} -
{\em type-constants} can only be defined once!
#}

(defclass th+theory (help+help)
  ((imports :initarg :imports
	    :initform nil
	    :accessor th=imports
	    :documentation "The list of imported subtheories.")
   (env :initarg :env
	:initform (env~create)
	:accessor th=env
	:documentation "An environment containing all declared variables, constants, functions.... of the theory.")
   (senv :initarg :senv
	 :initform nil
	 :accessor th=senv
	 :documentation "An environment containing all sorts, term and subsort declarations of the theory.")
   (assumptions :initform (make-hash-table :test #'eq)
	       :reader th=assumptions
	       :documentation "A hashtable storing the assumptions (axioms, definitions and theorems) of the theory.")
   (problems :initform (make-hash-table :test #'eq)
	     :reader th=problems
	     :documentation "A hashtable storing the unproved problems of the theory.")
   (methods :initform (make-hash-table :test #'eq)
	    :reader th=methods
	    :documentation "A hashtable storing all accessible methods in the theory.")
   (simplifiers :initform (make-hash-table :test #'eq)
                :reader th=simplifiers
                :documentation "A hashtable storing all simplifiers in the theory.")
   (connectives :initarg :connectives
		:initform nil
		:accessor th=connectives
		:documentation "A list in that all connectives and there according types are stored.")
  (agent-default :initform nil
		 :accessor th=agent-default
		 :documentation "Contains the theory-defaults for the agent mechanism."))
  (:documentation "The class for all THEORIES."))


(defvar th*theory-hash-table (make-hash-table :test #'eq)
  "A hash table, that keeps track of all the existing theories at any time.")

;; Constructors and Queries
#{\subsection{Constructors and Query Functions}#}

(defun th~create (name imports help)
  (declare (edited  "14-FEB-2000" "01-APR-1997" "19-JUL-1995 21:23")
	   (authors Pollet Fehrer SORGE)
	   (input   "A symbol, a parent theory or a list of parent theories, and a help string.")
	   (effect  "Creates an instance of th+theory.")
	   (value   "An object of type th+theory."))
  (let* ((thname (th=read-string name))
	 (imps (if (listp imports)
		   imports
		 (list imports)))
	 (env (env~create (mapcar #'th~env imps) thname))
	 (senv (sort~env-create :parents (mapcar #'th~senv imps)
				:name thname
				:unsortedenv env)))
    (make-instance 'th+theory
		   :name name
		   :imports imps
		   :env  env
		   :senv senv
		   :help help)))
  
(defgeneric th=read-string (name)
  (declare (edited  "20-JUL-1995 13:55")
	   (authors SORGE)
	   (input   "A name (symbol or string).")
	   (effect  "None.")
	   (value   "NAME as an uppercase string."))
  (:method (name)
	   (error "~A has to be of type symbol or string" name))
  (:method ((name keim+name))
	   (th=read-string (keim~name name)))
  (:method ((name symbol))
	   (read-from-string (symbol-name name)))
  (:method ((name string))
	   (read-from-string name)))

(defun th~p (obj)
  (declare (edited  "19-JUL-1995 21:57")
	   (authors SORGE)
	   (input   "A lisp object.")
	   (effect  "None.")
	   (value   "T if OBJ is a theory."))
  (typep obj 'th+theory))

(defgeneric th~find-theory (name)
  (declare (edited  "19-JUL-1995 22:40")
	   (authors SORGE)
	   (input   "A name of a theory (symbol, string or even a theory).")
	   (effect  "None.")
	   (value   "The theory with this name or NIL if none exists."))
  (:method (name)
	   (gethash (th=read-string name) th*theory-hash-table))
  (:method ((name th+theory))
	   (gethash (th=read-string (keim~name name)) th*theory-hash-table)))

(defun th~find-assumption (name theory)
  (declare (edited  "01-APR-1997")
	   (authors Fehrer Lassaad) 
	   (input   "An assumption name, and that of a theory (symbol, string or even a theory).")
	   (effect  "None.")
	   (value   "The assumption of the theory with this name or NIL if none exists."))
  (let ((thy (th~find-theory theory)))
    (if thy
	(let ((ass (gethash (th=read-string name) (th=assumptions thy))))
	  (if ass ass
	    (some #'(lambda (x) (th~find-assumption name x)) (th~imports theory))))
      (error "TH~~FIND-ASSUMPTION: The theory ~A does not exist." theory))
    ))

(defun th~find-problem (name theory)
  (declare (edited  "22-MAY-1997")
	   (authors Sorge) 
	   (input   "A problem name, and that of a theory (symbol, string or even a theory).")
	   (effect  "None.")
	   (value   "The problem of the theory with this name or NIL if none exists."))
  (let ((thy (th~find-theory theory)))
    (if thy
	(let ((ass (gethash (th=read-string name) (th=problems thy))))
	  (if ass ass
	    (some #'(lambda (x) (th~find-problem name x)) (th~imports theory))))
      (error "TH~~FIND-PROBLEM: The theory ~A does not exist." theory))
    ))

(defun th~find-problem&assumption (name theory)
  (declare (edited  "22-MAY-1997")
	   (authors Sorge) 
	   (input   "A problem or assumption name, and that of a theory (symbol, string or even a theory).")
	   (effect  "None.")
	   (value   "The problem (or assumption) of the theory with this name or NIL if none exists."))
  (let ((thy (th~find-theory theory)))
    (if thy
	(let ((ass (th~find-assumption name theory))
	      (problem (th~find-problem name theory)))
	  (cond ((and ass problem)
		 (warn ";;;TH~~FIND-PROBLEM&ASSUMPTION: There exists both a problem and an assumption with the name ~A." name)
		 (values problem ass))
		(ass ass)
		(problem problem)))
      (error "TH~~FIND-PROBLEM&ASSUMPTION: The theory ~A does not exist." theory))
    ))


(defgeneric th~defined-in (item)
  (declare (edited  "21-JUL-1999")
	   (authors Pollet)
	   (input   "The name of a constant.")
	   (effect  "nothing")
	   (value   "The theory that contains the declaration of the constant."))
  (:method ((item symbol))
	   (let* ((theories (th~all-theories))
		  (in-th (mapcan #'(lambda (th)
				     (when (env~lookup-object item (th~env th))
				       (list th)))
				 theories)))
	     (car (remove-if #'(lambda (th1)
				 (let ((parents (th~imports th1)))
			      (some #'(lambda (th2)
					(find th2 in-th))
				    parents)))
			     in-th))))	     
  (:method ((item integer))                  
	   (th~find-theory 'natural))
  (:method ((item rational))
	   (th~find-theory 'rational))
  (:method ((item float))
	   (th~find-theory 'rational))
  (:method ((item real))
	   (th~find-theory 'real))
  (:method ((item keim+name))
	   (th~defined-in (keim~name item))))



;;;
;; simplifiers
;;


(defclass th+simplifier (help+help)
  ((theory :initarg :theory
           :initform nil
           :reader th=simplifier-theory
           :documentation "The theory for which the simplifier is valid.")
   (antecedent :initarg :antecedent
               :initform nil
               :reader th~simplifier-antecedent
               :documentation "The antecedent of the rewrite rule")
   (consequent :initarg :consequent
               :initform nil
               :reader th~simplifier-consequent
               :documentation "The consequent of the rewrite rule")
   (assumption :initarg :assumption
	       :initform nil
	       :reader th~simplifier-assumption
	       :documentation "The assumption of an simplifier.")
   (bindable-type-vars :initarg :bindable-type-vars
          	       :initform nil
		       :reader th~simplifier-bindable-type-vars
		       :documentation "The bindable variables in a simplifier.")
   (status  :initarg :status
            :initform local
            :reader th=simplifier-status
            :documentation "Global or local simplifier?"))
  (:documentation "The class for all Simplifiers."))


(defmacro th~defsimplifier (name theory &rest attribs)  
  (declare (edited  "04-DEC-1997")
	   (authors Konrad AMEIER) ;; Ameier added the handling of type-variables !!
	   (input   "A written representation of a TH+SIMPLIFIER.")
	   (effect  "creates the simplifier and enters it into the theory.")
           (value   "None."))
                                        ;(print '(entered defsimplifier))
  (if (string-equal (car theory) :in)
      (setq theory (cadr theory))
    (setq theory (car theory)))
  (if (th~find-theory theory)
      (let ((theory (th~find-theory theory))
	    (node nil)
            (status :local)
            (direction :lr)
            (help ""))
        (do ((attribs (cdr attribs) (cdr attribs))
	     (attrib (car attribs) (car attribs)))
	    ((and (null attrib) (null attribs)))
	  (if (consp attrib)
	      (let ((carattrib (if (symbolp (car attrib)) 
				   (symbol-name (car attrib)) 
				 (car attrib))))
		(cond ((string-equal carattrib :equation)
                       (setq node (th~find-assumption (cadr attrib) theory)))
                      ((string-equal carattrib :direction)
                       (setq direction (cadr attrib)))
                      ((string-equal carattrib :status)
                       (setq status (cadr attrib)))
                      ((string-equal carattrib :help)
                       (setq help (cadr attrib)))
		      (t
		       (error
                        ";;;th~~defsimplifier ~A: Not expecting ~S" name
                        attrib))))
	    (error
             ";;;th~~defsimplifier ~A: ~A is not a valid attribute"
             name attrib)))
        (let* ((ass-form (th~ass-formula node))
	       (equation (if (data~schema-p ass-form)
			     (logic~remove-leading-universal-quantors
			      (data~schema-range ass-form)
			      :theory theory)
			   (logic~remove-leading-universal-quantors
			    ass-form
			    :theory theory)))
	       (bindable-type-vars (when (data~schema-p ass-form)
				     (data~schema-domain ass-form))))				     
          (setf (gethash (th=read-string name) (th=simplifiers theory))
                (make-instance 'th+simplifier
                               :name name
                               :assumption node
                               :status status
                               :theory theory
			       :bindable-type-vars bindable-type-vars
                               :antecedent (if (string-equal (symbol-name
                                                              direction) :lr)
                                               (first
						(data~appl-arguments equation))
					     (second
					      (data~appl-arguments equation)))
                               :consequent (if (string-equal
                                                (symbol-name direction)
                                                :lr)
					       (second
						(data~appl-arguments equation))
					     (first
					      (data~appl-arguments equation)))
                               :direction direction
                               :help help))))
    (error ";;;th~~defsimplifier ~A: ~A is not an existing theory" name theory)))



;; basic functionality for simplifiers

(defgeneric th~simplifiers (theory)
  (declare (edited  "04-DEC-1997")
	   (authors Konrad)
	   (input   "a theory")
	   (effect  "none")
	   (value   "all simplifiers defined for that theory."))
  (:method (theory)
	   (error "~A is not of type TH+THEORY" theory))
  (:method ((theory symbol))
	   (let ((th (th~find-theory theory)))
	     (if th
		 (th~simplifiers th)
	       (error "Theory ~A does not exist" theory))))
  (:method ((theory string))
	   (let ((th (th~find-theory theory)))
	     (if th
		 (th~simplifiers th)
	       (error "Theory ~A does not exist" theory))))
  (:method ((theory th+theory))
	   (th=hash-2-list (th=simplifiers theory))))


(defun th~simplifiers-recursed (theory)
  (declare (edited  "21-APR-1998" "01-APR-1997" "22-FEB-1997")
	   (authors Jzimmer Fehrer Lassaad Konrad)
	   (input   "A theory.")
	   (effect  "None.")
	   (value   "A list containing all the local and imported simplifiers"
		    "of THEORY."))
  ;;jzimmer: th~imports  liefert eine Liste! 
  (when theory
    (if (listp theory)
	(apply #'append
	       (mapcar #'th~simplifiers-recursed theory))
      (let ((import-ths (th~imports theory)))
	(if import-ths
	    (union (th~simplifiers theory)
		   (delete-if #'(lambda (x)
				  (string-equal (symbol-name
						 (th=simplifier-status x))
						"LOCAL"))
			      (th~simplifiers-recursed import-ths)))
	  (th~simplifiers theory))))))


(defun th~simplifier-p (obj)
  (declare (edited  "11-JUN-1997")
	   (authors Konrad)
	   (input   "A lisp object.")
	   (effect  "None.")
	   (value   "T if OBJ is an simplifier."))
  (typep obj 'th+simplifier))


;; Readers

#{\subsection{Accessor functions}#}

(defun th~all-theories ()
  (declare (edited  "17-JUN-1997")
	   (authors Sorge)
	   (input   "Rien.")
	   (effect  "Nada.")
	   (value   "A list of all existing theories."))
  (let ((th-list nil))
    (maphash #'(lambda (key val)
		 (declare (ignore key))
		 (setf th-list (cons val th-list)))
	     th*theory-hash-table)
    th-list))

(defgeneric th~imports (theory)
  (declare (edited  "19-JUL-1995 21:52")
	   (authors SORGE)
	   (input   "A theory.")
	   (effect  "None.")
	   (value   "The list of subtheories of this theory."))
  (:method (theory)
	   (error "~A is not of type TH+THEORY" theory))
  (:method ((theory symbol))
	   (let ((th (th~find-theory theory)))
	     (if th
		 (th~imports th)
	       (error "Theory ~A does not exist" theory))))
  (:method ((theory string))
	   (let ((th (th~find-theory theory)))
	     (if th
		 (th~imports th)
	       (error "Theory ~A does not exist" theory))))
  (:method ((theory th+theory))
	   (th=imports theory)))

(defun th~imports-recursed (theory)
  (declare (edited  "01-APR-1997" "22-FEB-1997")
	   (authors Fehrer Lassaad)
	   (input   "A theory.")
	   (effect  "None.")
	   (value   "All the import theories of THEORY."))
  (let ((import-ths (th~imports theory)))
    (when import-ths
      (union import-ths (th=imports-recursed import-ths)))
    ))

(defun th=imports-recursed (theories)
  (declare (edited  "01-APR-1997")
	   (authors Fehrer)
	   (input   "A list of theories")
	   (effect  "none")
	   (value   "the transitive closure of the imports relation"))
  (when theories
    (union (th~imports-recursed (car theories))
	   (th=imports-recursed (cdr theories)))))

(defgeneric th~env (theory)
  (declare (edited  "19-JUL-1995 21:52")
	   (authors SORGE)
	   (input   "A theory.")
	   (effect  "None.")
	   (value   "An environment containing the declarations of the theory."))
  (:method (theory)
	   (error "~A is not of type TH+THEORY" theory))
  (:method ((theory symbol))
	   (let ((th (th~find-theory theory)))
	     (if th
		 (th~env th)
	       (error "Theory ~A does not exist" theory))))
  (:method ((theory string))
	   (let ((th (th~find-theory theory)))
	     (if th
		 (th~env th)
	       (error "Theory ~A does not exist" theory))))
  (:method ((theory th+theory))
	   (th=env theory)))

(defgeneric th~senv (theory)
  (declare (edited  "14-FEB-2000")
	   (authors Pollet)
	   (input   "A theory.")
	   (effect  "None.")
	   (value   "An environment containing the sort declarations of the theory."))
  (:method (theory)
	   (error "~A is not of type TH+THEORY" theory))
  (:method ((theory symbol))
	   (let ((th (th~find-theory theory)))
	     (if th
		 (th~senv th)
	       (error "Theory ~A does not exist" theory))))
  (:method ((theory string))
	   (let ((th (th~find-theory theory)))
	     (if th
		 (th~senv th)
	       (error "Theory ~A does not exist" theory))))
  (:method ((theory th+theory))
	   (th=senv theory)))

(defsetf th~connectives (theory) (connectives)
  `(setf (th=connectives ,theory) ,connectives))

(defgeneric th~connectives (theory)
  (declare (edited  "12-AUG-1998 17:52")
	   (authors AMEIER)
	   (input   "A theory.")
	   (effect  "None.")
	   (value   "The list of the connectives of the theory."))
  (:method (theory)
	   (error "~A is not of type TH+THEORY" theory))
  (:method ((theory symbol))
	   (let ((th (th~find-theory theory)))
	     (if th
		 (th~connectives th)
	       (error "Theory ~A does not exist" theory))))
  (:method ((theory string))
	   (let ((th (th~find-theory theory)))
	     (if th
		 (th~connectives th)
	       (error "Theory ~A does not exist" theory))))
  (:method ((theory th+theory))
	   (th=connectives theory)))

(defun th=hash-2-list (hash)
  (declare (edited  "19-JUL-1995 22:58")
	   (authors SORGE)
	   (input   "A hashtable.")
	   (effect  "None.")
	   (value   "A list containing the entries of the hash-table."))
  (let ((list '(nil)))
    (maphash #'(lambda (key val)
		 (declare (ignore key))
		 (push val list))
	     hash)
    (delete nil list)))

		  
(defgeneric th~assumptions (theory)
  (declare (edited  "26-JUL-1995 16:46")
	   (authors SORGE)
	   (input   "A theory.")
	   (effect  "None.")
	   (value   "A list containing all assumptions of the theory."))
  (:method (theory)
	   (error "~A is not of type TH+THEORY" theory))
  (:method ((theory symbol))
	   (let ((th (th~find-theory theory)))
	     (if th
		 (th~assumptions th)
	       (error "Theory ~A does not exist" theory))))
  (:method ((theory string))
	   (let ((th (th~find-theory theory)))
	     (if th
		 (th~assumptions th)
	       (error "Theory ~A does not exist" theory))))
  (:method ((theory th+theory))
	   (th=hash-2-list (th=assumptions theory))))

(defun th~assumptions-recursed (theory)
  (declare (edited  "01-APR-1997" "22-FEB-1997")
	   (authors Fehrer Lassaad)
	   (input   "A theory.")
	   (effect  "None.")
	   (value   "A list containing all the local and imported assumptions"
		    "of THEORY."))
  (let ((import-ths (th~imports theory)))
    (if import-ths
	(union (th~assumptions theory)
	       (th=assumptions-recursed import-ths))
      (th~assumptions theory))
    ))

(defun th=assumptions-recursed (theories)
  (declare (edited  "01-APR-1997")
	   (authors Fehrer)
	   (input   "a list of theories")
	   (effect  "none")
	   (value   "a list containing the union of all local and imported assumptions"))
  (when theories
    (union (th~assumptions-recursed (car theories))
	   (th=assumptions-recursed (cdr theories)))))


(defun th~axioms (theory)
  (declare (edited  "19-JUL-1995 21:52")
	   (authors SORGE)
	   (input   "A theory.")
	   (effect  "None.")
	   (value   "A list containing the axioms of the theory."))
  (delete nil (mapcar #'(lambda (x) (when (th~axiom-p x) x))
		      (th~assumptions theory))))

(defun th~definitions (theory)
  (declare (edited  "19-JUL-1995 21:52")
	   (authors SORGE)
	   (input   "A theory.")
	   (effect  "None.")
	   (value   "A list containing the definitions of the theory."))
  (delete nil (mapcar #'(lambda (x) (when (th~definition-p x) x))
		      (th~assumptions theory))))

(defun th~definitions-recursed (theory)
  (declare (edited  "19-Oct-1997 21:52")
	   (authors kk)
	   (input   "A theory.")
	   (effect  "None.")
	   (value   "A list containing the definitions of the theory."))
  (delete nil (mapcar #'(lambda (x) (when (th~definition-p x) x))
		      (th~assumptions-recursed theory))))

(defun th~theorems (theory)
  (declare (edited  "22-MAY-1997 13:26")
	   (authors SORGE)
	   (input   "A theory.")
	   (effect  "None.")
	   (value   "A list containing the proven problems of the theory."))
  (delete nil (mapcar #'(lambda (x) (when (th~problem-p x) x))
		      (th~assumptions theory))))

(defun th~theorems-recursed (theory)
  (declare (edited  "11-SEP-1998" "22-MAY-1997 13:26")
	   (authors Pollet SORGE)
	   (input   "A theory.")
	   (effect  "None.")
	   (value   "A list containing the proven problems of the theory."))
  (delete nil (mapcar #'(lambda (x) (when (th~problem-p x) x))
		      (th~assumptions-recursed theory))))

(defgeneric th~problems (theory)
  (declare (edited  "26-JUL-1995 16:46")
	   (authors SORGE)
	   (input   "A theory.")
	   (effect  "None.")
	   (value   "A list containing all problems of the theory."))
  (:method (theory)
	   (error "~A is not of type TH+THEORY" theory))
  (:method ((theory symbol))
	   (let ((th (th~find-theory theory)))
	     (if th
		 (th~problems th)
	       (error "Theory ~A does not exist" theory))))
  (:method ((theory string))
	   (let ((th (th~find-theory theory)))
	     (if th
		 (th~problems th)
	       (error "Theory ~A does not exist" theory))))
  (:method ((theory th+theory))
	   (th=hash-2-list (th=problems theory))))

(defun th~problems-recursed (theory)
  (declare (edited  "22-MAY-1997" "01-APR-1997" "22-FEB-1997")
	   (authors Sorge Fehrer Lassaad)
	   (input   "A theory.")
	   (effect  "None.")
	   (value   "A list containing all the local and imported assumptions"
		    "of THEORY."))
  (let ((import-ths (th~imports theory)))
    (if import-ths
	(union (th~problems theory)
	       (th=problems-recursed import-ths))
      (th~problems theory))
    ))

(defun th=problems-recursed (theories)
  (declare (edited  "22-MAY-1997" "01-APR-1997")
	   (authors Sorge Fehrer)
	   (input   "A list of theories")
	   (effect  "None")
	   (value   "A list containing the union of all local and imported assumptions"))
  (when theories
    (union (th~problems-recursed (car theories))
	   (th=problems-recursed (cdr theories)))))

(defgeneric th~methods (theory)
  (declare (edited  "19-JUL-1995 21:52")
	   (authors SORGE)
	   (input   "A theory.")
	   (effect  "None.")
	   (value   "A list containing the methods of the theory."))
  (:method (theory)
	   (error "~A is not of type TH+THEORY" theory))
  (:method ((theory symbol))
	   (let ((th (th~find-theory theory)))
	     (if th
		 (th~methods th)
	       (error "Theory ~A does not exist" theory))))
  (:method ((theory string))
	   (let ((th (th~find-theory theory)))
	     (if th
		 (th~methods th)
	       (error "Theory ~A does not exist" theory))))
  (:method ((theory th+theory))
	   (th=hash-2-list (th=methods theory))))


(defgeneric th~agent-default (theory)
  (declare (edited  "24-MAY-2001")
	   (authors Pollet)
	   (input   "A theory.")
	   (effect  "Sets the defaults for the agents mechanism.")
	   (value   "-"))
  (:method (theory)
	   (error "~A is not of type TH+THEORY" theory))
  (:method ((theory symbol))
	   (let ((th (th~find-theory theory)))
	     (if th
		 (th~agent-default th)
	       (error "Theory ~A does not exist" theory))))
  (:method ((theory string))
	   (let ((th (th~find-theory theory)))
	     (if th
		 (th~agent-default th)
	       (error "Theory ~A does not exist" theory))))
  (:method ((theory th+theory))
	   (let ((code (th=agent-default theory)))
	     (when code (eval code)))))

;;; The next two functions aren't actually used. But they could be useful someday in a distant future.
#|
(defun th=theory-repeated? (name imports)
  (declare (edited  "20-JUL-1995 14:43")
	   (authors SORGE)
	   (input   "A name of a theory to be created and a list of its imports.")
	   (effect  "None.")
	   (value   "T if name is not in imports, otherwise nil.")
	   (remark  "This function makes sure that a theory is not inside a given set of subtheories."))
  (let ((theory (th=read-string name))
	(subtheories (mapcar #'(lambda (x)
				 (typecase x
				   (th+theory (th=read-string (keim~name x)))
				   (t (th=read-string x))))
			     imports)))
    (not (member theory subtheories :test #'string-equal))))


(defun th=theory-recursed? (name imports)
  (declare (edited  "20-JUL-1995 15:22")
	   (authors SORGE)
	   (input   "A name of a theory and the importlist of the theory.")
	   (effect  "None.")
	   (value   "T if no subtheory contains the theory.")
	   (remark  "This function prevents any repetitions of theories,"
		    "i.e. no subtheory of a theory can have this theory as a subtheory"
		    "(sounds confusing, doesn't it?)."))
  (when (th=theory-repeated? name imports)
    (dolist (closure imports t)
      (unless (th=theory-recursed? name (th~imports (th~find-theory x)))
	(return nil)))))
|#	     
;;; above is discarded.....



#{\subsection{\post\ syntax}#}


(defmacro th~deftheory (name &rest attribs)
  (declare (edited  "14-FEB-2000" "20-JUL-1995 16:58")
	   (authors Pollet SORGE KOHLHASE)
           (input   "A written representation of a th+theory."
		    "Here is an example of the syntax:  
                     \\begin{codebox} 
                     \\vspace{.1cm}
 (th~deftheory my-theory  
	      (uses real-theory)
              (author URL)
              (credit URL)
              (reference (book1.ref book2.ref))
              (date URL)
	      (type-constants o i real) 
	      (type-variables aa bb)  
	      (constants (one real) 
			 (zero real))
	      (help \"My theory axioms .....\"))
 \\end{codebox}")
           (effect  "Read the theory and (if necessary) all of its ancestors,"
		    "construct a th+theory object and register it.")
           (value   "None."))
  (when (th~find-theory name)
    (error "~A is already an existing theory!" name))
  (let* ((help "")
	(imports nil)
	(env  (env~create nil name))
	(senv (sort~env-create :name name :unsortedenv env))
	(connectives nil)
	)
    (do ((attribs (cdr attribs) (cdr attribs))
	 (attrib (car attribs) (car attribs)))
	((and (null attrib) (null attribs)))
      (if (consp attrib)
	  (let ((carattrib (if (symbolp (car attrib)) 
                               (symbol-name (car attrib)) 
			     (car attrib))))
            (cond ((string-equal carattrib :help) (setq help (cadr attrib)))
                  ((string-equal carattrib :author)); do nothing
                  ((string-equal carattrib :credit)); do nothing
                  ((string-equal carattrib :reference)); do nothing
                  ((string-equal carattrib :date)); do nothing
                  ((string-equal carattrib :uses)
		   (let ((subths (mapcar #'(lambda (th)
					     (th~require-completely th :force nil))
					 (cdr attrib))))

		     ;; hier muss eigentlich th~require-completly stehen. Um das ganze aber auch benutzen zu koennen waehrend
		     ;; keim noch nicht complett nach keim-3 importiert ist, braucht man hier das th~require-only
		     ;; -> bitte nachher rueckgaengig machen HACK MADE BY AMEIER

		     (if (some #'null subths)
			 (error "theory ~S does not exist" (nth (position nil subths) (cdr attrib)))
		       (setq imports subths))
		     (setq connectives (remove-duplicates (apply 'append (mapcar #'th~connectives subths))
							  :test (lambda (it1 it2)
								  (and (eq (first it1) (first it2))
								       (eq (second it1) (second it2))))))
		     (setq env (env~create (mapcar #'th~env subths) name))
		     (setq senv (sort~env-create :name name :unsortedenv env :parents (mapcar #'th~senv subths)))))
		  
		  
					;                   (let ((subth (th~require (cadr attrib))))
					;                     (if subth (setq imports subth)
					;                       (error "~A is not a valid theory" (cadr attrib)))
					;                     (setq env (env~create (th~env subth) name))))

		  ((string-equal carattrib :connectives)
		   (let* ((pairs (rest attrib)))
		     (setq connectives (append connectives
					       (mapcar #'(lambda (pair)
							   (let* ((constant (env~lookup-object (first pair) env))
								  (type (string (second pair))))
							     (cond ((or (null constant)
									(and (null (term~constant-p constant))
									     (null (term~schema-p constant))
									     (null (term~constant-p (data~schema-range constant)))))
								    (error "~A in the list of connectives is either not in the environment of the theory or not a constant." (first pair)))
								   ((null (find type th*current-connective-types :test #'string=))
								    (error "~A is not an allowed type for a connective."
									   (second pair)))
								   (t
								    (list constant type)))))
						       pairs)))))
		  ((string-equal carattrib :constants)
		   (post~read-object (cdr attrib) env :constants))
		  ((string-equal carattrib :variables)
		   (post~read-object (cdr attrib) env :variables))
		  ((string-equal carattrib :type-constants)
		   (post~read-object (cdr attrib) env :type-constants))
		  ((string-equal carattrib :type-variables)
		   (post~read-object (cdr attrib) env :type-variables))
		  ((string-equal carattrib :type-constructors)
		   (post~read-object (cdr attrib) env :type-constructors))
		  (t
 	           (error ";;;th~~deftheory ~A: Not expecting ~S" name attrib))))
          (error ";;;th~~deftheory ~A: ~A is not a valid attribute specification" name attrib)))
                (let ((new-th (make-instance 'th+theory
					     :name name
					     :imports imports 
					     :env env
					     :senv senv
					     :connectives connectives
					     :help help)))
		  (setf (gethash (th=read-string name) th*theory-hash-table)
			new-th))))


#{\section{Assumptions}\label{mod:th-ass}
An important thing in a mathematical data base are of course assumptions. \keim provides
several types of assumptions:
\begin{description}
\item[Axioms:] Assumptions that only depend on already defined symbols of the theory.
\item[Definitions:] Once a theory is created, it can only be extended by using 
definitions. Therefore a {\em definition} might not only consist of one formula but of 
several declarations of the type {\em type-variables} and {\em constans} as well.
\item[Theorems:] A {\em theorem} has attached one or more proofs, derived from 
assumptions in the theory and further assumptions made within the {\em theorem} itself.
Additionally a {\em theorem} can contain a complete local environment of its own, 
including declarations for {\em constants}, {\em variables}, {\em type-constants} and
{\em type-variables}.
\item[Lemmata:] These are the same as {\em theorems}. The distinction between those 
is left to the taste of the user.
\end{description}
#}



;; Datastructures for assumptions

(defclass th+assumption (help+help)
  ((theory :initarg :theory
	   :initform nil
	   :reader th=assumption-theory
	   :documentation "The theory for which the assumption is valid.")
   (node :initarg :node
	 :initform nil
	 :reader th=assumption-node
	 :documentation "The node containing the formula of the assumption."))
  (:documentation "The basic Datastructure for all assumptions in a theory."))


(defclass th+axiom (th+assumption)
  ()
  (:documentation "The class for all axioms."))


(defclass th+def (th+assumption)
  ((constant :initarg :constant
             :reader th~definition-constant))
  (:documentation "The polymorph constant derived from the name of the def."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; We define some more dummy justification methods for
;; theory perposes, such as: AXIOM, DEFINITION, THEOREM, LEMMA, etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defdummy axiom (help "Dummy for axioms."))

(infer~defdummy def (help "Dummy for definitions."))

(infer~defdummy thm (help "Dummy for theorems."))

(infer~defdummy lemma (help "Dummy for lemmata."))

(infer~defdummy prop (help "Dummy for propositions."))

(infer~defdummy ass (help "Dummy for assumptions."))

(infer~defdummy local-def (help "Dummy for local definitions"))

#{\subsection{Auxiliary functions}#}
;; Predicates

(defun th~assumption-p (obj)
  (declare (edited  "11-JUN-1997")
	   (authors Sorge)
	   (input   "A lisp object.")
	   (effect  "None.")
	   (value   "T if OBJ is an assumption."))
  (typep obj 'th+assumption))
  
(defun th~axiom-p (obj)
  (declare (edited  "17-AUG-1995 16:16")
	   (authors SORGE)
	   (input   "A lisp object.")
	   (effect  "None.")
	   (value   "T if OBJ is an axiom."))
  (typep obj 'th+axiom))

(defun th~definition-p (obj)
  (declare (edited  "17-AUG-1995 16:16")
	   (authors SORGE)
	   (input   "A lisp object.")
	   (effect  "None.")
	   (value   "T if OBJ is a definition."))
  (typep obj 'th+def))

(defun th~problem-p (obj)
  (declare (edited  "17-AUG-1995 16:16")
	   (authors SORGE)
	   (input   "A lisp object.")
	   (effect  "None.")
	   (value   "T if OBJ is a problem."))
  (typep obj 'prob+problem))


(defgeneric th~assumption-method-p (method)
  (declare (edited  "11-JUN-1997")
	   (authors Sorge Lassaad)
	   (input   "An inference-method.")
	   (effect  "None.")
	   (value   "T if the method corresponds to any of the theory dummy-methods."))
  (:method (method)
	   (error ";;;TH~~ASSUMPTION-METHOD-P: ~A must be an inference-method." method))
  (:method ((method infer+inference))
	   nil)
  (:method ((method infer+dummy))
	   (find method (mapcar #'infer~find-method '(axiom def thm lemma ass prop))))
  (:method ((just just+justification))
	   (th~assumption-method-p (just~method just)))
  (:method ((node node+node))
	   (th~assumption-method-p (node~just-method node))))
	   
;; accessors


(defgeneric th~ass-node (assumption)
  (declare (edited  "25-MAY-1997 17:41")
	   (authors SORGE)
	   (input   "An assumption.")
	   (effect  "None.")
	   (value   "The node containing the formula of the assumption."))
  (:method (assumption)
	   (error "~A is not of the right type" assumption))
  (:method ((assumption th+assumption))
	   (th=assumption-node assumption))
  (:method ((problem prob+problem))
	   (cons (prob~conclusion problem)
		 (prob~assumptions problem))))
		
		 

(defgeneric th~ass-formula (assumption)
  (declare (edited  "27-JUL-1995 17:05")
	   (authors SORGE)
	   (input   "An assumption.")
	   (effect  "None.")
	   (value   "The formula of the assumption."))
  (:method (assumption)
	   (error "~A is not of the right type" assumption))
  (:method ((assumption th+assumption))
	   (node~formula (th~ass-node assumption)))
  (:method ((assumption th+def))
	   (th=assumption-node assumption))
  (:method ((problem prob+problem))
	   (node~formula (prob~conclusion problem))))

(defgeneric th~ass-theory (assumption)
  (declare (edited  "27-JUL-1995 17:05")
	   (authors SORGE)
	   (input   "An assumption.")
	   (effect  "None.")
	   (value   "The theory in which the assumption is valid."))
  (:method (assumption)
	   (error "~A is not of the right type" assumption))
  (:method ((assumption th+assumption))
	   (th=assumption-theory assumption))
  (:method ((assumption prob+problem))
	   (prob~theory assumption)))

  
;; Conversions

(defgeneric th~problem2theorem (problem &optional proofs)
  (declare (edited  "25-MAY-1997 22:04")
           (authors SORGE)
           (input   "A problem and optional a list of proofs.")
           (effect  "Changes the problem to a proven problem and adds the proofs.")
           (value   "The new theorem and a boolean. If everything is going well or there already exists"
		    "an assumption with the name of problem and the user chooses to replace it with the"
		    "given problem the boolean is set to T. In all other cases the boolean is set to NIL."))
  (:method (problem &optional proofs)
           (declare (ignore proofs))
           (error "~A must be of type PROB+PROBLEM" problem))
  (:method ((problem string) &optional proofs)
	   (let ((prob (prob~find-problem problem)))
	     (if prob
		 (th~problem2theorem prob proofs)
	       (error "Problem ~A does not exist" problem))))
  (:method ((problem symbol) &optional proofs)
	   (let ((prob (prob~find-problem problem)))
	     (if prob
		 (th~problem2theorem prob proofs)
	       (error "Problem ~A does not exist" problem))))
  (:method ((problem prob+problem) &optional proofs)
	   (let* ((name (th=read-string (keim~name problem)))
		  (theory (prob~theory problem))
		  (y-n (if (th~find-assumption name theory)
			   (inter~prompt-for-input (asi~create) (format nil ";;;th~~problem2theorem: There exists already an assumption with the name ~A in the theory ~A. Would you like to overwrite it? (y/n) :" name (keim~name theory)) 'boolean)
			 t)))
	     (when y-n
	       (setf (prob~status problem) 'proven)
	       (remhash name (th=problems theory))
	       (setf (gethash name (th=assumptions theory)) problem)  
	       (when proofs (prob~add-proof problem proofs)))
	     (values problem y-n))))


;; Functions for insertion


(defun th=env-entry (key object env)
  (declare (edited  "28-AUG-1995 18:01")
	   (authors SORGE)
	   (input   "A key, an object and an environment.")
	   (effect  "Enters a new object in an environment. Any old objects with the same key are removed from the environment.")
	   (value   "The object."))
;;  (loop
   (if (env~query key env)
       (env~remove key env))
;;     (return)))
  (env~enter key object env)
  object)
  

#{\subsection{\post\ syntax}#}

;; Macros for defining assumptions

(defmacro th~defaxiom (name theory &rest attribs)
  (declare (edited  "20-JUL-1995 16:58")
	   (authors SORGE)
           (input   "A written representation of a TH+AXIOM."
		    "Here is an example of the syntax:
                     \\begin{codebox}
                     \\vspace{.1cm} 
 (th~defaxiom my-axiom
	     (in my-theory)
             (author URL)
             (credit URL)
             (reference (book1.ref1 book2.ref2))
             (date DDMMJJJJ)
	     (formula (forall (lam (volker human)
				   (stupid volker))))
	     (help \"Most important axiom!\"))
            	    \\end{codebox}"
		    "\\newline That means an axiom is represented in terms of its name and its formula.")
           (effect  "Read the axiom, construct a TH+AXIOM object and register it.")
           (value   "None."))

  (if (string-equal (car theory) :in)
      (setq theory (cadr theory))
    (setq theory (car theory)))
  (if (th~find-theory theory)
      (let ((theory (th~find-theory theory))
	    (type-vars) (node) (termdecl)
	    (sort) (post-term)   
	    (env (th~env theory))
	    (senv (th~senv theory))
	    (help ""))
	(do ((attribs (cdr attribs) (cdr attribs))
	     (attrib (car attribs) (car attribs)))
	    ((and (null attrib) (null attribs)))
	  (if (consp attrib)
	      (let ((carattrib (if (symbolp (car attrib)) 
				   (symbol-name (car attrib)) 
				 (car attrib))))
		(cond ((string-equal carattrib :type-variables)
		       (post~read-object (cdr attrib) env :type-constants-multiple)

		       ;; waehrend des einlesens des axioms sind die typ-variablen konstanten
		       ;; nachher werden sie zu typ-variablen geaendert !!

		       (setq type-vars (cdr attrib)))
		      ((string-equal carattrib :author)) ; do nothing
		      ((string-equal carattrib :credit)) ; do nothing
                      ((string-equal carattrib :reference)) ; do nothing
		      ((string-equal carattrib :date)) ; do nothing
		      ((string-equal carattrib :termdecl)
			 (setq termdecl t))
		      ((string-equal carattrib :sort)
			 (setq sort t))
		      ((string-equal carattrib :formula)

		       (setq post-term (cadr attrib))
							    
		       (when (and type-vars (null (post~all-types-checker (cadr attrib) :forbidden 't)))
			 (error "~%During reading axiom ~A, it is not allowed to use 'all-types' in the term" (cadr attrib)))
		       
		       (let ((term (post~read-object (cadr attrib) env :existing-term)))
			 
			 ;; wandle typ-variablen um in variablen 
			 (mapc #'(lambda (type-var)
				   (let* ((obj (env~lookup-object type-var env)))
				     (change-class obj 'type+variable)))
			       type-vars)
			 
			 ;; entferne Typ-variablen aus dem environment
			 (mapc #'(lambda (var) (env~remove var env)) type-vars)
			 
			 (setq node (node~create 			     
				     name
				     (term~schema-close term)
				     (just~create (infer~find-method 'axiom) nil)))))
		      ((string-equal carattrib :help)    (setq help (cadr attrib)))
		      (t
		       (error ";;;th~~defaxiom ~A: Not expecting ~S" name attrib))))
	    (error ";;;th~~defaxiom ~A: ~A is not a valid attribute specification" name attrib)))
	
	(when termdecl (post~read-object (list post-term name) senv :termdecl))
	(when sort (post~read-object post-term senv :sort-constant))
	(setf (gethash (th=read-string name) (th=assumptions theory))
	      (make-instance 'th+axiom
			     :name name
			     :theory theory
			     :node node
			     :help help)))

    (error ";;;th~~defaxiom ~A: ~A is not an existing theory" name theory)))

(defun th=definition-constant (name kappa-node env)
  
  (let* ((kappas (if (data~schema-p kappa-node)
		     (data~schema-domain kappa-node)
		   nil))
	 (new-const (term~create-primitive-in-environment
		     name
		     (if kappas
			 (type~schema-create (data~copy (term~type (data~schema-range kappa-node))
							:downto '(data+primitive))
					     :rename 't)
		       (data~copy (term~type kappa-node)
				  :downto '(data+primitive)))
		     'term+constant
		     env)))
    new-const))

(defmacro th~defdef (name theory &rest attribs)
  (declare (edited  "20-JUL-1995 16:58")
	   (authors SORGE)
           (input   "A written representation of a TH+DEF."
		    "Here is an example of the syntax:
                     \\begin{codebox}
                     \\vspace{.1cm} 
 (th~defdef my-definition
	   (in my-theory)
           (author URL)
           (credit URL)
           (reference (book1.ref1 book2.ref2))
           (connective-type equality)
           (date DDMMJJJJ)
	   (type-variables aa)
	   (constants (= (all-types (aa) (o (aa aa)))))
	   (formula (forall (lam (X aa) (= X X))))
	   (help \"Reflexivness of equality\"))
            	     \\end{codebox}"
		    "\\newline That means a definition is represented in terms of its name and its formula."
		    "Furthermore some declarations are added to the environment of the theory."
		    "Such additional declarations can only be type-variables or constants (not variables or type-constants.")
           (effect  "Read the definition, construct a TH+DEF object and register it.")
           (value   "None."))
  (if (string-equal (car theory) :in)
      (setq theory (cadr theory))
    (setq theory (car theory)))
  (if (th~find-theory theory)
      (let* ((theory (th~find-theory theory))
	     (env (th~env theory))
	     (senv (th~senv theory))
	     (sort)
	     (connective-type)
	     (type-vars)
	     (node)
	     (help ""))
	(do ((attribs (cdr attribs) (cdr attribs))
	     (attrib (car attribs) (car attribs)))
	    ((and (null attrib) (null attribs)))
	  (if (consp attrib)
	      (let ((carattrib (if (symbolp (car attrib)) 
				   (symbol-name (car attrib)) 
				 (car attrib))))
		(cond ((string-equal carattrib :definition)

		       (when (and type-vars (null (post~all-types-checker (cadr attrib) :forbidden 't))) 
			 (error "~%During reading definition ~A, it is not allowed to use 'all-types' in the term" (cadr attrib)))
		       
		       (let* ((term (post~read-object (cadr attrib) env :existing-term)))
			 
			 ;; wandle typ-variablen um in variablen 
			 (mapc #'(lambda (type-var)
				   (let* ((obj (env~lookup-object type-var env)))
				     (change-class obj 'type+variable)))
			       type-vars)
			 
			 ;; entferne Typ-variablen aus dem environment
			 (mapc #'(lambda (var) (env~remove var env)) type-vars)

			 (setq node (term~schema-close term))))
		      
		      ((string-equal carattrib :author)); do nothing
		      ((string-equal carattrib :credit)); do nothing
                      ((string-equal carattrib :reference)); do nothing
		      ((string-equal carattrib :date)); do nothing
		      ((string-equal carattrib :sort)
		       (setq sort (cons name (rest attrib))))
                      ((string-equal carattrib :help)
		       (setq help (cadr attrib)))
		      ((string-equal carattrib :connective-type)
		       (if (second attrib)
			   (let* ((type (string (second attrib))))
			     (cond ((null (find type th*current-connective-types :test #'string=))
				    (error "~A is not an allowed type for a connective."))
				   (t
				    (setq connective-type type))))
			 nil))
		      ((string-equal carattrib :type-variables)
		       (post~read-object (cdr attrib) env :type-constants-multiple)
		       
		       ;; waehrend des einlesens des axioms sind die typ-variablen konstanten
		       ;; nachher werden sie zu typ-variablen geaendert !!
		       
		       (setq type-vars (cdr attrib)))
		      
		      (t
		       (error
                        ";;;th~~defdef ~A: Not expecting ~S" name attrib))))
	    (error
             ";;;th~~defdef ~A: ~A is not a valid attribute specification"
             name attrib)))
	
	(let* ((new-const (th=definition-constant
			   name 			   ;; (intern name :omega)
			   node env)))
	  (when connective-type
	    (setf (th~connectives theory) (append (th~connectives theory) (list (list new-const connective-type)))))
	  (when sort (post~read-object sort senv :sort-constant))

	  (setf (gethash (th=read-string name) (th=assumptions theory))
		(make-instance 'th+def
			       :name name ;; (intern name :omega)
			       :constant new-const
			       :theory theory
			       :node node
			       :help help))))
    (error ";;;th~~defdef ~A: ~A is not an existing theory" name theory)))

(defmacro th~defconstant (name theory &rest attribs)
  (declare (edited  "11-NOV-1999")
	   (authors Pollet)
           (input   "A written representation of a theory-constant."
		    "Here is an example of the syntax:
                     \\begin{codebox}
                     \\vspace{.1cm} 
 (th~defconstant my-const
	   (in my-theory)
           (author URL)
           (credit URL)
           (reference (book1.ref1 book2.ref2))
           (date DDMMJJJJ)
           (connective-type my-implication)
	   (type (all-types aa (o (aa aa))))
	   (help \"Helpstring\"))
            	     \\end{codebox}"
		    "\\newline That means a definition is represented in terms of its name and its formula."
		    "Furthermore some declarations are added to the environment of the theory."
		    "Such additional declarations can only be type-variables or constants (not variables or type-constants.")
           (effect  "Read the definition, construct a constant object.")
           (value   "None."))
  (if (string-equal (car theory) :in)
      (setq theory (cadr theory))
    (setq theory (car theory)))
  (if (th~find-theory theory)
      (let* ((theory (th~find-theory theory))
	     (env (th~env theory))
	     (senv (th~senv theory))
	     (connective-type)
	     (type)
	     (sort)
	     (node)
	     (help "")
	     (new-const))
	(do ((attribs (cdr attribs) (cdr attribs))
	     (attrib (car attribs) (car attribs)))
	    ((and (null attrib) (null attribs)))
	  (if (consp attrib)
	      (let ((carattrib (if (symbolp (car attrib)) 
				   (symbol-name (car attrib)) 
				 (car attrib))))
		(cond ((string-equal carattrib :author)); do nothing
		      ((string-equal carattrib :credit)); do nothing
                      ((string-equal carattrib :reference)); do nothing
		      ((string-equal carattrib :date)); do nothing
                      ((string-equal carattrib :help)); do nothing
		      ((string-equal carattrib :sort)
		       (setq sort (cons name (rest attrib))))
		      ((string-equal carattrib :connective-type)
		       (if (second attrib)
			   (let* ((type (string (second attrib))))
			     (cond ((null (find type th*current-connective-types :test #'string=))
				    (error "~A is not an allowed type for a connective."))
				   (t
				    (setq connective-type type))))
			 nil))
		      ((string-equal carattrib :type)
		       (setf type (second attrib)))
		      (t
		       (error
                        ";;;th~~defconstant ~A: Not expecting ~S" name attrib))))
	    (error
             ";;;th~~defconstant ~A: ~A is not a valid attribute specification"
             name attrib)))
	
	(let ((new-const (post~read-object (list name type) env :constant)))
	  (when connective-type
	    (setf (th~connectives theory) (append (th~connectives theory) (list (list new-const connective-type)))))
	  (when sort (post~read-object sort senv :sort-constant))))
    
    (error ";;;th~~defconstant ~A: ~A is not an existing theory" name theory)))

(defmacro th~deftype (name theory &rest attribs)
  (declare (edited  "23-NOV-1999")
	   (authors Pollet)
           (input   "A written representation of a theory-typeconstant."
		    "Here is an example of the syntax:
                     \\begin{codebox}
                     \\vspace{.1cm} 
 (th~deftype my-type
	   (in my-theory)
           (author URL)
           (credit URL)
           (reference (book1.ref1 book2.ref2))
           (date DDMMJJJJ)
	   (arguments 0)
	   (help \"Helpstring\"))
            	     \\end{codebox}"
		    "\\newline That means a type-constant is represented in terms of its name and its arguments."
		    "The slot \"arguments\" is optional and is only used for type-constructors, where the number"
		    "specifies the number of arguments of a type-constructor. 0 will define a type-constant.")
           (effect  "Read the definition, construct a constant object.")
           (value   "None."))
  (if (string-equal (car theory) :in)
      (setq theory (cadr theory))
    (setq theory (car theory)))
  (if (th~find-theory theory)
      (let* ((theory (th~find-theory theory))
	     (env (th~env theory))
	     (expansion)
	     (help "")
	     (numberofargs))
	(do ((attribs (cdr attribs) (cdr attribs))
	     (attrib (car attribs) (car attribs)))
	    ((and (null attrib) (null attribs)))
	  (if (consp attrib)
	      (let ((carattrib (if (symbolp (car attrib)) 
				   (symbol-name (car attrib)) 
				 (car attrib))))
		(cond ((string-equal carattrib :author)); do nothing
		      ((string-equal carattrib :credit)); do nothing
                      ((string-equal carattrib :reference)); do nothing
		      ((string-equal carattrib :date)); do nothing
                      ((string-equal carattrib :help)); do nothing
		      ((string-equal carattrib :expansion)
		       (setf expansion (cadr attrib)))
		      ((string-equal carattrib :arguments)
		       (when (second attrib)
			 (progn
			     (setq numberofargs (second attrib))
			     (unless (numberp numberofargs)
			       (error ";;;th~~deftype ~A: expecting a number not ~S" name numberofargs)))))
		      (t
		       (error
                        ";;;th~~deftype ~A: Not expecting ~S" name attrib))))
	    (error
             ";;;th~~deftype ~A: ~A is not a valid attribute specification"
             name attrib)))

	(cond ((and expansion
		    numberofargs
		    (null (zerop numberofargs)))

	       (error ";;;th~~deftype ~A: ~A cannot be defined with an expansion and number-of-args is not an existing theory" name theory))
	      (expansion
	       (post~read-object (list name expansion) env :type-def))
	      ((or (null numberofargs) (zerop numberofargs))
	       (post~read-object name env :type-constant))
	      (t
	       (post~read-object (list name numberofargs) env :type-constructor)))
	nil)
    
    (error ";;;th~~deftype ~A: ~A is not an existing theory" name theory)))

(defmacro th=defproblem (caller name theory attribs status hash-in requirements)
  (declare (edited  "30-AUG-1995 09:10")
	   (authors SORGE AMEIER)
	   (input   "The calling macro, name, theory, attributes as in the calling macros,"
		    "a make-instance form (with some surroundings) and an additional clause for the cond"
		    "clause: (test val)"
		    "additional variable declaration and condition.")
;;; I'm up for about 24 hours now, I really don't know
;;; how to explain this........
	   (effect  "The same as the calling macros.")
	   (value   "The same as the calling macros."))
  `(let ((name (quote ,name))
	 (theory (if (string-equal (car (quote ,theory)) :in)
		     (cadr (quote ,theory))
		   (car (quote ,theory))))
	 (attribs (quote ,attribs)))
     (if (funcall ,requirements theory)     
      (let* ((theory (th~find-theory theory))
	     (category 'theorem)
	     (help "") (termdecl) (post-term) (sort)	    
	     (env (env~create (th~env theory) name))
	     (senv (th~senv theory))
	     (conclusion nil)
	     (assumptions nil)
	     (proofs nil))
			
	(do ((attribs (cdr attribs) (cdr attribs))
	     (attrib (car attribs) (car attribs)))
	    ((and (null attrib) (null attribs)))
	  (if (consp attrib)
	      (let ((carattrib (if (symbolp (car attrib)) 
				   (symbol-name (car attrib)) 
				 (car attrib))))
		(cond ((string-equal carattrib :help) (setq help (cadr attrib)))
		      ((string-equal carattrib :author)); do nothing
		      ((string-equal carattrib :credit)); do nothing
                      ((string-equal carattrib :reference)); do nothing
		      ((string-equal carattrib :date)); do nothing
		      ((string-equal carattrib :category) (setq category (cadr attrib)))
		      ((string-equal carattrib :termdecl)
		       (setq termdecl t))
		      ((string-equal carattrib :sort)
		       (setq sort t))
		      ((string-equal carattrib :type-defs)
		       (post~read-object (cdr attrib) env :type-defs))
		      ((string-equal carattrib :constants)
		       (post~read-object (cdr attrib) env :constants))
		      ((string-equal carattrib :variables)
		       (post~read-object (cdr attrib) env :variables))
		      ((string-equal carattrib :meta-variables)
		       (post~read-object (cdr attrib) env :meta-variables))
		      ((string-equal carattrib :type-constants)
		       (post~read-object (cdr attrib) env :type-constants))
		      ((string-equal carattrib :type-constructors)
		       (post~read-object (cdr attrib) env :type-constructors))
		      ;; type-variables sind nicht mehr erlaubt !!
		      ;;((string-equal carattrib :type-variables)
		      ;; (post~read-object  (cdr attrib) env :type-variables))
		      ((string-equal carattrib :assumption)
		       (setq assumptions (append assumptions (list (post~read-object (list (first (cdr attrib))
											   (second (cdr attrib)))
										     env
										     :assumption)))))
		      ((string-equal carattrib :conclusion)
		       (setq conclusion (if (> (length (cdr attrib)) 1)
					    (progn
					      (setq post-term (second (cdr attrib)))
					      (post~read-object (list (first (cdr attrib)) (second (cdr attrib))) env :conclusion))
					  (progn
					    (setq post-term (first (cdr attrib)))
					    (post~read-object (list name (first (cdr attrib))) env :conclusion)))))
		      ((string-equal carattrib :let)
		       (setq assumptions (append assumptions (list (post~read-object (rest attrib) env :let)))))
		      (t
		       (error ";;;th~~def~A ~A: Not expecting ~S" ,caller name attrib))))
	    (error ";;;th~~def~A ~A: ~A is not a valid attribute specification" ,caller name attrib)))

	(cond ((and assumptions (or termdecl sort))
	       (error ";;;th~~def~A ~A: sort or term declarations with assumptions are not allowed" ,caller name))
	      (termdecl (post~read-object (list post-term name) senv :termdecl))
	      (sort (post~read-object post-term senv :sort-constant)))
	(setf (gethash (th=read-string name) ,hash-in)
	      (th=env-entry name
			    (make-instance 'prob+problem
					   :name name
					   :theory theory
					   :status ,status
					   :category category
					   :environment env
					   :assumptions assumptions
					   :conclusion conclusion
					   :help help)
			    env)))

      (error ";;;th~~def~A ~A: ~A is not an existing theory" ,caller name theory))))


(defmacro th~defproblem (name theory &rest attribs)
  (declare (edited  "20-JUL-1995 16:58")
	   (authors SORGE)
           (input   "A written representation of a PROB+PROBLEM."
		    "Here is an example of the syntax:  
                     \\begin{codebox} 
                     \\vspace{.1cm}
 (th~defproblem my-problem  
	      (in my-theory)
              (author URL)
              (credit URL)
              (reference (book1.ref1 book2.ref2))
              (date DDMMJJJJ)
              (category theorem)
	      (constants (P (i i))
			 (C i)
			 (D i)) 
	      (assumption A1 (= C D))
	      (conclusion THM (= (P C) (P D)))
	      (help \"My theory 1st problem .....\"))
                    \\end{codebox}"
		    "\\newline That means a problem is represented in terms of its name, its assumptions and its conclusion."
		    "Furthermore local declarations are possible.")
           (effect  "Read the problem, construct a PROB+PROBLEM object and register it.")
           (value   "None."))
  `(th=defproblem "problem" ,name ,theory ,attribs 'conjectured (th=problems theory) #'th~require-completely))

(defmacro th~deftheorem (name theory &rest attribs)
  (declare (edited  "20-JUL-1995 16:58")
	   (authors SORGE)
           (input   "A written representation of a proven PROB+PROBLEM."
		    "Here is an example of the syntax:  
                     \\begin{codebox} 
                     \\vspace{.1cm}
 (th~deftheorem my-theorem
	      (in my-theory)
              (author URL)
              (credit URL)
              (reference (book1.ref1 book2.ref2))
              (date DDMMJJJJ)
              (category theorem)
	      (constants (P (i i))
			 (C i)
			 (D i)) 
	      (assumption A1 (= C D))
	      (conclusion THM (= (P C) (P D)))
	      (help \"My theory 1st theorem .....\"))
                    \\end{codebox}"
		    "\\newline That means a problem is represented in terms of its name, its assumptions and its conclusion."
		    "Furthermore local declarations are possible.")
           (effect  "Read the theorem, construct a PROB+PROBLEM object and register it.")
           (value   "None."))
  `(let ((thm (th=defproblem "theorem" ,name ,theory ,attribs 'proven (th=assumptions theory) #'th~require-only))) 
     (when thm
       ;;(not (infer~open-p (node~just-method (prob~conclusion thm)))))
       (let* ((cat (prob~category thm))
         ;;; the following is a rather elaborate way of dealing with different categories of proven problems
         ;;; and their respective justifications. The corresponding justification-method should be of type
         ;;; INFER+DUMMY, otherwise the whole stuff wouldn't make a lot of sense... VS
              (jmh (infer~find-method cat))
              (just-meth (if jmh jmh
                           (cond ((or (string-equal cat :lem) (string-equal cat :lemma))
				  (infer~find-method :lemma))
                                 ((string-equal cat :proposition) (infer~find-method :prop))
                                 ;;; You're perfectly welcome to add more...
                                 (t (infer~find-method :thm))))))
         (setf (node~justification (prob~conclusion thm)) (just~create just-meth nil))))
     thm)
  )

(defmacro th~defagentdefault (name &rest attribs)
  (declare (edited  "24-MAY-2001")
	   (authors Pollet)
           (input   "A written representation of a th+theory."
		    "Here is an example of the syntax:  
                     \\begin{codebox} 
                     \\vspace{.1cm}
 (th~defagentdefault theory  
	      (lispfunction1 args1)
	      (lispfunction2 args2)
              ...
	      (help \"My defaults .....\"))
 \\end{codebox}")
           (effect  "Reads and stores the commands.")
           (value   "None."))
  (let ((theory (th~find-theory name)))
    (if theory
	(setf (th=agent-default theory) (cons 'progn attribs))
      (error "Theory ~A not loaded!" name))))



;; methods for POST~READ-OBJECT

;; We will need some new methods and indicators for post~read-object
;; or maybe not!!!!!
;; Die Umstellung auf POST-Syntax auch fuer Theorien scheint mir momentan nicht so dringend, da sie mit Sicherheit groessere
;; Aenderungen in der Syntax erzwingt. Habe deshalb vorlaeufig mal alles einfach auskommentiert. HACK MADE BY AMEIER

#|
(defmethod post~read-object ((thing list) (env env+environment)
			     (indicator (eql :theory)))
  (eval (cons 'th~deftheory thing)))

(defmethod post~read-object ((thing list) (env env+environment)
			     (indicator (eql :axiom)))
  (eval (cons 'th~defaxiom thing)))

(defmethod post~read-object ((thing list) (env env+environment)
			     (indicator (eql :def)))
  (eval (cons 'th~defdef thing)))

(defmethod post~read-object ((thing list) (env env+environment)
			     (indicator (eql :theorem)))
  (eval (cons 'th~deftheorem thing)))


|#
;; methods for POST~PRINT and an attempt at some PP~STYLES.

;;;(pp~defstyle th-assumptions
;;;             :help "A simple style for printing theory assumptions"
;;;             :)

(defmethod post~print ((theory th+theory) stream)
  (declare (edited  "01-APR-1997" "30-AUG-1995 05:54")
	   (authors Fehrer SORGE)
	   (input   "A theory.")
	   (effect  "Prints a POST-representation of THEORY on STREAM.")
	   (value   "Undefinded."))
  (cond ((null stream) (with-output-to-string (string-stream)
					      (post~print theory string-stream)))
	((eq stream t) (post~print theory *standard-output*))
	(t
	 (format stream "~%(Theory ~S ~%" (keim~name theory))
	 (when (th~imports theory) (format stream "(uses~{ ~S~}) ~%" (mapcar #'keim~name (th~imports theory))))
	 (let* ((env (th~env theory))
		(type-vars  (env~class-keys env 'type+variable))
		(type-constants (env~class-keys env 'type+constant))
		(constants (env~class-keys env 'term+constant))
		(variables (env~class-keys env 'term+variable)))
	   (mapc #'(lambda (x) (env~post-print x 
					       (env~lookup-object x env)
					       stream))
		 (append type-vars type-constants constants variables))
	   (format stream "(help ~S)" (help~help-string theory))
	   (format stream ")")))))


(defmethod post~print ((axiom th+axiom) stream)
  (declare (edited  "30-AUG-1995 05:54")
	   (authors SORGE)
	   (input   "An axiom.")
	   (effect  "Prints a POST-representation of AXIOM on STREAM.")
	   (value   "Undefinded."))
  (cond ((null stream) (with-output-to-string (string-stream)
					      (post~print axiom string-stream)))
	((eq stream t) (post~print axiom *standard-output*))
	(t
	 (format stream "~%(Axiom ~S (in ~S) ~%" (keim~name axiom) (keim~name (th~ass-theory axiom)))
	 (format stream "(formula ")
	 (post~print (th~ass-formula axiom) stream)
	 (format stream ") ~%")
	 (format stream "(help ~S)" (help~help-string axiom))
	 (format stream ")"))))


(defmethod post~print ((def th+def) stream)
  (declare (edited  "30-AUG-1995 05:54")
	   (authors SORGE)
	   (input   "A definition.")
	   (effect  "Prints a POST-representation of DEF on STREAM.")
	   (value   "Undefinded."))
  (cond ((null stream) (with-output-to-string (string-stream)
					      (post~print def string-stream)))
	((eq stream t) (post~print def *standard-output*))
	(t
	 (format stream "~%(Def ~S (in ~S) ~%" (keim~name def) (keim~name (th~ass-theory def)))
	 (let* ((env (th~env (th~ass-theory def)))
		(type-vars  (env~class-keys env 'type+variable))
		(constants (env~class-keys env 'term+constant)))
	   (mapc #'(lambda (x) (env~post-print x 
					       (env~lookup-object x env)
					       stream))
		 (append type-vars constants))
	   (format stream "(formula ")
	   (post~print (th~ass-formula def) stream)
	   (format stream ") ~%")
	   (format stream "(help ~S)" (help~help-string def))
	   (format stream ")")))))

;; a little extension for keim~name

(defmethod keim~name ((obj (eql nil)))
  (values))

(defun th=print-hashtable (stream table)
  (maphash #'(lambda (key val)
	       (declare (ignore val))
	       (format stream "~A " key))
	   table))


(defmethod print-object ((obj th+theory) stream)
  (format stream "~%THEORY ~A:" (keim~name obj))
  (format stream "~%Parent-theories: ")(format stream"~{ ~A~}" (mapcar #'keim~name (th~imports obj)))
  (format stream "~%Environment: ")  (format stream "~A" (th=env obj))
  (format stream "~%Assumptions: ")   (th=print-hashtable stream (th=assumptions obj))
  (format stream "~%Methods: ")      (th=print-hashtable stream (th=methods obj))
  (format stream "~%Problems: ")     (th=print-hashtable stream (th=problems obj))
  (format stream "~%Help: ")         (format stream "~A" (help~help-string obj)))

(defmethod print-object ((obj (eql th*theory-hash-table)) stream)
  (format stream "#<TH ")
  (th=print-hashtable stream obj)
  (format stream ">")
)

(defmethod print-object :before ((obj th+axiom) stream)
  (format stream "Axiom:     "))

(defmethod print-object ((obj th+def) stream)
  (format stream "~A" (keim~name obj)))

(defmethod print-object ((obj th+assumption) stream)
  (format stream " ~A  (~A)    ~A    ~A ~%" (keim~name obj)
	                             (keim~name (th~ass-theory obj))
				     (th~ass-formula obj)
				     (help~help-string obj)))



(defmethod post~read-object ((cmd list) (env env+environment) 
			     (indicator (eql :problem)))
  (eval (cons 'th~defproblem cmd))) 


(defmethod post~read-object ((in list) (env env+environment)
			     (indicator (eql :let)))
  (let* ((term (post~read-object (second in) env :existing-term))
	 (term-type (term~type term))
	 (new-const (term~create-primitive-in-environment (first in) term-type 'term+constant env))
	 (definition (term~appl-create (env~lookup-object '=def env)
				       (list new-const term) 
				       ))
	 (node (node~create (keim~name new-const)
			    definition 
			    (just~create (infer~find-method 'local-def) nil))))

    node))
	 
;; File-Management
#{\subsection{File-Management}
We have developed a system to maintain a database of theories. (explain further while developing...)
#}

#{\subsubsection{Loading}#}

(defvar th*loaded (make-hash-table :test #'equal)
  "A hash table to protocol already loaded files.")

(defconstant th*urltypes (list "http:" "ftp:")
  "A list containing the headers of urls." )

(defun th=url-p (url)
  (if (stringp url)
      (some #'(lambda (urltype)
		(equal (subseq url 0 (length urltype))
		       urltype))
	    th*urltypes)
    nil))

(defun th=url-protocol (url)
  (if (th=url-p url)
	     (subseq url 0  (search "//" url))
    (error "~A ist not a URL." url)))

(defun th=url-address (url)
  (if (th=url-p url)
	     (subseq url (search "//" url))
    (error "~A ist not a URL." url)))

(defun th=loaded (file)
  (declare (edited  "03-JUL-1997")
	   (authors Afiedler)
	   (input   "A filename.")
	   (value   "T, iff FILE has already been loaded."))
  (gethash file th*loaded))

(defsetf th=loaded (file) (bool)
  (declare (edited  "03-JUL-1997")
	   (authors Afiedler)
	   (input   "A filename and a boolean.")
	   (effect  "Marks FILE as loaded, iff BOOL.")
	   (value   "BOOL."))
  `(setf (gethash ,file th*loaded) ,bool))

(defun th~load-file (file &key (force nil))
 (declare (edited  "05-FEB-1999" "03-JUL-1997" "26-JUL-1995 17:38")
          (authors Pollet Afiedler SORGE)
          (input   "A filename and a boolean key.")
          (effect  "Reads and creates theories from a file/url containing some th~deftheory"
                   "etc., if that has not yet been happened and FORCE is NIL.") 
          (value   "T if everything went allright."))
 (if (or force (not (th=loaded file)))
     (if (th=url-p file)
	 (let ((answer (loui~load-url file)))      ;;load from url
	   (when (plusp (length answer))
	     (with-input-from-string (in answer)
				     (do ((x (read in nil) (read in nil)))
					 ((null x) t)
				       (eval x)))
	     (setf (th=loaded file) t)))
       (let ((ok (with-open-file (in file          ;; load from file
				  :direction :input
				  :if-does-not-exist nil)
			      (when (streamp in)
				(do ((x (read in nil) (read in nil)))
				    ((null x) t)
				  (eval x))))))
	 (if ok (setf (th=loaded file) t))))
   t))

(defun th=compose-file-name (thy-name &optional (file :theory) (directory "") (type "thy"))
  (declare (edited  "05-FEB-1999" "03-JUL-1997" "22-MAY-1997")
	   (authors Pollet Afiedler Sorge)
	   (input   "A theory name and optional the name of a file, a subsequent directory"
		    "and an extension type. Note that there is a downcase convention for"
		    "all files and directories in the theory-database. However this does"
		    "not apply for the general pathname of the *theory-registry*.") 
	   (value   "A list of valid pathnames of a theory."))
  (when file
    (mapcar #'(lambda (path)
		(let* ((onlypath (if (th=url-p path) (th=url-address path) path))
		      (header (when (th=url-p path) (th=url-protocol path)))
		      (fullpath  (make-pathname :directory
						(user::append-directories onlypath (string-downcase directory)
									  (string-downcase thy-name)
									  (string-downcase directory))
						:name (if (string-equal file :theory)
							  (string-downcase thy-name)
							(concatenate 'string
								     (string-downcase thy-name)
								     "-"
								     (string-downcase file)))
						:type (string-downcase type))))
		  (if header (concatenate 'string header "/" (namestring fullpath)) fullpath)))
	    (if (listp *theory-registry*)
		*theory-registry*
	      (list *theory-registry*)))))
 
(defun th~require (thy-name &key (force nil))
  (declare (edited  "04-JUL-1997" "29-NOV-1996")
	   (authors Afiedler Kohlhase)
	   (input   "A name of a theory (symbol or string) and a boolean key.")
	   (effect  "The theory (and all theories it inherits from) is loaded from the"
		    "theory-registry, it that has not yet happend. If FORCE is T, they"
		    "are always loaded. Unlike in th~require-completely no theorems,"
		    "methods and rules are loaded for the particular theory. However all"
		    "the theories it inherits from are loaded completely.")  
	   (value   "The theory, if everything went allright, else nil."))
  (unless (th~find-theory thy-name)                         
    (if th*current-db
	(th=mb-load-theory thy-name :force force)
      (some #'(lambda (file) (th~load-file file :force force))
	      (th=compose-file-name thy-name))))
  (th~load-rules thy-name :force force)
  (th~find-theory thy-name))

;; this function is only needed for testing, while keim isn't completly imported to keim-3 HACK MADE BY AMEIER
(defun th~require-only (thy-name &key (force nil))
  (declare (edited  "11-DEC-1997")
	   (authors Ameier)
	   (input   "A name of a theory (symbol or string) and a boolean key.")
	   (effect  "Only the .thy of the theory and other .thy files of"
		    "underlaying theories are loaded.")
	   (value   "The theory, if everything went allright, else nil."))
  (unless (th~find-theory thy-name)
    (if th*current-db
	(th=mb-load-theory thy-name :force force)                ;;mbase
      (some #'(lambda (file) (th~load-file file :force force))   ;;file-theory
	    (th=compose-file-name thy-name))))
  (th~find-theory thy-name))

(defun th~require-completely (thy-name &key (force nil))
  (declare (edited  "04-JUL-1997" "29-NOV-1996")
	   (authors Afiedler Kohlhase)
	   (input   "A name of a theory (symbol or string) and a boolean key.")
	   (effect  "The theory (and all theories it inherits from) is loaded from the"
		    "theory-registry (including all its theorems, methods and rules), if"
		    "that has not yet happend. If FORCE is T, it is always loaded.") 
	   (value   "The theory, if everything went allright, else nil."))
  (let ((theory? (th~find-theory thy-name)))
    (unless theory?
      (if th*current-db
	  (th=mb-load-theory thy-name :force force)                 ;;mbase
	(some #'(lambda (file) (th~load-file file :force force))    ;;file-theory
	      (th=compose-file-name thy-name))))
    (th~load-rules thy-name :force force)
    (th~load-theorems thy-name :force force)
    (th~load-tactics thy-name :force force)
    (th~load-methods thy-name :force force)
    (th~load-crules thy-name :force force)
    (th~load-metaprds thy-name :force force)
    (th~load-strategies thy-name :force force)
    (th~load-agents thy-name :force force)
    (th~load-constraintsolver thy-name :force force)
					;(th~load-lingu thy-name :force force)
  (th~find-theory thy-name)))

(defun th~load-crules (thy-name &key (force t))
  (declare (edited  "07-OCT-1997")
	   (authors Cullrich)
	   (input   "A name of a theory (symbol or string) and a boolean key.")
	   (effect  "The control-rules of the theory are loaded, if that has not yet happened."
		    "If FORCE is T, they are always loaded.. If the theory itself is not"
		    "yet loaded it will be. If FORCE is T, it is always reloaded.") 
	   (value   "The theory if everthing went all right, otherwise nil."))
  (when (th~require-only thy-name :force force)
    (when (some #'(lambda (file) (th~load-file file :force force))
		(th=compose-file-name thy-name :crules))
      (th=message ";;; Control-rules loaded for theory ~A." thy-name)))
  (th~find-theory thy-name))

(defun th~load-constraintsolver (thy-name &key (force t))
  (declare (edited  "07-OCT-1997")
	   (authors Cullrich)
	   (input   "A name of a theory (symbol or string) and a boolean key.")
	   (effect  "The constraint-solver of the theory is loaded, if that has not yet happened."
		    "If FORCE is T, it is always loaded.. If the theory itself is not"
		    "yet loaded it will be. If FORCE is T, it is always reloaded.") 
	   (value   "The theory if everthing went all right, otherwise nil."))
  (when (th~require-only thy-name :force force)
    (when (some #'(lambda (file) (th~load-file file :force force))
		  (th=compose-file-name thy-name :constraintsolver))
      (th=message ";;; Contraint-solver loaded for theory ~A." thy-name)))
  (th~find-theory thy-name))

(defun th~load-metaprds (thy-name &key (force t))
  (declare (edited  "07-OCT-1997")
	   (authors Cullrich)
	   (input   "A name of a theory (symbol or string) and a boolean key.")
	   (effect  "The meta-predicates of the theory are loaded, if that has not yet happened."
		    "If FORCE is T, they are always loaded.. If the theory itself is not"
		    "yet loaded it will be. If FORCE is T, it is always reloaded.") 
	   (value   "The theory if everthing went all right, otherwise nil."))
  (when (th~require-only thy-name :force force)
    (when (some #'(lambda (file) (th~load-file file :force force))
		(th=compose-file-name thy-name :metaprds))
      (th=message ";;; Meta-predicates loaded for theory ~A." thy-name)))
  (th~find-theory thy-name))

(defun th~input (thy-name cont)
  (declare (edited  "29-NOV-1996")
	   (authors Kohlhase)
	   (input   "A name of a theory (symbol or string) and a name of a continuation file.")
	   (effect  "The next file of the theory called {\bf cont.thy} is loaded from the theory-registry.") 
	   (value   "Undefined."))
    (some #'th~load-file (th=compose-file-name thy-name cont)))

(defun th~load-strategies (thy-name &key (force t))
  (declare (edited  "10-MAY-2001")
	   (authors Pollet)
	   (input   "A name of a theory (symbol or string) and a boolean key.")
	   (effect  "The strategies of the theory are loaded, if that has not yet happened."
		    "If FORCE is T, they are always loaded.. If the theory itself is not"
		    "yet loaded it will be. If FORCE is T, it is always reloaded.") 
	   (value   "The theory if everthing went all right, otherwise nil."))
  (when (th~require-only thy-name :force force)
    (when (some #'(lambda (file) (th~load-file file :force force))
		(th=compose-file-name thy-name :strategies))
      (th=message ";;; Strategies loaded for theory ~A." thy-name)))
  (th~find-theory thy-name))

(defun th~load-agents (thy-name &key (force t))
  (declare (edited  "10-MAY-2001")
	   (authors Pollet)
	   (input   "A name of a theory (symbol or string) and a boolean key.")
	   (effect  "The agents of the theory are loaded, if that has not yet happened."
		    "If FORCE is T, they are always loaded.. If the theory itself is not"
		    "yet loaded it will be. If FORCE is T, it is always reloaded.") 
	   (value   "The theory if everthing went all right, otherwise nil."))
  (when (th~require-only thy-name :force force)
    (when (some #'(lambda (file) (th~load-file file :force force))
		(th=compose-file-name thy-name :agents))
      (th=message ";;; Agents loaded for theory ~A." thy-name)))
  (th~find-theory thy-name))

(defun th~load-theorems (thy-name &key (force t))
  (declare (edited  "07-FEB-1999" "04-JUL-1997" "25-MAY-1997")
	   (authors Pollet Afiedler Sorge)
	   (input   "A name of a theory (symbol or string) and a boolean key.")
	   (effect  "The theorems of the theory are loaded, if that has not yet happened."
		    "If FORCE is T, they are always loaded. If the theory itself is not"
		    "yet loaded it will be. If FORCE is T, it is always reloaded.") 
	   (value   "The theory if everthing went all right, otherwise nil."))
  (if th*current-db
      	(th=message ";;; Theorems for theory ~A already loaded from MBase." thy-name)
    (when (th~require-only thy-name :force force)
      (when (some #'(lambda (file) (th~load-file file :force force))
		  (th=compose-file-name thy-name :theorems))
	(th=message ";;; Theorems loaded for theory ~A." thy-name))))
  (th~find-theory thy-name))

(defun th~load-problems (thy-name &key (force t))
  (declare (edited  "07-FEB-1999" "04-JUL-1997" "25-MAY-1997")
	   (authors Pollet Afiedler Sorge)
	   (input   "A name of a theory (symbol or string) and a boolean key.")
	   (effect  "The problems of the theory are loaded, if that has not yet happened."
		    "If FORCE is T, they are always loaded. If the theory itself is not"
		    "yet loaded it will be. If FORCE is T, it is always reloaded.") 
	   (value   "The theory if everthing went all right, otherwise nil."))
  (if th*current-db
      (th=message ";;; Problems for theory ~A already loaded from MBase." thy-name)
    (when (th~require-only thy-name :force force)
      (if (some #'(lambda (file) (th~load-file file :force force))
		(th=compose-file-name thy-name :problems))
	  (th=message ";;; Problems loaded for theory ~A." thy-name)
	(warn ";;; There aren't any open problems for the theory ~A." thy-name))))
  (th~find-theory thy-name))

(defun th~load-rules (thy-name &key (force t))
  (declare (edited  "04-JUL-1997" "25-MAY-1997")
	   (authors Afiedler Sorge)
	   (input   "A name of a theory (symbol or string) and a boolean key.")
	   (effect  "The rules of the theory are loaded, if that has not yet happened."
		    "If FORCE is T, they are always loaded.") 
	   (value   "The theory if everthing went all right, otherwise nil."))
  (when (th~require-only thy-name :force force)
    (when (some #'(lambda (file) (th~load-file file :force force))
		(th=compose-file-name thy-name :rules))
      (th=message ";;; Rules loaded for theory ~A." thy-name)))
  (th~find-theory thy-name))

(defun th~load-methods (thy-name &key (force t))
  (declare (edited  "04-JUL-1997" "25-MAY-1997")
	   (authors Afiedler Sorge)
	   (input   "A name of a theory (symbol or string) and a boolean key.")
	   (effect  "The methods of the theory are loaded, if that has not yet happened."
		    "If FORCE is T, they are always loaded.. If the theory itself is not"
		    "yet loaded it will be. If FORCE is T, it is always reloaded.") 
	   (value   "The theory if everthing went all right, otherwise nil."))
  (when (th~require-only thy-name :force force)
    (when (some #'(lambda (file) (th~load-file file :force force))
		(th=compose-file-name thy-name :methods))
      (th=message ";;; Methods loaded for theory ~A." thy-name)))
  (th~find-theory thy-name))

(defun th~load-tactics (thy-name &key (force t))
  (declare (edited  "04-JUL-1997" "25-MAY-1997")
	   (authors Afiedler Sorge)
	   (input   "A name of a theory (symbol or string) and a boolean key.")
	   (effect  "The tactics of the theory are loaded, if that has not yet happened."
		    "If FORCE is T, they are always loaded.. If the theory itself is not"
		    "yet loaded it will be. If FORCE is T, it is always reloaded.") 
	   (value   "The theory if everthing went all right, otherwise nil."))
  (when (th~require-only thy-name :force force)
    (when (some #'(lambda (file) (th~load-file file :force force))
		(th=compose-file-name thy-name :tactics))
      (th=message ";;; Tactics loaded for theory ~A." thy-name)))
  (th~find-theory thy-name))

#|obsolete with P.rex
(defun th~load-lingu (thy-name &key (force t))
  (declare (edited  "31-MAY-1999")
	   (authors Pollet)
	   (input   "A name of a theory (symbol or string) and a boolean key.")
	   (effect  "The linguistic knowledge of the theory are loaded, if that has not yet happened."
		    "If FORCE is T, they are always loaded.") 
	   (value   "The theory if everthing went all right, otherwise nil."))
  (if th*current-db (th=mb-load-lingu thy-name :force force)
    (progn
      (when (th~require-only thy-name :force force)
	(when (some #'(lambda (file) (th~load-file file :force force))
		    (th=compose-file-name thy-name :lingu))
	  (th=message ";;; Linguistic knowledge loaded for theory ~A." thy-name)))
      (th~find-theory thy-name))))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following is some mean of supplying sexpressions from a given file in incremental manner.
;; The idea of using an external let to wrap some functions was contributed by Karsten Konrad.
;; All that is not very pretty (mind me using some setfs!!) but rather useful for the purpose.
;;
;; Some words of explanation:
;; The function th=read-sexp-from-file creates a closure that enables us with every single call
;; to extract the sequentially next sexpression from the file or to close the file without
;; reading from it any further. (For hints of its handling see the documentation of this
;; function.)
;; In order not to have to deal with funcalls to the closure itself in every function we want
;; to make use of its functionality, we provide two functions which elegantly access the
;; closure, although they are ugly implemented themself.
;; th=read-next-sexp returns the next sexpression.
;; th=stop-reading-sexp ends the process of reading from the file and closes it.
;; Both functions are wrapped inside a global let, in which the closure is bound, while it
;; exists.
;; All that only works for one open file at a given time. That could be changed but I thought
;; it useful as it prevents the system from crashing with too many simoulantously open files,
;; in case of an error somewhere. (Maybe one of my successors feels like changing it....)
;;                                                                                             VS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun th=read-sexp-from-file (file)
  (declare (edited  "26-MAY-1997")
	   (authors Sorge)
	   (input   "A valid logical file-name.")
	   (effect  "The FILE is opened but not automatically closed! It will be closed when the end of"
		    "the file is hit during a  call to the closure or when the closure is called with"
		    "the key-word CLOSE set T. In both cases the closure returns T."
		    "WARNING: One should be careful when using this function not to end up with plenty"
		    "of open files.")
	   (value   "A closure that accesses the open FILE. Each call to the closure returns one sexp."))
  (let ((in (open file :direction :input
		  :if-does-not-exist :error)))
    #'(lambda (&key (close nil))
	(let ((input-list (read in nil)))
	  (if (and input-list
		   (not close))
	      input-list
	    (close in))))))

(eval-when (load eval compile)
(let ((closure nil))         ;;; a global let-environment

  (defun th=read-next-sexp (&optional file)
    (declare (edited  "26-MAY-1997")
	     (authors Sorge)
	     (input   "An optional file-argument.")
	     (effect  "Creates a closure when none exists for the specified file otherwise it accesses the closure.")
	     (value   "The next sexpression in the file or NIL if there is none."))
    (unless closure
      (cond ((null file) (error "Filename not specified!"))
	    ((pathnamep file) (unless (probe-file file) (error "Not a valid pathname: ~A" file)))
	    (t (setf file (pathname file))
	       (unless (probe-file file) (error "Not a valid pathname: ~A" file))))
      (setf closure (th=read-sexp-from-file file)))
    (let ((res (funcall closure)))
      (if (eq t res)
	  (setf closure nil)
	res)))

  (defun th=stop-reading-sexp ()
    (declare (edited  "26-MAY-1997")
	     (authors Sorge)
	     (effect  "CLoses the current file opened inside the closure.")
	     (value   "NIL."))
    (when closure (funcall closure :close t))
    (setf closure nil))

  )          ;;; THE END of the global let-environment
)

#{\subsubsection{Saving}#}

(defgeneric th~append2file (theory th-file string)
  (declare (edited  "26-MAY-1997")
	   (authors Sorge)
	   (input   "A theory, a filename in the theory-database and a string representing"
		    "an object to be saved.")
	   (effect  "Appends the object to the given file.")
	   (value   "T if everything went alright."))
;;  (:argument-precedence-order th-file theory object)
  (:method (theory th-file object)
	   (declare (ignore object th-file))

	   (error ";;;TH~~APPEND2FILE: ~A is not a valid theory." theory))
  (:method ((theory symbol) th-file object)
	   (let ((th (th~find-theory theory)))
	     (if th
		 (th~append2file th th-file object)
	       (error ";;;TH~~APPEND2FILE: Theory ~A does not exist" theory))))
  (:method ((theory string) th-file object)
	   (let ((th (th~find-theory theory)))
	     (if th
		 (th~append2file th th-file object)
	       (error ";;;TH~~APPEND2FILE: Theory ~A does not exist" theory))))
  (:method ((theory th+theory) th-file object)
	   (declare (ignore object))
	   (error ";;;TH~~APPEND2FILE: ~A is not a valid file." th-file))
  (:method ((theory th+theory) (th-file symbol) object)
	   (let ((pathname (car (th=compose-file-name (keim~name theory) th-file))))
	     (if pathname (th~append2file theory pathname object)
	       (error ";;;TH~~APPEND2FILE: ~A is not a valid file." th-file))))
  (:method ((theory th+theory) (th-file string) object)
	   (let ((pathname (car (th=compose-file-name (keim~name theory) th-file))))
	     (if pathname (th~append2file theory pathname object)
	       (error ";;;TH~~APPEND2FILE: ~A is not a valid file." th-file))))
  (:method ((theory th+theory) (th-file pathname) (object string))
	   (th~append2file 0 th-file object))
  (:method (theory (th-file pathname) (object string))
	   (declare (ignore theory))
	   (when (probe-file (make-pathname :directory (pathname-directory th-file)))
	     (with-open-file (out th-file
				 :direction :output
				 :if-exists :append
				 :if-does-not-exist :create)
			     (format out "~2%~A" object))
	     t)))
	   
(defun th=rewrite-prefix (string old new)
  (declare (edited  "27-MAY-1997")
	   (authors Sorge)
	   (input   "Three strings.")
	   (value   "STRING, where the OLD-prefix is replaced with NEW."))
  (let ((hstr (string-left-trim '(#\Space #\Tab #\Newline) string))
	(hold (concatenate 'string "(" old))
	(hnew (concatenate 'string "(" new)))
    (concatenate 'string hnew  (string-left-trim hold hstr))))


(defgeneric th~write2theory (object)
  (declare (edited  "26-MAY-1997")
	   (authors Sorge)
	   (input   "An object belonging to a theory.")
	   (effect  "Writes the object to the corresponding file in the theory.")
	   (value   "T if everything went alright."))
  (:method (object)
	   (error ";;;TH~~WRITE2THEORY expects an object belonging to a theory which ~A does not." object))
  (:method ((object prob+problem))
	   (let ((obj-string (post~print object nil))
		 (theory (prob~theory object)))
	     (if (prob~proven-p object)
		 (let ((old-prefix "Theorem")
		       (new-prefix "th~deftheorem"))
		   (th~append2file theory 'theorems (th=rewrite-prefix obj-string old-prefix new-prefix))
		   )
	       (let ((old-prefix "Problem")
		     (new-prefix "th~defproblem"))
		   (th~append2file theory 'problems (th=rewrite-prefix obj-string old-prefix new-prefix))))))
  )
	     

;;; Seek and Destroy
#{\subsubsection{Removing}#}

(defun th=output-standard-header (stream)
  (declare (edited  "30-MAY-1997")
	   (authors Sorge)
	   (input   "A stream.")
	   (effect  "Writes the theory-header to STREAM.")
	   (value   "None."))
  (format stream ";;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: THEORY -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;; This file is part of a KEIM-Theory-Database                              ;;
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
;;   software for any purpose.  It is provided ~S without express or   ;;
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
" "AS IS")
  (format stream " ~2%~S" '(in-package "KEIM")))
  

(defgeneric th~delete-from-file (theory th-file object)
  (declare (edited  "30-MAY-1997")
	   (authors Sorge)
	   (input   "A theory, an object and a file.")
	   (effect  "Seeks the object in the given file of the theory and deletes it.")
	   (value   "T if something was deleted, otherwise NIL."))
  (:method (theory th-file object)
	   (declare (ignore th-file object))
	   (error ";;;TH~~DELETE-FROM-FILE: ~A is not a valid theory." theory))
  (:method ((theory symbol) th-file object)
	   (let ((th (th~find-theory theory)))
	     (if th
		 (th~delete-from-file th th-file object)
	       (error ";;;TH~~DELETE-FROM-FILE: Theory ~A does not exist" theory))))
  (:method ((theory string) th-file object)
	   (let ((th (th~find-theory theory)))
	     (if th
		 (th~delete-from-file th th-file object)
	       (error ";;;TH~~DELETE-FROM-FILE: Theory ~A does not exist" theory))))
  (:method ((theory th+theory) th-file object)
	   (declare (ignore object))
	   (error ";;;TH~~DELETE-FROM-FILE: ~A is not a valid file." th-file))
  (:method ((theory th+theory) (th-file symbol) object)
	   (let ((pathname (car (th=compose-file-name (keim~name theory) th-file))))
	     (if pathname (th~delete-from-file theory pathname object)
	       (error ";;;TH~~DELETE-FROM-FILE: ~A is not a valid file." th-file))))
  (:method ((theory th+theory) (th-file string) object)
	   (let ((pathname (car (th=compose-file-name (keim~name theory) th-file))))
	     (if pathname (th~delete-from-file theory pathname object)
	       (error ";;;TH~~DELETE-FROM-FILE: ~A is not a valid file." th-file))))
  (:method ((theory th+theory) (th-file pathname) object)
	   (th~delete-from-file 0 th-file object))
  (:method (theory (th-file pathname) (object symbol))
	   (th~delete-from-file theory th-file (th=read-string object)))
  (:method (theory (th-file pathname) (object string))
	   (declare (ignore theory))
	   (when (probe-file th-file)
	     (let ((aux-file (make-pathname :directory (pathname-directory th-file)
					    :name :intermed
					    :type :aux))
		   (flag nil))
	       (th=read-next-sexp th-file)
	       (with-open-file (out aux-file
				    :direction :output
				    :if-exists :overwrite
				    :if-does-not-exist :create)
			       (th=output-standard-header out)
			       (do ((x (th=read-next-sexp) (th=read-next-sexp)))
				   ((null x) flag)
				 (if (string-equal (cadr x) object)
				     (setf flag t)
				   (format out "~2%~S" x))))
	       (rename-file aux-file th-file)
	       flag
	       ))))

(defgeneric th~delete-from-theory (object)
  (declare (edited  "30-MAY-1997")
	   (authors Sorge)
	   (input   "An object of a theory.")
	   (effect  "Deletes the object form the theory.")
	   (value   "T if successful, otherwise NIL."))
  (:method (object)
	   (error ";;;TH~~DELETE-FROM-THEORY expects an object beloging to a theory which ~A does not." object))
  (:method ((object prob+problem))
	   (let ((theory (prob~theory object)))
	     (if (prob~proven-p object)
		 (th~delete-from-file theory :theorems (keim~name object))
	       (th~delete-from-file theory :problems (keim~name object))))))
	     
(defgeneric th~problem2theorem-file (theorem &optional proofs)
  (declare (edited  "02-JUN-1997")
	   (authors Sorge)
	   (input   "A theorem.")
	   (effect  "Deletes the corresponding problem in the problem-file of the theory and adds it to the theorem-file.")
	   (value   "T if everything went alright."))
  (:method (problem &optional proofs)
           (declare (ignore proofs))
           (error "~A must be of type PROB+PROBLEM" problem))
  (:method ((problem string) &optional proofs)
	   (let ((prob (prob~find-problem problem)))
	     (if prob
		 (th~problem2theorem-file prob proofs)
	       (error "Problem ~A does not exist" problem))))
  (:method ((problem symbol) &optional proofs)
	   (let ((prob (prob~find-problem problem)))
	     (if prob
		 (th~problem2theorem-file prob proofs)
	       (error "Problem ~A does not exist" problem))))
  (:method ((problem prob+problem) &optional proofs)
	   (if (prob~proven-p problem)
	       (prog1
		   (th~delete-from-file (prob~theory problem) :problems (keim~name problem))
		 (th~write2theory problem)
		 (format t "do something with the proofs: ~A." proofs))
	     (warn ";;;TH~~PROBLEM2THEOREM-FILE: ~A is not proven yet. It will not be changed into a theorem in the database as this would violate its integrity!" problem))))


;;; check integrity 
#{\subsubsection{Checking}#}

(defgeneric th~file-integrity (theory file)
  (declare (edited  "03-JUN-1997")
	   (authors Sorge)
	   (input   "A theory (or its name) and a symbol representing a file in the theory.")
	   (effect  "So far none.")
	   (value   "T if the specified file meets the criteria of the theory."))
  (:method (theory file)
	   (declare (ignore theory file))
	   (error ";;;TH~~FILE-INTEGRITY: Input should be a theory (or its name) and a symbol."))
  (:method ((theory symbol) file)
	   (th~file-integrity (th=read-string theory) file))
  (:method ((theory th+theory) file)
	   (th~file-integrity (keim~name theory) file))
  (:method ((theory string) (file symbol))
	   (if (string-equal theory file)
	       (th~file-integrity theory :theory)
	     (error ";;;TH~~FILE-INTEGRITY: ~A is not a valid filename in a theory." file)))
  (:method ((theory string) (file (eql :theory)))
	   (let ((pathname (th=compose-file-name theory file)))
	     (if pathname
		 (every #'null
		  (mapcar #'(lambda (file)
				    (th=execute-and-compile-expressions-list         
				     (th=test-first-sexp (th=read-next-sexp file))   
				     (th=seek-theory-definition file)
				     (th=compare-sexp-with-list file '(th~defdef th~defaxiom))
				     ))
				pathname))
	       (error ";;;TH~~FILE-INTEGRITY: Theory ~A does not exist." theory))))
  (:method ((theory string) (file (eql :theorems)))
	   (let ((pathname (th=compose-file-name theory file)))
	     (if pathname 
		 (every #'null
		  (mapcar #'(lambda (file)
				    (th=execute-and-compile-expressions-list          ;;; we test the theory-file with 
				     (th=test-first-sexp (th=read-next-sexp file))    ;;; some functions, each returning
				     (th=compare-sexp-with-list file '(th~deftheorem));;; nil if nothing special has occured.
				     ))
				pathname))
	       (or (warn ";;;TH~~FILE-INTEGRITY: No file ~A in Theory ~A." file theory) t))))
  (:method ((theory string) (file (eql :problems)))
	   (let ((pathname (th=compose-file-name theory file)))
	     (if pathname 
		 (every #'null
		  (mapcar #'(lambda (file)
				    (th=execute-and-compile-expressions-list          ;;; we test the theory-file with 
				     (th=test-first-sexp (th=read-next-sexp file))    ;;; some functions, each returning
				     (th=compare-sexp-with-list file '(th~defproblem));;; nil if nothing special has occured.
				     ))
				pathname))
	       (or (warn ";;;TH~~FILE-INTEGRITY: No file ~A in Theory ~A." file theory) t)))))

(defun th=test-first-sexp (sexp)
  (unless (or (equal sexp '(in-package :keim))
	  (equal sexp '(in-package "keim"))
	  (equal sexp '(in-package "Keim"))
	  (equal sexp '(in-package "KEIM")))
      (progn (warn ";;;Theory-Integrity: Violation of header convention (IN-PACKAGE :KEIM)")
	     (th=stop-reading-sexp)
	     t)))

(defun th=execute-and-compile-expressions-list (&rest expr)
  (declare (edited  "03-JUN-1997")
	   (authors Sorge)
	   (input   "Some expressions.")
	   (effect  "The expressions are being evaluated and the results cumulated in a list.")
	   (value   "NIL if all elements of the list are NIL, otherwise T."))
  (notevery #'null
	(mapcar #'eval expr)))

(defun th=compare-sexp-with-list (file list)
  (declare (edited  "03-JUN-1997")
	   (authors Sorge)
	   (input   "A file and a list of symobls.")
	   (effect  "Compares the car of every sexp (starting incrementally where we stopped before)"
		    "in the file if they are matching the given in the list.")
	   (value   "NIL if they do o/w T."))
  (let ((flag nil))
    (do ((x (th=read-next-sexp file) (th=read-next-sexp)))
	((null x) flag)
      (unless (and (consp x) (find (car x) list))
	(warn ";;;Theory-Integrity: Violation of theory-database convention in file ~A: Unexpected expression ~A"
	      (pathname-name file) x)
	(setf flag t)))
    flag))
  
(defun th=seek-theory-definition (file)
  (let ((flag nil))
    (do ((x (th=read-next-sexp file) (th=read-next-sexp)))
	((and (consp x) (equal (car x) 'TH~DEFTHEORY)) flag)
      (warn ";;;Theory-Integrity: Violation of theory-database convention: Expected theory definition but received ~A" x)
      (setf flag t))
    flag))

#+I-might-need-it--I-might-need-it-not(defun th~read-file (file)
  (declare (edited  "26-MAY-1997")
	   (authors Sorge)
	   (input   "A valid pathname.")
	   (effect  )
	   (value   ))
  (with-open-file (in file
		      :direction :input
		      :if-does-not-exist nil)
 		  (when (streamp in)
		    (do ((x (read in nil) (read in nil)))
			((null x) t)
		      (print x)))))

;;;;;;;;;proofs noch machen!!!!!!!!!!!!!!!!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The section with proofs
;;
;; Proofs are stored seperately in the theory-db. Each theory has a
;; folder PROOFS in which the proofs are stored and added.
;; Proofs are loaded seperately to the theorems.
;; Naming convention for proof files is <theorem-name>-X.pds with
;; X a natural number.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; whats missing: functions for removing and then reordering proofs in the DB.  VS

(defgeneric th~proof-storage-slot (proof)
  (declare (edited  "03-JUN-1997")
	   (authors Sorge)
	   (input   "A proof or a theorem.")
	   (value   "A list of new slots (filenames) the proof can be stored to. Theoretically proofs"
		    "of open problems can be stored. It depends on the implentors choice if he/she"
		    "wants to permit this or not."))
  (:method (theorem)
	   (error ";;;TH~~PROOF-STORAGE-SLOT: ~A is not a theorem." theorem))
  (:method ((theorem prob+problem))
	   (th=compose-proof-file-name
	    (keim~name (prob~theory theorem))
	    (th~new-proof-name theorem)))
  (:method ((proof prob+proof))
	   (th=compose-proof-file-name
	    (keim~name (prob~proof-theory proof))
	    (th~new-proof-name (prob~proof-problem proof)))))

(defgeneric th~new-proof-name (theorem)
  (declare (edited  "03-JUN-1997")
	   (authors Sorge)
	   (input   "A theorem or a proof.")
	   (value   "A new proof-name in the database."))
  (:method (theorem)
	   (error ";;;TH~~NEW-PROOF-NAME: ~A is not a theorem." theorem))
  (:method ((theorem prob+problem))
	   (th=compose-proof-name (keim~name theorem) (1+ (th~theorem-proof-number theorem))))
  (:method ((proof prob+proof))
	   (th=compose-proof-name (keim~name (prob~proof-problem proof)) (1+ (th~theorem-proof-number proof)))))

(defgeneric th~load-theorem-proofs (theorem &key (all nil) (silent nil))
  (declare (edited  "03-JUN-1997")
	   (authors Sorge)
	   (input   "A theorem and two boolean.")
	   (effect  "The proofs of the theorem are loaded and attached to the theorem."
		    "This is being done with user interaction unless all is set."
		    "If the silent flag is set everything will run without issueing any"
		    "warnings, questions etc. (CAUTION! Existent proofs for theorem will"
		    "be automatically destroyed if silent is switched on!)")
	   (value   "The theorem."))
  (:method (theorem &key all silent)
	   (declare (ignore all silent))
	   (error ";;;TH~~LOAD-THEOREM-PROOFS: ~A is not a theorem." theorem))
  (:method ((theorem prob+problem) &key (all nil) (silent nil))
	   (let* ((proof-number (th~theorem-proof-number theorem))
		  (theory (keim~name (prob~theory theorem)))
		  (name (keim~name theorem))
		  (existent (prob~proofs theorem))
		  (inpchar (when (and existent (not silent))
			     (inter~prompt-for-input
			      (asi~create)
			      (format nil "Theory-Lookup: There already exist proofs for theorem ~A. Should new proofs (o)verwrite all old proofs, be (a)ppended or (n)ot loaded? (o/a/n) :" name) 'symbol))))
	     (macrolet ((th=loading-with-prompt ()
						`(if (= proof-number 0)
						     (unless silent
						       (warn "Theory-Lookup: No Proofs for the theorem ~A." theorem))
						   (do* ((n 1 (1+ n))
							 (proof (th=first-proof-file theory name n silent)
								(th=first-proof-file theory name n silent))
							 (plist (list (th=load-single-proof proof)) 
								(cons (th=load-single-proof proof) plist)))
						       ((= n proof-number) plist)
						     (let ((y-n (if (or all silent) t
								  (inter~prompt-for-input
								   (asi~create)
								   (format nil "Theory-Lookup: ~A proofs read for theorem ~A.    Load next proof? (y/n) :" n name) 'boolean))))
						       (unless y-n (return plist)))))))
	       (when (or (null inpchar) (string-equal inpchar 'o))
		 (setf (prob~proofs theorem) (th=loading-with-prompt)))
	       (when (string-equal inpchar 'a)
		 (prob~add-proof theorem (th=loading-with-prompt)))
	       theorem))))

(defun th=first-proof-file (theory name n silent)
  (let ((list  (remove-if #'null
			  (mapcar #'probe-file
				  (th=compose-proof-file-name theory name n)))))
    (when (and (not silent) (> (length list) 1))
      (warn "Theory-Lookup: More than one proof of theorem ~A of name ~A-~A in distributed database. Will only load the first one."
	    name name n))
    (car list)))

(defun th=load-single-proof (file)
  (with-open-file (in file
		      :direction :input
		      :if-does-not-exist :error)
		  (post~read-object (read in) (env~create) nil)))

(defgeneric th~write-theorem-proofs (theorem)
  (declare (edited  "04-JUN-1997")
	   (authors Sorge)
	   (input   "A theorem or a proof.")
	   (effect  "All the proofs of the theorem are saved to the DB."
		    "If only one proof is provided, this one is saved.")
	   (value   "Undefined."))
  (:method (theorem)
	   (error ";;;TH~~WRITE-THEOREM-PROOFS: ~A is not a theorem." theorem))
  (:method ((theorem prob+problem))
	   (let ((proofs (prob~proofs theorem)))
	     (mapc #'(lambda (proof) (th=save-single-proof theorem proof)) proofs)))
  (:method ((proof prob+proof))
	   (th=save-single-proof
	    (prob~proof-problem proof)
	    proof)))

(defgeneric th~write-theorem-last-proof (theorem)
  (declare (edited  "04-JUN-1997")
	   (authors Sorge)
	   (input   "A theorem.")
	   (effect  "The last proof of the theorem is saved to the DB.")
	   (value   "T if everything went alright."))
  (:method (theorem)
	   (error ";;;TH~~WRITE-THEOREM-LAST-PROOF: ~A is not a theorem." theorem))
  (:method ((theorem prob+problem))
	   (let ((proofs (prob~proofs theorem)))
	     (th=save-single-proof theorem (car (last proofs)))))
  (:method ((proof prob+proof))
	   (th=save-single-proof
	    (prob~proof-problem proof)
	    proof)))

(defun th=save-single-proof (theorem proof)
  (let ((file (car (th~proof-storage-slot theorem))))
    (with-open-file (out file
			 :direction :output
			 :if-does-exist :error
			 :if-does-not-exist :create)
		    (post~print proof out))))


(defgeneric th~theorem-proofs? (theorem)
  (declare (edited  "03-JUN-1997")
	   (authors Sorge)
	   (input   "A theorem.")
	   (value   "T if the theorem has proofs stored in the DB."))
  (:method (theorem)
	   (error ";;;TH~~THEOREM-PROOFS?: ~A is not a theorem." theorem))
  (:method ((theorem prob+problem))
	   (> (th~theorem-proof-number theorem) 0))
  (:method ((proof prob+proof))
	   (> (th~theorem-proof-number proof) 0))
  )

(defgeneric th~theorem-proof-number (theorem)
  (declare (edited  "03-JUN-1997")
	   (authors Sorge)
	   (input   "A theorem.")
	   (value   "The number of proofs for theorem in the DB."))
  (:method (theorem)
	   (error ";;;TH~~THEOREM-PROOF-NUMBER: ~A is not a theorem." theorem))
  (:method ((theorem prob+problem))
	   (th=count-proof-files (keim~name (prob~theory theorem)) (keim~name theorem)))
  (:method ((proof prob+proof))
	   (th=count-proof-files (keim~name (prob~proof-theory proof)) (keim~name (prob~proof-problem proof))))
  )

(defun th=count-proof-files (theory name)
  (declare (edited  "03-JUN-1997")
	   (authors Sorge)
	   (input   "A theory-name and a string.")
	   (value   "Counts the number of proof files beginning with name."))
  (do* ((n 1 (1+ n))
	(count 0 (1+ count)))
      ((notany #'probe-file (th=compose-proof-file-name theory name n)) count)))

(defun th=compose-proof-file-name (theory name &optional (number nil))
  (mapcan #'(lambda (path)
	      (when (not (th=url-p path))
		(list
		 (make-pathname :directory (concatenate 'string
							path
							(string-downcase theory)
							"/"
							(string-downcase :proofs))
				:name (if number (th=compose-proof-name name number)
					name)
				:type (string-downcase :pds)))))
	  (if (listp *theory-registry*)
	      *theory-registry*
	    (list *theory-registry*))))

(defun th=compose-proof-name (name number)
  (concatenate 'string
	       (string-downcase name)
	       "-"
	       (princ-to-string number)))


;;; included by Afiedler

(defun th~all-ancestors (theory)
  (declare (edited  "21-OCT-1998")
	   (authors Afiedler)
	   (input   "A theory.")
	   (value   "A list of all ancestors of THEORY including itself."))
  (labels ((ancestors (th result)
		      (if (member th result)
			  result
			(progn (setq result (cons th result))
			       (dolist (pa (th~imports th) result)
				 (setq result (ancestors pa result)))))))
    (ancestors theory nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The section with replays
;;
;; Replays are stored seperately in the theory-db. Each theory has a
;; folder RPY in which the replays are stored and added.
;; Naming convention for replay files is <theorem-name>-X.rpy with
;; X a natural number.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric th~execute-theory-replay (theorem &key (all nil))
  (declare (edited  "16-DEC-1998")
	   (authors Pollet)
	   (input   "A theorem of the current theory and a key.")
	   (effect  "Executes the replay-file and asks, if there"
		    "are more than one replays. If ALL is true,"
		    "all replay files of the theorem will be executed.")
	   (value   "The theorem."))
  (:method (theorem &key all)
	   (declare (ignore all))
	   (error ";;;TH~~EXECUTE-THEORY-REPLAY: ~A is not a theorem." theorem))
  (:method ((theorem prob+problem) &key (all nil))
	   (let* ((theory (keim~name (prob~theory theorem)))
		  (name (keim~name theorem))
	          (replay-quantity (th=count-replay-files theory name))
	          (replay-number (if (and (< 1 replay-quantity) (not all))
				     (inter~prompt-for-input (asi~create)
							     (format nil "There are ~A replay files for ~A. Enter a number."
								     replay-quantity name) 'number) replay-quantity))
		  (replay-start (if all 1 replay-number)))
	     
	     (cond ((= replay-number 0)
		    (error ";;;TH~~EXECUTE-THEORY-REPLAY: ~A has no replay file." name))
		   ((> replay-number replay-quantity)
		    (error ";;;TH~~EXECUTE-THEORY-REPLAY: ~A has only ~A replay files." name replay-quantity))
		   (T (do ((n replay-start (1+ n)))
			  ((> n replay-number))
			(th~prove
			 (pds~start-proof-plan theorem (th=new-proof-plan-name theorem)))
			    (th=execute-log (car (th=compose-replay-file-name theory name n)))))))))

(defun th=new-proof-plan-name (problem)    
  (declare (edited  "16-DEC-1999")
	   (authors Pollet)
	   (input   "A problem.")
	   (value   "A new standard proof-plan name for the problem."))
  (let ((name (keim~name problem)))
    (do* ((x 1 (1+ x))
	  (symb (make-symbol (format nil "~A-~A" name x)) (make-symbol (format nil "~A-~A" name x))))
	((not (prob~find-proof symb)) symb))))

(defun th=execute-log (path)
(declare (edited  "16-DEC-1998")
	   (authors Lassaad)
	   (input  "A file name." ) 
	   (effect "Reads and applies the commands from the log file PATH."
		   "Same as ccom=execute-log.")
	   (value  "None." ))
(let ((current (sys~current-directory)))
    (sys~handler-case
     (with-open-file (in path :direction :input
			 :if-does-not-exist :error)
		     (sys~chdir (directory-namestring path))
		     (loop (let ((input-list
				  (sys~handler-case
				   (asi~linereadp in)
				   (asi+no-input-error () nil)))
				 (next-input (peek-char t in nil 'eof)))
			     (when input-list (ccom~apply-command input-list))
			     (cond ((equal next-input #\;) (read-line in))
				   ((equal next-input 'eof) (return-from th=execute-log)))))
		     (sys~chdir current))
     (file-error (c)
		 (sys~chdir current)
		 (inter~print-error (comint~interface comint*current-comint) c))
     (error (c)
	    (sys~chdir current)
	    (error c)))))

(defun th=count-replay-files (theory name)
  (do* ((n 1 (1+ n))
	(count 0 (1+ count)))
      ((notany #'probe-file (th=compose-replay-file-name theory name n)) count)))

(defun th=compose-replay-file-name (theory name &optional (number nil))
  (mapcar #'(lambda (path)
	      (when (not (th=url-p path))
		 (make-pathname :directory (concatenate 'string
							path
							(string-downcase theory)
							"/"
							(string-downcase :replays))
				:name (if number (th=compose-proof-name name number)
					name)
				:type (string-downcase :rpy))))
		(if (listp *theory-registry*)
		    *theory-registry*
		  (list *theory-registry*))))

(defgeneric th~prove (proof-plan))
;  (declare (edited  "16-DEC-1998")
;           (authors Pollet)
;           (input   "A problem or proof-plan.")
;           (effect  "Changes the current proof-plan to proof-plan."
;                    "The other part of the generic function is"
;                    "defined in omega-com.lisp. Hack!")
;           (value   "undefinded."))
 

(defun th=collect-replay-files (theory)
  (declare (edited  "16-DEC-1998")
	   (authors Pollet)
	   (input   "A theory name.")
	   (effect  "nil.")
	   (value   "A list with all Problems and Theorems"
		    "that have a replay file."))		    
  (let ((problem-list (append (th~problems theory) (th~theorems theory))))
    (mapcan #'(lambda (prob)
		(when (some #'(lambda (file) (probe-file file)) (th=compose-replay-file-name theory (keim~name prob) 1))
		(list prob)))
    problem-list)))
   
(defun th~execute-all-theory-replay (&optional theo-list)
  (declare (edited  "16-DEC-1998")
	   (authors Pollet)
	   (input   "Nil or optionally a list with names of theories.")
	   (effect  "Executes the replay-files of every theory in the list"
		    "or every available theory when the list is empty.")
	   (value   "undefined"))
  (do ((thy-list
	(or theo-list
            (th=all-theories))
	(cdr thy-list)))
      ((null thy-list))
    (let* ((theo (car thy-list)))
	(th~require-completely theo :force nil)
	(th~load-problems theo :force nil)
	(dolist (prob (th=collect-replay-files theo))
	  (th~execute-theory-replay prob :all t)))))

(defun th=all-theories ()
  (declare (edited  "16-DEC-1998")
	   (authors Pollet)
	   (value   "A list with the names (as symbols) of all theories."))
  (if th*current-db
      (th=mb-get-object (th=mb-compose-gettheories))
  (let ((thy-reg (if (listp *theory-registry*)
		      *theory-registry*
		      (list *theory-registry*))))
    (mapcan #'(lambda (reg)
		(when (not (th=url-p reg))
		  (mapcan #'(lambda (theo)
			    (unless (string-equal "CVS" (subseq (namestring theo) (length reg)))
			      (list (read-from-string (subseq (namestring theo) (length reg)))))) (directory reg))))
	    thy-reg))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MBASE: the following functions allow the use of mbase with
;; the same functionality like for the file-based (old) theory
;; above. With the switch th*current-db, load-operations are
;; redirected to mbase-functions. 
;; not implemented yet: writing into mbase, loading of single
;; theorems, axioms... on demand
;; 30.03.2002: switched to new MBase, only basic functionality:
;; Omega gets one file with the whole theory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load stuff from MBase 

(defun th=mb-address ()
  (if (sys~getenv 'mbase)
      (http~geturlnameporturi (sys~getenv 'mbase))
    (values "mbase.mathweb.org" 8080 "/RPC2/mbase")))

(defun th=mb-send-command (command args)
  (labels ((sock-close (sock)
		   (socket~close sock)
		   (socket~delete sock)))
    (let ((socket (gentemp :mbase-socket)))
      (socket~define socket)
    (multiple-value-bind (host port uri)
	(th=mb-address)
      (sys~handler-case
       (progn
	 (socket~connect host port socket)
	 (http~send-request socket (rpc~compose-methodcall command args) :uri uri)
	 (let ((response (http~read-page socket)))
	   (if (and (cadar response) (= (cadar response) 200))
	       (multiple-value-bind (args ok?)
		   (rpc~parse-methodresponse (third response))
		 (if ok? 
		     (progn (sock-close socket) args)
		   (error "RPC error ~A" args)))
	     (error "HTTP error ~A" response))))
       (error (c)
	      (sock-close socket)
	      (error "MBase: ~A" c)))))))

(defun th=mb-ogettheory (name)
   (th=mb-send-command "oGetTheory" (list (string-upcase name))))

(defun th=mb-ofindtheorems (array ozpattern)
  (let ((result (th=mb-send-command "oFindTheorems1" (list (mapcar #'string-upcase array) ozpattern))))
    (when result (car result))))

(defun th=mb-ogetassertionbymid (mid)
  (let ((result  (th=mb-send-command "oGetAssertionByMID" (list mid))))
    (when (stringp (car result)) (read-from-string (car result)))))
		    
(defun th=mb-load-theory (name &key (force nil))
  (declare (ignore force))
  (let ((answer (car (th=mb-ogettheory name))))
    (when (plusp (length answer))
      (with-input-from-string (in answer)
			      (do ((x (read in nil) (read in nil)))
				  ((null x) t)
				(eval x)))))
  (setf (th=loaded (read-from-string (format nil "MBASE-~A" name))) t)
  (th=message ";;; Theory ~A loaded from MBase." name)
  (th~find-theory name))



#|
(defun th=mb-load-signature (thy-name &key (force t))
  (declare (edited  "08-FEB-1999")
	   (authors Pollet)
	   (input   "A theory and a boolean key.")
	   (effect  "Loads the signature and the definitions of the"
		    "given theory from MBase.")
	   (value   "The theory if everthing went all right."))
  (unless (th~find-theory thy-name)
    (th=mb-load-object (th=mb-compose-get thy-name) :force force)
    (dolist (def (th=mb-get-object (th=mb-compose-getnames thy-name 'defs))) 
      (th=mb-load-object (th=mb-compose-get thy-name def 'def) :force force))
    (th=message ";;; MBase Signature loaded for theory ~A." thy-name)
    (th~find-theory thy-name)))

(defun th=mb-load-axioms (thy-name &key (force t))
  (declare (edited  "08-FEB-1999")
	   (authors Pollet)
	   (input   "A name of a theory (symbol or string) and a boolean key.")
	   (effect  "The axioms of the theory are loaded from MBase."
		    "If FORCE is T, they are always loaded. If the theory itself is not"
		    "yet loaded it will be. If FORCE is T, it is always reloaded.") 
	   (value   "The theory if everthing went all right, otherwise nil."))
  (when (th~require-only thy-name :force force)
    (dolist (def (th=mb-get-object (th=mb-compose-getnames thy-name 'axioms))) 
      (th=mb-load-object (th=mb-compose-get thy-name def 'axiom) :force force))
    (th=message ";;; MBase Axioms loaded for theory ~A." thy-name)
    (th~find-theory thy-name)))

(defun th=mb-load-simplifiers (thy-name &key (force t))
  (declare (edited  "08-FEB-1999")
	   (authors Pollet)
	   (input   "A name of a theory (symbol or string) and a boolean key.")
	   (effect  "The simplifiers of the theory are loaded from MBase."
		    "If FORCE is T, they are always loaded. If the theory itself is not"
		    "yet loaded it will be. If FORCE is T, it is always reloaded.") 
	   (value   "The theory if everthing went all right, otherwise nil."))
  (when (th~require-only thy-name :force force)
    (dolist (def (th=mb-get-object (th=mb-compose-getnames thy-name 'simplifiers))) 
      (th=mb-load-object (th=mb-compose-get thy-name def 'simplifier) :force force))
    (th=message ";;; MBase Simplifiers loaded for theory ~A." thy-name)
    (th~find-theory thy-name)))

(defun th=mb-load-theorems (thy-name &key (force t))
  (declare (edited  "08-FEB-1999")
	   (authors Pollet)
	   (input   "A name of a theory (symbol or string) and a boolean key.")
	   (effect  "The theorems of the theory are loaded from MBase."
		    "If FORCE is T, they are always loaded. If the theory itself is not"
		    "yet loaded it will be. If FORCE is T, it is always reloaded.") 
	   (value   "The theory if everthing went all right, otherwise nil."))
  (when (th~require-only thy-name :force force)
    (dolist (def (th=mb-get-object (th=mb-compose-getnames thy-name 'theorems))) 
      (th=mb-load-object (th=mb-compose-get thy-name def 'theorem) :force force))
    (th=message ";;; MBase Theorems loaded for theory ~A." thy-name)
    (th~find-theory thy-name)))

(defun th=mb-load-problems (thy-name &key (force t))
  (declare (edited  "08-FEB-1999")
	   (authors Pollet)
	   (input   "A name of a theory (symbol or string) and a boolean key.")
	   (effect  "The problems of the theory are loaded from MBase."
		    "If FORCE is T, they are always loaded. If the theory itself is not"
		    "yet loaded it will be. If FORCE is T, it is always reloaded.") 
	   (value   "The theory if everthing went all right, otherwise nil."))
  (when (th~require-only thy-name :force force)
    (dolist (def (th=mb-get-object (th=mb-compose-getnames thy-name 'problems))) 
      (setf (gethash (th=read-string def) prob*problem-hash-table) (list :mbase thy-name)))
      ;;;(th=mb-load-object (th=mb-compose-get thy-name def 'problem) :force force))
    (th=message ";;; MBase Problems loaded for theory ~A." thy-name)
    (th~find-theory thy-name)))

(defun th=mb-load-rules (thy-name &key (force t))
  (when (th~require-only thy-name :force force)
    (when (th=mb-load-object (th=mb-compose-getfile thy-name 'rules) :force force)
      (th=message ";;; MBase Rules loaded for theory ~A." thy-name)))
  (th~find-theory thy-name))

(defun th=mb-load-tactics (thy-name &key (force t))
  (when (th~require-only thy-name :force force)
    (when (th=mb-load-object (th=mb-compose-getfile thy-name 'tactics) :force force)
      (th=message ";;; MBase Tactics loaded for theory ~A." thy-name)))
  (th~find-theory thy-name))

(defun th=mb-load-methods (thy-name &key (force t))
  (when (th~require-only thy-name :force force)
    (when (th=mb-load-object (th=mb-compose-getfile thy-name 'methods) :force force)
      (th=message ";;; MBase Methods loaded for theory ~A." thy-name)))
  (th~find-theory thy-name))

(defun th=mb-load-crules (thy-name &key (force t))
  (when (th~require-only thy-name :force force)
    (when (th=mb-load-object (th=mb-compose-getfile thy-name 'crules) :force force)
      (th=message ";;; MBase Control-rules loaded for theory ~A." thy-name)))
  (th~find-theory thy-name))

(defun th=mb-load-metaprds (thy-name &key (force t))
  (when (th~require-only thy-name :force force)
    (when (th=mb-load-object (th=mb-compose-getfile thy-name 'metaprds) :force force)
      (th=message ";;; MBase Meta-predicates loaded for theory ~A." thy-name)))
  (th~find-theory thy-name))

(defun th=mb-load-lingu (thy-name &key (force t))
;  (when (th~require-only thy-name :force force)
;    (when (th=mb-load-object (th=mb-compose-getfile thy-name 'lingu) :force force)
;      (th=message ";;; MBase linguistic knowledge loaded for theory ~A." thy-name)))
;  (th~find-theory thy-name))
  (th=message ";;; Linguistic knowledge from MBase is currently not available.")
  (th~find-theory thy-name))

;; Load single items 

(defun th=mb-load-problem (thy-name prob-name &key (force t))
  (when (th~require-only thy-name :force force)
      (th=mb-load-object (th=mb-compose-get thy-name prob-name 'problem) :force force))
  (th~find-problem prob-name thy-name))

(defun th=mb-load-theorem (thy-name prob-name &key (force t))
  (when (th~require-only thy-name :force force)
      (th=mb-load-object (th=mb-compose-get thy-name prob-name 'theorem) :force force))
  (th~find-problem prob-name thy-name))

;; General MBase interface functions

(defun th=mb-get-object (command)
  (declare (edited  "08-FEB-1999")
	   (authors Pollet)
	   (input   "A MBase command.")
	   (effect  "none")
	   (value   "MBase's reply converted to symbols or an error message."))
  (let ((answer (mbase~call command)))
    (if (null answer)  (mbase~error-message) (read-from-string answer))))

(defun th=mb-load-object (command &key (force nil))
  (declare (edited  "08-FEB-1999")
	   (authors Pollet)
	   (input   "A MBase command.")
	   (effect  "Evaluates the MBase reply." )
	   (value   "T if everthing went all right or an error message."))
     (if (or force (not (th=loaded command)))
	 (let ((answer (mbase~call command)))
	   (when (plusp (length answer))
	     (with-input-from-string (in answer)
				     (do ((x (read in nil) (read in nil)))
					 ((null x) t)
				       (eval x)))
	     (setf (th=loaded command) t)))
       t))

;; Compose Mbase commands

(defun th=mb-compose-getfile(thy-name &optional part)
  (declare (edited  "08-FEB-1999")
	   (authors Pollet)
	   (input   "A theory name and something like rules, methods, etc.")
	   (effect  "none")
	   (value   "The MBase command getFile(thy-name part $) as string."))
      (concatenate 'string "getFile\(\""
			  (string-downcase thy-name)
			  "\" \'"
			  (string-downcase part)
			  "\' $\)"))

(defun th=mb-compose-get (thy-name &optional obj-name kind)
  (declare (edited  "08-FEB-1999")
	   (authors Pollet)
	   (input   "A theory name and as option the name of a definition or"
		    "theorem or simplifier and def or theorem etc. as kind.")
	   (effect  "none")
	   (value   "The MBase command get(thy-name kind(obj-name) $) or"
		    "get(thy-name theory $) if the optional params are missing."))
  (if kind  (concatenate 'string "get\(\""
			       (string-downcase thy-name)
			       "\" "
			       (string-downcase kind)
			       "\(\""
			       (string-downcase obj-name)
			       "\"\) $\)")
    (concatenate 'string "get\(\""
		       (string-downcase thy-name)
		       "\" theory $\)")))

(defun th=mb-compose-getnames (thy-name kind)
  (declare (edited  "08-FEB-1999")
	   (authors Pollet)
	   (input   "A theory name and something like def or theorem as kind.")
	   (effect  "none")
	   (value   "The MBase command getNames(thy-name kind $) as string."))
      (concatenate 'string "getNames\(\""
			  (string-downcase thy-name)
			  "\" "
			  (string-downcase kind)
			  " $\)"))

(defun th=mb-compose-getproofobj (id kind)
      (concatenate 'string "getProofObj\("
			  (string-downcase id)
			  " "
			  (string-downcase kind)
			  " $\)"))

(defun th=mb-compose-getproofs (thy-name thm-name kind)
  (concatenate 'string "getProofs\(\""
	       (string-downcase thy-name)
	       "\" \""
	       (string-downcase thm-name)
	       "\" "
	       (string-downcase kind)
	       " $\)"))

(defun th=mb-compose-gettheories ()
  (concatenate 'string "getTheories\( $\)"))
|#


#|
;; Execute logs from MBase 
(defgeneric th=mb-execute-replay (theo &key (all nil))
(declare (edited  "16-DEC-1998")
         (authors Pollet)
         (input   "A theorem of the current theory and a key.")
         (effect  "Executes the replay-file and asks, if there"
                  "are more than one replays. If ALL is true,"
                  "all replay files of the theorem will be executed.")
         (value   "undef."))
(:method ((theo string) &key all)
         (declare (ignore all))
         (error ";;;TH~~EXECUTE-THEORY-REPLAY: ~A" theo))
(:method ((theo (eql t)) &key all)
         (declare (ignore all))
         (th=mb-execute-replay (th=all-theories)))
(:method ((theo cons) &key all)
         (declare (ignore all))
         (dolist (theory theo)
           (th=mb-execute-replay (or (th~find-theory theory)
                                     (th~require-completely theory)
                                     "Theory not available."))))
(:method ((theo th+theory) &key all)
         (declare (ignore all))
         (let* ((thy-name (keim~name theo))
                (all (append (th=mb-get-object
                              (th=mb-compose-getnames thy-name 'problems))
                             (th=mb-get-object
                              (th=mb-compose-getnames thy-name 'theorems))))
                (all-with-prf (mapcan #'(lambda (x)
                                          (not (th=mb-get-object
                                                (th=mb-compose-getproofs
                                                 thy-name x 'rpys))) (list x))
                                      all)))
           (dolist (prob all-with-prf)
             (th=mb-execute-replay (or (prob~find-problem prob)
                                       (th=mb-load-theorem thy-name prob)
                                       (th=mb-load-problem thy-name prob)
                                       "Problem not available.") :all t))))
(:method ((theo prob+problem) &key all)
         (let* ((theory (keim~name (prob~theory theo)))
                (name (keim~name theo))
                (rpys (th=mb-get-object (th=mb-compose-getproofs theory name 'rpys)))
                (replay-number (if (and (plusp (length rpys)) (not all))
                                    (list (inter~prompt-for-input
                                     (asi~create)
                                     (format nil "The replays ~A are available for ~A. Enter a number." rpys name)
                                     'number)) rpys)))
           (cond ((null rpys)
                  (th=mb-execute-replay "No replays."))
                 ((member (car replay-number) rpys :test-not)
                  (th=mb-execute-replay "You entered a wrong proof number."))
                 (T (dolist (n replay-number)
                      (th~prove
                       (pds~start-proof-plan name (th=new-proof-plan-name name)))
                      (th=mb-execute-log n)))))))

;; Load Proofs from MBase
                    
(defgeneric th=mb-load-theorem-proofs (theorem &key (all nil) (silent nil))
 (declare (edited  "03-JUN-1997")
          (authors Sorge)
          (input   "A theorem and two boolean.")
          (effect  "The proofs of the theorem are loaded and attached to the theorem."
                   "This is being done with user interaction unless all is set."
                   "If the silent flag is set everything will run without issueing any"
                   "warnings, questions etc. (CAUTION! Existent proofs for theorem will"
                   "be automatically destroyed if silent is switched on!)")
          (value   "The theorem."))
 (:method (theorem &key all silent)
          (declare (ignore all silent))
          (error ";;;TH~~LOAD-THEOREM-PROOFS: ~A is not a theorem." theorem))
 (:method ((theorem prob+problem) &key (all nil) (silent nil))
          (let* ((theory (keim~name (prob~theory theorem)))
                 (name (keim~name theorem))
                 (proofids (th=mb-get-object (th=mb-compose-getproofs theory name 'pds)))
                 (existent (prob~proofs theorem))
                 (inpchar (when (and existent (not silent))
                            (inter~prompt-for-input
                             (asi~create)
                             (format nil "Theory-Lookup: There already exist proofs for theorem ~A.
                                          Should new proofs (o)verwrite all old proofs,
                                          be (a)ppended or (n)ot loaded? (o/a/n) :" name) 'symbol))))
            (macrolet ((th=loading-with-prompt ()
                                               `(if (null proofids)
                                                    (unless silent
                                                      (warn "Theory-Lookup: No Proofs for the theorem ~A." theorem))
                                                  (do* ((n 1 (1+ n))
                                                        (prf proofids (cdr prf))
                                                        (plist (list (th=mb-load-single-proof (car prf)) 
                                                               (cons (th=mb-load-single-proof (car prf)) plist))))
                                                        ((null (cdr prf)) plist)
                                                    (let ((y-n (if (or all silent) t
                                                                 (inter~prompt-for-input
                                                                  (asi~create)
                                                                  (format nil "Theory-Lookup: ~A proofs read for theorem ~A. Load proof-object ~A ? (y/n) :" n name (second prf)) 'boolean))))
                                                      (unless y-n (return plist)))))))
              (when (or (null inpchar) (string-equal inpchar 'o))
                (setf (prob~proofs theorem) (th=loading-with-prompt)))
              (when (string-equal inpchar 'a)
                (prob~add-proof theorem (th=loading-with-prompt)))
              theorem))))

(defun th=mb-load-single-proof (proofid)
 (let ((answer (th=mb-get-object (th=mb-compose-getproofobj proofid 'pds))))
   (with-input-from-string (in answer)
                           (post~read-object (read in) (env~create) nil))))
|#


#|
;;;;;;;;;;;;;;;;;;;;;;;;
;;The section with sorts (obsolete)
;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro th~deftd (name theory &rest attribs)
 (if (string-equal (car theory) :in)      
     (setq theory (cadr theory))
   (setq theory (car theory)))
 (if (th~find-theory theory)
     (let* ((theory (th~find-theory theory))
            (env (th~env theory))
            (connective-type)
            (type-vars)
            (decl)
            (node)
            (help ""))
       (do ((attribs (cdr attribs) (cdr attribs))
            (attrib (car attribs) (car attribs)))
           ((and (null attrib) (null attribs)))
         (if (consp attrib)
             (let ((carattrib (if (symbolp (car attrib)) 
                                  (symbol-name (car attrib)) 
                                (car attrib))))
               (cond ((string-equal carattrib :vars))
                     ((string-equal carattrib :termdecl)
                      (setq decl (cdr attrib)))
                     ((string-equal carattrib :author)); do nothing
                     ((string-equal carattrib :credit)); do nothing
                     ((string-equal carattrib :reference)); do nothing
                     ((string-equal carattrib :date)); do nothing
                     ((string-equal carattrib :help)
                      (setq help (cadr attrib)))
                     (t
                      (error
                       ";;;th~~defdef ~A: Not expecting ~S" name attrib))))
           (error
            ";;;th~~defdef ~A: ~A is not a valid attribute specification"
            name attrib)))
       (post~read-object decl env :termdecl)
       nil)
   (error ";;;th~~defdef ~A: ~A is not an existing theory" name theory)))

(defmacro th~defsubsort (name theory &rest attribs)
 (if (string-equal (car theory) :in)      
     (setq theory (cadr theory))
   (setq theory (car theory)))
 (if (th~find-theory theory)
     (let* ((theory (th~find-theory theory))
            (env (th~env theory))
            (connective-type)
            (subrelation)
            (help ""))
       (do ((attribs (cdr attribs) (cdr attribs))
            (attrib (car attribs) (car attribs)))
           ((and (null attrib) (null attribs)))
         (if (consp attrib)
             (let ((carattrib (if (symbolp (car attrib)) 
                                  (symbol-name (car attrib)) 
                                (car attrib))))
               (cond ((string-equal carattrib :subsort)
                      (setq subrelation (cdr attrib)))
                     ((string-equal carattrib :author)); do nothing
                     ((string-equal carattrib :credit)); do nothing
                     ((string-equal carattrib :reference)); do nothing
                     ((string-equal carattrib :date)); do nothing
                     ((string-equal carattrib :help)
                      (setq help (cadr attrib)))
                     (t
                      (error
                       ";;;th~~defdef ~A: Not expecting ~S" name attrib))))
           (error
            ";;;th~~defdef ~A: ~A is not a valid attribute specification"
            name attrib)))
       (post~read-object subrelation env :subsort)
       nil)
   (error ";;;th~~defdef ~A: ~A is not an existing theory" name theory)))


(defmacro th~defsort (name theory &rest attribs)
 (if (string-equal (car theory) :in)      
     (setq theory (cadr theory))
   (setq theory (car theory)))
 (if (th~find-theory theory)
     (let* ((theory (th~find-theory theory))
            (env (th~env theory))
            (by)
            (type)
            (range)
            (domain)
            (node)
            (help ""))
       (do ((attribs (cdr attribs) (cdr attribs))
            (attrib (car attribs) (car attribs)))
           ((and (null attrib) (null attribs)))
         (if (consp attrib)
             (let ((carattrib (if (symbolp (car attrib)) 
                                  (symbol-name (car attrib)) 
                                (car attrib))))
               (cond ((string-equal carattrib :type)
                      (setf type (cadr attrib)))
                     ((string-equal carattrib :domain)
                      (setf domain (cadr attrib)))
                     ((string-equal carattrib :range)
                      (setf range (cadr attrib)))
                       ((string-equal carattrib :by)
                      (setf range (cadr attrib)))
                     ((string-equal carattrib :author)); do nothing
                     ((string-equal carattrib :credit)); do nothing
                     ((string-equal carattrib :reference)); do nothing
                     ((string-equal carattrib :date)); do nothing
                     ((string-equal carattrib :help)
                      (setq help (cadr attrib)))
                     (t
                      (error
                       ";;;th~~defsort ~A: Not expecting ~S" name attrib))))
           (error
            ";;;th~~defsort ~A: ~A is not a valid attribute specification"
            name attrib)))
       (post~read-object (list name
                               (if (and domain range)
                                   (list domain range)
                                 type))
                               env :sort-constant)
       nil)
   (error ";;;th~~defsort ~A: ~A is not an existing theory" name theory)))
|#

;;; things for output

(eval-when (load compile eval)
(defmacro th=output (stream)
  (declare (edited  "02-MAR-1999" "03-MAR-1998")
	   (authors Pollet Sorge Konrad)
	   (input   "A function specifying the interface-stream.")
	   (effect  "Prints arguments to the specified stream of the OMEGA interface."
		    "Same as omega=output.")
	   (value   "NIL"))
  `(let ((interface (if (and (boundp 'comint*current-comint) comint*current-comint)
			(comint~interface comint*current-comint)
		      (asi~create))))
     (,stream interface (apply #'format nil args))))
)

(defun th=message (&rest args)
  (when (member 'theory th*output) (th=output inter~print-message)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Things for the HTML-Manual in omega-help.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defgeneric th~lml (theorem)
  (:method (theorem)
	   (error ";;;TH~~EXECUTE-THEORY-REPLAY: ~A is not a theorem." theorem))
  (:method ((theorem prob+problem) )
	   (let* ((theory (keim~name (prob~theory theorem)))
		  (name (keim~name theorem))
	          (replay-quantity (th=count-replay-files theory name)))
	     (when  (plusp replay-quantity )
			    (th=make-command-list (car (th=compose-replay-file-name theory name 1))) ))))

(defun th=make-command-list (path)
  (let ((current (sys~current-directory))
	commandlist)
    (sys~handler-case
     (with-open-file (in path :direction :input
			 :if-does-not-exist :error)
		     (sys~chdir (directory-namestring path))
		     (loop (let ((input-list
				  (sys~handler-case
				   (asi~linereadp in)
				   (asi+no-input-error () nil)))
				 (next-input (peek-char t in nil 'eof)))
			     (when input-list 
			       (setf commandlist (cons input-list commandlist)))
			     (cond ((equal next-input #\;) (read-line in))
				   ((equal next-input 'eof) (return-from th=make-command-list
							      (reverse commandlist))))))
		     (sys~chdir current))
     (file-error (c)
		 (sys~chdir current)
		 (inter~print-error (comint~interface comint*current-comint) c))
     (error (c)
	    (sys~chdir current)
	    (error c)))))


;;;;;;;;;;;;;
;;; Testing:  New Method for Prob~Find-Problem
;;;;;;;;;;;;;

;(defmethod prob~find-problem (name)
;  (let ((obj (gethash (prob=read-string name) prob*problem-hash-table)))
;    (if (consp obj)
;        (let ((mb? (car obj))
;              (th  (cadr obj)))
;          (if (and (or (symbolp mb?) (stringp mb?))
;                   (string-equal mb? :mbase))
;              (th=mb-load-problem th name :force t)
;            (error ";;; Something went horribly wrong with loading problems from MBASE!!!"))
;          (gethash (prob=read-string name) prob*problem-hash-table))
;      obj)))


