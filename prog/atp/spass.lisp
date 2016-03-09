;;; -*- syntax: common-lisp; package: OMEGA; base: 10; mode: LISP -*-
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


(in-package :omega)
 
(mod~defmod spass :uses ()
	    :documentation "Calling SPASS in OMEGA"
	    :exports (
		      spass~program
		      spass~complete-spass-problem!
		      spass~generate-spass-problem
		      spass~add-in-string!
		      spass~call-spass
		      spass~generate-spass-problem-default!
		      ))

#|
(eval-when (load compile eval)
  (defclass spass+spass (tp+prover)
    ()
    (:documentation "The SPASS automatic theorem prover.")))
|#


;---------------------------------- defining global variables -----------------------
(defvar doof nil)

(defvar spass*local-clause-vars nil)
;; The list to store in locally (= in a clause) created variables

; the spass-prover
(defun spass~program ()
  (sys~getenv "SPASSHOME"))
;more flexible for dumping purposes; DEF
;(defvar *spass-prover* (sys~getenv "SPASSHOME"))

; the current proof problem
(defvar spass*current-proof-plan nil)

; the spass settings
(defvar spass*settings-list-old '("Splits" "Auto" "SortDel" "Memory" "TimeLimit" "FPModel" "DocSplit" "DocSortDel" "DocProof" "PFSub" "PBSub" "PRew" "PCond" "PTaut" "PObv" "PSoSim" "PSortDel" "PClRed" "PDer" "PEmptyClause" "PGiven" "PKept" "PProblem" "PStatistic" "PFlags"))

; the spass=new-settings (ohne CNF-Flags)
(defvar spass*settings-list '(
"Auto"            "CPara"           "COrdPara"        "CSupE"
"CSupP"           "Splits"          "Memory"          "TimeLimit"
"DocSST"          "DocDST"          "DocProof"        "DocSplit"        
"Loops"           "PSub"            "PRew"            "PCon"            
"PTaut"           "PObv"            "PSSi"            "PSST"            
"PClR"            "PUnC"            "PDer"            "PGiven"          
"PKept"           "PProblem"        "PEmptyClause"    "PStatistic"      
"FPModel"         "FPDFGProof"      "PFlags"          "POptSkolem"      
"Select"          "RInput"          "Sorts"           "SatInput"        
"WDRatio"         "FullRed"         "FuncWeight"      "VarWeight"       
"PrefVar"         "IEmS"            "ISoR"            "IEqR"            
"IEqF"            "IMPm"            "ISpR"            "ISpL"            
"IGeR"            "IGeF"            "IUnR"            "IBUR"            
"RFRew"           "RBRew"           "RFClR"           "RBClR"           
"RObv"            "RUnC"            "RTaut"           "RSST"            
"RSSi"            "RDST"            "RFSub"           "RBSub"           
"RCon"))

; the flag-setting - only for test purpose
; (defvar spass*flag-list '(-1 1 -1 -1 0 1 1 1 0 0 0 0 0 0 0 0 0 0 1 0 0 1 0 0))

; the current resolution-proof
(defvar spass*current-resolution-problem nil)

; the current clause-list of the problem with extendet Informations
(defvar spass*current-clause-list nil)

; the list of allowed chars in spass
(defvar spass*name-symbols '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L
			   #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X
			   #\Y #\Z #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j
			   #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v
			   #\w #\x #\y #\z #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7
			   #\8 #\9 #\_))

; the pairlist of the renamed functions, predicates and variables. First: New name, second term+variable or term+constant - object
(defvar spass*renamed-objects nil)

; the list of allowed functions in DFG-syntax
(defvar spass*dfg-predicates '("not" "implies" "implied" "equiv" "and" "or" "equal" "forall" "exists" "true" "false"))

; the counter of the new names to avoid duplications
(defvar spass*new-name-counter 0)

; the list of string-lines from the proof-file spass.dfg
(defvar spass*proof-file-lines nil)

; the allowed splitting-depth

(defvar spass*allowed-splittings 0)
					
;the list of the open splitting-levels

(defvar spass*open-splitting-levels nil)

; the list of the obvious-steps, contains the spass-clauses-numbers, where an obvious reduction was applied
(defvar spass*obvious-reductions nil)

(defvar spass*problem-contains-equality nil)
(defvar spass*reflex-clause nil)
;(defvar spass*symmetrie-clause nil)
;(defvar spass*transitive-clause nil)

; spass-string-file
(defvar spass*in-string "")

;----------------------the spass-main-routine-----------------------------------------------------

#|

(defun spass~call-spass (open-node ho-pds directory ressource auto-mode splitting-level parse-back)
  (declare (edited  "22-JUN-1997")
	   (authors Naumann)
	   (input   "A current problem and a directory.")
	   (effect  "The file spass.cnf and spass.proof are obtaines in directory.")
	   (value   "First computes the cnf of the problem. Then produce a spass.cnf file in dfg-syntax, then tries to compute the proof with SPASS and then parses the spass.proof file und translate it into a omega-consistent resolution-proof."))

  (setq spass*current-resolution-problem
        (atptop~resolution-proof-create-from-pds-open-node open-node ho-pds))

  (p2f~translate spass*current-resolution-problem)
  (format t "~% Normalizing ...")
  (hocnf~normalize-res-proof! spass*current-resolution-problem)
  
  ;; Fliegt raus ab Version 3.3
  ;; (spass=set-current-type-var-subst!)

  (if (env~lookup-object '= (res~proof-environment spass*current-resolution-problem))
      (progn ;(format t "Normalizing successful again1.......................~%")
	     (setq spass*problem-contains-equality t)
             ;(format t "Normalizing successful again1.......................~%")
	     (let*
	       ((new-var1 (term~variable-create 
			 (do* ((counter 1 (+ counter 1)))
			     ((null (env~lookup-object (intern (string-upcase (format nil "x~A" counter))
							       (find-package :omega))
						       (res~proof-environment spass*current-resolution-problem)))
			      (intern (string-upcase (format nil "x~A" counter))
				      (find-package :omega))))
			 (data~c-domain (term~type (env~lookup-object '= (res~proof-environment spass*current-resolution-problem))))))
		
		(just1 (res~reflex-create))
		(new-term (term~appl-create
			   (env~lookup-object '= (res~proof-environment spass*current-resolution-problem))
			   (list new-var1 new-var1)
			   ))
		(ref-clause (cl~create (list (lit~literal-create new-term t)))))
	       ;(format t "Normalizing successful again2.......................~%")
               (setf (node~justification ref-clause) just1)
	       (keim~put ref-clause :spass-clause-number (list 'used)) ; kann man sicher drueber streiten
					;(keim~put komm-clause :spass-clause-number (list 'used))
					;(keim~put trans-clause :spass-clause-number (list 'used))
	       ;(format t "Normalizing successful again3.......................~%")

	       ;; Entfaellt ab Version 3.3
	       ;; (setf (data~kappa new-term)
	       ;;   (list (data~n-domain
	       ;; 		    (term~type (env~lookup-object '= (res~proof-environment spass*current-resolution-problem))))))

	       (setq spass*reflex-clause ref-clause)
					;(setq spass*symmetrie-clause komm-clause)
					;(setq spass*transitive-clause trans-clause)
	       (keim~put spass*reflex-clause :splitting-level 0)
					;(keim~put komm-clause :splitting-level 0)
					;(keim~put trans-clause :splitting-level 0)
	       )))
  ;(format t "Base-clauses initialized...................~%")
  (multiple-value-bind
      (assumption-clauses conclusion-clauses)
      (spass=assumption-and-conclusion-clauses spass*current-resolution-problem)
    (setf (res~proof-clauses spass*current-resolution-problem)
	  (append assumption-clauses conclusion-clauses (list spass*reflex-clause))))
					; spass*symmetrie-clause spass*transitive-clause ; politische Entscheidung
  (setq spass*problem-contains-equality nil)
  (setq spass*flag-list (spass=take-flags auto-mode splitting-level)) ; hier ist der Benutzer gefordert
  (setq spass*renamed-objects nil)
  (setq spass*new-name-counter 0)
  (setq spass*proof-file-lines nil)
  (setq spass*current-clause-list nil)
  (setq spass*obvious-reductions nil)
  (setq spass*open-splitting-levels nil)
  (sys~call-system (format nil "mkdir ~A/spass-problem" directory))
  (spass~produce-input-file spass*current-resolution-problem
			    (format nil "~A/spass-problem" directory) spass*flag-list)
  (sys~call-system (format nil "cd ~A/spass-problem" directory))
  (if (probe-file (format nil "~A/spass-problem/spass.dfg" directory))
      (delete-file (format nil "~A/spass-problem/spass.dfg" directory)))
  (format t "SPASS is searching for proof ...~%")
  (sys~call-system (format nil
			   "~A ~A/spass-problem/spass.cnf > ~A/spass-problem/spass.dfg"
			   (spass~program)
			   directory
			   directory))
  (spass~translate-spass-proof spass*current-resolution-problem (format nil "~A/spass-problem" directory) parse-back))

|#

#|

Fliegt raus ab Version 3.3

(defun spass=set-current-type-var-subst! ()
  (declare (edited  "27-MAR-1998")
	   (authors "Ameier, Naumann")
	   (input   "None.")
	   (effect  "If the otter*current-environment contains a type-var-subst, the global"
		    "variable otter*current-type-var-subst is set to this value, otherwise"
		    "a new substitution is created, added with key type-var-subst in the"
		    "environment and otter*current-type-var-subst is set to this new substitution.")
	   (value   "Undefined."))
  (let* ((type-var-subst (env~lookup-object 'type-var-subst (res~proof-environment spass*current-resolution-problem))))
    (if type-var-subst
	(setq spass*current-type-var-subst type-var-subst)
      (let ((new-subst (subst~create nil nil)))
	(setq spass*current-type-var-subst new-subst)
	(env~enter 'type-var-subst new-subst (res~proof-environment spass*current-resolution-problem))))))

|#

;----------------------producing the spass.cnf - file----------------------------------------------

#|(defun spass~produce-input-file (problem directory flags)
  (declare (edited  "13-AUG-1997")
	   (authors Naumann)
	   (input   "A Problem in cnf, a list of flags and a directory, where the spass.cnf file should be written.")
	   (effect  "Produces a file in DFG-Syntax with the cnf of the problem")
	   (value   "None."))
  (let* ((file (format nil "~A~A" directory "/spass.cnf")))
    (with-open-file (stream file :direction :output :if-exists :supersede)
    (format stream "begin_problem(MACHT_SPASS).~2%")
    (format stream "list_of_descriptions.~%")
    (format stream "name({*none*}).~%author({*omega*}).~%status(unknown).~%description({*none*}).~%")
    (format stream "end_of_list.~2%")
    (format stream "list_of_symbols.~%")
    (spass=write-functions stream problem)
    (spass=write-predicates stream problem)
    (format stream "end_of_list.~2%")
    (format stream "list_of_clauses (axioms, cnf).~%")
    (spass=write-axiom-clauses stream problem)
    (format stream "end_of_list.~2%")
    (format stream "list_of_clauses (conjectures, cnf).~%")
    (spass=write-conclusion-clauses stream problem)
    (format stream "end_of_list.~2%")
    (format stream "list_of_settings(SPASS).~%{*~%")
    (spass=write-flags stream flags spass*settings-list)
    (format stream "*}~%end_of_list.~2%")
    (format stream "end_problem.")))) |#

(defun spass=add-string-to-in-string (string)
  (declare (edited  "25-MAR-1998")
	   (authors Ameier)
	   (input   "A string.")
	   (effect  "Adds the string to the spass*in-string.")
	   (value   "Undefined."))
  (setq spass*in-string (format nil "~A~A" spass*in-string string)))

(defun spass=produce-input-string (problem flags)
  (declare (edited  "13-AUG-1997")
	   (authors Naumann Ameier)
	   (input   "A Problem in cnf, a list of flags and a directory.")
	   (effect  "Produces a in-string for spass in DFG-Syntax with the cnf of the problem in the"
		    "global variable spass*in-string.")
	   (value   "None."))

  (setq spass*in-string "")
  
  (spass=add-string-to-in-string "begin_problem(MACHT_SPASS).")
  (spass=add-string-to-in-string #\Newline)
  (spass=add-string-to-in-string #\Newline)
  (spass=add-string-to-in-string "list_of_descriptions.")
  (spass=add-string-to-in-string #\Newline)
  (spass=add-string-to-in-string "name({*none*}).")
  (spass=add-string-to-in-string #\Newline)
  (spass=add-string-to-in-string "author({*omega*}).")
  (spass=add-string-to-in-string #\Newline)
  (spass=add-string-to-in-string "status(unknown).")
  (spass=add-string-to-in-string #\Newline)
  (spass=add-string-to-in-string "description({*none*}).")
  (spass=add-string-to-in-string #\Newline)
  (spass=add-string-to-in-string "end_of_list.")
  (spass=add-string-to-in-string #\Newline)
  (spass=add-string-to-in-string #\Newline)
  (spass=add-string-to-in-string "list_of_symbols.")
  (spass=add-string-to-in-string #\Newline)
 
  (spass=write-functions problem)
  (spass=write-predicates problem)

  (spass=add-string-to-in-string "end_of_list.")
  (spass=add-string-to-in-string #\Newline)
  (spass=add-string-to-in-string #\Newline)
  (spass=add-string-to-in-string "list_of_clauses (axioms, cnf).")
  (spass=add-string-to-in-string #\Newline)

  (spass=write-axiom-clauses problem)

  (spass=add-string-to-in-string "end_of_list.")
  (spass=add-string-to-in-string #\Newline)
  (spass=add-string-to-in-string #\Newline)
  (spass=add-string-to-in-string "list_of_clauses (conjectures, cnf).")
  (spass=add-string-to-in-string #\Newline)
  
  (spass=write-conclusion-clauses problem)

  (spass=add-string-to-in-string "end_of_list.")
  (spass=add-string-to-in-string #\Newline)
  (spass=add-string-to-in-string #\Newline)
  (spass=add-string-to-in-string "list_of_settings(SPASS).")
  (spass=add-string-to-in-string #\Newline)
  (spass=add-string-to-in-string "{*")
  (spass=add-string-to-in-string #\Newline)
  
  (spass=write-flags flags spass*settings-list)

  (spass=add-string-to-in-string "*}")
  (spass=add-string-to-in-string #\Newline)
  (spass=add-string-to-in-string "end_of_list.")
  (spass=add-string-to-in-string #\Newline)
  (spass=add-string-to-in-string #\Newline)
  (spass=add-string-to-in-string "end_problem."))

(defun spass=write-functions (res-proof)
  (declare (edited  "25-OCT-1999")
	   (authors Ameier)
	   (input   "A resolution proof.")
	   (effect  "Prints the n-ary-functions in DFG-Format to spass*in-string.")
	   (value   "Undefined."))

  (let* ((env (res~proof-environment res-proof))
	 (literal-atoms (mapcar #'lit~atom (apply #'append (mapcar #'cl~literals (res~proof-initial-clauses res-proof)))))
	 (literal-atoms-terms (apply #'append (mapcar #'(lambda (atom)
							  (if (data~appl-p atom)
							      (data~appl-arguments atom)
							    nil))
						      literal-atoms)))
	 (function-constant-arity-pair-list (remove-duplicates (apply #'append (mapcar #'(lambda (term)
											 (spass=all-subterm-constants-and-arities
											  term))
										     literal-atoms-terms))
							     :test #'(lambda (pair1 pair2)
								       (and (or (eq (first pair1) (first pair2))
										(and (data~equal (first pair1) (first pair2))
										     (data~equal (term~type (first pair1))
												 (term~type (first pair2)))))
									    (equal (second pair1) (second pair2)))))))
    (cond (function-constant-arity-pair-list
	   (let* ((head-pair (first function-constant-arity-pair-list)))
	     (progn (spass=add-string-to-in-string "functions[")
		    (spass=add-string-to-in-string (format nil "(~A, ~A)"
							   ;; Ab Version 3.3 werden direkt die Objekte aus den Termen geschrieben
							   ;; und nicht die Env-Versionen, um die Polymorphy richtig zu handeln!
							   ;;(spass=rename-symbol (env~lookup-object (first head-pair) env))
							   (spass=rename-symbol (first head-pair))
							   (second head-pair)))
		    (mapc #'(lambda (pair)
			      (spass=add-string-to-in-string (format nil ", (~A, ~A)"
								     ;; Ab Version 3.3 werden direkt die Objekte aus den Termen geschrieben
								     ;; und nicht die Env-Versionen, um die Polymorphy richtig zu handeln!
								     ;; (spass=rename-symbol (env~lookup-object (first pair) env))
								     (spass=rename-symbol (first pair))
								     (second pair))))
			  (rest function-constant-arity-pair-list))
		    (spass=add-string-to-in-string "].")
		    (spass=add-string-to-in-string #\Newline))))
	  (t
	   nil))))


(defgeneric spass=all-subterm-constants-and-arities (term)
  (declare (edited  "25-OCT-1999")
	   (authors Ameier)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "A list of all pairs (c a) where c is a constant in the term and a is the"
		    "arity of the occurrence of this symbol."))
  
  (:method ((appl term+appl))
	   (let* ((function (data~appl-function appl))
		  (args (data~appl-arguments appl)))
	     (cons (list function (length args))
		   (apply #'append (mapcar #'(lambda (term)
					       (spass=all-subterm-constants-and-arities term))
					   args)))))
  (:method ((const term+constant))
	   (list (list const 0)))
  (:method ((var term+variable))
	   nil))



(defun spass=write-predicates (res-proof)
  (declare (edited  "25-OCT-1999")
	   (authors Ameier)
	   (input   "A resolution problem.")
	   (effect  "Prints the n-ary-predicates in DFG-Format to spass*in-string (exept of the predicates known by SPASS).")
	   (value   "Undefined."))
  (let* ((env (res~proof-environment res-proof))
	 (literal-atoms (mapcar #'lit~atom (apply #'append (mapcar #'cl~literals (res~proof-initial-clauses res-proof)))))
	 (predicates-constant-and-arity-pairs (remove-duplicates (mapcar #'(lambda (atom)
									     (if (data~appl-p atom)
										 (list (data~appl-function atom)
										       (length (data~appl-arguments atom)))
									       (list atom 0)))
									 literal-atoms)
								 :test #'(lambda (pair1 pair2)
									   (and (or (eq (first pair1) (first pair2))
										    (and (data~equal (first pair1) (first pair2))
											 (data~equal (term~type (first pair1))
												     (term~type (first pair2)))))
										(equal (second pair1) (second pair2))))))
	 ;; = ist SPASS bereits bekannt und muss/darf nicht nocheinmal ruebergeschickt werden!
	 ;; -> Paar mit = (falls vorhanden) loeschen!
	 (checked-predicates-constant-and-arity-pairs (remove-if #'(lambda (pair)
								     (string-equal (keim~name (first pair)) '=))
								 predicates-constant-and-arity-pairs))
	 (=predicate-pair (remove-if-not #'(lambda (pair)
					     (string-equal (keim~name (first pair)) '=))
					 predicates-constant-and-arity-pairs)))
    
    (when =predicate-pair
      ;; falls = kommt vor
      ;; -> auch = muss uebersetzt werden, und zwar nach equal
      ;; Bei = brauchen wir wirklich die env Version die gekappate, die dreckige ...
      (spass=rename-symbol (env~lookup-object '= env)))
      
      
    (cond (checked-predicates-constant-and-arity-pairs 
	   (let* ((head-pair (first checked-predicates-constant-and-arity-pairs)))
	     (progn (spass=add-string-to-in-string "predicates[")
		    (spass=add-string-to-in-string (format nil "(~A, ~A)"
							   ;; Ab Version 3.3 werden direkt die Objekte aus den Termen geschrieben
							   ;; und nicht die Env-Versionen, um die Polymorphy richtig zu handeln!
							   ;;(spass=rename-symbol (env~lookup-object (first head-pair) env))
							   (spass=rename-symbol (first head-pair))
							   (second head-pair)))
		    (mapc #'(lambda (pair)
			      (spass=add-string-to-in-string (format nil ", (~A, ~A)"
								     ;; Ab Version 3.3 werden direkt die Objekte aus den Termen geschrieben
								     ;; und nicht die Env-Versionen, um die Polymorphy richtig zu handeln!
								     ;;(spass=rename-symbol (env~lookup-object (first pair) env))
								     (spass=rename-symbol (first pair))
								     (second pair))))
			  (rest checked-predicates-constant-and-arity-pairs))
		    (spass=add-string-to-in-string "].")
		    (spass=add-string-to-in-string #\Newline))))
	  (t
	   nil))))


(defun spass=equality-p (symbol)
  (declare (edited  "24-JUN-1997")
	   (authors Naumann)
	   (input   "A symbol.")
	   (effect  "None.")
	   (value   "True, if symbol is in the environment and is =."))
  (string= (keim~name (env~lookup-object symbol (res~proof-environment spass*current-resolution-problem))) "="))

(defun spass=assumption-and-conclusion-clauses (res-proof)
  (declare (edited  "17-SEP-1996")
	   (authors "Ameier, Naumann")
	   (input   "A resolution-proof.")
	   (effect  "None.")
	   (value   "Multiple-value:"
		    "First: All clauses which accords (delta-relation) to assumption-lines."
		    "Second: All clauses which accords (delta-relation) to conclusion-line."
		    "The reflexivity-clause (if used) is added to the clauses of the conclusion-line."))
  (let* ((delta-relation (res~proof-delta-relation res-proof))
	 (clauses (res~proof-initial-clauses res-proof)))
    (do* ((rest-clauses clauses (rest rest-clauses))
	  (assumption-clauses nil)
	  (conclusion-clauses nil))
	((null rest-clauses) (values assumption-clauses conclusion-clauses))
      (let* ((head (first rest-clauses)) ; koennte umgebaut werden, so dass die eq-clauses assumptions werden
	     (pair-with-head (if (or (keim~equal head spass*reflex-clause)
				     ;(keim~equal head spass*symmetrie-clause)
				     ;(keim~equal head spass*transitive-clause)
				     )
				 'eq-clauses
			       (first (remove-if-not #'(lambda (pair)
							 (eq (delta~delta-clause pair) head))
						     (delta~relation-pairs delta-relation))))))
	(if (eq pair-with-head 'eq-clauses) nil
	  (if (eq (delta~delta-formula pair-with-head) (res~proof-conclusion res-proof))
	      (setq conclusion-clauses (append conclusion-clauses (list head)))
	    (setq assumption-clauses (append assumption-clauses (list head)))))))))

(defun spass=write-axiom-clauses (problem)
  (declare (edited  "18-JUN-1997")
	   (authors Naumann Ameier)
	   (input   "A problem.")
	   (effect  "The axiom-clauses of the problem are written to spass*in-string in DFG-Syntax.")
	   (value   "Undefined."))
  (mapc #'(lambda (clause)
	    (progn (spass=add-string-to-in-string "clause(")
		   (spass=write-clause clause)
		   (spass=add-string-to-in-string ").")
		   (spass=add-string-to-in-string #\Newline)))
	(first
	 (multiple-value-list (spass=assumption-and-conclusion-clauses problem)))))

(defun spass=write-conclusion-clauses (problem)
  (declare (edited  "18-JUN-1997")
	   (authors Naumann Ameier)
	   (input   "A problem.")
	   (effect  "The conclusion-clauses of the problem are written to spass*in-string in DFG-Syntax.")
	   (value   "Undefined."))
   (mapc #'(lambda (clause)
	    (progn (spass=add-string-to-in-string "clause(")
		   (spass=write-clause clause)
		   (spass=add-string-to-in-string ").")
		   (spass=add-string-to-in-string #\Newline)))
	 (second
	  (multiple-value-list (spass=assumption-and-conclusion-clauses problem)))))

(defun spass=write-clause (clause)
  (declare (edited  "18-JUN-1997")
	   (authors Naumann Ameier)
	   (input   "A clause.")
	   (effect  "The clause is written to spass*in-string in DFG-Format.")
	   (value   "Undefined."))
  (cond ((spass=var-in-literals (cl~literals clause) nil)
         (progn
	   (spass=add-string-to-in-string
	    (format nil "forall([~A" (spass=rename-symbol (car (spass=var-in-literals (cl~literals clause) nil)))))
	   (mapc #'(lambda (var)
		  	    (spass=add-string-to-in-string (format nil ",~A" (spass=rename-symbol var))))
		 (rest (spass=var-in-literals (cl~literals clause) nil)))
	   (spass=add-string-to-in-string "],"))))
  (spass=add-string-to-in-string "or(")
  (spass=write-term (first (cl~literals clause)))
  (mapc #'(lambda (literal)
	    (spass=add-string-to-in-string ",")
	    (spass=write-term literal))
	(rest (cl~literals clause)))
  (spass=add-string-to-in-string ")")
  (cond ((spass=var-in-literals (cl~literals clause) nil)
	 (spass=add-string-to-in-string ")"))))

(defgeneric spass=write-term (thing)
  (declare (edited  "18-JUN-1997")
	   (authors Naumann Ameier)
	   (input   "A literal or an term.")
	   (effect  "The literal or term is written to the spass*in-string.")
	   (value   "Undefined."))
  (:method ((literal lit+literal))
	   (if (lit~positive-p literal) (spass=write-term (lit~atom literal))
	     (progn (spass=add-string-to-in-string "not(")
		    (spass=write-term (lit~atom literal))
		    (spass=add-string-to-in-string ")"))))
  (:method ((symconst term+constant))
	   (cond ((keim~equal symconst (env~lookup-object 'true (res~proof-environment spass*current-resolution-problem)))
                  (spass=add-string-to-in-string "true"))
		 ((keim~equal symconst (env~lookup-object 'false (res~proof-environment spass*current-resolution-problem)))
		  (spass=add-string-to-in-string "false"))
		 (t
		  (spass=add-string-to-in-string
		   (format nil "~A" (spass=new-name symconst spass*renamed-objects))))))
  (:method ((symvar term+variable))
	   (spass=add-string-to-in-string
	    (format nil "~A" (spass=new-name symvar spass*renamed-objects))))
  (:method ((term term+appl))
	   (spass=add-string-to-in-string
	    (format nil "~A(" (spass=new-name (data~appl-function term) spass*renamed-objects)))
           (spass=write-term (first (data~appl-arguments term)))
	   (mapc #'(lambda (argument)
			   (spass=add-string-to-in-string ",")
			   (spass=write-term argument))
		 (rest (data~appl-arguments term)))
	   (spass=add-string-to-in-string ")")))

(defun spass=var-in-literals (literal-list free-var-list)
  (declare (edited  "18-JUN-1997")
	   (authors Naumann)
	   (input   "A literal-list and a free-var-list (input nil).")
	   (effect  "None.")
	   (value   "The list of symbols of all free Variables of the clause (duplicate-removed)."))
  (cond ((null literal-list) free-var-list)
	(t (spass=var-in-literals (cdr literal-list) (remove-duplicates (append free-var-list
							     (remove-duplicates (data~free-variables (lit~atom (car literal-list))) :test 'keim~equal)) :test 'keim~equal)))))

; the flags to change with ~A

(defun spass=write-flags (flags settings)
  (declare (edited  "20-JUN-1997")
	   (authors Naumann Ameier)
	   (input   "A list of flags and the settings (spass*setting-list")
	   (effect  "Flags and settings are added to the spass*in-string")
	   (value   "Undefined."))
  (cond ((null flags)
	 nil)
	(t
	 (spass=add-string-to-in-string
	  (format nil "set_flag(~A,~A)." (car settings) (car flags)))
	 (spass=add-string-to-in-string #\Newline)
	 (spass=write-flags (cdr flags) (cdr settings)))))

(defun spass=rename-symbol (const-var-thing)
  (declare (edited  "21-JUN-1997")
	   (authors Naumann)
	   (input   "A term+constant or term+variable object.")
	   (effect  "None.")
	   (value   "The name of the thing is changed to a DFG-compatible name and to"
		    "spass*renamed-objects a new pair is added: the old and the new name."))

  (setq new-name "")

  (let* ((o-name (keim~name (if (term~schema-p const-var-thing)
				  (data~schema-range const-var-thing)
				const-var-thing)))
	 (old-name (cond ((stringp o-name) o-name)
	 		 (t (format nil "~A" o-name)))))
    
    (setq new-name (cond ((string-equal old-name '=) "equal")
			 ((string-equal old-name 'true) "true")
			 ((string-equal old-name 'false) "false")
			 (t (progn
			      (do ((i 0 (+ i 1)))
				  ((>= i (length old-name)))
				(when (member (char old-name i) spass*name-symbols)
				  (setq new-name (format nil "~A~A" new-name (char old-name i)))))
			      (incf spass*new-name-counter)
			      (format nil "~A_~A" new-name spass*new-name-counter)))))

    (when (term~constant-p const-var-thing)
      ;; Constants start with downcase letters
      (setq new-name (string-downcase new-name)))
          
    (setq spass*renamed-objects (cons (list new-name const-var-thing) spass*renamed-objects))
    
    new-name))

(defun spass=new-name (const-var-thing liste)
  (declare (edited  "21-JUN-1997")
	   (authors Naumann)
	   (input   "A term+constant or term+variable object.")
	   (effect  "None.")
	   (value   "Return from liste (spass*renamed-objects) the new name from the keim-name of the"
		    "thing, if not found, (keim~name const-var-thing)."))
  (let* ((name-string (if (stringp (keim~name const-var-thing))
			  (keim~name const-var-thing)
			(format nil "~A" (keim~name const-var-thing)))))
    
    (if (string-equal name-string '=)
	"equal"
      (let* ((partner-string (first (first (member const-var-thing liste
						   :test #'(lambda (thing pair)
							     (let* ((thing2 (second pair)))
							       (or (eq thing thing2)
								   (and (data~equal thing thing2)
									(data~equal (term~type thing) (term~type thing2)))))))))))
	
	(if partner-string
	    partner-string
	  (keim~name const-var-thing))))))
  

  
;;  (cond ((null liste)
;;	 (keim~name const-var-thing))
;;	((keim~equal const-var-thing (second (car liste)))
;;	 (first (car liste)))
;;	(t
;;	 (spass=new-name const-var-thing (cdr liste)))))

(defun spass=old-keim-type (string liste) 
  (declare (edited  "02-JUL-1997")
	   (authors Naumann)
	   (input   "A string.")
	   (effect  "None.")
	   (value   "Returns from liste (spass*renamed-objects) the keim-object referring to string, if not found nil."))
  (cond ((null liste) nil)
	((string= (first (car liste)) string) (second (car liste)))
	(t (spass=old-keim-type string (cdr liste)))))
   
 
;----------------------------translate the spass-proof (spass.dfg - file)---------------

(defun spass=translate-spass-proof-from-string (res-proof spass-out-string parse-back)
  (declare (edited  "24-APR-1998")
	   (authors Ameier)
	   (input   "The partiall resolution proof (before parsing=without steps), the spass-out-string"
		    "and a flag to sign,  whether found proofs should be parsed or not.")
	   (effect  "If a proof was found and parse-back is true the spass-out-string is read"
		    "and the steps are inserted into the resolution proof.")
	   (value   "If: a proof was found and parse-back is true: the completed resolution proof"
		    "    a proof was found and parse-back is nil:  't"
		    "    no proof was found, but a completion: 'completion"
		    "    no proof was found and no completion: nil."))
  (setq spass*proof-file-lines (remove-if #'(lambda (line-string)
					      (string= line-string ""))					  
					  (atptop~divide-string spass-out-string #\Newline)))
  
  (let* ((proof-p (find "SPASS beiseite: Proof found." spass*proof-file-lines :test #'atptop~string-is-prefix-of-string-p))
	 (completion-p (if (null proof-p)
			   (find "SPASS beiseite: Completion found." spass*proof-file-lines :test #'atptop~string-is-prefix-of-string-p)
			 nil)))
    
    (cond (completion-p
	   'completion)
	  ((and proof-p (null parse-back))
	   'proof)
	  ((and proof-p parse-back)
	   (spass=translate-proof spass*proof-file-lines)
	   res-proof)
	  (t ;; kein proof, keine completion
	   nil))))

#|

(defun spass~translate-spass-proof (problem directory parse-back)
  (declare (edited  "22-JUN-1997")
	   (authors Naumann)
	   (input   "A Problem in cnf, the init-clauses of the spass-proof and in existing directory with the spass.cnf and spass.dfg files.")
	   (effect  "None.")
	   (value   "Computes the translation of the spass-proof (if there is one) to a omega-compatible resolution-proof."))
  (if (not (spass=found-proof-p "Proof" directory))
      (progn (format t "Sorry, no proof found .................")
	    (if (spass=found-proof-p "Completion" directory)
		(format t " but a completion."))
	    nil)
    (if (not parse-back) (progn (format t "~%Proof found.~%") t)
    (progn (spass=parse-and-translate-proof problem directory)
	   spass*current-resolution-problem))))

(defun spass=found-proof-p (string directory)
  (declare (edited  "24-JUN-1997")
	   (authors Naumann)
	   (input   "A directory with the spass.dfg - file.")
	   (effect  "None.")
	   (value   "Returns t, if 'SPASS beiseite: `string' found' is in spass.dfg, otherwise nil. 'string' makes only sense with 'Proof' or 'Completion'."))
  (setq proof-found-p nil)
  (let* ((file (format nil "~A/spass.dfg" directory)))
    (with-open-file (stream file :direction :input)
			    (do* ((eof-p nil))
				((or eof-p)
				 proof-found-p)
			      (let ((file-line (read-line stream nil '$$$))) ;file macht spass, aber keine dollars
				(if (equal file-line '$$$)
				    (setq eof-p t)
				    (if (spass=string-suffix (format nil "SPASS beiseite: ~A found" string) file-line)
					(setq proof-found-p t))))))))
			      
(defun spass=parse-and-translate-proof (resolution-problem directory)
  (declare (edited  "25-JUN-1997")
	   (authors Naumann)
	   (input   "A Resolution-problem in CNF and a existing directory with the spass.dfg - file of the problem.")
	   (effect  "Extends spass=current-resolution-proof with the proof and set the proof-file-line to spass*proof-file-lines.")
	   (value   "Undefined."))
  (format t "Proof found.~%")
  (let* ((file (format nil "~A/spass.dfg" directory)))
    (with-open-file (stream file :direction :input)
		    (do* ((eof-p nil))
			(eof-p nil)
		      (let ((file-line (read-line stream nil '$$$)))
			(if (equal file-line '$$$)
			    (setq eof-p t)
			  (if (string= file-line "") nil
			   (setq spass*proof-file-lines (append spass*proof-file-lines (list file-line)))))))))
  ;(spass=read-obvious-reduction-steps) ; hoffentlich macht er nur amtliche reductions!
  (spass=translate-proof spass*proof-file-lines))

|#

(defun spass=translate-proof (string-list)
  (declare (edited  "25-JUN-1997")
	   (authors Naumann)
	   (input   "A list of strings, the lines of the spass-proof.")
	   (effect  "Extends spass=current-resolution-proof with the proof.")
	   (value   "Undefinded."))
  (setq pure-clause-list nil)
  (setq parent-list nil)					; zum testen

  (do* ((rest-proof-lines (spass=proof-lines string-list) (rest rest-proof-lines)))
      ;;((null rest-proof-lines)
      ;; t)
      ((or (null rest-proof-lines)
	   (remove-if-not #'cl~empty-p (res~proof-clauses spass*current-resolution-problem))) ;; weitere Zeilen werden abgeschnitten
       t)
    
    (spass=translate-proof-line (first rest-proof-lines)))

  (spass=mark-used-clauses)
  (spass=remove-unused-clauses)
					;(spass=check-proof)
  )

(defun spass=mark-used-clauses ()
  (declare (edited  "18-SEP-1997")
	   (authors Naumann)
	   (input   "None.")
	   (effect  "Marks the used clauses according from the empty clause.")
	   (value   "Undefined."))
  (omega~message "Removing unused clauses.~%")
  (let* ((empty-clause (first (last (res~proof-clauses spass*current-resolution-problem)))))
    (spass=mark-used-clauses-aux empty-clause)))

(defun spass=mark-used-clauses-aux (clause)
  (declare (edited  "18-SEP-1997")
	   (authors Naumann)
	   (input   "A clause or a clause-list.")
	   (effect  "Marks the used clauses according from the empty clause.")
	   (value   "Undefined."))
  (let* ((justification (node~justification clause)))
    (keim~put clause :used t)
    (cond ((res~resolution-p justification)
	   
	   (spass=mark-used-clauses-aux (first (res~resolution-clauses justification)))
	   (spass=mark-used-clauses-aux (second (res~resolution-clauses justification))))
	  ((res~factoring-p justification)
	   (spass=mark-used-clauses-aux (res~factoring-clause justification)))
	  ((res~paramodulation-p justification)
	   (spass=mark-used-clauses-aux (res~paramod-mother justification))
	   (spass=mark-used-clauses-aux (res~paramod-father justification)))
	  ((res~instance-p justification)
	   (spass=mark-used-clauses-aux (res~instance-clause justification))))))
		  
(defun spass=remove-unused-clauses ()
  (declare (edited  "18-SEP-1997")
	   (authors Naumann)
	   (input   "None.")
	   (effect  "Removes the unused clauses from spass*current-resolution-problem.")
	   (value   "Undefinded."))
  (let* ((clauses (res~proof-clauses spass*current-resolution-problem)))
    (do* ((rest-clauses clauses (rest rest-clauses))
	  (clause (first rest-clauses) (first rest-clauses))
          (new-clauses (if (keim~get clause :used) (list clause))
		       (if (keim~get clause :used)
			   (append new-clauses (list clause))
			 new-clauses)))
	((equal (length rest-clauses) 1) (setf (res~proof-clauses spass*current-resolution-problem)
					  new-clauses)))))

(defun spass=proof-lines (string-list)
  (declare (edited  "25-JUN-1997")
	   (authors Naumann)
	   (input   "A list of strings.")
	   (effect  "None.")
	   (value   "The rest of string-list, so the first string begins with 'Here is a proof'."))
  (cond ((spass=string-suffix "Here is a proof" (car string-list)) (cdr string-list))
	(t (spass=proof-lines (cdr string-list)))))

(defun spass=translate-proof-line (proof-line)
  (declare (edited  "25-JUN-1997")
	   (authors Naumann)
	   (input   "A proof-line in string-format.")
	   (effect  "Extends spass*current-resolution-problem with the translatet semantic of the proof-line.")
	   (value   "Undefinded."))
  (multiple-value-bind
      (clause-number splitting-level justification parents new-clause)
      (spass=read-clause-line proof-line)
    (setq pure-clause-list (append pure-clause-list (list new-clause)))
    (setq parent-list (append parent-list (list parents)))
    (cond ((string= justification "Inp") (progn (omega~message "~A - translation.~%" justification)
						(spass=translate-inp-justification (spass=spass-clause-to-keim-clause new-clause) clause-number splitting-level)
						(spass=check-after-line-translation new-clause clause-number splitting-level)))
	  ((string= justification "SpL") (progn (omega~message "~A - translation.~%" justification)
						(spass=translate-spl-justification (spass=spass-clause-to-keim-clause new-clause) clause-number parents splitting-level)
                                                (spass=check-after-line-translation new-clause clause-number splitting-level)))
	  ((string= justification "SpR") (progn (omega~message "~A - translation.~%" justification)
						(spass=translate-spr-justification (spass=spass-clause-to-keim-clause new-clause) clause-number parents splitting-level)
						(spass=check-after-line-translation new-clause clause-number splitting-level)))
	  ((string= justification "Rew") (progn (omega~message "~A - translation.~%" justification)
						(spass=translate-rew-justification (spass=spass-clause-to-keim-clause new-clause) clause-number parents splitting-level)
						(spass=check-after-line-translation new-clause clause-number splitting-level)))
          ((string= justification "EqR") (progn (omega~message "~A - translation.~%" justification)
						(spass=translate-eqr-justification (spass=spass-clause-to-keim-clause new-clause) clause-number parents splitting-level)
						(spass=check-after-line-translation new-clause clause-number splitting-level)))
          ((string= justification "EqF") (progn (omega~message "~A - translation.~%" justification)
						(spass=translate-eqf-justification (spass=spass-clause-to-keim-clause new-clause) clause-number parents splitting-level)
						(spass=check-after-line-translation new-clause clause-number splitting-level)))
	  ((string= justification "GeR") (progn (omega~message "~A - translation.~%" justification)
						(spass=translate-ger-justification (spass=spass-clause-to-keim-clause new-clause) clause-number parents splitting-level)
						(spass=check-after-line-translation new-clause clause-number splitting-level)))
	  ((string= justification "GeF") (progn (omega~message "~A - translation.~%" justification)
						(spass=translate-gef-justification (spass=spass-clause-to-keim-clause new-clause) clause-number parents splitting-level)
						(spass=check-after-line-translation new-clause clause-number splitting-level)))
	  ((string= justification "ClR") (progn (omega~message "~A - translation.~%" justification)
						(spass=translate-clr-justification (spass=spass-clause-to-keim-clause new-clause) clause-number parents splitting-level)
						(spass=check-after-line-translation new-clause clause-number splitting-level)))
	  ((string= justification "EmS") (progn (omega~message "~A - translation.~%" justification)
						(spass=translate-ems-justification (spass=spass-clause-to-keim-clause new-clause) clause-number parents splitting-level)
						(spass=check-after-line-translation new-clause clause-number splitting-level)))
	  ((string= justification "SoR") (progn (omega~message "~A - translation.~%" justification)
						(spass=translate-sor-justification (spass=spass-clause-to-keim-clause new-clause) clause-number parents splitting-level)
						(spass=check-after-line-translation new-clause clause-number splitting-level)))
	  ((string= justification "Con") (progn (omega~message "~A - translation.~%" justification)
						(spass=translate-con-justification (spass=spass-clause-to-keim-clause new-clause) clause-number parents splitting-level)
						(spass=check-after-line-translation new-clause clause-number splitting-level)))
	  ((string= justification "SSi") (progn (omega~message "~A - translation.~%" justification)
						(spass=translate-ssi-justification (spass=spass-clause-to-keim-clause new-clause) clause-number parents splitting-level)
						(spass=check-after-line-translation new-clause clause-number splitting-level)))
	  ((string= justification "Spt") (progn (omega~message "~A - translation.~%" justification)
						(spass=translate-spt-justification (spass=spass-clause-to-keim-clause new-clause) clause-number parents splitting-level)
						(spass=check-after-line-translation new-clause clause-number splitting-level)))
	  ((string= justification "Obv") (progn (omega~message "~A - translation.~%" justification)
						(spass=translate-obv-justification (spass=spass-clause-to-keim-clause new-clause) clause-number parents splitting-level)
						(spass=check-after-line-translation new-clause clause-number splitting-level)))
	  ((string= justification "UnC") (progn (omega~message "~A - translation.~%" justification)
						(spass=translate-unc-justification (spass=spass-clause-to-keim-clause new-clause) clause-number parents splitting-level)
						(spass=check-after-line-translation new-clause clause-number splitting-level)))
	  (t (omega~message "Sorry, clause ~A: justification ~A is not yet translatet.~%" clause-number justification)))))


(defun spass=check-after-line-translation (new-clause clause-number splitting-level)
  (declare (edited  "03-AUG-1997")
	   (authors Naumann)
	   (input   "The spass-clause (string-format), the spass-clause-number, the splitting-level.")
	   (effect  "Iff the empty-clause was produced and the splitting-level is 0, so empty-clause is set. If a empty clause was found but the proof is no finished, branch-closed of actual-clause is set to split-level.")
	   (value   "checks if the translation of the line is correct."))
  (let* ((actual-spass-clause (spass=spass-clause-to-keim-clause new-clause))
         (keim-clauses (res~proof-clauses spass*current-resolution-problem))
         (actual-keim-clause (if no-input-steps (nth (- clause-number 1) keim-clauses)
			       (spass=find-keim-clause clause-number)))
	 (reduced-actual-clause (spass=reduce-clause-to-splitting-clause actual-keim-clause)))
					
    (setq no-input-steps nil)
    (omega~message "check spass-clause ~A: ~A~%" clause-number (spass=check-equality-of-clauses-p reduced-actual-clause actual-spass-clause clause-number (keim~get actual-keim-clause :spass-clause-number) splitting-level))
    (if (and (cl~empty-p actual-keim-clause) (null spass*open-splitting-levels))
	(progn (omega~message "Proof translatet.~%")
	       (setf (res~proof-empty-clause spass*current-resolution-problem) actual-keim-clause)
	       (setf (res~proof-clauses spass*current-resolution-problem) (append (spass=used-initial-clauses (res~proof-initial-clauses spass*current-resolution-problem)) (res~proof-step-clauses spass*current-resolution-problem)))
	       ;(spass=check-proof)    
	       (setq omega*current-resolution-proof spass*current-resolution-problem)
	       )
      (if (cl~empty-p reduced-actual-clause)
	  (progn (keim~put actual-keim-clause :branch-closed splitting-level)
		 (omega~message "Empty clause found at splitting level ~A.~%" splitting-level))))))

(defun spass=used-initial-clauses (clause-list)
  (declare (edited  "03-AUG-1997")
	   (authors Naumann)
	   (input   "A clauses-list.")
	   (effect  "None.")
	   (value   "A list with the used clauses."))
  
  (do* ((rest-clause-list clause-list (rest rest-clause-list))
	(clause (first rest-clause-list) (first rest-clause-list))
	(new-clause-list (if (keim~get clause :spass-clause-number)
			     (list clause) nil)
			 (if clause
			     (if (keim~get clause :spass-clause-number)
				 (append new-clause-list (list clause))
			       new-clause-list)
			   new-clause-list)))
      ((null rest-clause-list) new-clause-list)))
  
(defun spass=read-clause-line (clause-line)
  (declare (edited  "28-JUN-1997")
	   (authors Naumann)
	   (input   "A clause-proof-line.")
	   (effect  "None.")
	   (value   "A multiple value: First the number of the clause in the spass-proof, second the splitting-level, third the Justification (a string), fourth a list of lists with the parents, fifth the spass-clause (a string)."))
  (let* ((rest-clause-line clause-line)
	 (clause-number (spass=get-number (spass=extract-number rest-clause-line)))
	 (rest-clause-line (spass=string-suffix (format nil "~A[" clause-number) rest-clause-line))
	 (splitting-level (spass=get-number (spass=extract-number rest-clause-line)))
	 (rest-clause-line (spass=string-suffix (format nil "~A:" splitting-level) rest-clause-line))
	 (justification (first (multiple-value-list (spass=divide-string rest-clause-line 3))))
	 (rest-clause-line (spass=string-suffix (format nil "~A~A" justification (if (string= justification "Inp")
										     "" ":")) rest-clause-line))
	 (parents (spass=get-parents-from-clause-line-tuned (spass=remove-chars rest-clause-line '(#\Space))))
	 (new-clause (spass=string-suffix (format nil "~A]" (spass=read-word rest-clause-line '(#\])))  rest-clause-line)))
    (values clause-number splitting-level justification parents new-clause)))


(defun spass=get-parents-from-clause-line-tuned (rest-clause)
  (setq solution nil)
  (let* ((parent-string (spass=read-word rest-clause '(#\]))))
    (do* ((rest-parent-string parent-string (spass=string-suffix (format nil "~A," pair) rest-parent-string))
	  (leng (length rest-parent-string) (length rest-parent-string))
	  (pair (if (> leng 0) (spass=read-word rest-parent-string '(#\,)))
		(if (> leng 0) (spass=read-word rest-parent-string '(#\,)))))
	((= leng 0) solution)
	(let* ((clause-number-string (if pair (spass=extract-number pair)))
	       (clause-number (spass=get-number clause-number-string))
	       (literal-number (spass=get-number (spass=extract-number (spass=string-suffix clause-number-string pair)))))
	  (setq solution (append solution (list (list clause-number literal-number))))))))


;------------handling Inp - translation-----------

(defun spass=translate-inp-justification (spass-clause number splitting-level)
  (declare (edited  "18-JUL-1997")
	   (authors Naumann)
	   (input   "A spass-input-clause (in keim-format), and its number")
	   (effect  "The delta-relation of spass*current-resolution-problem is changes, also spass*proof-clauses.")
	   (value   "Undefined."))
  (let* ((keim-input-clauses (res~proof-initial-clauses spass*current-resolution-problem))
	 (keim-clause (nth (- number 1) keim-input-clauses)))
    
    (keim~put keim-clause :spass-clause-number (list 'used))
    (keim~put keim-clause :splitting-level splitting-level)
    
    (multiple-value-bind
	(new-positions eq-flip-positions obvious-reductions)
	(spass=compute-positions-keim-spass-clause keim-clause spass-clause splitting-level :obvious nil)

      (spass=always-needed-steps keim-clause spass-clause number new-positions eq-flip-positions
				 obvious-reductions splitting-level :input t)
      
      (if (not (or eq-flip-positions obvious-reductions))
	  (progn (setq no-input-steps t)
					;(spass=set-new-delta-relation keim-clause new-positions)
		 (keim~put (nth (- number 1) (res~proof-clauses spass*current-resolution-problem)) :spass-clause-number (list number)))
        (progn
	  (setq no-input-steps nil)
	  (keim~put (first (last (res~proof-clauses spass*current-resolution-problem))) :spass-clause-number (list number)))))))


; ------handling factoring - translation--------------------

(defun spass=apply-factoring-step (parent-clause pos1 pos2 splitting-level)
  (declare (edited  "02-AUG-1997")
	   (authors Naumann)
	   (input   "A parent-clause and to literal-positions. pos2 must be greater than pos1.")
	   (effect  "None.")
	   (value   "The correct justified factored clause."))
  ;(omega~message "clause: ~A~%pos1: ~A~%pos2: ~A~%" parent-clause pos1 pos2)
 
  (let* ((lit1 (data~struct-at-position parent-clause pos1))
	 (lit2 (data~struct-at-position parent-clause pos2))
	 (positions (list pos1 pos2))
	 (renaming (subst~create nil nil))
	 (unifier (term~unify lit1 lit2))
	 ;(k (omega~message "unif: ~A, lit1:~A, lit2: ~A~%" unifier lit1 lit2))
	 (name 'fact-step)
	 (factoring-resolvent (cl~create (spass=subst-apply unifier (remove lit2 (cl~literals parent-clause) :count 1 :test 'keim~equal :start (pos~first pos2)))))
	 (justification (res~factoring-create parent-clause positions renaming unifier name)))
    (setf (node~justification factoring-resolvent) justification)
    (keim~put factoring-resolvent :splitting-level (if (keim~get parent-clause :branch-open)
						       (keim~get parent-clause :branch-open)
						     (keim~get parent-clause :splitting-level)))
				      
    factoring-resolvent))

; ------handling resolution - translation-------------------
; inklusive factoring der alten schritte!

(defun spass=apply-resolution-step (parent-clause1 old-parent-clause2 pos1 pos2 spass-clause splitting-level)
  (declare (edited  "02-AUG-1997")
	   (authors Naumann)
	   (input   "Two parents-clauses and the positions of the used literals")
	   (effect  "None.")
	   (value   "The correct justified resolvent-list."))
  
					;(omega~message "parent1: ~A~%parent2: ~A~%" parent-clause1 old-parent-clause2)
  (multiple-value-bind (parent-clause2 renaming2)
      (res~separate-clauses parent-clause1 old-parent-clause2)
					;(omega~message "lit1: ~A, lit2: ~A~%" (cl~literals parent-clause1) (cl~literals parent-clause2))
    (spass=copy-splitting-branches-of-clauses old-parent-clause2 parent-clause2)
    ;(omega~message "parent1: ~A~%" parent-clause1)
    ;(omega~message "~A~%" (keim~plist parent-clause1))
    ;(omega~message "parent2: ~A~%" old-parent-clause2)
    ;(omega~message "~A~%" (keim~plist old-parent-clause2))
    (let* ((lit1 (data~struct-at-position parent-clause1 pos1))
	   (lit2 (data~struct-at-position parent-clause2 pos2))
	   (k (setq l1 lit1))
	   (k (setq l2 lit2))
	   ;(k (print-lits (cl~create (list lit1 lit2))))
	   
	   ;(k (omega~message "l1: ~A, l2: ~A" lit1 lit2))
	   (unifier (term~unify lit1 lit2))
	   (resolvent (cl~create (spass=subst-apply unifier (append (remove lit1 (cl~literals parent-clause1) :count 1 :test 'keim~equal :start (pos~first pos1))
								    (remove lit2
									    (cl~literals
									     parent-clause2) :count 1 :test 'keim~equal :start (pos~first pos2))))))
	   ;(k (omega~message "resolvent created.~%"))
	   (justification (res~resolution-create (list parent-clause1 old-parent-clause2)
						 (list pos1 pos2)
						 (list (subst~create nil nil) renaming2)
						 unifier 'resolution-step)))
      ;(omega~message "Resolvent: ~A~%" resolvent)
      ;(print-lits resolvent)
      (setf (node~justification resolvent) justification)
      (keim~put resolvent :splitting-level (max (if (keim~get parent-clause1 :branch-open)
						    (keim~get parent-clause1 :branch-open)
						  (keim~get parent-clause1 :splitting-level))
						(if (keim~get old-parent-clause2 :branch-open)
						    (keim~get old-parent-clause2 :branch-open)
						  (keim~get old-parent-clause2 :splitting-level))))
      (spass=factore-out-resolvent (list resolvent) spass-clause splitting-level))))

(defun spass=factore-out-resolvent (resolvent-list spass-clause splitting-level &key (all-literals nil) (neg-branch-search nil))
  (declare (edited  "17-AUG-1997")
	   (authors Naumann)
	   (input   "A resolvent-clause.")
	   (effect  "None.")
	   (value   "A list of correct justified clauses, where the double-split-clase-two-literals are deleted."))
  (let* ((actual-resolvent (first (last resolvent-list)))
	 (factored-clause (spass=get-next-factore actual-resolvent splitting-level all-literals)))
    (if (and neg-branch-search factored-clause (spass=neg-branch-found-p factored-clause spass-clause splitting-level))
	(setq solution-resolvent (first (last (spass=factore-out-resolvent (list factored-clause) spass-clause splitting-level :all-literals t)))))
    (if factored-clause
	(spass=factore-out-resolvent (append resolvent-list (list factored-clause)) spass-clause splitting-level :all-literals all-literals)
      resolvent-list)))

(defun spass=get-next-factore (clause splitting-level all-literals)
  (declare (edited  "17-AUG-1997")
	   (authors Naumann)
	   (input   "A clause.")
	   (effect  "None.")
	   (value   "The next factored clause, if the :splitting-branch of the factorable is equal and the factorables belongs to the rigth case: nil if there is none."))
  (setq solution nil)
  (if (null (cl~literals clause)) nil
    (do* ((rest-literals1 (cl~literals clause) (rest rest-literals1))
	  (literal1 (first rest-literals1) (first rest-literals1))
	  (pos1 0 (+ 1 pos1)))
	((or (equal (length rest-literals1) 1) solution) solution)
      (do* ((rest-literals2 (rest rest-literals1) (rest rest-literals2))
	    (literal2 (first rest-literals2) (first rest-literals2))
	    (pos2 (+ 1 pos1) (+ 1 pos2)))
	  ((or (null rest-literals2) solution))
					;(omega~message "l1: ~A, plist: ~A~%l2: ~A, plist ~A~%" literal1 (if literal1 (keim~plist literal1)) literal2 (if literal2 (keim~plist literal2)))
	(if (spass=check-factoring-allowed-p literal1 literal2 all-literals splitting-level)
	    (let* ((unifier (term~unify literal1 literal2)))
              (if (and unifier
		       (keim~equal (subst~apply unifier literal1)
				   (subst~apply unifier literal2)))
		  (setq solution (spass=apply-factoring-step clause (pos~list-position (list pos1)) (pos~list-position (list pos2)) splitting-level)))))))))
		
(defun spass=check-factoring-allowed-p (literal1 literal2 all-literals splitting-level)
  (declare (edited  "19-AUG-1997")
	   (authors Naumann)
	   (input   "Two literals.")
	   (effect  "None.")
	   (value   "True, if the two literals are allowed to factore (means they are in the same splitting-level."))
  (let* ((splitting-branch1 (keim~get literal1 :splitting-branch))
	 (splitting-branch2 (keim~get literal2 :splitting-branch))
	 (levels1 (maplist #'caar splitting-branch1))
	 (levels2 (maplist #'caar splitting-branch2))
	 (right1 (first (last splitting-branch1)))
	 (right2 (first (last splitting-branch2)))
	 (level0 (not (or levels1 levels2))))
					;(omega~message "sb1: ~A, sb2: ~A~%" splitting-branch1 splitting-branch2)
    (or (and (intersection levels1 levels2)
	     right1
	     right2
	     (if (and (equal (length (intersection levels1 levels2)) 1)
		      (equal (first (intersection levels1 levels2)) splitting-level))
		 (equal (length splitting-branch1) (length splitting-branch2))
	       t))
	(and level0 all-literals (not (data~free-variables literal1)) (not (data~free-variables literal2)))
	(and level0 (equal all-literals 'all)))))


; ------handling paramodulation - translation---------------


(defun spass=apply-paramodulation-step
  (spass-clause number parent-clause1 old-parent-clause2 p1 p2 splitting-level)
  (declare (edited  "01-AUG-1997")
	   (authors Naumann)
	   (input   "A clause, a number and the parents-clauses, and the positions of the used literals")
	   (effect  "The new-clause with the paramod-justification is produced (if there is an obvious-reduction or a rotated equation it is also solved) and appended to the proof-steps of spass*current-resolution-problem.")
	   (value   "Undefinded."))
  (multiple-value-bind
      (parent-clause2 renaming2)
      (res~separate-clauses parent-clause1 old-parent-clause2)
    (spass=copy-splitting-branches-of-clauses old-parent-clause2 parent-clause2)
    (let* ((pos1 (pos~list-position (list p1)))
           (pos2 (pos~list-position (list p2)))
	   (lit-equation (nth p1 (cl~literals parent-clause1)))
	   (equation (lit~atom (nth p1 (cl~literals parent-clause1))))
	   (s (first (data~appl-arguments equation)))
	   (tr (second (data~appl-arguments equation)))
	   (lit-applicant (nth p2 (cl~literals parent-clause2)))
	   (applicant (lit~atom (nth p2 (cl~literals parent-clause2))))
	   (subterm-list (spass=get-subterms applicant))
	   (appl-equation (and (typep applicant 'term+appl)
			       (data~equal (data~appl-function applicant)
					   (data~schema-range
					    (env~lookup-object '= (res~proof-environment spass*current-resolution-problem)))))))
      ;(omega~message "bis vor do.~%") 
      (setq end-flag nil)		
      (do* ((rest-subterm-list (if appl-equation (rest subterm-list) subterm-list) (rest rest-subterm-list))
	    (actual-subterm (first (first rest-subterm-list)) (first (first rest-subterm-list)))
            (actual-subterm-position (second (first rest-subterm-list)) (second (first rest-subterm-list)))
	    (act-unif (term~unify s actual-subterm) (if actual-subterm (term~unify s actual-subterm)))
	    (actual-unifier (if act-unif (spass=check-unifier-type act-unif))
			    (if act-unif (spass=check-unifier-type act-unif)))
	    (act-unif2 (term~unify tr actual-subterm) (if actual-subterm (term~unify tr actual-subterm)))
	    (actual-unifier2 (if act-unif2 (spass=check-unifier-type act-unif2))
			     (if act-unif2 (spass=check-unifier-type act-unif2))))
	  ((or (null rest-subterm-list) end-flag))
	(if (and actual-unifier (not end-flag))
	    (let* ((new-literal (lit~literal-create (data~replace-at-position
						     (spass=subst-apply actual-unifier
									applicant)
						     actual-subterm-position
						     (spass=subst-apply actual-unifier tr)) (lit~positive-p lit-applicant)))
		   (new-clause (cl~create (append (cl~literals (data~replace-at-position
								(spass=subst-apply actual-unifier
										   parent-clause2)
								pos2
								new-literal))
						  (remove (spass=subst-apply actual-unifier lit-equation) (spass=subst-apply actual-unifier (cl~literals parent-clause1)) :count 1 :test 'keim~equal :start p1)))))
	      ;(setf (lit~clause new-literal) (lit~clause lit-applicant)) ; muss noch
	      ;geaendert werden!
	      (keim~put new-literal :splitting-branch (keim~get lit-applicant :splitting-branch))
	      (keim~put new-clause :splitting-level (max (if (keim~get parent-clause1 :branch-open)
							     (keim~get parent-clause1 :branch-open)
							   (keim~get parent-clause1 :splitting-level))
							 (if (keim~get old-parent-clause2 :branch-open)
							     (keim~get old-parent-clause2 :branch-open)
							   (keim~get old-parent-clause2 :splitting-level))))
	      (multiple-value-bind (new-positions eq-flip-positions obvious-reductions)
		  (spass=compute-positions-keim-spass-clause new-clause spass-clause splitting-level :obvious nil)
		(if new-positions
		    (progn (setq end-flag t)
			   (let* ((mother old-parent-clause2)
				  (mother-position (pos~add-front p2 (pos~add-front 1 actual-subterm-position)))
				  (mother-renaming renaming2)
				  (father parent-clause1)
				  (father-position (pos~list-position (list p1)))
				  (father-renaming (subst~create nil nil))
				  (direction 'LR)
				  (unifier actual-unifier)
				  (name nil)
				  (justification (res~paramod-create mother mother-position mother-renaming father father-position father-renaming direction unifier name)))
			     (setf (node~justification new-clause) justification)
			     (keim~put new-clause :splitting-level splitting-level)
			     (setf (res~proof-clauses spass*current-resolution-problem) (append (res~proof-clauses spass*current-resolution-problem) (spass=translate-paramodulation-step new-clause spass-clause splitting-level)))
			     ;(omega~message "vor mult-val.~%")
			     (multiple-value-bind
				 (new-positions eq-flip-positions obvious-reductions)
				 (spass=compute-positions-keim-spass-clause (first (last (res~proof-clauses spass*current-resolution-problem))) spass-clause splitting-level :obvious nil)
			       (spass=always-needed-steps (first (last (res~proof-clauses spass*current-resolution-problem))) spass-clause number new-positions eq-flip-positions obvious-reductions splitting-level))))))))
	(if (and actual-unifier2 (not end-flag))
	    (let* ((new-literal (lit~literal-create (data~replace-at-position
						     (spass=subst-apply actual-unifier2
									applicant)
						     actual-subterm-position
						     (spass=subst-apply actual-unifier2 s)) (lit~positive-p lit-applicant)))
		   (new-clause (cl~create (append (cl~literals(data~replace-at-position
							       (spass=subst-apply actual-unifier2
										  parent-clause2)
							       pos2
							       new-literal))
						  (remove (spass=subst-apply actual-unifier2 lit-equation) (spass=subst-apply actual-unifier2 (cl~literals parent-clause1)) :count 1 :test 'keim~equal :start p1)))))
	      ;(setf (lit~clause new-literal) (lit~clause lit-applicant))
	      (keim~put new-literal :splitting-branch (keim~get lit-applicant :splitting-branch))
	      (keim~put new-clause :splitting-level (max (if (keim~get parent-clause1 :branch-open)
							     (keim~get parent-clause1 :branch-open)
							   (keim~get parent-clause1 :splitting-level))
							 (if (keim~get old-parent-clause2 :branch-open)
							     (keim~get old-parent-clause2 :branch-open)
							   (keim~get old-parent-clause2 :splitting-level))))
	      (multiple-value-bind (new-positions eq-flip-positions obvious-reductions)
		  (spass=compute-positions-keim-spass-clause new-clause spass-clause splitting-level :obvious nil)
					;(omega~message "new-pos: ~A~%obv-red: ~A~%" new-positions obvious-reductions)
		(if new-positions
		    (progn (setq end-flag t)
			   (let* ((mother old-parent-clause2)
				  (mother-position (pos~add-front p2 (pos~add-front 1 actual-subterm-position)))
		                  (mother-renaming renaming2)
				  (father parent-clause1)
				  (father-position (pos~list-position (list p1)))
				  (father-renaming (subst~create nil nil))
				  (direction 'RL)
				  (unifier actual-unifier2)
				  (name nil)
				  (justification (res~paramod-create mother mother-position mother-renaming father father-position father-renaming direction unifier name)))
			     (setf (node~justification new-clause) justification)
			     (keim~put new-clause :splitting-level splitting-level)
					;(omega~message "vor setf paramod-case2~%")
					;(setq steps (spass=translate-paramodulation-step spass-clause new-clause))
					;(omega~message "param-steps: ~A~%" steps)
			     (setf (res~proof-clauses spass*current-resolution-problem) (append (res~proof-clauses spass*current-resolution-problem) (spass=translate-paramodulation-step new-clause spass-clause splitting-level)))
					;(omega~message "last clause before case2-end: ~A~%" (first (last (res~proof-clauses spass*current-resolution-problem))))
			     (multiple-value-bind
				 (new-positions eq-flip-positions obvious-reductions)
				 (spass=compute-positions-keim-spass-clause (first (last (res~proof-clauses spass*current-resolution-problem))) spass-clause splitting-level :obvious nil)
			       (spass=always-needed-steps (first (last (res~proof-clauses spass*current-resolution-problem))) spass-clause number new-positions eq-flip-positions obvious-reductions splitting-level))))))))))))


(defun spass=check-unifier-type (substitution)
  (declare (edited  "05-AUG-1997")
	   (authors Naumann)
	   (input   "A substitution.")
	   (effect  "None.")
	   (value   "The substitution if the members of the pairs have the same type, otherwise nil."))
  (if (subst~empty-p substitution) substitution
  (let* ((domain (subst~domain substitution))
	 (codomain (subst~codomain substitution)))
    (do* ((count 0 (+ 1 count)))
	((or (equal count (- (length domain) 1)) (not (keim~equal (term~type (nth count domain)) (term~type (nth count codomain))))) (if (not (keim~equal (term~type (nth count domain)) (term~type (nth count codomain))))
																	 nil substitution))))))

;-------translating paramodulation to resolution-----------

(defun spass=translate-paramodulation-step (clause spass-clause splitting-level)
  (declare (edited  "11-AUG-1997")
	   (authors Naumann)
	   (input   "A clause which is justified by paramodulation.")
	   (effect  "None.")
	   (value   "A list with correct justfied clause with translates the paramodulation-step to resolution-steps."))
					;(omega~message "einstieg paramod-trans.~%")
					;(omega~message "paramod-clause: ~A~%" clause)
  (if (equal spass*allowed-splittings 0)
      (list clause)
    (let* ((justification (node~justification clause))
	   (mother-renaming (first (res~just-renamings justification)))
	   (father-renaming (second (res~just-renamings justification)))
	   (mother (res~paramod-mother justification))
	   (father (res~paramod-father justification))
					;(k (omega~message "~A~%" (keim~plist (res~paramod-mother justification))))
					;(k (omega~message "~A~%" (keim~plist (res~paramod-father justification))))
	   (renamed-mother (spass=subst-apply mother-renaming mother))
	   (renamed-father (spass=subst-apply father-renaming father))
					;(k (print-lits mother))
					;(terpri)
					;(k (print-lits father))
					;(terpri)
					;(k (omega~message "~A" (keim~plist mother)))
					;(terpri)
					;(k (omega~message "~A" (keim~plist father)))
					;(terpri)
	   (mother-position (res~paramod-mother-position justification))
	   (mother-literal-position (pos~first mother-position))
					;(k (omega~message "mp: ~A~%" mother-position))
	   (father-position (res~paramod-father-position justification))
					;(k (omega~message "fp: ~A~%" father-position))
	   (applicant (nth mother-literal-position (cl~literals renamed-mother)))
	   (applicant-positive (lit~positive-p applicant))
	   (equation (data~struct-at-position renamed-father father-position))
	   (direction-lr (equal (res~paramod-direction justification) 'LR))
	   (unifier (res~just-unifier justification))
	   (s (data~struct-at-position renamed-mother mother-position)) ; achtung new-pos!
	   (tr (if direction-lr (first (data~appl-arguments (lit~atom equation)))
		 (second (data~appl-arguments (lit~atom equation)))))
	   (r (if direction-lr (second (data~appl-arguments (lit~atom equation)))
		(first (data~appl-arguments (lit~atom equation)))))
					;(k (omega~message "s: ~A, tr: ~A, r: ~A~%" s tr r))
					;(k (omega~message "unif: ~A~%" (res~just-unifier justification)))
					;(k (omega~message "oa: ~A~%" applicant))
					;(k (omega~message ""))
	   (new-applicant (spass=subst-apply unifier (data~replace-at-position applicant (pos~rest mother-position) r)))
	 
					;(k (omega~message "na: ~A~%" new-applicant))
	   (new-lit1 (spass=subst-apply unifier (lit~literal-create (lit~atom applicant) (not applicant-positive))))
	   (new-lit2 (if direction-lr
			 (spass=subst-apply unifier
					    (lit~literal-create
					     (term~appl-create
					      (env~lookup-object '= (res~proof-environment spass*current-resolution-problem))
					      (list tr r))
					     nil))
		       (spass=subst-apply unifier (lit~literal-create
						   (term~appl-create
						    (env~lookup-object '= (res~proof-environment spass*current-resolution-problem))
						    (list r tr))
						   nil))))
	   (new-lit3 new-applicant)
	   (new-clause1 (r2ntop~create-paramod-clause (list new-lit1 new-lit2 new-lit3)
						      :justification (res~initial-create (format nil "PARAMOD-~A" (gensym)))
						      :paramod-position (pos~rest (pos~rest mother-position)))))
      
      ;; (new-clause1 (r2ntop~create-paramod-clause (list new-lit1 new-lit2 new-lit3)
      ;; :justification (res~initial-create (format nil "PARAMOD-~A" (gensym))))))
      ;; (keim~put new-clause1 :paramod-position (pos~rest (pos~rest mother-position)))
      
      (keim~put new-clause1 :splitting-level 0)
      (keim~put new-clause1 :spass-clause-number (list 'used))

      (let* ((new-clause2-list (spass=apply-resolution-step
				new-clause1
				mother
				(pos~list-position (list 0))
				(pos~list-position (list mother-literal-position))
				spass-clause 
				splitting-level))
					;(k (omega~message "new-clause2: ~A~%" new-clause2-list))
	     (new-clause3-list (spass=apply-resolution-step
				(first (last new-clause2-list))
				father
				(pos~list-position (list 0))
				father-position
				spass-clause 
				splitting-level))
					;(k (omega~message "new-clause3: ~A~%" new-clause3-list))
	     )
	(append (list new-clause1) new-clause2-list new-clause3-list)))))


; ------handling translation obvious-reductions-------------

(defun spass=translate-obv-justification (spass-clause number parents splitting-level)
  (declare (edited  "09-SEP-1997")
	   (authors Naumann)
	   (input   "A SPASS-clause, its number and its parents and the splitting-level.")
	   (effect  "The changed proof-steps of spass*current-resolution-problem.")
	   (value   "Undefined."))
  (let* ((parent-clause (spass=find-keim-clause (first (first parents))))
	 (obvious-steps (spass=translate-obvious-reductions (list parent-clause) spass-clause splitting-level)))
    (setf (res~proof-clauses spass*current-resolution-problem) (append (res~proof-clauses spass*current-resolution-problem) (rest obvious-steps)))
    (multiple-value-bind
	(new-positions eq-flip-positions obvious-reductions)
	(spass=compute-positions-keim-spass-clause (first (last (res~proof-clauses spass*current-resolution-problem))) spass-clause splitting-level :obvious nil)
      (spass=always-needed-steps (first (last (res~proof-clauses spass*current-resolution-problem))) spass-clause number new-positions eq-flip-positions nil splitting-level))
    (keim~put (first (last (res~proof-clauses spass*current-resolution-problem))) :spass-clause-number (list number))))


(defun spass=translate-obvious-reductions (parent-clauses spass-clause splitting-level)
  (declare (edited  "23-JUL-1997")
	   (authors Naumann)
	   (input   "A clause in KEIM-format.")
	   (effect  "The changed (res~proof-clauses spass*current-resolution-proof with the carried out obvious-reduction-steps.")
	   (value   "Undefined."))
  (cond ((typep parent-clauses 'cl+clause)
         
	 (spass=translate-obvious-reductions (list parent-clauses) spass-clause splitting-level))
	((typep parent-clauses 'cons)
	 (let* ((parent-clause (first (last parent-clauses)))
		(actual-clause (spass=reduce-clause-to-splitting-clause parent-clause))
		(merge-positions (spass=obvious-merge actual-clause))
		(taut-position (spass=obvious-taut actual-clause)))
					;(print-lits parent-clause)
	   (cond ((and merge-positions (equal (length merge-positions) 3)) ; falls eine equal-flip + mit obv red.
		  (let* ((pos1 (pos~list-position (list (spass=compute-new-position parent-clause (pos~first (second merge-positions))))))
			 (lit1 (data~struct-at-position actual-clause pos1))
			 (lit1-atom (lit~atom lit1))
			 (pos2 (pos~list-position (list 0)))
			 (old-parent2 (r2ntop~create-flip-clause (list (lit~literal-create
									lit1-atom
									(not (lit~positive-p lit1)))
								       (lit~literal-create
									(term~appl-create
									 (data~appl-function lit1-atom)
									 (list (second (data~appl-arguments lit1-atom))
									       (first (data~appl-arguments lit1-atom))))
									(lit~positive-p lit1)))
								 :justification
								 (res~initial-create
								  (format nil "FLIP-~A" (gensym))))))
		    (keim~put old-parent2 :splitting-level 0)
		    (keim~put old-parent2 :spass-clause-number (list 'used))
					;(omega~message "flip: ~A~%" old-parent2)
					;(setf (res~proof-clauses spass*current-resolution-problem)
					;	  (append (res~proof-clauses spass*current-resolution-problem)
					;		  (list old-parent2)))
		    (spass=translate-obvious-reductions
		     (append parent-clauses (spass=apply-resolution-step
					     parent-clause
					     old-parent2
					     pos1
					     pos2
					     spass-clause
					     splitting-level))
		     spass-clause splitting-level)))
		 ((and merge-positions (equal (length merge-positions) 2))
		  (let* ((pos1 (pos~list-position
				(list (spass=compute-new-position 
				       parent-clause
				       (pos~first (first merge-positions))
				       :only-branch t))))
			 (pos2 (pos~list-position
				(list (spass=compute-new-position
				       parent-clause
				       (pos~first (second merge-positions))
				       :only-branch t))))
			 (resolvent (spass=apply-factoring-step parent-clause pos1 pos2 splitting-level)))
		    (spass=translate-obvious-reductions (append parent-clauses (list resolvent)) spass-clause splitting-level)))
		 (taut-position
		  (let* ((pos1 (pos~list-position (list (spass=compute-new-position
							 parent-clause (pos~first
									taut-position)
							:only-branch t))))
			 (pos2 (pos~list-position (list 0)))
			 (old-parent2 spass*reflex-clause)
			 (resolvent-list (spass=apply-resolution-step parent-clause old-parent2 pos1 pos2 spass-clause splitting-level)))
					;(omega~message "res: ~A~%just: ~A~%" resolvent (node~justification resolvent))
		    (spass=translate-obvious-reductions (append parent-clauses resolvent-list) spass-clause splitting-level)))
		 (t parent-clauses))))))

(defun spass=obvious-merge (clause)
  (declare (edited  "23-JUL-1997")
	   (authors Naumann)
	   (input   "A clause.")
	   (effect  "None.")
	   (value   "A pair of two numbers with the positions of two syntactilly identical literals (or rotatet if it was an equality), otherwise nil."))
  (setq solution nil)
  (do* ((rest-literals (cl~literals clause) (rest rest-literals))
	(pos1 0 (+ 1 pos1)))
      ((or (null rest-literals) solution) solution)
    (do* ((lit1 (first rest-literals) (first rest-literals))
	  (rest-literals2 (rest rest-literals) (rest rest-literals2))
	  (pos2 (+ 1 pos1) (+ 1 pos2)))
	((or (null rest-literals2) solution))
      (if (and (keim~equal lit1 (first rest-literals2))
	       (not (or (first (last (first (reverse (keim~get lit1 :splitting-branch)))))
			(first (last (first (reverse (keim~get (first rest-literals2) :splitting-branch))))))))
	  (progn			;(omega~message "1.Fall")
	    (setq solution (list (pos~list-position (list pos1)) (pos~list-position (list pos2)))))
	(let* ((atom1 (lit~atom lit1))
	       (lit2 (first rest-literals2))
	       (atom2 (lit~atom lit2))
	       (lit1-1 (if (and (typep atom1 'term+appl)
				(string= (keim~name (data~appl-function atom1)) "="))
			   (lit~literal-create (term~appl-create (data~appl-function atom1)
								 (reverse (data~appl-arguments atom1)))
					       (lit~positive-p lit1))
			 lit1)))
	  (if (and (keim~equal lit1-1 lit2)
		   (not (or (first (last (first (reverse (keim~get lit1-1 :splitting-branch)))))
			    (first (last (first (reverse (keim~get lit2 :splitting-branch))))))))
	      (setq solution (list 'eq-flip (pos~list-position (list pos1)) (pos~list-position (list pos2))))))))))

(defun spass=obvious-taut (clause)
  (declare (edited  "23-JUL-1997")
	   (authors Naumann)
	   (input   "A clause.")
	   (effect  "None.")
	   (value   "A position of negative true literals (not x=x) if there exist, otherwise nil."))
  (do* ((rest-literals (cl~literals clause) (rest rest-literals))
	(pos 0 (+ 1 pos)))
      ((or (null rest-literals) (and (typep (lit~atom (first rest-literals)) 'term+appl)
				     (not (lit~positive-p (first rest-literals)))
				     (string= (keim~name (data~appl-function (lit~atom (first rest-literals)))) "=")
				     (keim~equal (first (data~appl-arguments (lit~atom (first rest-literals))))
						 (second (data~appl-arguments (lit~atom (first rest-literals)))))))
       (if (not (null rest-literals)) (pos~list-position (list pos))))))


;-------handling translation of fliped equations------------

(defun spass=translate-fliped-equations (clause eq-flip-positions spass-clause
						splitting-level input)
  (declare (edited  "01-AUG-1997")
	   (authors Naumann)
	   (input   "A clause and positions of flipes equalities.")
	   (effect  "The equations are rotatet and proof-steps of spass*current-resolution-problem are changed.")
	   (value   "Undefinded."))
  (do* ((rest-eq-flip-positions eq-flip-positions (rest rest-eq-flip-positions))
	(actual-parent1 clause (first (last (res~proof-clauses spass*current-resolution-problem)))))
      ((null rest-eq-flip-positions))
    (let* ((pair (first rest-eq-flip-positions))
           (pos1 (pos~list-position (list (if input (pos~first (first pair))
					    (spass=compute-new-position actual-parent1
									(pos~first (first
										    pair))
									:only-branch t)))))
	   (lit1 (data~struct-at-position actual-parent1 pos1))
	   (lit1-atom (lit~atom lit1))
	   (pos2 (pos~list-position (list 0)))
	   (old-parent2 (r2ntop~create-flip-clause (list (lit~literal-create
							  lit1-atom
							  (not (lit~positive-p lit1)))
							 (lit~literal-create
							  (term~appl-create
							   (data~appl-function lit1-atom)
							   (list (second (data~appl-arguments lit1-atom))
								 (first (data~appl-arguments lit1-atom))))
							  (lit~positive-p lit1)))
						   :justification (res~initial-create (format nil "FLIP-~A" (gensym))))))
      (keim~put old-parent2 :splitting-level 0)
      (keim~put old-parent2 :spass-clause-number (list 'used))
      (setf (res~proof-clauses spass*current-resolution-problem)
	    (append (res~proof-clauses spass*current-resolution-problem)
		    (list old-parent2)))
      ;(omega~message "flip: ~A~%" old-parent2)
      (let* ((resolvent-list (spass=apply-resolution-step actual-parent1 old-parent2 pos1 pos2 spass-clause splitting-level)))
					;(omega~message "resolvent-list: ~A~%" resolvent-list)
	(setf (res~proof-clauses spass*current-resolution-problem) (append (res~proof-clauses spass*current-resolution-problem) resolvent-list))))))
      ;(omega~message "resolvent nach flip: ~A~%" resolvent)
      
  
;-------handling EqR-translations--------------------------

(defun spass=translate-eqr-justification (spass-clause number parents splitting-level)
 (declare (edited  "04-AUG-1997")
	  (authors Naumann)
	  (input   "The actual spass-clause, the actual number and the parents of the step.")
	  (effect  "The changed proof-steps of spass*current-resolution-problem.")
	  (value   "Undefined."))
   (let* ((number-parent (first (first parents)))
          (parent-clause1 (spass=find-keim-clause number-parent))
	  (pos1 (pos~list-position (list (spass=compute-new-position parent-clause1 (second (first parents))))))
	  (pos2 (pos~list-position (list 0)))
	  (old-parent-clause2 spass*reflex-clause)
	  (resolvent-list (spass=apply-resolution-step parent-clause1 old-parent-clause2 pos1 pos2 spass-clause splitting-level)))
     (setf (res~proof-clauses spass*current-resolution-problem) (append (res~proof-clauses spass*current-resolution-problem) resolvent-list))
     (multiple-value-bind (new-positions eq-flip-positions obvious-reductions)
	 (spass=compute-positions-keim-spass-clause (first (last resolvent-list)) spass-clause splitting-level :obvious nil)
       (spass=always-needed-steps (first (last resolvent-list)) spass-clause number new-positions eq-flip-positions obvious-reductions splitting-level)))
   (keim~put (first (last (res~proof-clauses spass*current-resolution-problem))) :spass-clause-number (list number)))
			     

;-------translate EqF-justification------------------------

(defun spass=translate-eqf-justification (spass-clause number parents splitting-level)
  (declare (edited  "4-AUG-1997")
	   (authors Naumann)
	   (input   "The actual spass-clause, the actual number and the parents of the step.")
	   (effect  "The changed proof-steps of spass*current-resolution-problem.")
	   (value   "Undefined."))
  (let* ((number-parent (first (first parents)))
	 (number-pos1 (second (first parents)))
	 (number-pos1-2 (second (second parents)))
	 (parent-clause1 (spass=find-keim-clause number-parent))
	 (pos1 (pos~list-position (list (spass=compute-new-position parent-clause1 number-pos1))))
	 (pos1-2 (pos~list-position (list (spass=compute-new-position parent-clause1 number-pos1-2))))
	 (lit1 (data~struct-at-position parent-clause1 pos1))
	 (lit1-atom (lit~atom lit1))
	 (lit1-2 (data~struct-at-position parent-clause1 pos1-2))
	 (lit1-2-atom (lit~atom lit1))
	 (pos2 (pos~list-position (list 0)))
	 (old-parent-clause2 (r2ntop~create-transitivity-clause (list (lit~literal-create
								       lit1-atom
								       (not (lit~positive-p lit1)))
								      (lit~literal-create
								       lit1-2-atom
								       (not (lit~positive-p lit1-2)))
								      (lit~literal-create
								       (term~appl-create
									(data~appl-function lit1-atom)
									(list (first (data~appl-arguments lit1-atom))
						~			      (second (data~appl-arguments lit1-2-atom))))
								       t))
								:justification (res~initial-create (format nil "TRANS-~A" (gensym))))))
    (keim~put old-parent-clause-2 :splitting-level 0)
    (keim~put old-parent2 :spass-clause-number (list 'used))
    (setf (res~proof-clauses spass*current-resolution-problem)
			  (append (res~proof-clauses spass*current-resolution-problem)
				  (list old-parent-clause-2)))
    (let* ((resolvent-list (spass=apply-resolution-step parent-clause1 old-parent-clause2 pos1 pos2 spass-clause splitting-level)))
      (setf (res~proof-clauses spass*current-resolution-problem) (append (res~proof-clauses spass*current-resolution-problem) resolvent-list))
      (let* ((new-pos1 (if (> number-pos1 number-pos1-2) pos1-2
			 (pos~list-position (list (- number-pos1-2 1)))))
	     (new-pos2 (pos~list-position (list (- (length (cl~literals (first (last resolvent-list)))) 1))))
	     (fact-resolvent (spass=apply-factoring-step (first (last resolvent-list)) new-pos1 new-pos2 splitting-level)))
	(setf (res~proof-clauses spass*current-resolution-problem) (append (res~proof-clauses spass*current-resolution-problem) (list fact-resolvent)))
	(multiple-value-bind (new-positions eq-flip-positions obvious-reductions)
	    (spass=compute-positions-keim-spass-clause fact-resolvent spass-clause splitting-level :obvious nil)
	  (spass=always-needed-steps fact-resolvent spass-clause number new-positions eq-flip-positions obvious-reductions splitting-level))))
    (keim~put (first (last (res~proof-clauses spass*current-resolution-problem))) :spass-clause-number (list number))))


;------translate GeF-justification------------------------

(defun spass=translate-gef-justification  (spass-clause number parents splitting-level)
 (declare (edited  "4-AUG-1997")
	  (authors Naumann)
	  (input   "The actual spass-clause, the actual number and the parents of the step.")
	  (effect  "The changed proof-steps of spass*current-resolution-problem.")
	  (value   "Undefined."))
   (let* ((number-parent (first (first parents)))
          (parent-clause (spass=find-keim-clause number-parent))
	  (pos1 (pos~list-position (list (spass=compute-new-position parent-clause (second (first parents))))))
          (pos2 (pos~list-position (list (spass=compute-new-position parent-clause (second (second parents))))))
          (resolvent (spass=apply-factoring-step parent-clause pos1 pos2 splitting-level)))
     (setf (res~proof-clauses spass*current-resolution-problem) (append (res~proof-clauses spass*current-resolution-problem) (list resolvent)))
    (multiple-value-bind (new-positions eq-flip-positions obvious-reductions)
	(spass=compute-positions-keim-spass-clause resolvent spass-clause splitting-level :obvious nil)
      (spass=always-needed-steps resolvent spass-clause number new-positions eq-flip-positions obvious-reductions splitting-level)))
    (keim~put (first (last (res~proof-clauses spass*current-resolution-problem))) :spass-clause-number (list number)))
			     

;-------handling SpL - translation -----------------------

(defun spass=translate-spl-justification (spass-clause number parents splitting-level)
  (declare (edited  "29-JUL-1997")
	   (authors Naumann)
	   (input   "The actual spass-clause, the actual number and the parents of the step.")
	   (effect  "The changed proof-steps of spass*current-resolution-problem.")
	   (value   "Undefinded."))
   (let* ((number-parent1 (first (first parents)))
	  (number-parent2 (first (second parents)))
	  (parent-clause1 (spass=find-keim-clause number-parent1))
          (old-parent-clause2 (spass=find-keim-clause number-parent2))
	  (pos1 (spass=compute-new-position parent-clause1 (second (first parents))))
	  (pos2 (spass=compute-new-position old-parent-clause2 (second (second parents)))))
     (spass=apply-paramodulation-step spass-clause number parent-clause1 old-parent-clause2 pos1 pos2 splitting-level)
     ;(omega~message "last clause: ~A~%" (first (last (res~proof-clauses spass*current-resolution-problem))))
     (keim~put (first (last (res~proof-clauses spass*current-resolution-problem))) :spass-clause-number (list number))))
			     
;-------translate GeR-justification-----------------------

(defun spass=translate-ger-justification  (spass-clause number parents splitting-level)
 (declare (edited  "4-AUG-1997")
	  (authors Naumann)
	  (input   "The actual spass-clause, the actual number and the parents of the step.")
	  (effect  "The changed proof-steps of spass*current-resolution-problem.")
	  (value   "Undefined."))
   (let* ((number-parent1 (first (first parents)))
          (number-parent2 (first (second parents)))
          (parent-clause1 (spass=find-keim-clause number-parent1))
	  (parent-clause2 (spass=find-keim-clause number-parent2))
	  (pos1 (pos~list-position (list (spass=compute-new-position parent-clause1 (second (first parents))))))
          (pos2 (pos~list-position (list (spass=compute-new-position parent-clause2 (second (second parents))))))
          (resolvent-list (spass=apply-resolution-step parent-clause1 parent-clause2 pos1 pos2 spass-clause splitting-level)))
     ;(omega~message "step applied.~%")
     (setf (res~proof-clauses spass*current-resolution-problem) (append (res~proof-clauses spass*current-resolution-problem) resolvent-list))
     (multiple-value-bind (new-positions eq-flip-positions obvious-reductions)
	 (spass=compute-positions-keim-spass-clause (first (last resolvent-list)) spass-clause splitting-level :obvious nil)
       (spass=always-needed-steps (first (last resolvent-list)) spass-clause number new-positions eq-flip-positions obvious-reductions splitting-level)))
   (keim~put (first (last (res~proof-clauses spass*current-resolution-problem))) :spass-clause-number (list number)))
			     

;-------handling UnC - translation -----------------------

(defun spass=translate-unc-justification (spass-clause number parents splitting-level)
  (declare (edited  "09-SEP-1997")
	   (authors Naumann)
	   (input   "The actual spass-clause, the actual number and the parents of the step.")
	   (effect  "The changed proof-steps of spass*current-resolution-problem.")
	   (value   "Undefinded."))
  (spass=translate-ger-justification spass-clause number parents splitting-level))


;-------handling SpR - translation -----------------------

(defun spass=translate-spr-justification (spass-clause number parents splitting-level)
  (declare (edited  "29-JUL-1997")
	   (authors Naumann)
	   (input   "The actual spass-clause, the actual number and the parents of the step.")
	   (effect  "The changed proof-steps of spass*current-resolution-problem.")
	   (value   "Undefinded."))
  (let* ((number-parent1 (first (first parents)))
	 (number-parent2 (first (second parents)))
	 (parent-clause1 (spass=find-keim-clause number-parent1))
	 (old-parent-clause2 (spass=find-keim-clause number-parent2))
	 (pos1 (spass=compute-new-position parent-clause1 (second (first parents))))
	 (pos2 (spass=compute-new-position old-parent-clause2 (second (second parents)))))
    (spass=apply-paramodulation-step spass-clause number parent-clause1 old-parent-clause2 pos1 pos2 splitting-level)
    (keim~put (first (last (res~proof-clauses spass*current-resolution-problem))) :spass-clause-number (list number))
					;(omega~message "KC: ~A~%SC: ~A~%" (first (last (res~proof-clauses spass*current-resolution-problem))) spass-clause)
    ))


;-------handling Ems - translation------------------------
; Annahme: Reihenfolge der Parents stimmt!---------------
; bisher: fuer 2 parents

(defun spass=translate-ems-justification (spass-clause number parents splitting-level)
  (declare (edited  "07-AUG-1997")
	   (authors Naumann)
	   (input   "The actual spass-clause, the actual number and the parents of the step.")
	   (effect  "The changed proof-steps of spass*current-resolution-problem.")
	   (value   "Undefined."))
  (if (> (length parents) 2) (omega~message "Not yet installed: More than two parents. But nice example!~%")
  (do* ((parent-list parents (rest parent-list))
	(pair1 (first parent-list) (second parent-list))
	(pair2 (second parent-list) (second parent-list)))
     ((equal (length parent-list) 1) ; ist noch pfusch!
      (keim~put (first (last (res~proof-clauses spass*current-resolution-problem))) :spass-clause-number (list number)))
    (let* ((parent-number1 (first pair1))
	   (parent-number2 (first pair2))
           (parent1 (spass=find-keim-clause parent-number1))
	   (old-parent2 (spass=find-keim-clause parent-number2))
	   (pos1 (pos~list-position (list (spass=compute-new-position parent1 (second pair1)))))
	   (pos2 (pos~list-position (list (spass=compute-new-position old-parent2 (second pair2)))))
	   (resolvent-list (spass=apply-resolution-step parent1 old-parent2 pos1 pos2 spass-clause splitting-level)))
      ;(omega~message "resolvent: ~A~%" resolvent)
      (setf (res~proof-clauses spass*current-resolution-problem) (append (res~proof-clauses spass*current-resolution-problem) resolvent-list))
      (multiple-value-bind (new-positions eq-flip-positions obvious-reductions)
	 (spass=compute-positions-keim-spass-clause (first (last resolvent-list)) spass-clause splitting-level :obvious nil)
       (spass=always-needed-steps (first (last resolvent-list)) spass-clause number new-positions eq-flip-positions obvious-reductions splitting-level))))))

;-------handling SoR - translation------------------------
; Annahme: Reihenfolge der Parents stimmt!---------------
; bisher: fuer 2 parents

(defun spass=translate-sor-justification (spass-clause number parents splitting-level)
  (declare (edited  "07-AUG-1997")
	   (authors Naumann)
	   (input   "The actual spass-clause, the actual number and the parents of the step.")
	   (effect  "The changed proof-steps of spass*current-resolution-problem.")
	   (value   "Undefined."))
  (if (> (length parents) 2) (omega~message "Not yet installed: More than two parents. But nice example!~%")
  (do* ((parent-list parents (rest parent-list))
	(pair1 (first parent-list) (second parent-list))
	(pair2 (second parent-list) (second parent-list)))
     ((equal (length parent-list) 1) ; ist noch pfusch!
      (keim~put (first (last (res~proof-clauses spass*current-resolution-problem))) :spass-clause-number (list number)))
    (let* ((parent-number1 (first pair1))
	   (parent-number2 (first pair2))
           (parent1 (spass=find-keim-clause parent-number1))
	   (old-parent2 (spass=find-keim-clause parent-number2))
	   (pos1 (pos~list-position (list (spass=compute-new-position parent1 (second pair1)))))
	   (pos2 (pos~list-position (list (spass=compute-new-position old-parent2 (second pair2)))))
	   (resolvent-list (spass=apply-resolution-step parent1 old-parent2 pos1 pos2 spass-clause splitting-level)))
      ;(omega~message "resolvent: ~A~%" resolvent)
      (setf (res~proof-clauses spass*current-resolution-problem) (append (res~proof-clauses spass*current-resolution-problem) resolvent-list))
      (multiple-value-bind (new-positions eq-flip-positions obvious-reductions)
	 (spass=compute-positions-keim-spass-clause (first (last resolvent-list)) spass-clause splitting-level :obvious nil)
       (spass=always-needed-steps (first (last resolvent-list)) spass-clause number new-positions eq-flip-positions obvious-reductions splitting-level))))))
   
  		     
;-------handling Splitting-translation-------------------

; 3 Moeglichkeiten: Beginn Ast1, Einfuehrung neg. Behauptung Ast1, Beginn Ast2, Achtung: Ast der nicht geschlossen wird!

(defun spass=translate-spt-justification (spass-clause number parents splitting-level)
  (declare (edited  "11-AUG-1997")
	   (authors Naumann)
	   (input   "The actual spass-clause, the actual number and the parents of the step.")
	   (effect  "The changed proof-steps of spass*current-resolution-problem.")
	   (value   "Undefined."))
  (let* ((actual-open-level (first (last spass*open-splitting-levels)))
         (negated-case (spass=spt-negated-case-p parents)) 
	 (equal-parent-clauses (if (equal (length parents) 1) t (spass=check-equal-parents-p parents))))
					;(omega~message "equal-parent-clauses: ~A~%" equal-parent-clauses)
    (cond ((or (null actual-open-level) (> splitting-level actual-open-level)) ; Beginn Ast1
	   (let* ((splitting-clause (spass=find-keim-clause (first (first parents))))
		  (splitting-clause-number (keim~get splitting-clause :spass-clause-number)))
             (omega~message "Translating branch one at splitting-level ~A.~%" splitting-level)
	     (setq spass*open-splitting-levels (append spass*open-splitting-levels (list splitting-level)))
             (keim~put splitting-clause :branch-open splitting-level)
	     (keim~put splitting-clause :spass-clause-number (append splitting-clause-number (list number)))
	     (spass=mark-splitting-literals splitting-clause parents splitting-level)
	     (multiple-value-bind
		 (new-positions eq-flip-positions obvious-reductions)
		 (spass=compute-positions-keim-spass-clause splitting-clause spass-clause splitting-level :obvious nil)
	       (spass=always-needed-steps splitting-clause spass-clause number new-positions eq-flip-positions obvious-reductions splitting-level))))

	       ((and (equal splitting-level actual-open-level)
		     negated-case)	; Einfuehrung neg. Behauptung Ast1 ; nicht position-change vergessen
		(progn (omega~message "Translating the negation of the refuted assumptions at splitting-level ~A.~%" splitting-level)
		       (spass=translate-spt-justification-neg-branch-one spass-clause number parents splitting-level)))

	       ((and (equal splitting-level actual-open-level) (not negated-case))
		(progn (omega~message "Solving branch 2 from splitting-level ~A.~%" splitting-level)
		       (do* ((rest-clauses (res~proof-clauses spass*current-resolution-problem) (reverse (rest (reverse rest-clauses))))
			     (last-clause (first (last rest-clauses)) (first (last rest-clauses))))
			   ((or (equal (length rest-clauses) 1) (keim~get last-clause :branch-closed))
			    (progn	;(omega~message "last-clause: ~A~%" last-clause)
			      (spass=demark-split-literals last-clause splitting-level)
			      (multiple-value-bind
				  (new-positions eq-flip-positions obvious-reductions)
				  (spass=compute-positions-keim-spass-clause last-clause spass-clause splitting-level :obvious nil)
				(spass=always-needed-steps last-clause spass-clause number new-positions eq-flip-positions obvious-reductions splitting-level)
					;(omega~message "bis.~%")
				(keim~put (if eq-flip-positions
					      (first (last (res~proof-clauses spass*current-resolution-problem)))
					    last-clause)
					  :spass-clause-number
					  (append (keim~get (if eq-flip-positions
								(first (last (res~proof-clauses spass*current-resolution-problem)))
							      last-clause)
							    :spass-clause-number)
						  (list number))))
			      (spass=reduce-open-splitting-levels splitting-level))))))

	       (t omega~message "Splitting-case can't be solved: number ~A, level ~A, parents: ~A.~%" number splitting-level parents))))

(defun spass=reduce-open-splitting-levels (splitting-level)
  (declare (edited  "24-AUG-1997")
	   (authors Naumann)
	   (input   "None.")
	   (effect  "sets spass*open-splitting-levels down do the actual open splitting-level.")
	   (value   "Undefinded."))
  (cond ((not (equal (first (last spass*open-splitting-levels)) splitting-level))
	 (progn (omega~message "It seems, that the open branch ~A was not used.~%" (first (last spass*open-splitting-levels)))
		(setq spass*open-splitting-levels (reverse (rest (reverse spass*open-splitting-levels))))
		(spass=reduce-open-splitting-levels splitting-level)))
	(t (setq spass*open-splitting-levels (reverse (rest (reverse spass*open-splitting-levels)))))))

(defun spass=spt-negated-case-p (parents)
  (declare (edited  "09-SEP-1997")
	   (authors Naumann)
	   (input   "The parents o fthe step.")
	   (effect  "None.")
	   (value   "t if the parents belong to the negated split-case, otherwise nil."))
  (if (> (length parents) 1)
      (let* ((first-parent (caar (reverse parents)))
	     (second-parent (first (second (reverse parents))))
	     (clause1 (spass=find-keim-clause first-parent))
	     (clause2 (spass=find-keim-clause second-parent)))
	(and (keim~equal clause1 clause2)
	     (not (equal first-parent second-parent))))))


(defun spass=check-equal-parents-p (parents)
  (declare (edited  "11-AUG-1997")
	   (authors Naumann)
	   (input   "The parents of a spass-clause-justification.")
	   (effect  "None.")
	   (value   "t, if all parents are the same clause, otherwise nil."))
  (do* ((rest-parents parents (rest rest-parents))
	(pair (first rest-parents) (first rest-parents))
	(parent-number1 (first (first rest-parents)) (first (first rest-parents)))
	(parent-number2 (first (second rest-parents)) (first (second rest-parents)))
	(solution (equal parent-number1 parent-number2) (if (equal parent-number1 parent-number2) solution nil)))
      ((equal (length rest-parents) 2) solution)))

(defun spass=mark-splitting-literals (splitting-clause parents splitting-level)
  (declare (edited  "11-AUG-1997")
	   (authors Naumann)
	   (input   "A splitting-clause and the actual parents.")
	   (effect  "Extends the keim~property :splitting-branch with t if the literal is solved in the right branch and nil if it is solved in the left branch at the actual splitting-level.")
	   (value   "Undefined."))
  (let* ((splitting-positions
	  (do* ((rest-parents parents (rest rest-parents))
		(pair (first rest-parents) (first rest-parents))
		(lit-pos (pos~list-position (list (spass=compute-new-position splitting-clause (second pair))))
			 (if pair (pos~list-position (list (spass=compute-new-position splitting-clause (second pair))))))
		(lit-pos-list (list lit-pos) (if lit-pos (append lit-pos-list (list lit-pos)) lit-pos-list)))
	      ((null rest-parents) lit-pos-list))))
    ;(omega~message "splitting-positions: ~A~%" splitting-positions)
    (do* ((rest-literals (cl~literals splitting-clause) (rest rest-literals))
	  (literal (first rest-literals) (if rest-literals (first rest-literals))))
	((null rest-literals))
      ;(omega~message "lit: ~A~%" literal)
      ;(omega~message "member: ~A~%" (member (first (data~substruct-positions literal splitting-clause)) splitting-positions :test 'keim~equal))
      (if literal (keim~put literal :splitting-branch (if (member (first (data~substruct-positions literal splitting-clause)) splitting-positions :test 'keim~equal)
							  (append (keim~get literal :splitting-branch) (list (list splitting-level nil)))
							(append (keim~get literal :splitting-branch) (list (list splitting-level t)))))))))

(defun spass=demark-split-literals (splitting-clause splitting-level)
  (declare (edited  "11-AUG-1997")
	   (authors Naumann)
	   (input   "A splitting-clause.")
	   (effect  "The keim-property :splitting-branch is set down one level in the literals.")
	   (value   "Undefinded."))
  (do* ((lit-list (cl~literals splitting-clause) (rest lit-list))
	(literal (if lit-list (first lit-list)) (if lit-list (first lit-list))))
      ((null lit-list))
    (if (equal (first (first (reverse (keim~get literal :splitting-branch)))) splitting-level)
	(keim~put literal :splitting-branch (reverse (rest (reverse (keim~get literal :splitting-branch))))))))

; wozu war das gut?
(defun spass=compute-old-position (splitting-clause position)
  (declare (edited  "11-AUG-1997")
	   (authors Naumann)
	   (input   "A clause and a position of a literal in the clause.")
	   (effect  "None.")
	   (value   "Computes the position of the refering SPASS-splitted clause."))
  (let* ((literal (data~struct-at-position splitting-clause position)))
    (do* ((rest-lit-list (cl~literals splitting-clause) (rest rest-lit-list))
	  (literal (first rest-lit-list) (if rest-lit-list (first rest-lit-list)))
	  (old-position 0 (+ old-position (if (first (last (first (reverse (keim~get literal :splitting-branch))))) 0 1))))
	((keim~equal literal (data~struct-at-position splitting-clause position)) old-position-list))))

(defun spass=compute-new-position-only-branch (clause position)
  (declare (edited  "11-AUG-1997")
	   (authors Naumann)
	   (input   "A clause and a position.")
	   (effect  "None.")
	   (value   "Computes the positions of the refering spass-clause (depends on split-level)."))
  (do* ((new-position 0 (+ 1 new-position))
	(count (+ -1 (if (first (last (first (reverse (keim~get (nth new-position (cl~literals clause)) :splitting-branch))))) 0 1))
	       (+ count (if (first (last (first (reverse (keim~get (nth new-position (cl~literals clause)) :splitting-branch))))) 0 1))))
      ((equal count position) new-position)))

(defun spass=compute-new-position (clause position &key (only-branch nil))
  (declare (edited  "19-SEP-1997")
	   (authors Naumann)
	   (input   )
	   (effect  )
	   (value   ))
  (if only-branch
      (spass=compute-new-position-only-branch clause position)
    (do* ((count 0 (+ 1 count))
	  (rest-literals (cl~literals clause) (rest rest-literals))
	  (literal (first rest-literals) (first rest-literals))
					;(k (omega~message "plist lit: ~A~%" (keim~plist literal))
					;(omega~message "plist lit: ~A~%" (keim~plist literal)))
	  (right-branch (first (last (first (reverse (keim~get literal
							       :splitting-branch)))))
			(first (last (first (reverse (keim~get literal
							       :splitting-branch)))))))
	((or (and (not right-branch)
		  (equal (first (reverse (keim~get literal :spass-literal-position)))
			 position))
	     (null rest-literals))
	 (if (null rest-literals) (progn (omega~message "SPASS-literal-position not found.") (spass=geloosed))
	   count)))))
  
(defun spass=reduce-clause-to-splitting-clause (keim-clause)
  (declare (edited  "11-AUG-1997")
	   (authors Naumann)
	   (input   "A computed KEIM-clause.")
	   (effect  "None.")
	    (value   "A clause with the literals of the split-case."))
  ;(omega~message "KEIM-clause before reducing: ~A~%" keim-clause)
  (if (null (cl~literals keim-clause)) keim-clause
  (do* ((rest-lit-list (cl~literals keim-clause) (rest rest-lit-list))
	(literal (first rest-lit-list)
		 (if rest-lit-list (first rest-lit-list)))
	(new-lit-list (if (first (last (first (reverse (keim~get literal :splitting-branch))))) nil (list literal))
		      (if literal (if (first (last (first (reverse (keim~get literal :splitting-branch))))) new-lit-list (append new-lit-list (list literal))) new-lit-list)))
      ((null rest-lit-list) (progn
                              ;(omega~message "KEIM-clause after reducing: ~A~%" (cl~create new-lit-list))
			      (cl~create new-lit-list)))
    ;(omega~message "literal: ~A~%plist: ~A~%new-lit-list: ~A~%" literal (keim~plist literal) new-lit-list)
    )))


(defun spass=test (number splitting-level)		; clauselnummer rein, er gibt die res-moeglichkeiten aus
  (setq solution-resolvent nil)
  (if (numberp number)
      (spass=get-branch-one-solution (spass=compute-clause-tree
				      (spass=find-keim-clause number)
				      splitting-level)
				     spass-clause splitting-level)
    (spass=get-branch-one-solution (spass=compute-clause-tree number splitting-level) (cl~create nil) splitting-level)))


(defun spass=translate-spt-justification-neg-branch-one (spass-clause number parents splitting-level)
  (declare (edited  "14-AUG-1997")
	   (authors Naumann)
	   (input   "A spass-clause, it's number the parents and the splitting-level")
	   (effect  "res~proof-clauses is extended with the correct steps to produced the negated, refuted splitt-case.")
	   (value   "Undefinded."))
  (setq spass*only-one-assumption-literal (equal 1 (length (cl~literals (spass=reduce-clause-to-splitting-clause (spass=find-keim-clause (caadr parents)))))))
  ;(omega~message "parents: ~A~%" parents)
  ;(omega~message "par: ~A~%" (spass=find-keim-clause (caadr parents)))
  ;(omega~message "One assumption: ~A~%" spass*only-one-assumption-literal)
  ;(setq spass*only-one-assumption-literal nil)
  (setq solution-resolvent nil)
  (do* ((clause-list (res~proof-clauses spass*current-resolution-problem) (reverse (rest (reverse clause-list))))
	(node-clause (first (last clause-list)) (first (last clause-list))))
      ((keim~get node-clause :branch-closed)
       (progn
	 (omega~message "Searching negated split-case by backtracking tree ...")
	 (let* ((clause (spass=get-branch-one-solution (spass=compute-clause-tree node-clause splitting-level) spass-clause splitting-level)))
	   ;(omega~message "clause: ~A~%" clause)
	   ;(setq a (first clause)) 
	   (if (not solution-resolvent) ; ist noch marx aeh murx
	       (omega~message "~%Very sorry, the negatet case is not found.~%")
	     (multiple-value-bind
		 (new-positions eq-flip-positions obvious-reductions)
		 (spass=compute-positions-keim-spass-clause solution-resolvent spass-clause splitting-level :obvious nil)
	       (omega~message " found.~%")
	       (let* ((resolvents (reverse (spass=extract-resolvents-from-solution solution-resolvent splitting-level))))
		 (spass=set-splitting-levels resolvents)
		 (setf (res~proof-clauses spass*current-resolution-problem)
		       (append (res~proof-clauses spass*current-resolution-problem) resolvents))
		 (spass=always-needed-steps solution-resolvent spass-clause number new-positions eq-flip-positions obvious-reductions splitting-level)
		 (keim~put solution-resolvent :spass-clause-number (append (keim~get solution-resolvent :spass-clause-number) (list number)))
		 ))))))))
		      
(defun spass=set-splitting-levels (clause-list)
  (declare (edited  "17-AUG-1997")
	   (authors Naumann)
	   (input   "A clause-list.")
	   (effect  "Set the correct splitting-level to the clauses.")
	   (value   "Undefined."))
  (do* ((rest-clause-list clause-list (rest rest-clause-list))
	(clause (first rest-clause-list) (first rest-clause-list)))
      ((null rest-clause-list))
    (if clause
	(let* ((justification (node~justification clause))
	       (resolution (res~resolution-p justification))
	       (parent1 (if resolution (first (res~resolution-clauses justification))
			  (res~factoring-clause justification)))
	       (parent2 (if resolution (second (res~resolution-clauses justification))
			  (res~factoring-clause justification)))
	       (split-level1 (if (keim~get parent1 :branch-open) (keim~get parent1 :branch-open)
			       (keim~get parent1 :splitting-level)))
	       (split-level2 (if (keim~get parent2 :branch-open) (keim~get parent2 :branch-open)
			       (keim~get parent2 :splitting-level))))
	  (keim~put clause :splitting-level (max split-level1 split-level2))))))


(defun spass=extract-resolvents-from-solution (clause splitting-level)
  (if (and (keim~get clause :splitting-level)
	   (or (< (keim~get clause :splitting-level) splitting-level)
	       (keim~get clause  :branch-open)))
      nil
    (cond ((res~resolution-p (node~justification clause))
	   (append (list clause)
		   (spass=extract-resolvents-from-solution (first (res~resolution-clauses (node~justification clause))) splitting-level)
		   (spass=extract-resolvents-from-solution (second (res~resolution-clauses (node~justification clause))) splitting-level)))
	  ((res~factoring-p (node~justification clause))
	   (append (list clause)
		   (spass=extract-resolvents-from-solution (res~factoring-clause (node~justification clause)) splitting-level))))))


(defun spass=compute-clause-tree (clause-tree splitting-level)
  (declare (edited  "16-AUG-1997")
	   (authors Naumann)
	   (input   "A node-clause.")
	   (effect  "None.")
	   (value   "Computes all parents from the node-clause in the way, that a two number's list means resolution, a list with length one means factoring.")
	   (example "((cl1 (cl2 (cl3))) cl4) means ((cl1 RES (cl2 RES (FAC cl3))) RES cl4)."))
  (cond ((cl~clause-p clause-tree)
	 (progn
	   ;(omega~message "Number: ~A~%" (keim~get clause-tree :spass-clause-number))
	 (let* ((justification (node~justification clause-tree))
					;(initial (res~initial-p justification))
		(factoring (res~factoring-p justification))
		(resolution (res~resolution-p justification))
		(parent1 (if factoring (res~factoring-clause justification)
			   (if resolution (first (res~resolution-clauses justification)))))
		(parent2 (if resolution (second (res~resolution-clauses justification)) parent1)))
	   ;(omega~message "just: ~A~%" justification)
	   (if (and (not factoring) (not resolution) (not (res~initial-p justification)) (not (res~reflex-p justification)))
	       (omega~message "Unknown justification found during backtracking tree.~%")
	     (if (and (< (keim~get clause-tree :splitting-level) splitting-level))
		 clause-tree
	       (if factoring
		   (spass=compute-clause-tree (list parent1) splitting-level)
		 (progn (cond ((equal (keim~get parent1 :branch-open) splitting-level)
			       (keim~put parent2 :unifier (res~just-unifier justification)))
			    
			      ((equal (keim~get parent2 :branch-open) splitting-level)
			       (keim~put parent1 :unifier (res~just-unifier justification))))
			(spass=compute-clause-tree (list parent1 parent2) splitting-level))))))))

	((and (listp clause-tree) (equal (length clause-tree) 1)) ; factoring-case
	 (let* ((clause1 (first clause-tree)))
	   (if (and (cl~clause-p clause1) (equal (keim~get clause1 :branch-open) splitting-level))
	       (spass=compute-clause-tree clause1 splitting-level)
	     (list (spass=compute-clause-tree (first clause-tree) splitting-level)))))

	((and (listp clause-tree) (equal (length clause-tree) 2)) ; resolution-case
	 (let* ((clause1 (first clause-tree))
		(clause2 (second clause-tree)))
	   (if (and (cl~clause-p clause1) (equal (keim~get clause1 :branch-open) splitting-level))
	       (progn (omega~message "mother. Nr. ~A~%" (keim~get clause1 :spass-clause-number))
		      (spass=compute-clause-tree (spass=instantiate-clause clause2) splitting-level))
	     (if (and (cl~clause-p clause2) (equal (keim~get clause2 :branch-open) splitting-level))
		 (progn ;(omega~message "father. Nr. ~A~%" (keim~get clause2 :spass-clause-number))
			(spass=compute-clause-tree (spass=instantiate-clause clause1) splitting-level))
	       (list (spass=compute-clause-tree clause1 splitting-level)
		     (spass=compute-clause-tree clause2 splitting-level))))))

	(t (omega~message "Unknown: ~A~%" clause-tree))))

(defun spass=instantiate-clause (clause)
  (declare (edited  "10-SEP-1997")
	   (authors Naumann)
	   (input   "An clause which exits in the steps of spass*current-resolution-problem.")
	   (effect  "Extents res~proof-clauses with the instantianted clause.")
	   (value   "The instantiated clause."))
  (let* ((unifier (keim~get clause :unifier))
	 (justification (res~instance-create clause unifier (format nil "Instanced-~A" (gensym))))
	 (instanciated-clause (spass=subst-apply unifier clause))
	 )
    (if (not (subst~empty-p unifier))
	(progn (setf (node~justification instanciated-clause) justification)
	       (keim~put instanciated-clause :splitting-level (keim~get clause :splitting-level))
	       (setf (res~proof-clauses spass*current-resolution-problem) (append (res~proof-clauses spass*current-resolution-problem) (list instanciated-clause)))
	       instanciated-clause)
      clause)))


(defun spass=get-branch-one-solution (tree spass-clause splitting-level)
 (if solution-resolvent nil
  (cond ((cl~clause-p tree)
	   (list tree))
	  ((equal (length tree) 1)
	   (spass=get-factors-from-list (spass=get-branch-one-solution (first tree) spass-clause splitting-level) spass-clause splitting-level))
	  ((equal (length tree) 2)
	   (spass=get-resolvents-from-list (spass=get-branch-one-solution (first tree) spass-clause splitting-level)
					   (spass=get-branch-one-solution (second tree) spass-clause splitting-level)
					   spass-clause splitting-level)))))
	  
(defun spass=get-factors-from-list (clause-list spass-clause splitting-level)
  (if (null clause-list) nil
    (do* ((rest-clause-list clause-list (rest rest-clause-list))
	  (clause (first rest-clause-list) (first rest-clause-list))
	  (solution (last
		     (spass=factore-out-resolvent
		      (list clause)
		      spass-clause
		      splitting-level
		      :all-literals 'all
		      :neg-branch-search t))
		    (if clause
			(append solution
				(last
				 (spass=factore-out-resolvent
				  (list clause)
				  spass-clause
				  splitting-level
				  :all-literals 'all
				  :neg-branch-search t)))
		      solution)))
	((null rest-clause-list) (remove-duplicates solution :test 'keim~equal)))))

(defun spass=get-resolvents-from-list (clause-list1 clause-list2 spass-clause splitting-level)
  (setq solut nil)
  (if (or (null clause-list1) (null clause-list2))
      (append clause-list1 clause-list2)
    (do* ((rest-clause-list1 clause-list1 (rest rest-clause-list1))
	  (clause1 (first rest-clause-list1) (first rest-clause-list1)))
	((null rest-clause-list1) (remove-duplicates
				   (append
				    solut
				    (if (not spass*only-one-assumption-literal)
					(append clause-list1 clause-list2)))
				   :test 'keim~equal))
					;(omega~message "clause1: ~A~%" clause1)
      (do* ((rest-clause-list2 clause-list2 (rest rest-clause-list2))
	    (clause2 (first rest-clause-list2) (first rest-clause-list2)))
	  ((null rest-clause-list2))
					;(omega~message "clause2: ~A~%" clause2)
	(let* ((resolvents (spass=res-binary-resolution clause1 clause2 spass-clause splitting-level)))
					;(omega~message "Bis.~A~%" resolvents)
	  (setq solut (append solut resolvents)))))))

(defun spass=res-binary-factoring (clause spass-clause splitting-level)
  (declare (edited  "06-JUL-1995")
	   (authors "Ameier, Naumann")
	   (input  "A clause.")
	   (effect "None.")
	   (value  "The list of all binary factoring clauses."))
  (if (cl~clause-p clause)
      (multiple-value-bind
	  (factlists unifiers literal-positions)
	  (spass=res-clause-factoring clause)
	(spass=res-create-clauses factlists (list clause) literal-positions nil unifiers spass-clause splitting-level :kind-of-justification 'factoring))
    (post~error "~A is not a clause" clause)))   

(defun spass=neg-branch-found-p (clause spass-clause splitting-level)
  (declare (edited  "18-AUG-1997")
	   (authors Naumann)
	   (input   "A clause.")
	   (effect  "None.")
	   (value   "t if solution is found, otherwise nil."))
  (setq clausi clause)
  (let* ((reduced-clause (spass=reduce-clause-to-splitting-clause clause)))
    (and (spass=only-same-literals reduced-clause)
	 (spass=compute-positions-keim-spass-clause reduced-clause spass-clause splitting-level)))) ; hier wird bewusst auf merge gecheckt!

(defun spass=only-same-literals (clause)
  (declare (edited  "18-AUG-1997")
	   (authors Naumann)
	   (input   "A clause.")
	   (effect  "None.")
	   (value   "true, if the clause only contains the same Literals."))
  (if (null (cl~literals clause)) nil
    (if (equal (length (cl~literals clause)) 1) t
      (do* ((rest-literals (cl~literals clause) (rest rest-literals))
	    (lit1 (first rest-literals) (first rest-literals))
	    (lit2 (second rest-literals) (second rest-literals))
	    (solu (keim~equal lit1 lit2) (if (not (keim~equal lit1 lit2)) nil solu)))
	  ((equal (length rest-literals) 2) solu)))))

  
(defun spass=res-clause-factoring (clause)
  (declare (edited  "21-MAY-1996")
	   (authors "Ameier, Naumann")
	   (input   "A clauses.")
	   (effect  "None.")
	   (value   "A multiple-value:"
		    "First: A list of all literallists arising by binary factoring"
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
      (let ((head-literal (first rest-literals)))
	(do* ((pot-fact-literals (rest rest-literals) (rest pot-fact-literals))
	      (used-fact-literals nil))
	    ((null pot-fact-literals) nil)
	  (let* ((head-pot-literal (first pot-fact-literals))
		 (mgu (term~unify head-pot-literal head-literal)))
	    (if (and mgu
		     (keim~equal (subst~apply mgu head-pot-literal)
				 (subst~apply mgu head-literal))
		     (not (first (last (first (reverse (keim~get head-literal :splitting-branch)))))) ; nur sinnvolle Fact. erzeugen
		     (not (first (last (first (reverse (keim~get head-pot-literal :splitting-branch)))))))
		(progn
		  (setq literal-lists (cons (mapcar #'(lambda (literal)
							
							 (spass=subst-apply mgu literal))
							
						    (append used-literals (list head-literal) used-fact-literals
							    (rest pot-fact-literals)))
					    literal-lists))
		  (setq fact-clause-pairs (cons (cons head-literal head-pot-literal) fact-clause-pairs))
		  (setq unifier-list (cons mgu unifier-list))))
	    (setq used-fact-literals (append used-fact-literals (list head-pot-literal)))))
	(setq used-literals (append used-literals (list head-literal)))))))

(defun spass=res-binary-resolution (clause1 clause2 spass-clause splitting-level)
  (declare (edited  "06-JUL-1995")
	   (authors "Ameier, Naumann")
	   (input   "Two clauses ,a keyword eliminate-tautologies (t/nil) to decide whether"
		    "arising tautologies should be returned or should be eliminated directly."
		    "(default nil)")
	   (effect  "None.")
	   (value   "The list of all binary resolvent clauses of the two input clauses."))
  (if (and (cl~clause-p clause1) (cl~clause-p clause2))
      (multiple-value-bind
	  (newclause2 renamingclause2)
	  (res~separate-clauses clause1 clause2)
	(spass=copy-splitting-branches-of-clauses clause2 newclause2)
      	(multiple-value-bind
	    (resolventslists unifiers position-literals)
	    (spass=res-clause-clause-resolution clause1 newclause2)
	  (spass=res-create-clauses resolventslists (list clause1 clause2 newclause2) position-literals (list (subst~create nil nil) renamingclause2) unifiers spass-clause splitting-level :kind-of-justification 'resolution)))
    (post~error "Only clauses are accepted ~A ~A" clause1 clause2)))

(defun spass=res-clause-clause-resolution (clause1 clause2)
  (declare (edited  "21-MAY-1996")
	   (authors "Ameier, Naumann")
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
		(let ((mgu (term~unify head-literal1 head-literal2)))
		  (if (and mgu
			   (not (first (last (first (reverse (keim~get head-literal1 :splitting-branch)))))) ; nur sinnvolle resolventen
			   (not (first (last (first (reverse (keim~get head-literal2 :splitting-branch)))))))
		      (progn
			(setq resol-lit-list-list (cons (mapcar #'(lambda (literal)
								    
								     (spass=subst-apply mgu literal))
								     
								(append used-lit-list1 (rest rest-lit-list1)
									used-lit-list2 (rest rest-lit-list2)))
							resol-lit-list-list))
			(setq unifier-list (cons mgu unifier-list))
			(setq position-literal-list (cons (cons head-literal1 head-literal2) position-literal-list))))))
	    (setq used-lit-list2 (append used-lit-list2 (list head-literal2)))))
	(setq used-lit-list1 (append used-lit-list1 (list head-literal1)))))))

(defun spass=res-create-clauses (list-of-literallists parents list-of-positions renamings list-of-unifiers spass-clause splitting-level &key (kind-of-justification 'resolution))
  (declare (edited  "06-JUL-1995")
	   (authors "Ameier, Naumann")
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
		  (let* ((justification (res~resolution-create
					 (list (first parents) (second parents))
					 (list (keim::res=literal-position (first positions) (first parents)) 
					       (keim::res=literal-position (rest positions) (third parents)))
					 renamings
					 unifier
					 (format nil "RES-~A" (incf keim::res*res-justification-counter))))
			 (clause (cl~create litli :justification justification)))
		    (if (spass=neg-branch-found-p clause spass-clause splitting-level)
			(setq solution-resolvent (first (last (spass=factore-out-resolvent (list clause) spass-clause splitting-level :all-literals t))))
		      (first (last (spass=factore-out-resolvent (list clause) spass-clause splitting-level :all-literals t))))))
		    
	      list-of-literallists list-of-positions list-of-unifiers)
    (mapcar #'(lambda (litli positions unifier)
		(let ((justification (res~factoring-create
				      (car parents)
				      (list (keim::res=literal-position (first positions) (first parents))
					    (keim::res=literal-position (rest positions) (first parents)))
				      (subst~create nil nil)
				      unifier
				      (format nil "FACT-~A" (incf keim::res*fact-justification-counter)))))
		  (cl~create litli :justification justification)))
	    list-of-literallists list-of-positions list-of-unifiers)))


(defun spass=copy-splitting-branches-of-clauses (clause1 clause2)
  (declare (edited  "14-AUG-1997")
	   (authors Naumann)
	   (input   "Two clauses.")
	   (effect  "Copies the plist splitting-branch from clause1 to clause2.")
	   (value   "Undefined."))
  (if (null (cl~literals clause1)) nil
    (do* ((rest-literals1 (cl~literals clause1) (rest rest-literals1))
	  (rest-literals2 (cl~literals clause2) (rest rest-literals2))
	  (literal1 (first rest-literals1) (first rest-literals1))
	  (literal2 (first rest-literals2) (first rest-literals2)))
	((null rest-literals1))
      ;(setf (lit~clause literal2) (lit~clause literal1))
      (keim~put literal2 :splitting-branch (keim~get literal1 :splitting-branch)))))

(defun spass=subst-apply (substitution object)
  (declare (edited  "15-AUG-1997")
	   (authors Naumann)
	   (input   "A substitution an a object.")
	   (effect  "If the object contains literals, the :splitting-branch is copied.")
	   (value   "The new object with the applied substitution."))
  (cond ((typep object 'cons)		; literal-liste
	 (let ((new-lit-list (subst~apply substitution object)))
	   (spass=copy-splitting-branches-of-clauses (cl~create object) (cl~create new-lit-list))
	   new-lit-list))
	((typep object 'lit+literal)
	 (let ((new-lit (subst~apply substitution object)))
	   (spass=copy-splitting-branches-of-clauses (cl~create (list object)) (cl~create (list new-lit)))
	   ;(setf (lit~clause new-lit) (lit~clause object))
	   new-lit))
        ((typep object 'cl+clause)
	 (let ((new-clause (cl~create (spass=subst-apply substitution (cl~literals object)))))
           (keim~put new-clause :branch-open (keim~get object :branch-open))
	   (keim~put new-clause :splitting-level (keim~get object :splitting-level))
	   new-clause))
	(t (subst~apply substitution object))))


;-------handling Con-translation--------------------------

(defun spass=translate-con-justification (spass-clause number parents splitting-level)
  (declare (edited  "07-AUG-1997")
	   (authors Naumann)
	   (input   "The actual spass-clause, the actual number and the parents of the step.")
	   (effect  "The changed proof-steps of spass*current-resolution-problem.")
	   (value   "Undefined."))
  (let* ((number-parent (first (first parents)))
	 (parent-clause (spass=find-keim-clause number-parent)))
    (setf (res~proof-clauses spass*current-resolution-problem)
	  (append (res~proof-clauses spass*current-resolution-problem)
		  (rest (spass=reduce-factorings parent-clause spass-clause splitting-level))))
    (multiple-value-bind
	(new-positions eq-flip-positions obvious-reductions)
	(spass=compute-positions-keim-spass-clause (first (last (res~proof-clauses spass*current-resolution-problem))) spass-clause splitting-level :obvious nil)
      (spass=always-needed-steps (first (last (res~proof-clauses spass*current-resolution-problem))) spass-clause number new-positions eq-flip-positions obvious-reductions splitting-level))
    (keim~put (first (last (res~proof-clauses spass*current-resolution-problem))) :spass-clause-number (list number))))

;-------handling Rew - translation ------------------------

(defun spass=translate-rew-justification (spass-clause number parents splitting-level)
  (declare (edited  "05-AUG-1997")
	   (authors Naumann)
	   (input   "The actual spass-clause, the actual number and the parents of the step.")
	   (effect  "The changed proof-steps of spass*current-resolution-problem.")
	   (value   "Undefined."))
  (if (> (length parents) 2)
      (spass=translate-rew-justification-many-parents spass-clause number parents splitting-level)
  (if (not (spass=translate-rew-justification-aux spass-clause number parents splitting-level))
      (if (not (spass=translate-rew-justification-aux spass-clause number (reverse parents) splitting-level))
	  (omega~message "Sorry, it was not possible to translate rew-step.~%Possible solution:
Don't use the SPASS-Auto-Mode.~%")
	(progn
	  (omega~message "With parents rotated used (first rewritten clause).~%")
	  (keim~put (first (last (res~proof-clauses spass*current-resolution-problem))) :spass-clause-number (list number))))
    (progn
      (omega~message "With parents normal used (second rewritten clause).~%")
      (keim~put (first (last (res~proof-clauses spass*current-resolution-problem))) :spass-clause-number (list number))))))

(defun spass=translate-rew-justification-aux (spass-clause number parents splitting-level)
  (declare (edited  "02-AUG-1997")
	   (authors Naumann)
	   (input   "The actual spass-clause, the actual number and the parents of the step.")
	   (effect  "The changed proof-steps of spass*current-resolution-problem, if solution possible.")
	   (value   "t, if a solution was found, otherwise nil."))
  (let* ((number-parent1 (first (first parents)))
	 (number-parent2 (first (second parents)))
	 (parent-clause1 (spass=find-keim-clause number-parent1))
	 (old-parent-clause2 (spass=find-keim-clause number-parent2))
         ;(k (omega~message "literals: ~a~%" (cl~literals parent-clause1)))
	 ;(k (omega~message "literals: ~a~%" (cl~literals old-parent-clause2)))
	 (p1 (spass=compute-new-position parent-clause1 (second (first parents))))
	 (p2 (spass=compute-new-position old-parent-clause2 (second (second parents)))))
    (multiple-value-bind
	(parent-clause2 renaming2)
	(res~separate-clauses parent-clause1 old-parent-clause2)
      (spass=copy-splitting-branches-of-clauses old-parent-clause2 parent-clause2)
      (let* ((pos1 (pos~list-position (list p1)))
             (pos2 (pos~list-position (list p2)))
	     (lit-equation (nth p1 (cl~literals parent-clause1)))
             ;(k (omega~message "literals: ~a~%" (cl~literals parent-clause1)))
	     (equation (lit~atom (nth p1 (cl~literals parent-clause1))))
					;(k (omega~message "equa: ~A~%" equation))
	     (really-equation (and (typep equation 'term+appl)
				   (data~equal (data~appl-function equation)
					       (data~schema-range (env~lookup-object '=
										     (res~proof-environment spass*current-resolution-problem))))))
	     (s (if really-equation (first (data~appl-arguments equation))))
	     (tr (if really-equation (second (data~appl-arguments equation))))
	     (lit-applicant (nth p2 (cl~literals parent-clause2)))
					;(k (omega~message "keimplist appl: ~A~%" (keim~plist lit-applicant)))
	     (applicant (lit~atom (nth p2 (cl~literals parent-clause2))))
					;(k (omega~message "keimplist appl: ~A~%" (keim~plist applicant)))
	     (subterm-list (spass=get-subterms applicant))
	     (appl-equation (and (typep applicant 'term+appl)
				 (data~equal (data~appl-function applicant)
					     (data~schema-range (env~lookup-object '=
										   (res~proof-environment spass*current-resolution-problem)))))))

					
	(if really-equation
	    (progn
	      (setq end-flag nil)
              
	      (do* ((rest-subterm-list (if appl-equation (rest subterm-list)
					 (if (not really-equation) nil subterm-list))
				       (rest rest-subterm-list))
		    
		    (actual-subterm (first (first rest-subterm-list)) (first (first
									      rest-subterm-list)))
		    (actual-subterm-position (second (first rest-subterm-list)) (second
										 (first rest-subterm-list)))
		    (act-unif (term~unify s actual-subterm) (if rest-subterm-list (term~unify s actual-subterm)))
              
		    (actual-unifier (if act-unif (spass=check-unifier-type act-unif))
				    (if act-unif (spass=check-unifier-type act-unif))) 
		    (act-unif2 (term~unify tr actual-subterm) (if rest-subterm-list (term~unify tr actual-subterm)))
		    (actual-unifier2 (if act-unif2 (spass=check-unifier-type act-unif2))
				     (if act-unif2 (spass=check-unifier-type act-unif2))))
	      
		  ((or (null rest-subterm-list) end-flag) end-flag)
					;(omega~message "actual-unifier: ~A, actual-unifier2: ~A, actual-subterm: ~A~%subterm-list: ~A~%" actual-unifier actual-unifier2 actual-subterm rest-subterm-list)
		(if (and actual-unifier (keim~equal actual-subterm (spass=subst-apply actual-unifier actual-subterm))) ;matcher?
                    (let* ((new-literal (lit~literal-create (data~replace-at-position
							     (spass=subst-apply
							      actual-unifier applicant)
							     actual-subterm-position
							     (spass=subst-apply actual-unifier tr)) (lit~positive-p lit-applicant)))
			   (new-clause (cl~create (append (cl~literals (data~replace-at-position
									(spass=subst-apply actual-unifier
											   parent-clause2)
									pos2
									new-literal))
							  (remove (spass=subst-apply actual-unifier
										     lit-equation)
								  (spass=subst-apply actual-unifier
										     (cl~literals
										      parent-clause1))
								  :count 1 :test 'keim~equal :start
								  p1))))
			   (factored-clause-list (spass=reduce-factorings new-clause spass-clause splitting-level))
					;(k (omega~message "fac-clause1: ~A~%" factored-clause-list))
			   )
					;(omega~message "act1: nlits1: ~A, nlit2: ~A, nappl: ~A new-clause: ~A~%" new-lits1 new-lits2 new-applicant new-clause)
                      ;(setf (lit~clause new-literal) (lit~clause lit-applicant))
		      (keim~put new-literal :splitting-branch (keim~get lit-applicant :splitting-branch))
		      (if factored-clause-list
					;(omega~message "act1: nlits1: ~A, nlit2: ~A, nappl: ~A new-clause: ~A~%" new-lits1 new-lits2 new-applicant new-clause)
			  (multiple-value-bind (new-positions eq-flip-positions obvious-reductions)
			      (spass=compute-positions-keim-spass-clause (first (last factored-clause-list)) spass-clause splitting-level :obvious nil)
			    (if new-positions
				(progn (setq end-flag t)
					;(omega~message "bis if-new-positions1")
				       (let* ((mother old-parent-clause2)
					      (mother-position (pos~add-front p2
									      (pos~add-front 1 actual-subterm-position)))
									 (mother-renaming renaming2)
					      (father parent-clause1)
					      (father-position (pos~list-position (list p1)))
					      (father-renaming (subst~create nil nil))
					      (direction 'LR)
					      (unifier actual-unifier)
					      (name nil)
					      (justification (res~paramod-create mother mother-position mother-renaming father father-position father-renaming direction unifier name)))
					 (setf (node~justification new-clause) justification)
                                         (if (equal spass*allowed-splittings 0)
					     (setf (res~proof-clauses spass*current-resolution-problem)
						   (append (res~proof-clauses spass*current-resolution-problem)
							   factored-clause-list))
					   (let* ((para-translation-list
						   (spass=translate-paramodulation-step
						    new-clause
						    spass-clause
						    splitting-level))
						  (factored-out-list
						   (spass=reduce-factorings
						    (first (last para-translation-list))
						    spass-clause splitting-level)))
					     (setf (res~proof-clauses spass*current-resolution-problem)
						   (append (res~proof-clauses spass*current-resolution-problem)
							   para-translation-list
							   (rest factored-out-list)))))
					 (multiple-value-bind
					     (new-positions eq-flip-positions obvious-reductions)
					     (spass=compute-positions-keim-spass-clause (first (last (res~proof-clauses spass*current-resolution-problem))) spass-clause splitting-level :obvious nil)
					   (spass=always-needed-steps (first (last (res~proof-clauses spass*current-resolution-problem))) spass-clause number new-positions eq-flip-positions obvious-reductions splitting-level))
					;(omega~message "end-flag: ~A~%" end-flag)
					 )))))))
		(if (and actual-unifier2  (not end-flag) (keim~equal actual-subterm (spass=subst-apply actual-unifier2 actual-subterm)))
		    (let* ((new-literal (lit~literal-create (data~replace-at-position
							     (spass=subst-apply
							      actual-unifier2 applicant)
							      actual-subterm-position
							      (spass=subst-apply actual-unifier2 s)) (lit~positive-p lit-applicant)))
			   (new-clause (cl~create (append (cl~literals(data~replace-at-position
								       (spass=subst-apply actual-unifier2
											  parent-clause2)
								       pos2
								       new-literal))
							  (remove (spass=subst-apply actual-unifier2 lit-equation) (spass=subst-apply actual-unifier2 (cl~literals parent-clause1)) :count 1 :test 'keim~equal :start p1))))
					;(k (omega~message "after new-clause.~%"))
			   (factored-clause-list (spass=reduce-factorings new-clause spass-clause splitting-level))
					;(k (omega~message "fac-clause2: ~A~%" factored-clause-list))
			   )
		      ;(setf (lit~clause new-literal) (lit~clause lit-applicant))
		      (keim~put new-literal :splitting-branch (keim~get lit-applicant :splitting-branch))
		      (if factored-clause-list
			  (multiple-value-bind (new-positions eq-flip-positions obvious-reductions)
			      (spass=compute-positions-keim-spass-clause (first (last factored-clause-list)) spass-clause splitting-level :obvious nil)
					;(omega~message "2nc: ~A~%" new-clause)
			    (if new-positions
				(progn (setq end-flag t)
					;(omega~message "bis if-new-positions2")
				       (let* ((mother old-parent-clause2)
					      (mother-position (pos~add-front p2 (pos~add-front 1 actual-subterm-position)))
					      (mother-renaming renaming2)
					      (father parent-clause1)
					      (father-position (pos~list-position (list p1)))
					      (father-renaming (subst~create nil nil))
					      (direction 'RL)
					      (unifier actual-unifier2)
					      (name nil)
					      (justification (res~paramod-create mother mother-position mother-renaming father father-position father-renaming direction unifier name)))
					 (setf (node~justification new-clause) justification)
					 (if (equal spass*allowed-splittings 0)
					     (setf (res~proof-clauses spass*current-resolution-problem)
						   (append (res~proof-clauses spass*current-resolution-problem)
							   factored-clause-list))
					   (let* ((para-translation-list
						   (spass=translate-paramodulation-step
						    new-clause
						    spass-clause
						    splitting-level))
						  (factored-out-list
						   (spass=reduce-factorings
						    (first (last para-translation-list))
						    spass-clause splitting-level)))
					     (setf (res~proof-clauses spass*current-resolution-problem)
						   (append (res~proof-clauses spass*current-resolution-problem)
							   para-translation-list
							   (rest factored-out-list)))))
					 (multiple-value-bind
					     (new-positions eq-flip-positions obvious-reductions)
					     (spass=compute-positions-keim-spass-clause (first (last (res~proof-clauses spass*current-resolution-problem))) spass-clause splitting-level :obvious nil)
					   (spass=always-needed-steps (first (last (res~proof-clauses spass*current-resolution-problem))) spass-clause number new-positions eq-flip-positions obvious-reductions splitting-level))))))))))))))))

(defun spass=translate-rew-justification-many-parents (spass-clause number parents splitting-level)
  (declare (edited  "21-AUG-1997")
	   (authors Naumann)
	   (input   "The actual spass-clause, the actual number and the parents of the step.")
	   (effect  "The changed proof-steps of spass*current-resolution-problem.")
	   (value   "Undefined."))
  (omega~message "Rewriting with many parents - maybe take a while ... ")
  (setq solution-found nil)
  (let* ((sortet-numbers (spass=extract-parents-for-rew parents))
	 (rew-clauses (first sortet-numbers))
	 (help-clauses (second sortet-numbers)))
    (setq solution-list (list (list (spass=find-keim-clause (first (first rew-clauses))))))
    (spass=translate-rew-justification-many-parents-aux rew-clauses help-clauses spass-clause splitting-level)
    
    (multiple-value-bind
	(new-positions eq-flip-positions obvious-reductions)
	(spass=compute-positions-keim-spass-clause (first (last (res~proof-clauses spass*current-resolution-problem))) spass-clause splitting-level :obvious nil) 
      (spass=always-needed-steps (first (last (res~proof-clauses spass*current-resolution-problem))) spass-clause number new-positions eq-flip-positions obvious-reductions splitting-level))
    (omega~message "solved.~%")
    (keim~put (first (last (res~proof-clauses spass*current-resolution-problem))) :spass-clause-number (list number))))

(defun spass=translate-rew-justification-many-parents-aux (rew-clauses help-clauses
								       spass-clause
								       splitting-level
								       &key
									(round 0))
  (declare (edited  "21-AUG-1997")
	   (authors Naumann)
	   (input   "The actual spass-clause, the actual number and the parents of the step.")
	   (effect  "The changed proof-steps of spass*current-resolution-problem.")
	   (value   "Undefined."))
  ;(omega~message "Runde: ~A~%" round)
  (cond ((null rew-clauses)
	 (let* ((factors (spass=reduce-factorings (first (last (first (reverse solution-list)))) spass-clause splitting-level)))
	   (if factors
	       (progn ;(omega~message "fac: ~A~%" factors)
		 (setf (res~proof-clauses spass*current-resolution-problem) (append
									     (res~proof-clauses spass*current-resolution-problem) (spass=make-list (rest solution-list) spass-clause splitting-level) (rest factors)))
		      (setq solution-found t))
	     (setq solution-list (reverse (rest (reverse solution-list)))))))
        (t  (let* ((number-rew-clause (first (first rew-clauses)))
	           (number-help-clause (first (first help-clauses)))
	           (rew-clause (first (last (first (reverse solution-list)))))
                   ;(k (omega~message "rew-clause: ~A~%" rew-clause))
		   (old-help-clause (spass=find-keim-clause number-help-clause))
                   ;(k (omega~message "plist: ~A~%" (keim~plist old-help-clause)))
		   (rew-p (if (zerop round) (spass=compute-new-position rew-clause (second
										  (first
										   rew-clauses)))
			    (pos~first (res~paramod-mother-position (node~justification
								     rew-clause)))))
                   ;(k (omega~message "rew-p: ~A~%" rew-p))
		   (help-p (spass=compute-new-position old-help-clause (second (first
										help-clauses))))
		   (rew-pos (pos~list-position (list rew-p)))
		   (help-pos (pos~list-position (list help-p))))
	      (multiple-value-bind
		  (help-clause renaming2)
		  (res~separate-clauses rew-clause old-help-clause)
		(spass=copy-splitting-branches-of-clauses old-help-clause help-clause)
		;(omega~message "bis.~%")
		(let* ((lit-equation (nth help-p (cl~literals help-clause)))
                       ;(k (omega~message "lit-eq: ~A~%" lit-equation))
		       (equation (lit~atom lit-equation))
		       (s (first (data~appl-arguments equation)))
                       (tr (second (data~appl-arguments equation)))
                       ;(k (omega~message "s: ~A, tr: ~A~%" s tr))
		       (lit-applicant (nth rew-p (cl~literals rew-clause)))
		       ;(k (omega~message "lit-appl: ~A~%" lit-applicant))
		       (applicant (lit~atom lit-applicant))
		       ;(k (omega~message "appl-prev: ~A~%" applicant))
		       (subterm-list (spass=get-subterms applicant)))
		  ;(omega~message "bis2.~%")
                  ;(omega~message "subterms: ~A~%" subterm-list) 
		  (do* ((rest-subterm-list subterm-list (rest rest-subterm-list))
			(actual-subterm (first (first rest-subterm-list)) (first (first
										  rest-subterm-list)))
			(actual-subterm-position (second (first rest-subterm-list))
						 (second (first rest-subterm-list)))
			(act-unif (term~unify s actual-subterm) (if rest-subterm-list (term~unify s actual-subterm)))
			(actual-unifier (if act-unif (spass=check-unifier-type act-unif))
					(if act-unif (spass=check-unifier-type act-unif))) 
			(act-unif2 (term~unify tr actual-subterm)
				   (if rest-subterm-list (term~unify tr actual-subterm)))
					;(k (omega~message "rest-subterm: ~A, sol-found: ~A~%" rest-subterm-list solution-found))
			(actual-unifier2 (if act-unif2 (spass=check-unifier-type act-unif2))
					 (if act-unif2 (spass=check-unifier-type act-unif2))))
		      ((or (null rest-subterm-list) solution-found) solution-found)
                                        ;(omega~message "s: ~A, tr: ~A~%" s tr)


		    ;(omega~message "au1: ~A, au2: ~A~%" actual-unifier actual-unifier2)
					;(omega~message "actual-subterm: ~A, pos: ~A~%" actual-subterm
						;(keim~get actual-subterm :term-position))
					;(omega~message "appl: ~A~%" applicant)
		    (if (and actual-unifier
			     (not solution-found)
			     (keim~equal actual-subterm (spass=subst-apply actual-unifier actual-subterm)))
			(let* ((new-applicant (lit~literal-create
					       (data~replace-at-position
						(spass=subst-apply actual-unifier applicant)
						actual-subterm-position
						(spass=subst-apply actual-unifier tr))
					       (lit~positive-p lit-applicant)))
			       (new-clause (cl~create (append (cl~literals (data~replace-at-position
						   (spass=subst-apply actual-unifier
								      rew-clause)
						   rew-pos
						   new-applicant))
					   (remove (spass=subst-apply actual-unifier lit-equation) (spass=subst-apply actual-unifier (cl~literals help-clause)) :count 1 :test 'keim~equal :start help-p))))
                               (mother rew-clause)
			       (mother-position (pos~add-front rew-p
							       (pos~add-front
								1 actual-subterm-position)))
			       (mother-renaming (subst~create nil nil))
			       (father old-help-clause)
			       (father-position (pos~list-position (list help-p)))
			       (father-renaming renaming2)
			       (direction 'LR)
			       (unifier actual-unifier)
			       (name nil)
                               (justification (res~paramod-create
					       mother mother-position
					       mother-renaming father
					       father-position
					       father-renaming
					       direction
					       unifier
					       name)))
			  (keim~put new-applicant
				    :splitting-branch
				    (keim~get lit-applicant :splitting-branch))
                          ;(setf (lit~clause new-applicant) (lit~clause lit-applicant))
			  (setf (node~justification new-clause) justification)
			  (setq solution-list
				(append solution-list (list (list new-clause))))
			   (spass=translate-rew-justification-many-parents-aux
			   (rest rew-clauses)
			   (rest help-clauses)
                           spass-clause
			   splitting-level
			   :round (+ 1 round))))
		    (if (and actual-unifier2
			     (not solution-found)
			     (keim~equal actual-subterm (spass=subst-apply actual-unifier2 actual-subterm)))
			(let* ((k (setq subterm actual-subterm))
			       (k (setq appl applicant))
			       (new-applicant (lit~literal-create
					       (data~replace-at-position
						(spass=subst-apply actual-unifier2 applicant)
						actual-subterm-position
						(spass=subst-apply actual-unifier2 s))
					       (lit~positive-p lit-applicant)))
			       (new-clause (cl~create (append (cl~literals (data~replace-at-position
						   (spass=subst-apply actual-unifier2
								      rew-clause)
						   rew-pos
						   new-applicant))
					   (remove (spass=subst-apply actual-unifier2 lit-equation) (spass=subst-apply actual-unifier2 (cl~literals help-clause)) :count 1 :test 'keim~equal :start help-p))))
                               (mother rew-clause)
			       (mother-position (pos~add-front rew-p
							       (pos~add-front
								1 actual-subterm-position)))
			       (mother-renaming (subst~create nil nil))
			       (father old-help-clause)
			       (father-position (pos~list-position (list help-p)))
			       (father-renaming renaming2)
			       (direction 'RL)
			       (unifier actual-unifier2)
			       (name nil)
			       (justification (res~paramod-create
					       mother
					       mother-position
					       mother-renaming
					       father
					       father-position
					       father-renaming
					       direction
					       unifier
					       name)))
			  (keim~put new-applicant
				    :splitting-branch
				    (keim~get lit-applicant :splitting-branch))
                          ;(setf (lit~clause new-applicant) (lit~clause lit-applicant))
			  (setf (node~justification new-clause) justification)
			  (setq solution-list
				(append solution-list (list (list new-clause))))
			  (spass=translate-rew-justification-many-parents-aux
			   (spass=compute-new-rew-positions rew-clauses)
			   (rest help-clauses)
                           spass-clause
			   splitting-level
			   :round (+ 1 round)))))))))))

(defun spass=compute-new-rew-positions (rew-clauses)
  (let* ((actual-position (second (first rew-clauses))))
    (do* ((rest-rew-clauses (rest rew-clauses) (rest rest-rew-clauses))
	  (pair (first rest-rew-clauses) (first rest-rew-clauses))
	  (new-rew-clauses (if pair (list (list (first pair) (if (equal (second pair) actual-position) 0 (+ 1 (second pair))))))
			   (if pair (append new-rew-clauses (list (list (first pair) (if (equal (second pair) actual-position) 0 (+ 1 (second pair)))))) new-rew-clauses)))
	((null rest-rew-clauses) new-rew-clauses))))
				 

(defun spass=extract-parents-for-rew (parents) ; achtung: annahme, das parent-pos aufsteigen...wie bei clr ... diese annahme war leiders falsch .. wir machen aber erstmal easy und nehmen an, es waere umgekehrt.
  (declare (edited  "21-AUG-1997")
	   (authors Naumann)
	   (input   "The parents of a rew-step.")
	   (effect  "None.")
	   (value   "Two lists: First a list of all the rewritten-clause-positions, second a list of all correntponding help-clauses."))
  (do* ((rest-parents parents (rest (rest rest-parents)))
        (help-clause (first rest-parents) (first rest-parents))
	(rew-clause (second rest-parents) (second rest-parents))
	(rew-clause-list (list rew-clause) (cons rew-clause rew-clause-list))
	(help-clause-list (list help-clause) (cons help-clause help-clause-list)))
      ((equal (length rest-parents) 2) (if (< (first (last (first rew-clause-list))) (first (last (first (reverse rew-clause-list)))))
					   (list (reverse rew-clause-list) (reverse help-clause-list))
					 (list rew-clause-list help-clause-list)))))
	
(defun spass=make-list (list spass-clause splitting-level)
  (declare (edited  "22-AUG-1997")
	   (authors Naumann)
	   (input   "A list of lists.")
	   (effect  "None.")
	   (value   "A list with the members of the list on top-level."))
  (if (null list) nil
    (do* ((rest-list list (rest rest-list))
	  (list1 (first rest-list) (first rest-list))
	  (solution list1 (append solution list1)))
	((null rest-list) (spass=expand-paramodulation solution spass-clause splitting-level)))))

(defun spass=expand-paramodulation (list spass-clause splitting-level)
  (declare (edited  "24-SEP-1997")
	   (authors Naumann)
	   (input   "A list of correct justified clauses.")
	   (effect  "None.")
	   (value   "If splitting-allowed is not zero, the paramodulation-steps will be
expanded to resolution-steps."))
  (if (zerop spass*allowed-splittings) list
    (do* ((rest-list list (rest rest-list))
	  (clause (first rest-list) (first rest-list))
	  (new-list (if (res~paramodulation-p (node~justification clause))
			(spass=translate-paramodulation-step clause spass-clause
							     splitting-level)
		      (list clause))
		    (if (res~paramodulation-p (node~justification clause))
			(append new-list (spass=translate-paramodulation-step clause spass-clause
							     splitting-level))
		      (append new-list (list clause)))))
	((equal (length rest-list) 1) new-list))))
		    
		    

(defun spass=reduce-factorings (keim-clause spass-clause splitting-level)
  (declare (edited  "21-AUG-1997")
	   (authors Naumann)
	   (input   "A KEIM-clause, a SPASS-clause (in KEIM-format and the splitting-level.")
	   (effect  "None.")
	   (value   "nil, if the spass-clause can't achieved, otherwise a list of correct justfied steps."))
  (setq cl keim-clause)
  (if (cl~empty-p keim-clause)
      (list keim-clause)
    (progn
  (keim~put keim-clause :splitting-level splitting-level)
  ;(omega~message "~A~%" (keim~plist keim-clause))
  (setq solution-fact-list (list keim-clause))
  (setq solution-factoring nil)
  (spass=reduce-factorings-aux keim-clause spass-clause splitting-level (- (length
									    (cl~literals
									     keim-clause)) 1))
  solution-fact-list)))
  

(defun spass=reduce-factorings-aux (rest-clause spass-clause splitting-level position)
  ;(omega~message "sol-list before: ~A~%" solution-fact-list)
  (if (zerop position)
      (if (spass=compute-positions-keim-spass-clause (first (last solution-fact-list)) spass-clause splitting-level :obvious nil)
	  (setq solution-factoring t)
		(setq solution-fact-list (reverse (rest (reverse solution-fact-list)))))
    (do* ((factors (append (spass=get-first-factors rest-clause position splitting-level) (list rest-clause))
		   (rest factors))
	  (factor (first factors) (first factors)))
	((or (null factors) solution-factoring) solution-factoring)
      ;(omega~message "sol-list: ~A~%" solution-fact-list)
      ;(omega~message "factor: ~A~%" factor)
      (if (and (not solution-factoring)
	       (not (keim~equal rest-clause factor)))
	  (setq solution-fact-list (append solution-fact-list (list factor))))
      (spass=reduce-factorings-aux factor
				   spass-clause
				   splitting-level
				   (if (keim~equal factor rest-clause) (- position 1) (- position 1))))))

(defun spass=get-first-factors (clause pos1 splitting-level)
  (if (cl~empty-p clause) nil
  (do* ((literal1 (nth pos1 (cl~literals clause)) (nth pos1 (cl~literals clause)))
	;(k (omega~message "lit1: ~A~%" literal1)
	;   (omega~message "lit1: ~A~%" literal1))
	(pos2  0 (+ 1 pos2))
	(literal2 (nth pos2 (cl~literals clause)) (nth pos2 (cl~literals clause)))
	(unifier (if literal2 (term~unify literal1 literal2)) (if literal2 (term~unify literal1 literal2)))
	(fact-allowed (and unifier
			   (keim~equal (subst~apply unifier literal1)
				       (subst~apply unifier literal2))
			   (not (or (first (last (first (reverse (keim~get literal1 :splitting-branch)))))
				    (first (last (first (reverse (keim~get literal1 :splitting-branch))))))))
		      (and unifier
			   (keim~equal (subst~apply unifier literal1)
				       (subst~apply unifier literal2))
			   (not (or (first (last (first (reverse (keim~get literal1 :splitting-branch)))))
				    (first (last (first (reverse (keim~get literal1 :splitting-branch)))))))))
	(factorand (if fact-allowed (spass=apply-factoring-step clause
								(pos~list-position (list pos2))
								(pos~list-position (list pos1))
								splitting-level))
		   (if fact-allowed (spass=apply-factoring-step clause
								(pos~list-position (list pos2))
								(pos~list-position (list pos1))
								splitting-level)))
	(factor-list (if factorand (list factorand))
		     (if factorand (cons factorand factor-list)
		       factor-list)))
      ((equal pos2 (- pos1 1)) factor-list)
					;(omega~message "clause: ~A~%l1: ~A, l2: ~A, p1: ~A, p2: ~A~%unif: ~A, allowd: ~A, factorand: ~A~%"
    ;clause literal1 literal2 pos1 pos2 unifier fact-allowed factorand)
  )))


;-------handling ClR-justification-------------------------

(defun spass=translate-clr-justification (spass-clause number parents splitting-level)
  (declare (edited  "05-AUG-1997")
	   (authors Naumann)
	   (input   "The actual spass-clause, the actual number and the parents of the step.")
	   (effect  "The changed proof-steps of spass*current-resolution-problem.")
	   (value   "Undefined."))
  (if (> (length parents) 2)
      (spass=translate-clr-justification-many-parents spass-clause number parents splitting-level)
  (let* ((parent-number1 (first (first parents)))
	 (parent-number2 (first (second parents)))
	 (parent1 (spass=find-keim-clause parent-number1))
	 (parent2 (spass=find-keim-clause parent-number2))
	 (pos1 (spass=compute-new-position parent1 (second (first parents))))
	 (pos2 (spass=compute-new-position parent2 (second (second parents))))
	 (parent1 (spass=find-keim-clause parent-number1))
	 (parent2 (spass=find-keim-clause parent-number2))
	 (lit-list1 (cl~literals parent1))
	 (lit-list2 (cl~literals parent2))
	 (test (>= (length (cl~literals (spass=reduce-clause-to-splitting-clause (cl~create lit-list1))))
		   (length (cl~literals (spass=reduce-clause-to-splitting-clause (cl~create lit-list2))))))
         ;(k (omega~message "test: ~A~%" test))
	 (rew-clause (if test parent1 parent2))
	 (help-clause (if test parent2 parent1))
	 (rew-literals (cl~literals rew-clause))
	 (help-literals (cl~literals help-clause))
	 (rew-pos (pos~list-position (list (if test pos1 pos2))))
	 (help-pos (pos~list-position (list (if test pos2 pos1))))
	 ;(k (omega~message "help-clause: ~A~%rew-clause: ~A~%" help-clause rew-clause))
	 (resolvent-list (spass=apply-resolution-step help-clause rew-clause help-pos rew-pos spass-clause splitting-level))
					;(unifier (res~just-unifier (node~justification resolvent)))
	 (rest-rew-literals (remove (data~struct-at-position rew-clause rew-pos) rew-literals :count 1 :test 'keim~equal :start (pos~first rew-pos)))
	 (rest-help-literals (remove (data~struct-at-position help-clause help-pos) help-literals :count 1 :test 'keim~equal :start (pos~first help-pos))))
					;(omega~message "bis lort.")
    (setf (res~proof-clauses spass*current-resolution-problem) (append (res~proof-clauses spass*current-resolution-problem) resolvent-list (rest (spass=reduce-factorings (first (last resolvent-list)) spass-clause splitting-level))))
    (multiple-value-bind
	(new-positions eq-flip-positions obvious-reductions)
	(spass=compute-positions-keim-spass-clause (first (last (res~proof-clauses spass*current-resolution-problem))) spass-clause splitting-level :obvious nil)
      (spass=always-needed-steps (first (last (res~proof-clauses spass*current-resolution-problem))) spass-clause number new-positions eq-flip-positions obvious-reductions splitting-level)
      (keim~put (first (last (res~proof-clauses spass*current-resolution-problem))) :spass-clause-number (list number))))))
    

(defun spass=translate-clr-justification-many-parents (spass-clause number parents splitting-level)
  (let* ((rew-clauses-old (member (nth (/ (length parents) 2) (reverse parents)) (reverse parents)))
         (help-clauses-old (reverse (member (nth (/ (length parents) 2) parents)
					    parents)))
	 (new-parents (spass=get-clr-parents rew-clauses-old help-clauses-old)))
    ;(omega~message "~%rc: ~A, hc: ~A~%" rew-clauses help-clauses)
    (omega~message "Clause Reduction with many parents - maybe take a while ... ")
    (setq solution-list (list (spass=find-keim-clause (first (first parents)))))
    (spass=translate-clr-justification-many-parents-aux new-parents spass-clause splitting-level)
    (multiple-value-bind
	(new-positions eq-flip-positions obvious-reductions)
	(spass=compute-positions-keim-spass-clause (first (last (res~proof-clauses spass*current-resolution-problem))) spass-clause splitting-level :obvious nil)
      (spass=always-needed-steps (first (last (res~proof-clauses spass*current-resolution-problem))) spass-clause number new-positions eq-flip-positions obvious-reductions splitting-level))
    (omega~message "solved.~%")
    (keim~put (first (last (res~proof-clauses spass*current-resolution-problem))) :spass-clause-number (list number))))


(defun spass=translate-clr-justification-many-parents-aux (rest-parents spass-clause splitting-level)
  (cond ((null rest-parents)
	 ;(omega~message "null rew-clauses~%")
	 (setf (res~proof-clauses spass*current-resolution-problem)
	       (append (res~proof-clauses spass*current-resolution-problem)
		       solution-list
		       (rest (spass=reduce-factorings (first (last solution-list)) spass-clause splitting-level)))))
	(t (let* ((rew-clause (first (last solution-list)))
		  (help-clause (spass=find-keim-clause (first (second (first rest-parents)))))
		  (rew-position (pos~list-position (list (first (first rest-parents)))))
		  (help-position (pos~list-position (list (spass=compute-new-position
							   help-clause (second (second (first rest-parents)))))))
     
		  (resolvent-list (spass=apply-resolution-step rew-clause help-clause rew-position help-position spass-clause splitting-level)))
	     (setq solution-list (append solution-list resolvent-list))
             ;(omega~message "sol-list: ~A~%" solution-list)
	     (spass=translate-clr-justification-many-parents-aux (rest rest-parents) spass-clause splitting-level)))))


(defun spass=get-clr-parents (rew-parents help-parents)
  (let* ((new-rew-parents (spass=get-clr-positions rew-parents))
	 (rew-positions (rest new-rew-parents)))
    (do* ((rest-help-parents help-parents (rest rest-help-parents))
	  (rest-rew-positions rew-positions (rest rest-rew-positions))
	  (new-parents (list (list (first rest-rew-positions)
				   (first rest-help-parents)))
		       (append new-parents (list (list (first rest-rew-positions)
						 (first rest-help-parents))))))
	((equal (length rest-help-parents) 1) (spass=sort-clr-parents new-parents)))))

(defun spass=sort-clr-parents (new-parents)
  (do* ((rest-list new-parents (remove new-element rest-list :test 'eq)) 
	(new-element (spass=search-maximum rest-list)
		     (spass=search-maximum rest-list))
	(new-list (list new-element) (append new-list (list new-element))))
      ((equal (length rest-list) 1) new-list)))

(defun spass=search-maximum (rest-list)
  (do* ((rest-rest-list rest-list (rest rest-rest-list))
	(element (first (first rest-rest-list))
		 (first (first rest-rest-list)))
	(max-element (first rest-rest-list)
		     (if (> element (first max-element))
			 (first rest-rest-list)
		       max-element))
	)
      ((equal (length rest-rest-list) 1) max-element)))
		       

(defun spass=get-clr-positions (parents)
  (declare (edited  "25-SEP-1997")
	   (authors Naumann)
	   (input   "The parents of the rewritten clause.")
	   (effect  "None.")
	   (value   "A list: first the spass-clause-number, then the new positions."))
  (let* ((rew-clause (spass=find-keim-clause (first (first parents)))))
    (do* ((rest-parents parents (rest rest-parents))
	  (pair (first rest-parents) (first rest-parents))
	  (new-parents (list (first pair) (spass=compute-new-position rew-clause (second
										  pair)))
		       (append new-parents (list (spass=compute-new-position rew-clause
										  (second
										   pair))))))
	((equal (length rest-parents) 1) new-parents))))


;-------handling SSi - translation------------------------
; Annahme: Reihenfolge der Parents stimmt!---------------

(defun spass=translate-ssi-justification (spass-clause number parents splitting-level)
  (declare (edited  "07-AUG-1997")
	   (authors Naumann)
	   (input   "The actual spass-clause, the actual number and the parents of the step.")
	   (effect  "The changed proof-steps of spass*current-resolution-problem.")
	   (value   "Undefined."))
    (let* ((clause-list (spass=get-ssi-parents parents)))
    (omega~message "Sort-simplification-step - may take a while ... ")
    (setq solution-list nil)
    (setq solution-found nil)
    (spass=translate-ssi-justification-aux clause-list spass-clause number parents splitting-level)
    (multiple-value-bind
	(new-positions eq-flip-positions obvious-reductions)
	(spass=compute-positions-keim-spass-clause (first (last (res~proof-clauses spass*current-resolution-problem))) spass-clause splitting-level :obvious nil)
      (spass=always-needed-steps (first (last (res~proof-clauses spass*current-resolution-problem))) spass-clause number new-positions eq-flip-positions obvious-reductions splitting-level)
      (omega~message "solved.~%")
      (keim~put (first (last (res~proof-clauses spass*current-resolution-problem))) :spass-clause-number (list number)))))

(defun spass=translate-ssi-justification-aux (clause-list spass-clause number parents splitting-level)
  (declare (edited  "17-SEP-1997")
	   (authors Naumann)
	   (input   )
	   (effect  )
	   (value   ))
  (cond ((< (length clause-list) 2)
	 (let* ((factors (spass=reduce-factorings (first (last (first (reverse solution-list)))) spass-clause splitting-level)))
	   (if factors
	       (progn 
		 (setf (res~proof-clauses spass*current-resolution-problem) (append
									     (res~proof-clauses spass*current-resolution-problem) (spass=make-list solution-list spass-clause splitting-level) (rest factors)))
		 (setq solution-found t)
		 ;(omega~message "sol-found: ~A~%" solution-found)
		 ;(omega~message "sol-list: ~A~%" solution-list)
		 ;(omega~message "act-clause: ~A~%" (first (last (res~proof-clauses spass*current-resolution-problem))))
		 )
	     (setq solution-list (reverse (rest (reverse solution-list)))))))
	(t (let* ((factors (if solution-list
			       (spass=reduce-factorings (first (last (first (reverse solution-list))))
							spass-clause
							splitting-level)))
		  (resolvents (append (res~binary-resolution (first clause-list) (second clause-list))
				      (list (first clause-list)))))
	     ;(omega~message "Resolvents: ~A~%" resolvents)
	     (if factors (spass=translate-ssi-justification-aux nil spass-clause number parents splitting-level)
	       (do* ((rest-resolvents resolvents (rest rest-resolvents))
		     (resolvent-list (if (> (length rest-resolvents) 1)
					 (spass=remake-resolution
					  (first rest-resolvents)
					  spass-clause
					  splitting-level)
				       (list (first rest-resolvents)))
				     (if (> (length rest-resolvents) 1)
					 (spass=remake-resolution
					  (first rest-resolvents)
					  spass-clause
					  splitting-level)
				       (list (first rest-resolvents)))))
		   ((or solution-found (null rest-resolvents)))
		 (setq solution-list (append solution-list (list (append resolvent-list (rest (spass=translate-obvious-reductions (list (first (last resolvent-list))) spass-clause splitting-level))))))
		 (spass=translate-ssi-justification-aux (cons (first (last (first (reverse solution-list)))) (rest (rest clause-list))) spass-clause number parents splitting-level)))))))


(defun spass=get-ssi-parents (parents)
  (declare (edited  "17-SEP-1997")
	   (authors Naumann)
	   (input   "The parents of the ssi-step.")
	   (effect  "None.")
	   (value   "A list of the parents of the ssi-step, with the main-clause only once."))
  (let* ((father-number (first (first parents)))
	 (father (spass=find-keim-clause father-number))
	 (clause-list (list father)))
    (do* ((rest-parents parents (rest rest-parents))
	  (clause-number (first (first parents)) (first (first rest-parents)))
	  (clauses-list clause-list (if (/= father-number clause-number)
					(append clauses-list (list (spass=find-keim-clause clause-number)))
				      clauses-list)))
	((equal (length rest-parents) 1) clauses-list))))


(defun spass=remake-resolution (resolvent spass-clause splitting-level)
  (declare (edited  "17-SEP-1997")
	   (authors Naumann)
	   (input   "A from KEIM obtained resolvent.")
	   (effect  "None.")
	   (value   "The resolvent-list in translation-structures."))
  (if resolvent
  (let* ((justification (node~justification resolvent))
	 (parent1 (first (res~resolution-clauses justification)))
	 (parent2 (second (res~resolution-clauses justification)))
	 (pos1 (first (res~resolution-positions justification)))
	 (pos2 (second (res~resolution-positions justification)))
	 )
    (spass=apply-resolution-step parent1 parent2 pos1 pos2 spass-clause splitting-level)))
	) 
	 
;------ various translation-support-functions -------------

(defun spass=find-keim-clause (spass-clause-number)
  (declare (edited  "29-JUL-1997")
	   (authors Naumann)
	   (input   "A spass-clause-number.")
	   (effect  "None.")
	   (value   "Returns the corresponding keim-clause, if computed, otherwise nil."))
  (let* ((clause-list (res~proof-clauses spass*current-resolution-problem)))
     (do* ((rest-search-list clause-list (rest rest-search-list)))
	((or (null rest-search-list) (member spass-clause-number (keim~get (first rest-search-list) :spass-clause-number)))
	 (if (null rest-search-list) (progn (omega~message "parent not found (number: ~A)!~%" spass-clause-number)
					    nil)
	   (first rest-search-list))))))

; unnoetig, steckt in spass=compute-new-position

(defun spass=find-spass-literal (clause lit-number)
  (declare (edited  "19-SEP-1997")
	   (authors Naumann)
	   (input   "A number and a KEIM-clause.")
	   (effect  "None.")
	   (value   "The literal at the SPASS-literal-position."))
  (do* ((rest-literals (cl~literals clause) (rest rst-literals))
	(literal (first rest-literals) (first rest-literals)))
      ((or (null literal)
	   (equal (first (reverse (keim~get literal :spass-literal-position))) lit-number))
       (if literal literal (omega~message "SPASS-literal not found.")))))


(defun spass=test-subterm-positions (term subterm-list)
  (do* ((rest-subterms subterm-list (rest rest-subterms))
	(k (omega~message "subterm: ~A, pos: ~A~%" (first rest-subterms) (keim~get (first rest-subterms) :term-position))
	   (omega~message "subterm: ~A, pos: ~A~%" (first rest-subterms) (keim~get (first rest-subterms) :term-position)))
	(position (data~replace-at-position term (keim~get (first rest-subterms) :term-position) (first rest-subterms))
		  (data~replace-at-position term (keim~get (first rest-subterms) :term-position) (first rest-subterms))))
      ((equal (length rest-subterms) 1) (omega~message "subterm-positions ok~%"))))

(defun spass=get-subterms (term)
  (declare (edited  "29-JUL-1997")
	   (authors Naumann)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "A list containing all subterms of term of the left side of the equation is returned, also the positions of the subterms are stored in 'term-position'."))
  (setq subterm-list nil)
  (setq source-term term)
  (spass=get-subterms-aux term)
  ;(spass=test-subterm-positions term subterm-list)
  (remove-duplicates subterm-list :test 'keim~equal))

(defgeneric spass=get-subterms-aux (term)
  (declare (edited  "29-JUL-1997")
	   (authors Naumann)
	   (input   "A term.")
	   (effect  "subterm-list is changed and returned.")
	   (value   "Undefined."))
  (:method ((const term+constant))
	   (setq subterm-list (append subterm-list
				      (list (list const (first (data~substruct-positions
								const source-term)))))))
					;statt first ggf. alle nehmen
  (:method ((var term+variable))
           (setq subterm-list (append subterm-list
				      (list (list var (first (data~substruct-positions
							      var source-term)))))))
  (:method ((appl term+appl))
	   (setq subterm-list (append subterm-list
				      (list (list appl (first (data~substruct-positions appl source-term))))))
	   (mapc #'(lambda (argument)
		     (spass=get-subterms-aux argument))
		 (data~appl-arguments appl))))

  
(defgeneric spass=equality-in-literal-p (literal)
  (declare (edited  "09-JUL-1997")
	   (authors Naumann)
	   (input   "A literal.")
	   (effect  "None.")
	   (value   "t, if literals contains equality, otherwise nil."))
  (:method ((literal lit+literal))
	   (spass=equality-in-literal-p (lit~atom literal)))
  (:method ((term term+appl))
	   (cond ((equal (data~appl-function term) '=) t)
		 (t (mapc #'(lambda (argument)
			      (spass=equality-in-literal-p argument))
		          (data~appl-arguments term)))))
  (:method ((var term+variable))
	   nil)
  (:method ((const term+constant))
	   nil))

; das nachfolgende wird wegen Unhandlichkeit nicht verwendet
;(defun spass=read-obvious-reduction-steps ()
;  (declare (edited  "28-JUN-1997")
;	   (authors Naumann)
;	   (input   "Nothing.")
;	   (effect  "Changes spass*obvious-reductions to the obvious reductions steps.")
;	   (value   "Undefinded."))
;  (mapc #'(lambda (proof-line)
;	    (if (spass=string-suffix "ObviousReduction:" proof-line)
;		(setq spass*obvious-reductions (append spass*obvious-reductions (list (spass=get-number (spass=extract-;number proof-line)))))))
;	spass*proof-file-lines))


(defun spass=always-needed-steps (clause spass-clause number new-positions eq-flip-positions obvious-reductions splitting-level &key (input nil))
  (declare (edited  "01-AUG-1997")
	   (authors Naumann)
	   (input   "A clause, the actual spass-number and the eq-flip-positions.")
	   (effect  "The proof-steps of spass*current-resolution-problem are expanded.")
	   (value   "Undefinded."))
  ;(omega~message "obv-red: ~A~%" obvious-reductions)
  (if obvious-reductions
      (progn				;(omega~message "obvred in always: ~A~%" obvious-reductions)
	(setf (res~proof-clauses spass*current-resolution-problem) (append (res~proof-clauses spass*current-resolution-problem) obvious-reductions))
	(setq clause (first (last (res~proof-clauses spass*current-resolution-problem))))))
					;(omega~message "nach obvious.~%")
  (if eq-flip-positions
      (progn (spass=translate-fliped-equations clause (reverse eq-flip-positions) spass-clause
					       splitting-level input)
	     (setq clause (first (last (res~proof-clauses spass*current-resolution-problem))))))
					; achtung, flippen aendert die lit-reihenfolge, die np sind fuern arsch!
					;(omega~message "splitting-level: ~A~%" splitting-level)
					;(omega~message "clause: ~A~%" clause)
  (multiple-value-bind
      (new-positions eq-flips2 obv-red2)
      (spass=compute-positions-keim-spass-clause clause spass-clause splitting-level :obvious nil) ; hier muss :obvious nil hin auch ohne den expliziten obvious-schritt
					;(omega~message "nach flip.~%")
					;(omega~message "np: ~A~%clause: ~A~%" new-positions clause)
    (if (not (pos~empty-p (first (first new-positions))))
	(let* ((actual-clause (if (and input (not (or eq-flip-positions obvious-reductions)))
				  (nth (- number 1) (res~proof-initial-clauses spass*current-resolution-problem))
				clause))
	       )
       ; ab hier neu
	  (do* ((rest-literals (cl~literals actual-clause) (rest rest-literals))
	        (literal (first rest-literals) (first rest-literals))
		;(k (omega~message "lit: ~A~%plist: ~A~%" literal (keim~plist literal))
		;   (omega~message "lit: ~A~%plist: ~A~%" literal (keim~plist literal)))
		   
		(right-branch (first (last (first (reverse (keim~get literal :splitting-branch)))))
			      (if literal (first (last (first (reverse (keim~get literal :splitting-branch)))))))
		(rest-new-positions new-positions
				    (if (not prev-right-branch) (rest rest-new-positions)
				      rest-new-positions))
		;(k (omega~message "r-n-p: ~A~%" rest-new-positions)
		;   (omega~message "r-n-p: ~A~%" rest-new-positions))
		(prev-right-branch right-branch right-branch)
		(spass-position (if (not right-branch) (first (pos~number-list (second (first rest-new-positions)))))
				(if (not right-branch) (first (pos~number-list (second (first rest-new-positions))))))
		(dummy (if (not right-branch) (keim~put literal :spass-literal-position (append (keim~get literal :spass-literal-position) (list spass-position))))
		       (if (not right-branch) (keim~put literal :spass-literal-position (append (keim~get literal :spass-literal-position) (list spass-position))))))
	      ((equal (length rest-literals) 1)))
       ; bis hier neu
	  ))))
    
;  (setf (res~proof-clauses spass*current-resolution-problem) (append (res~proof-clauses spass*current-resolution-prob;lem) (rest (spass=translate-obvious-reductions (first (last (res~proof-clauses spass*current-resolution-problem))) spass-clause splitting-level))))))

; der letzte schritt der oberen Fkt. schmiss insbesondere die doppelten raus, die beim splitting entstehen.

(defun spass=compute-new-positions-change (new-positions clause)
  (declare (edited  "24-AUG-1997")
	   (authors Naumann)
	   (input   "The new-positions and a clause.")
	   (effect  "None.")
	   (value   "Computes the new-new-positions in respect to the splitting-case."))
  (do* ((count 0 (+ 1 count))
	(actual-pos (pos~list-position (list count)) (pos~list-position (list count)))
	(rest-literals (cl~literals clause) (rest rest-literals))
	(literal (first rest-literals) (first rest-literals))
	(right-branch (first (last (first (reverse (keim~get literal :splitting-branch)))))
		      (if literal (first (last (first (reverse (keim~get literal :splitting-branch)))))))
	(rest-positions new-positions (if (not prev-right-branch) (rest rest-positions) rest-positions))
        (prev-right-branch right-branch right-branch)
	(pair (first rest-positions) (first rest-positions))
	;(k (omega~message "pair: ~A~%" pair) (omega~message "pair: ~A~%" pair))
	(new-pos1 (pos~list-position (list (spass=compute-new-position clause (pos~first (first pair)))))
		  (if pair (pos~list-position (list (spass=compute-new-position clause (pos~first (first pair)))))))
	(new-pos2 (pos~list-position (list (spass=compute-new-position clause (pos~first (second pair)))))
		  (if pair (pos~list-position (list (spass=compute-new-position clause (pos~first (second pair)))))))
        ;(k (omega~message "lit: ~A, rb: ~A~%" literal right-branch) (omega~message "lit: ~A, rb: ~A~%" literal right-branch))
        ;(k (omega~message "rest-pos: ~A~%" rest-positions) (omega~message "rest-pos: ~A~%" rest-positions))
	;(k (omega~message "pair: ~A~%" pair) (omega~message "pair: ~A~%" pair))
	;(k (omega~message "np1: ~A, np2: ~A~%" new-pos1 new-pos2)
	 ;  (omega~message "np1: ~A, np2: ~A~%" new-pos1 new-pos2))
        
	(new-position-list (if right-branch (list (list actual-pos actual-pos))
			     (list (list new-pos1 new-pos2)))
			   (if right-branch (append new-position-list (list (list actual-pos actual-pos)))
			     (if literal (append new-position-list (list (list new-pos1 new-pos2)))
			       new-position-list))))
      ((null rest-literals) new-position-list)))


(defun spass=check-equality-of-clauses-p (keim-clause spass-clause keim-clause-number number-list splitting-level)
  (declare (edited  "02-AUG-1997")
	   (authors Naumann)
	   (input   "The produced reduced keim-clause and the corresponding spass-clause.")
	   (effect  "None.")
	   (value   "t if the clauses are (e.g. of variable-renaming) equal, otherwise nil."))
  
  (if (member keim-clause-number number-list)
      (multiple-value-bind
	  (new-positions eq-flip-positions obvious-reductions)
	  (spass=compute-positions-keim-spass-clause keim-clause spass-clause splitting-level :obvious nil)
	;(omega~message "np: ~A, ep: ~A, or: ~A~%" new-positions eq-flip-positions obvious-reductions)
	(if (or (not new-positions) eq-flip-positions obvious-reductions) nil
	  (progn (setq sol t)
		 (mapc #'(lambda (pair literal)
			   (if (not (equal (first (pos~number-list (second pair)))
					   (first (reverse (keim~get literal :spass-literal-position)))))
			      
				      (setq sol nil)))
		       new-positions (cl~literals keim-clause))
		 sol)))
    nil))


  
;----- reading and transforming a spass-clause to keim-structures -----

(defun spass=spass-clause-to-keim-clause (clause-string)
  (declare (edited  "02-JUL-1997")
	   (authors Naumann)
	   (input   "A pure clause-string.")
	   (effect  "None.")
	   (value   "A clause with the semantik of the spass-clause string."))

  (setf spass*local-clause-vars nil)
  
  (if (equal (char clause-string 0) #\Space) ; fuehrende spaces rausschmeissen
      (spass=spass-clause-to-keim-clause (spass=string-suffix " " clause-string))
    (progn (setq pos-flag nil)
	   (setq end-flag nil)
         (let* ((rest-clause-string (spass=remove-chars clause-string '(#\* #\+ #\|))))
            (do* ((rest-string rest-clause-string (cond ((equal (char rest-string 0) #\Space)
				          		 (spass=string-suffix " " rest-string))
						        ((member (char rest-string 0) '(#\- #\>))
						         (progn (setq pos-flag t)
							        (spass=string-suffix (string (char rest-string 0)) rest-string)))
							((equal (char rest-string 0) #\.)
							 "$")
							(t (spass=string-suffix (spass=read-word rest-string '(#\Space #\.)) rest-string))))
		  (word (if (not (member (char rest-string 0) '(#\Space #\- #\> #\. #\$)))
								     (spass=read-word rest-string '(#\Space #\.))
								   nil)
		        (if (not (member (char rest-string 0) '(#\Space #\- #\> #\. #\$)))
								     (spass=read-word rest-string '(#\Space #\.))
								   nil))
		  (literal-list (if word (list (spass=spass-lit-to-keim-lit word)) nil)
				(if word (append literal-list (list (spass=spass-lit-to-keim-lit word)))
				  literal-list)))
		((string= rest-string "$")
		 (cl~create literal-list)))))))


(defun spass=spass-lit-to-keim-lit (literal-string)
  (declare (edited  "02-JUL-1997")
	   (authors Naumann)
	   (input   "A pure literal-string.")
	   (effect  "None.")
	   (value   "A literal with the semantik of the spass-literal (and changed variables)."))
 (lit~literal-create (spass=spass-term-to-keim-term literal-string) pos-flag))

(defun spass=spass-term-to-keim-term (term-string &key (type nil))
  (declare (edited  "25-OCT-1999")
	   (authors Ameier)
	   (input   "A term string.")
	   (effect  "None.")
	   (value   "The correposning keim-term."))
  
  (multiple-value-bind
      (functor-string rest-string)
      (atptop~get-next-word term-string #\()
    ;; reads till a "(" is reached 
    ;; if rest-string = "" (empty) -> term isn't an application -> otherwise term is an application
    (if (string= rest-string "")
	(let* ((obj (spass=old-keim-type functor-string (append spass*renamed-objects spass*local-clause-vars))))
	  (if obj
	      obj
	    (let ((new-var (term~variable-create (gensym "x-") type)))
	      (setf spass*local-clause-vars (append spass*local-clause-vars (list (list functor-string new-var))))
	      new-var)))
      (let* ((functor (spass=old-keim-type functor-string (append spass*renamed-objects spass*local-clause-vars)))
	     (args (mapcar #'(lambda (term-string awaiting-type)
			       (spass=spass-term-to-keim-term term-string :type awaiting-type))
			   (spass=parse-term-list (atptop~cut-last-char rest-string))
			   (data~n-domain (term~type (if (term~schema-p functor)
							 (data~schema-range functor)
						       functor))))))
	
	(term~appl-create functor args
			  )))))


(defun spass=parse-term-list (term-list-string)
  (declare (edited  "17-MAY-1996")
	   (authors Ameier)
	   (input   "A term-list-sting, this is something like s(a,b,c),p(d,e,f),r,q")
	   (effect  "None.")
	   (value   "This list is divided top-level by the commata:"
		    "returns a list of strings."
		    "( \"s(a,b,c)\"  \"p(d,e,f)\" \"r" \"q")"
		    "The function does not destroy the term-structur down in the terms."))
  (do ((rest-term-list-string term-list-string)
       (current-word "")
       (return-terms nil))
      ((string= rest-term-list-string "") return-terms)
    (multiple-value-bind 	      
	(next-word rest-string)
	(atptop~get-next-word rest-term-list-string #\, :handle-break-char 'pre)
      (setq current-word (format nil "~A~A" current-word next-word))
      (setq rest-term-list-string rest-string) 
      (if (= (atptop~number-of-char-in-string #\( current-word) (atptop~number-of-char-in-string #\) current-word)) 
	  (progn
	    (if (string= rest-string "")
		(setq return-terms (append return-terms (list current-word)))
	      (setq return-terms (append return-terms (list (atptop~cut-last-char current-word))))) 
	    (setq current-word "")))))) 

;-----------Obtaining a position-list for changed literal-ordering (input-clauses) and fliped equalities

(defun spass=compute-positions-keim-spass-clause (keim-clause clause2 splitting-level &key (eq-flip t) (obvious t))
  (declare (edited  "10-JUL-1997")
	   (authors Naumann)
	   (input   "Two clauses.")
	   (effect  "None.")
	   (value   "A multiple value: A list of list: (pos-in-clause1 pos-in-clause2) and a list of list of this pairs, where the equality-arguments have fliped. be careful: Maybe that clause one will be reduced in respect to the splitting-case."))
  (setq new-positions nil) ; the list of the new-position-pairs (new are front-added)
  (setq renaming-list nil) ; the list of subsitution (new are front-added)
  (setq equality-flip nil) ; the list of position-pairs, where the equality-arguments have changed.
  (setq obvious-reductions nil) ; t if there was found an obvious reduction
  (setq solution-found nil) ; the test-flag, if a solution is found
  ;(omega~message "clause before reduce: ~A~%" keim-clause)
  (setq clause1 (spass=reduce-clause-to-splitting-clause keim-clause))
  ;(omega~message "clause after reduce: ~A~%" clause1)
  (let* ((clause-list (if obvious (rest (spass=translate-obvious-reductions keim-clause spass-clause splitting-level)))))
    (if (and clause-list obvious)
	(progn				;(omega~message "after obvred.~%")
	  (setq obvious-reductions clause-list)
	  (setq clause1 (spass=reduce-clause-to-splitting-clause (first (last clause-list))))))
    (if (and (null (cl~literals clause1)) (null (cl~literals clause2)))
        (values (list (list (pos~empty) (pos~empty))) nil (if obvious clause-list))
      (if (not (eql (length (cl~literals clause1)) (length (cl~literals clause2))))
          nil
	(let* ((new-clause2 (spass=extend-equality-clause (first (multiple-value-list (res~separate-clauses clause1 clause2)))))
					;(first (multiple-value-list (res~separate-clauses clause1 clause2)))))
	       (new-clause1 (spass=extend-equality-clause clause1) ;clause1
			    ))
					;(mapc #'(lambda (lit) (omega~message "~A~%" (keim~plist lit))) (cl~literals new-clause1))
					;(terpri)
					;(mapc #'(lambda (lit) (omega~message "~A~%" (keim~plist lit))) (cl~literals new-clause2))
	  (spass=set-positions-of-literals new-clause1)
	  (spass=set-positions-of-literals new-clause2)
	  (if (spass=test-positions-spass-keim-clause new-clause1 new-clause2)
	      (progn (setq new-positions (reverse new-positions))
		     (setq eq-counter 0)
		     (do* ((positions new-positions  (rest positions)))
			 ((null positions)
			  (values new-positions equality-flip obvious-reductions))
		       (setq pair (first positions))
		       (setq pos1 (first pair))
		       (setq pos2 (second pair))
		       (setq number1 (+ eq-counter (first (pos~number-list pos1))))
		       (setq number2 (+ eq-counter (first (pos~number-list pos2))))
		       (if (keim~get (nth number1 (cl~literals new-clause1)) :equality)
			   (progn (if (> (- number2 eq-counter) (first (pos~number-list (second (first (rest positions))))))
				      (progn (setq new-pair (list (first pair) (pos~list-position (list (- (- number2 eq-counter) 1)))))
					     (setq new-positions (substitute new-pair pair new-positions :test 'keim~equal))
					     (setq equality-flip (append equality-flip (list new-pair)))))
				  (setq eq-counter (+ 1 eq-counter))
				  (setq positions (rest positions))
				  (setq new-positions (remove (first positions) new-positions :count 1 :test 'keim~equal))))))
	    nil))))))

(defun spass=extend-equality-clause (clause)
  (declare (edited  "15-JUL-1997")
	   (authors Naumann)
	   (input   "A clause.")
	   (effect  "The changed equality-position-list.")
	   (value   "A new clause with extended equalities: l1,l2,a=b,l4 -> l1,l2,a=b,b=a,l4"))
  (setq new-literals nil)
  (let* ((literals (cl~literals clause)))
    (do* ((rest-literals literals (rest rest-literals)))
	((null rest-literals)
	 (cl~create new-literals))
      (if (and (term~appl-p (lit~atom (first rest-literals)))
	       (string= (keim~name (data~appl-function (lit~atom (first rest-literals)))) "="))
	  (progn (keim~put (first rest-literals) :equality t)
	         (setq new-literals (append new-literals (list (first rest-literals))
					    (list (lit~literal-create
						   (term~appl-create (data~appl-function (lit~atom (first rest-literals)))
								     (reverse (data~appl-arguments (lit~atom (first rest-literals)))))
						   (lit~positive-p (first rest-literals)))))))
	(setq new-literals (append new-literals (list (first rest-literals))))))))

(defun spass=test-positions-spass-keim-clause (clause1 clause2)
  (declare (edited  "10-JUL-1997")
	   (authors Naumann)
	   (input   "Two clauses.")
	   (effect  "new-position and renamings is changed.")
	   (value   "True, if the clauses are equal, except of literal-ordering an variables."))
  (if (and (null (cl~literals clause1)) (null (cl~literals clause2))) (setq solution-found t) 
   (do* ((lit-equal-list (spass=find-equal-literals (first (cl~literals clause1)) (cl~literals clause2))
			 (rest lit-equal-list))
	(literal (first (first lit-equal-list)) (first (first lit-equal-list)))
	(renaming (second (first lit-equal-list)) (second (first lit-equal-list))))
       ((or solution-found (null lit-equal-list))
	(if solution-found solution-found (setq solution-found nil)))
    (progn (setq new-positions (append (list (list (keim~get (first (cl~literals clause1)) :position)
					           (keim~get literal :position)))
				       new-positions))
	   (setq renaming-list (append (list renaming) renaming-list))
	   (let* ((rest-clause1 (rest (cl~literals clause1)))
		  (rest-clause2 (remove literal (cl~literals clause2) :test 'keim~equal :count 1))

		  (new-clause1 (cl~create (subst~apply (spass=make-substitution-from-renaming-list renaming-list) rest-clause1)))
		  (new-clause2 (cl~create (subst~apply (spass=make-substitution-from-renaming-list renaming-list) rest-clause2))))
	     (spass=copy-positions-from-clause-to-clause rest-clause1 new-clause1)
	     (spass=copy-positions-from-clause-to-clause rest-clause2 new-clause2)
	     (if (spass=test-positions-spass-keim-clause new-clause1 new-clause2)
	         (setq solution-found t)
	       (progn (setq new-positions (rest new-positions))
		      (setq renaming-list (rest renaming-list))
	              (setq solution-found nil))))))))


(defun spass=set-positions-of-literals (clause)
  (declare (edited  "10-JUL-1997")
	   (authors Naumann)
	   (input   "A clause.")
	   (effect  "The property position of the literals of clause is changed to the position of the literal ni the clause.")
	   (value   "Undefined."))
  (setq eq-before nil)
  (setq first-eq nil)
  (if (cl~empty-p clause) nil
    (do* ((pos 0 (if eq-before pos (+ 1 pos)))
          (rest-literals (cl~literals clause) (rest rest-literals)))
	((null rest-literals))
      (keim~put (first rest-literals) :position (pos~list-position (list pos)))
      (if (keim~get (first rest-literals) :equality)
	  (progn (setq first-eq t)
		 (setq eq-before nil))
	(if first-eq (progn (setq eq-before t)
			    (setq first-eq nil))
	  (progn (setq eq-before nil)
		 (setq first-eq nil)))))))

(defun spass=copy-positions-from-clause-to-clause (old-clause new-clause)
  (declare (edited  "11-JUL-1997")
	   (authors Naumann)
	   (input   "Two clauses.")
	   (effect  "Copies positions of literals from old-clause to new-clause.")
	   (value   "Undefinded."))
  (do* ((rest-old-list (cl~literals old-clause) (rest rest-old-list))
	(rest-new-list (cl~literals new-clause) (rest rest-new-list)))
      ((or (null rest-old-list) (null rest-new-list)))
    (keim~put (first rest-new-list) :position (keim~get (first rest-old-list) :position))))

(defun spass=find-equal-literals (literal literal-list)
  (declare (edited  "10-JUL-1997")
	   (authors Naumann)
	   (input   "A literal and a literal-list.")
	   (effect  "None.")
	   (value   "A list of pairs: first the literals of the literal-list, which is equal (e.g. to variables) to literal, second the corresponding renaming."))
  (do* ((lit-list literal-list (rest lit-list))
	(solution (if (not (null lit-list))
		      (if (first (multiple-value-list (spass=lit-equal-except-variables-p literal (first lit-list))))
		          (list (list (first lit-list) (second (multiple-value-list (spass=lit-equal-except-variables-p literal (first lit-list))))))
		      nil) nil)
                  (if (not (null lit-list))
		  (if (first (multiple-value-list (spass=lit-equal-except-variables-p literal (first lit-list))))
		      (append solution (list (list (first lit-list) (second (multiple-value-list (spass=lit-equal-except-variables-p literal (first lit-list)))))))
		    solution) solution)))
      ((null lit-list)
       solution)))
		      
(defun spass=lit-equal-except-variables-p (obj1 obj2)
  (declare (edited  "10-JUL-1997")
	   (authors Naumann)
	   (input   "Two literals.")
	   (effect  "None.")
	   (value   "A multiple value: True, if the literals are equal (e.g. of variables) and a renaming."))
  (cond ((or (and (lit~positive-p obj1) (lit~positive-p obj2))
	     (and (not (lit~positive-p obj1)) (not (lit~positive-p obj2))))
	 (progn (setq sol t)
		(setq renaming (subst~create nil nil))
		(if (spass=term-equal-except-variables-p (lit~atom obj1) (lit~atom obj2))
		    (values (spass=term-equal-except-variables-p (lit~atom obj1) (lit~atom obj2)) renaming)
		  (values nil nil))))
	(t nil)))

(defun spass=term-equal-except-variables-p (term1 term2)
  (declare (edited  "10-JUL-1997")
	   (authors Naumann)
	   (input   "Two terms.")
	   (effect  "sol and renaming is changed.")
	   (value   "t if the terms are equal except of variables."))
  (cond ((and (term~variable-p term1) (term~variable-p term2))
         (if (keim~equal term1 term2) sol
	   (if (and (member term1 (spass=codomain-renaming-list renaming-list) :test 'keim~equal)
		    (not (keim~equal term1 term2)))
	       (setq sol nil)
	     (if (member term2 (spass=codomain-renaming-list renaming-list) :test 'keim~equal)
	         (setq sol nil)
	       (if (member term1 (subst~domain renaming) :test 'keim~equal)
	           (if (keim~equal term2 (subst~apply renaming term1))
		       sol
		     (setq sol nil))
		 (if (member term2 (subst~codomain renaming) :test 'keim~equal)
	             (if (keim~equal term2 (subst~apply renaming term1))
		         sol
		       (setq sol nil))
		   (progn (subst~insert-component! term1 term2 renaming) ; loeschen des ! sinnvoll/noetig?
	                  sol)))))))
	((and (term~constant-p term1) (term~constant-p term2) (keim~equal term1 term2)) sol)
	((and (term~appl-p term1) (term~appl-p term2))
	 (if (keim~equal (data~appl-function term1) (data~appl-function term2))
	     (do ((rest-arguments1 (data~appl-arguments term1) (rest rest-arguments1))
		  (rest-arguments2 (data~appl-arguments term2) (rest rest-arguments2)))
	         ((null rest-arguments1)
		 sol)
	       (spass=term-equal-except-variables-p (first rest-arguments1) (first rest-arguments2)))
	   (setq sol nil)))
	(t (setq sol nil))))

(defun spass=make-substitution-from-renaming-list (renaming-list)
  (declare (edited  "11-JUL-1997")
	   (authors Naumann)
	   (input   "A list of renamings.")
	   (effect  "None.")
	   (value   "A subsitution of all renamings in the list."))
  (if (null renaming-list) (subst~create nil nil)
    (do* ((rest-renaming-list renaming-list (rest rest-renaming-list))
	  (renaming (first rest-renaming-list) (first rest-renaming-list))
	  (substitution (subst~create (subst~domain renaming) (subst~codomain renaming))
			(subst~create (append (subst~domain substitution) (subst~domain renaming))
				      (append (subst~codomain substitution) (subst~codomain renaming)))))
	((equal (length rest-renaming-list) 1)
	 substitution))))

(defun spass=codomain-renaming-list (renaming-list)
  (declare (edited  "11-JUL-1997")
	   (authors Naumann)
	   (input   "A list of substitutions.")
	   (effect  "None.")
	   (value   "A list with the codomain of all substitutions."))
  (if (null renaming-list) nil
    (do* ((rest-renaming-list renaming-list (rest rest-renaming-list))
	  (substitution (first rest-renaming-list) (first rest-renaming-list))
	  (codomain-list (subst~codomain substitution) (append codomain-list (subst~codomain substitution))))
	((equal (length rest-renaming-list) 1)
	 codomain-list))))
 
;---------------------------parse/translate-utilities----------------------------------
  
(defun spass=string-suffix (prefix-string main-string)
  (declare (edited  "24-JUN-1997")
	   (authors Naumann)
	   (input   "Two strings.")
	   (effect  "None.")
	   (value   "(main-string / prefix-string, if prefix-string is the prefix from main-string. Otherwise nil."))
  (cond ((string= prefix-string "") main-string)
	((> (length prefix-string) (length main-string)) nil)
	(t (do* ((counter -1 (+ 1 counter))
		 (control-flag t (if (equal (char prefix-string counter) (char main-string counter)) t
				    nil)))
	       ((or (not control-flag) (equal counter (- (length prefix-string) 1)))
		(if (not control-flag) control-flag (second (multiple-value-list (spass=divide-string main-string (length prefix-string))))))
	     (format nil "~A : ~A : ~A~%" counter control-flag (- (length prefix-string) 1))))))
		 
(defun spass=divide-string (string number)
  (declare (edited  "24-JUN-1997")
	   (authors Naumann)
	   (input   "A string.")
	   (effect  "None.")
	   (value   "A multiple value: First the first n chars (a string) of the string, second the rest."))
  (cond ((equal (length string) 0)
         (values-list (list "" "")))
	((>= number (length string))
	 (values-list (list string "")))
	((equal number 0)
	 (values-list (list "" string)))
        (t (do* ((counter -1 (+ 1 counter))
                 (prefix-string "" (if (>= counter number)
				       prefix-string
				     (format nil "~A~A" prefix-string (char string counter))))
		 (rest-string "" (if (>= counter number)
			             (format nil "~A~A" rest-string (char string counter))
				   rest-string)))
	((equal counter (- (length string) 1))
      (values prefix-string rest-string))))))

(defun spass=remove-chars (string char-list)
  (declare (edited  "01-JUL-1997")
	   (authors Naumann)
	   (input   "A string and a list of chars.")
	   (effect  "None.")
	   (value   "The string without the chars."))
  (do* ((counter -1 (+ 1 counter))
	(new-string "" (format nil "~A~A" new-string (if (member (char string counter) char-list)
							 "" (char string counter)))))
      ((equal counter (- (length string) 1))
       new-string)))

(defun spass=extract-number (string)
  (declare (edited  "28-JUN-1997")
	   (authors Naumann)
	   (input   "A string.")
	   (effect  "None.")
	   (value   "Extracts the first number in the string as string and returns the string, if there is no number -> nil."))
  (cond ((string= string "") nil)
	((not (spass=char-to-number (char string 0)))
	 (spass=extract-number (second (multiple-value-list (spass=divide-string string 1)))))
	(t (do* ((counter 0 (+ 1 counter)))
	       ((or (equal counter (- (length string) 1))
		    (not (numberp (spass=char-to-number (char string counter)))))
                (if (equal counter (- (length string) 1))
		    (if (numberp (spass=char-to-number (char string counter)))
			(first (multiple-value-list (spass=divide-string string (+ 1 counter))))
		        (first (multiple-value-list (spass=divide-string string counter))))
		 (first (multiple-value-list (spass=divide-string string counter)))))))))
					
(defun spass=get-number (string)
  (declare (edited  "28-JUN-1997")
	   (authors Naumann)
	   (input   "A number-string.")
	   (effect  "None.")
	   (value   "Translates a number-string into a number."))
 (let* ((pos-length (- (length string) 1)))
   (do* ((position 0 (+ 1 position))
	 (exp 1 (* exp 10))
	 (number (spass=char-to-number (char string (- pos-length position)))
	         (+ number (* (spass=char-to-number (char string (- pos-length position))) exp))))
	((equal position pos-length)
	 number))))

(defun spass=char-to-number (char)
  (cond ((equal char #\0) 0)
	((equal char #\1) 1)
        ((equal char #\2) 2)
	((equal char #\3) 3)		       
        ((equal char #\4) 4)
        ((equal char #\5) 5)
        ((equal char #\6) 6)
        ((equal char #\7) 7)
        ((equal char #\8) 8)
        ((equal char #\9) 9)))

(defun spass=read-word (string stops-char-list)
  (declare (edited  "02-JUL-1997")
	   (authors Naumann)
	   (input   "A string and a list of stopper-chars.")
	   (effect  "None.")
	   (value   "The string until one of the stop-chars."))
  (do* ((counter -1 (+ 1 counter))
	(chare nil (char string counter)))
      ((or (equal counter (- (length string) 1))
	   (member chare stops-char-list))
       (cond ((equal counter -1) "")
	     ((equal counter (- (length string) 1)) (if (member chare stops-char-list)
							(first (multiple-value-list (spass=divide-string string counter)))
							string))
             (t (first (multiple-value-list (spass=divide-string string counter))))))))

(defun spass=read-word-two (string stops-char-list)
  (declare (edited  "08-JUL-1997")
	   (authors Naumann)
	   (input   "A string and a list of stopper-chars.")
	   (effect  "None.")
	   (value   "The string until the stoper-char but after all brackets are enclosed."))
  (do* ((counter -1 (+ 1 counter))
	(chare nil (char string counter))
	(brackets 0 (+ brackets (cond ((equal chare #\() 1)
				      ((equal chare #\)) -1)
				      (t 0)))))
      ((or (equal counter (- (length string) 1))
	   (and (member chare stops-char-list)
		(equal brackets 0)))
       (cond ((equal counter -1) "")
	     ((equal counter (- (length string) 1)) (if (member chare stops-char-list)
							(first (multiple-value-list (spass=divide-string string counter)))
							string))
             (t (first (multiple-value-list (spass=divide-string string counter))))))))
       

;----------------------------get the flags from the user--------------------------------

(defun spass=take-flags (auto-mode splitting-level)
  (declare (edited  "22-JUN-1997")
	   (authors Naumann)
	   (input   "None.")
	   (effect  "None.")
	   (value   "Returns the SPASS-Flags, including the user-defined ones."))
  (setq spass*allowed-splittings splitting-level)
  (let* ((auto-mode (if auto-mode 1 0)))
  (append 
	  (list auto-mode) '(0 0 0 ; Auto
				     0) (list spass*allowed-splittings) '(-1 -1 ;CSupP
				     0 0 1 1 ; DocSST
				     -1 0 0 0 ; Loops
				     0 0 0 0 ; PTaut
				     0 0 0 0 ; PClR
				     0 0 0 0 ; PKept
				     0 0 0 0 ; FPModel
				     1 1 1 0 ; Select
				     5 1 1 1 ; WDRatio
				     0 1 1 1 ; PrefVar
				     1 0 1 1 ; IEqF
				     1 1 0 0 ; IGeR
				     1 1 1 1 ; RFRew
				     1 1 1 1 ; RObv
				     1 1 1 1 ; RSSi
				     1 ) ; RCon
	  )))

;old (cons spass*allowed-splittings '(0 1     -1 -1 0    0     0         1     0    0    0   0    0    0   0     0       0     0   0     0   0    0   0 0)))))
; SpliAutoSortDelMemTiFPModDocSplDocSortDelDocProPFSubPBSubPRewPCondPTautPObvPSoSimPSortDelPClRedPDerPEmpClPGivPKeptPProPSPF




;---------------a proof-checker for spass*current-resolution-problem--------

;---------------wird rausgehaengt------------------------------------------



(defun spass=check-proof ()
  (declare (edited  "06-AUG-1997")
	   (authors Naumann)
	   (input   "Nothing.")
	   (effect  "None.")
	   (value   "Checks if spass*current-resolution-problem is correct justfied and contains the empty clause."))
  (omega~message "Checking proof.......~%")
  (setq solution t)
  (if (not (res~proof-empty-clause spass*current-resolution-problem))
      (omega~message "The problem is not proofed.")
    (let* ((proof-clauses (res~proof-clauses spass*current-resolution-problem)))
      (do* ((rest-clauses proof-clauses (rest rest-clauses)))
	  ((null rest-clauses) (if solution (omega~message "Proof checked - correct.~%")
	(omega~message "Justification failed.~%.")))
	(if (not (spass=check-clause (first rest-clauses) (node~justification (first rest-clauses)) proof-clauses))
	    (setq solution nil))))))

(defgeneric spass=check-clause (clause justification clauses)
  (declare (edited  "06-AUG-1997")
	   (authors Naumann)
	   (input   ".")
	   (effect  ".")
	   (value   "."))
  (:method ((clause cl+clause) (justification res+initial) (clauses cons))
	   t)
  (:method ((clause cl+clause) (justification res+reflex) (clauses cons))
	   t)
  (:method ((clause cl+clause) (justification res+instance) (clauses cons))
	   (let* ((parent (res~instance-clause justification))
		  (unifier (res~just-unifier justification)))
	     (if (not (member parent clauses))
		 (omega~message "Instance-parent-clause from clause ~A not existing.~%" clause)
	       (if (not (spass=keim-equal-clause clause (subst~apply unifier parent)))
		   (omega~message "Clauses are not equal (instance-step).~%")
		 t))))

  (:method ((clause cl+clause) (justification res+factoring) (clauses cons))
	   (let* ((parent (res~factoring-clause justification))
		  (positions (res~factoring-positions justification))
		  (pos1 (first positions))
		  (pos2 (second positions))
		  (renaming (res~just-renamings justification))
		  (unifier (res~just-unifier justification))
		  (literals (cl~literals parent))
		  (lit1 (nth (first (pos~number-list pos1)) literals))
		  (lit2 (nth (first (pos~number-list pos2)) literals))
		  (unifier2 (term~unify lit1 lit2))
		  (new-clause (cl~create (subst~apply unifier (remove lit2 literals :count
								      1 :test 'keim~equal
								      :start (pos~first pos2))))))
             ;(omega~message "factoring-step~%exi-clause: ~A~%new-clause: ~A~%" clause new-clause)
	     (if (not (member parent clauses)) (omega~message "Factoring-parent-clause not existing from clause ~A.~%" clause)
	       (if (not (keim~equal unifier unifier2)) (omega~message "Unifier not correct.~%")
		 (if (not (spass=keim-equal-clause clause new-clause))
		     (omega~message "Clauses are not equal (maybe in literal ordering).~%")
		   (progn ;(omega~message "Factoring ok.~%")
		     t))))))
  (:method ((clause cl+clause) (justification res+resolution) (clauses cons))
	   (let* ((parents (res~resolution-clauses justification))
                  (parent1 (first parents))
                  (parent2 (second parents))
		  (positions (res~resolution-positions justification))
		  (pos1 (first positions))
		  (pos2 (second positions))
		  (renamings (res~just-renamings justification))
		  (renaming1 (first renamings))
		  (renaming2 (second renamings))
		  (new-parent1 (subst~apply renaming1 parent1))
		  (new-parent2 (subst~apply renaming2 parent2))
		  (unifier (res~just-unifier justification))
		  (literals1 (cl~literals new-parent1))
		  (literals2 (cl~literals new-parent2))
		  (lit1 (nth (first (pos~number-list pos1)) literals1))
		  (lit2 (nth (first (pos~number-list pos2)) literals2))
		  (unifier2 (term~unify lit1 lit2))
		  (new-clause (cl~create (append (subst~apply unifier (remove lit1
									      literals1
									      :count 1
									      :test
									      'keim~equal
									      :start
									      (pos~first pos1)))
						 (subst~apply unifier (remove lit2
									      literals2
									      :count 1
									      :test
									      'keim~equal
									      :start
									      (pos~first pos2)))))))
             ;(omega~message "resolution-step~%exi-clause: ~A~%new-clause: ~A~%" clause new-clause)
             ;(omega~message "member: ~A, ~A~%" (member parent1 clauses) (member parent2 clauses))
	     (if (or (not (member parent1 clauses))
		     (not (member parent2 clauses)))
			  (omega~message "Resolution-parent-clause not existing from clause ~A.~%" clause)
	       (if (not (keim~equal unifier unifier2)) (omega~message "Unifier not correct.~%")
		 (if (not (or (and (lit~positive-p lit1)
				   (not (lit~positive-p lit2)))
			      (and (not (lit~positive-p lit1))
				   (lit~positive-p lit2))))
		     (omega~message "The polarity is not changed.~%")
		   (if (not (spass=keim-equal-clause clause new-clause))
		       (omega~message "Clauses are not equal (maybe in literal ordering).~%")
		     (progn ;(omega~message "Resolution ok.~%")
			    t)))))))
  (:method ((clause cl+clause) (justification res+paramodulation) (clauses cons))
	    (let* ((mother (res~paramod-mother justification))
                   (father (res~paramod-father justification))
                   (mother-position (res~paramod-mother-position justification))
                   (father-position (res~paramod-father-position justification))
		   (renamings (res~just-renamings justification))
		   (mother-renaming (first renamings))
		   (father-renaming (second renamings))
		   (new-mother (subst~apply mother-renaming mother))
		   (new-father (subst~apply father-renaming father))
		   (unifier (res~just-unifier justification))
		   (mother-literals (cl~literals new-mother))
		   (father-literals (cl~literals new-father))
                   (applicant (nth (pos~first mother-position) mother-literals))
                   ;(appl-pol (lit~positive-p applicant))
		   ;(appl-atom (lit~atom applicant))
		   (equation (data~struct-at-position new-father father-position))
                   ;(k (omega~message "eq: ~A~%" equation))
		   (direction (res~paramod-direction justification))
                   (equation-arguments (data~appl-arguments (lit~atom equation)))
		   (s (if (eq direction 'lr) (first equation-arguments)
			(if (eq direction 'rl) (second equation-arguments))))
		   (tr (if (eq direction 'rl) (first equation-arguments)
			(if (eq direction 'lr) (second equation-arguments))))
		   (term-position (pos~rest mother-position)) ; da das erste die lit-pos referenziert
		   (s- (data~struct-at-position applicant term-position))
		   (new-applicant (data~replace-at-position applicant term-position tr))
		   (unifier2 (term~unify s s-))
		   (new-clause (cl~create (append (cl~literals (data~replace-at-position
								      (subst~apply unifier
										   new-mother)
								      (pos~list-position
								       (list (pos~first mother-position)))
								      (subst~apply unifier new-applicant)))
							 
						  (subst~apply unifier (remove equation
									       father-literals :count 1 :test 'keim~equal :start (pos~first father-position)))))))
              ;(omega~message "paramodulation-step~%exi-clause: ~A~%new-clause: ~A~%" clause new-clause)
	      (if (not (lit~positive-p equation)) (omega~message "The equation is not positive.~%")
	      (if (not (or (member mother clauses)
			  (member father clauses)))
			  (omega~message "Parent-clause not existing.~%")
	       (if (not (keim~equal unifier unifier2)) (omega~message "Unifier not correct.~%")
		(if (not (spass=keim-equal-clause clause new-clause))
		    (omega~message "Clauses are not equal (maybe in literal ordering).~%")
		  (progn ;(omega~message "Paramodulation ok.~%")
		    t))))))))


(defun spass=keim-equal-clause (clause1 clause2)
  (declare (edited  "24-SEP-1997")
	   (authors Naumann)
	   (input   "Two clauses.")
	   (effect  "None.")
	   (value   "t ist the clauses are keim~equal and the literals have the same
ordering."))
  (setq c1 clause1)
  (setq c2 clause2)
  (if (not (keim~equal clause1 clause2)) nil
    (if (and (cl~empty-p clause1) (cl~empty-p clause2)) t
      (do* ((lits1 (cl~literals clause1) (rest lits1))
	    (lits2 (cl~literals clause2) (rest lits2))
	    (lit1 (first lits1) (first lits1))
	    (lit2 (first lits2) (first lits2))
	    (solution (keim~equal lit1 lit2)
		      (if solution (keim~equal lit1 lit2) nil)))
	  ((equal (length lits1) 1) solution)))))
  


#| ------------------------------------------------------- CALL SPASS MAIN ---------------------------------------------------------- |#

;; handling equality
(defun spass=handling-equality! ()
  (declare (edited  "24-APR-1998")
	   (authors Ameier)
	   (input   "")
	   (effect  "Creates a new reflexivity clause and sets the following global variables:"
		    "spass*problems-contains-equality -> t"
		    "spass*reflex-clause -> created ref-clause")
	   (value   "Undefined."))
  
  (setq spass*problem-contains-equality t)
  
  (let* ((new-type-var (type~variable-create 'aa))
	 (new-var (term~generate-term-primitive-with-new-name 'x- new-type-var 'term+variable
							      (res~proof-environment spass*current-resolution-problem)))
	 (new-term (term~appl-create
		    (env~lookup-object '= (res~proof-environment spass*current-resolution-problem))
		    (list new-var new-var)
		    ))
	 (ref-clause (cl~create (list (lit~literal-create new-term t))
				:justification (res~reflex-create (gensym)))))

    (env~remove (keim~name new-var) (res~proof-environment spass*current-resolution-problem))
    
    ;; Entfaellt ab Version 3.3
    ;; (setf (data~kappa new-term)
    ;;	  (list (data~n-domain
    ;;		 (term~type (env~lookup-object '= (res~proof-environment spass*current-resolution-problem))))))

    (keim~put ref-clause :spass-clause-number (list 'used)) ; kann man sicher drueber streiten
    (setq spass*reflex-clause ref-clause)
					;(setq spass*symmetrie-clause komm-clause)
					;(setq spass*transitive-clause trans-clause)
    (keim~put spass*reflex-clause :splitting-level 0)
					;(keim~put komm-clause :splitting-level 0)
					;(keim~put trans-clause :splitting-level 0)
    ))


(defun spass~complete-spass-problem! (spass-problem &key (parse-back 't))
  (if (null (atpprb~problem-atp-out-string spass-problem))
      nil
    (let* ((spass-out-string (atpprb~problem-atp-out-string spass-problem))
	   (res-proof (atpprb~problem-part-res-proof spass-problem))
	   (global-vars (atpprb~problem-global-vars spass-problem))
	   (translation-settings (atpprb~problem-translation-settings spass-problem)))
      
      (setq spass*current-resolution-problem res-proof)
      (setq spass*reflex-clause (first global-vars))
      (setq spass*renamed-objects (second global-vars))
      (setq spass*problem-contains-equality (if (env~lookup-object '= (res~proof-environment spass*current-resolution-problem))
						't
					      nil))
      (setq spass*open-splitting-levels nil)
      (setq spass*obvious-reductions nil)					   
      (setq spass*proof-file-lines nil)
      (setq spass*current-clause-list nil)

      ;; Fliegt raus ab Version 3.3
      ;; (spass=set-current-type-var-subst!)

      (setq p2f*domain (second translation-settings))  
      (setq p2f*codomain (third translation-settings))

      (let* ((proof-flag (spass=translate-spass-proof-from-string spass*current-resolution-problem spass-out-string parse-back)))
	(cond ((null proof-flag)
	       (omega~message "~% SPASS HAS FAILED TO FIND A PROOF ~%")
	       nil)
	      ((equal proof-flag 'completion)
	       (omega~message "~% SPASS HAS FAILED TO FIND A PROOF, BUT IT HAS FOUND A COMPLETION ~%")
	       nil)
	      ((equal proof-flag 'proof)
	       (omega~message "~% SPASS HAS FOUND A PROOF ~%")
	       't)
	      (t
	       (omega~message "~% SPASS HAS FOUND A PROOF ~%")
	       (omega~message "~% OMEGA*CURRENT-RESOLUTION-PROOF IS SET TO THE FOUND RESOLUTION PROOF")
	       proof-flag ;; = res-proof
	       ))
	))))

(defun spass~generate-spass-problem (conclusion-node assumption-nodes ho-pds)
  (declare (edited  "26-MAR-1998")
	   (authors Ameier)
	   (input   "A conclusion-node, a set of assumption-nodes and the pds the nodes are from.")
	   (effect  "None.")
	   (value   "A atp-problem with type spass, a partial resolution proof and partial settet"
		    "global vars."
		    "Remark: atp-in-string is NOT-SET, and global vars are not set completly !!"))
  
  (setq spass*current-resolution-problem (atptop~resolution-proof-create-from-nodes conclusion-node assumption-nodes ho-pds))
  
  ;; translate the initial resolution proof res-proof to f.o. and normalize it
  (p2f~translate spass*current-resolution-problem)
  
  (omega~message "~% Normalizing ...")
  (hocnf~normalize-res-proof! spass*current-resolution-problem)

  ;; Fliegt raus ab Version 3.3
  ;; (spass=set-current-type-var-subst!)

  (when (env~lookup-object '= (res~proof-environment spass*current-resolution-problem))
    (spass=handling-equality!))

  ;; Remove clauses that contain abstractions
  ;; After p2f translation it can happen that there are clauses that contain lambda-abstraction
  ;; Such clauses can not be handled by OTTER (or any f.o. ATP and are therefore removed from the
  ;; clauses list
  (atptop~remove-clauses-with-abstractions! spass*current-resolution-problem)
  
  (atpprb~create-fo-problem (gensym "spass-problem-")
			    'spass
			    nil      ;; spass-in-file kommt erst spaeter dazu: -> spass~add-in-string! 
			    nil
			    spass*current-resolution-problem
			    (list spass*reflex-clause)
			    (list 'p2f p2f*domain p2f*codomain))
  
  ;; in die atpprb~problem-translation-settings kommt eine Liste mit 
  ;; 'p2f p2f*domain p2f*codomain, falls p2f~translate

  ;; Reihenfolge in global-var-list:
  ;; 1. spass*reflex-clause
  ;; 2. spass*renamed-objects -> kommt erst spaeter beim erzeugen des in-string hinzu -> spass~add-in-string!

  
  ;; nicht in global var-list sind folgende Variablen:
  ;;  1. spass*current-resolution-problem    -> steht als part-res-proof drin
  ;;  2. spass*problem-contains-equality     -> rekonstruierbar aus env (steht in resolution proof)
  ;;  3. spass*settings-list-old             -> unveraenderlich global
  ;;  4. spass*settings-list                 ->        - ~ -
  ;;  5. spass*name-symbols                  ->        - ~ -
  ;;  6. spass*dfg-predicates                ->        - ~ - 
  ;;  7. spass*flag-list                     ????? -> wohl nur beim schreiben des input File benoetigt
  ;;  8. spass*allowed-splittings            ????? -> wohl nur beim schreiben des input File benoetigt
  ;;  9. spass*open-splitting-levels         -> erst beim parsen gebraucht -> dann default auf nil setzen
  ;; 10. spass*obvious-reductions            -> erst beim parsen gebraucht -> dann default auf nil setzen
  ;; 11. spass*proof-file-lines              -> erst beim parsen gebraucht -> dann default auf nil setzen
  ;; 12. spass*new-name-counter              -> nur beim schreiben gebraucht -> nicht mehr noetig
  ;; 13. spass*current-clause-list           -> anscheinend gar nicht benoetigt
  ;; 14. spass*current-proof-plan            -> anscheinend gar nicht benoetigt
  )

(defun spass~add-in-string! (spass-problem auto-mode splitting-level)
  (declare (edited  "26-MAR-1998")
	   (authors Ameier)
	   (input   "A spass-problem.")
	   (effect  "The spass*in-string is produced and added to the spass-problem.")
	   (value   "Undefined."))
  
  (multiple-value-bind
      (assumption-clauses conclusion-clauses)
      (spass=assumption-and-conclusion-clauses spass*current-resolution-problem)
    (setf (res~proof-clauses spass*current-resolution-problem)
	  (append assumption-clauses conclusion-clauses (list spass*reflex-clause))))
					; spass*symmetrie-clause spass*transitive-clause ; politische Entscheidung
  (setq spass*problem-contains-equality nil)
  (setq spass*flag-list (spass=take-flags auto-mode splitting-level)) ; hier ist der Benutzer gefordert
  (setq spass*renamed-objects nil)
  (setq spass*new-name-counter 0)
  (setq spass*proof-file-lines nil)
  (setq spass*current-clause-list nil)
  (setq spass*obvious-reductions nil)
  (setq spass*open-splitting-levels nil)

  ;; konstruiert das spass.in file im spass*in-string
  (spass=produce-input-string spass*current-resolution-problem spass*flag-list)

  ;; setzt atp-in-string im spass-problem
  (setf (atpprb~problem-atp-in-string spass-problem) spass*in-string)
  
  ;; setzt das 2. Argument in den global-vars auf spass*renamed-objects
  (setf (atpprb~problem-global-vars spass-problem) (append (atpprb~problem-global-vars spass-problem)
							   (list spass*renamed-objects))))


(defun spass~call-spass (open-node ho-pds dir ressource auto-mode splitting-level parse-back)
  (let* ((problem-name (keim~name ho-pds))
	 (spass-problem-dir (merge-pathnames (format nil "~A/" (string-downcase problem-name)) dir))
	 (in-file (merge-pathnames "spass.cnf" spass-problem-dir))
	 (out-file (merge-pathnames "spass.dfg" spass-problem-dir))
	 (spass-problem (spass~generate-spass-problem open-node
						      (just~premises (node~justification open-node))
						      ho-pds))
	 (res-proof (atpprb~problem-part-res-proof spass-problem)))
    
    ;; erzeugt directory falls es nicht existiert
    (unless (probe-file spass-problem-dir)
      (sys~call-system (format nil "mkdir ~A" (string-right-trim '(#\/) (namestring spass-problem-dir)))))

    ;; erzeugt spass.cnf file in spass*in-string, fuege spass*in-string zum spass-problem hinzu
    (spass~add-in-string! spass-problem auto-mode splitting-level)

    ;; call-spass vor ort -> schreibt out-string spass.dfg in das spass-problem
    (spass=call-spass! spass-problem spass-problem-dir ressource)

    ;;parsen des spass-beweises
    (spass~complete-spass-problem! spass-problem :parse-back parse-back)
    ))


(defun spass=call-spass! (spass-problem spass-problem-dir ressource)
  (declare (edited  "26-MAR-1998")
	   (authors Ameier)
	   (input   "A spass-problem, the directory for input, output and a time-ressource.")
	   (effect  "Writes the in-string of the spass-problem to the file spass.cnf in the"
		    "directory, calls spass on it, reads the file spass.dfg from the directory"
		    "and writes it into the out-string of the spass-problem.")
	   (value   "Undefined."))
  
  (let* ((in-file (merge-pathnames "spass.cnf" spass-problem-dir))
	 (temp-out-file (merge-pathnames "tmp.out" spass-problem-dir))
	 (out-file (merge-pathnames "spass.dfg" spass-problem-dir)))

    (atptop~print-string-in-file (atpprb~problem-atp-in-string spass-problem) in-file)

    (when (null (probe-file (spass~program)))
      (error "There is no spass-executable at ~A, please check your path to the spass-executable." (spass~program)))
    
    (let* ((call-flag (atptop~call-with-time-ressource (format nil "~A ~A >! ~A;mv ~A ~A &"
							       (spass~program) in-file temp-out-file
							       temp-out-file out-file)
						       out-file
						       "spass"
						       spass-problem-dir
						       ressource)))
      (if (null call-flag)
	  (omega~message "~% Spass was not able to find a proof in the given time ressource. ~%")
	
	;; read spass.dfg file as string ans set atp-out-string of the spass-problem
	(setf (atpprb~problem-atp-out-string spass-problem)
	      (atptop~read-file-as-string out-file))))))

(defun spass~generate-spass-problem-default! (conclusion-node assumption-nodes ho-pds)
  (declare (edited  "26-MAR-1998")
	   (authors Ameier)
	   (input   "A conclusion-node, a set of assumption-nodes and the pds the nodes are from.")
	   (effect  "The created spass-problem is inserted into the list in the plist entry"
		    "'atp-problems of the conclusion node.")
	   (value   "A atp-problem with type spass, a partial resolution proof."))
  
  (let* ((spass-problem (spass~generate-spass-problem conclusion-node assumption-nodes ho-pds)))
    
    (spass~add-in-string! spass-problem
			  't
			  0)
    
    (keim~put conclusion-node 'atp-problems (cons spass-problem (keim~get conclusion-node 'atp-problems)))
    
    spass-problem))


#| --------------------------------------------------- Read free stuff for SPASS ---------------------------------------------------- |#

;; Notice: To be able to read bacj the outfile of a spass-proof without the infile, we need the following convention:
;;         Funktions and Predicates start with less capitals, only Variable names start with capital letters!!


(defun spass~read-spass-output (open-node file)
  (declare (edited  "02-JUN-2000")
	   (authors Ameier)
	   (input   "An open node and a file.")
	   (effect  "Mayby changes the spass global variables.")
	   (value   "1. For the open node a new resolution proof is created."
		    "2. The file is tried to read as a spass proof for this resolution proof."
		    "   In particular, a mapping is computed from the initial clauses of the"
		    "   protein file and the initial clauses of the new resolution proof."
		    "If it was possible to read the file as a spass proof file, the resolution"
		    "proof is completes (an empty clauses is dreived according to the proof in"
		    "the file) and this complete resolution proof is returned."
		    "If it was not possible to read the file as a spass proof file nil is"
		    "returned."))
  (let* ((spass-problem (spass~generate-spass-problem open-node
						      (remove open-node (pds~node-supports open-node))
						      omega*current-proof-plan))
	 (res-proof (spass~complete-spass-problem-from-file! spass-problem file)))
    
    res-proof))

(defun spass~complete-spass-problem-from-file! (atp-problem file)
  (let* ((out-string (atptop~read-file-as-string file))
	 (res-proof (atpprb~problem-part-res-proof atp-problem))
	 (global-vars (atpprb~problem-global-vars atp-problem)))

    ;; was ist mit translation settings? -> braucht man zumindest nicht fuer reine first order probleme

    (setf (atpprb~problem-atp-out-string atp-problem) out-string)
    
    (setq spass*current-resolution-problem res-proof)
    (spass=handling-equality!) ;; spass*reflex-clause is set!
    ;; Note: in the atp-problem we have in the global-var-list:
    ;; 1.  spass*renamed-objects       -> we have to reconstruct by matching the clauses!
    (setq spass*renamed-objects (spass=reconstruct-convert-list! res-proof out-string))
    (setq spass*problem-contains-equality 't)
    
    ;; Other necessary settings
    (setq spass*open-splitting-levels nil)
    (setq spass*obvious-reductions nil)					   
    (setq spass*proof-file-lines nil)
    (setq spass*current-clause-list nil)
    
    
    (if (null spass*renamed-objects)
	(progn
	  (omega~message "~% Could not match the out-file to the problem.")
	  nil)
      (let* ((proof-flag (spass=translate-spass-proof-from-string spass*current-resolution-problem out-string 't)))
	(cond ((null proof-flag)
	       (omega~message "~% SPASS HAS FAILED TO FIND A PROOF ~%")
	       nil)
	      ((equal proof-flag 'completion)
	       (omega~message "~% SPASS HAS FAILED TO FIND A PROOF, BUT IT HAS FOUND A COMPLETION ~%")
	       nil)
	      ((equal proof-flag 'proof)
	       (omega~message "~% SPASS HAS FOUND A PROOF ~%")
	       't)
	      (t
	       (omega~message "~% SPASS HAS FOUND A PROOF ~%")
	       (omega~message "~% OMEGA*CURRENT-RESOLUTION-PROOF IS SET TO THE FOUND RESOLUTION PROOF")
	       proof-flag ;; = res-proof
	       ))
	))))

  
(defun spass=reconstruct-convert-list! (res-proof out-string)
  (declare (edited  "29-MAY-2000")
	   (authors Ameier)
	   (input   "The current resolution proof and the out-string of a spass call.")
	   (effect  "The content of spass*renamed-objects and spass*local-clause-vars can be changed.")
	   (value   "First the input clauses of the proof in the out-string are read. Thereby, the new"
		    "created constants have as names exactly the names they have in the out-file."
		    "Then these read clauses are matched against the input clauses in the resolution proof."
		    "This results (if successfull) in a mapping, that mapps each constant in the"
		    "read clauses to a constant in the input clauses. From this mapping we compute"
		    "a list of pairs of constants and strings (where the constant is taken from the"
		    "new resolutiomn proof whereas the string is from the out-file."))

  (let* ((res-proof-input-clauses (res~proof-clauses res-proof))
	 (out-file-input-clauses (spass=read-input-clauses out-string)))
    
    (multiple-value-bind
	(success mapping info-triples)
	(atptop~subset-by-equality-except-names-p out-file-input-clauses
						  (append res-proof-input-clauses (spass=complete-flipping res-proof-input-clauses)))
      ;; It can happen that spass uses directly some flips of some equations
      ;; -> complete the set by all possible flippings!
      ;; It does (from our experience) not happens that SPASS uses directly factors of the input clauses
      ;; -> complete factors is not necessary!
      
      (if success

	  (progn
	    (atptop~change-resolution-proof! res-proof info-triples)

	    (spass=reorder-clauses! res-proof info-triples)
	    
	    (let* ((mapp-domain (mapp~domain mapping))
		   (mapp-codomain (mapp~codomain mapping))
		   (mapp-constant-domain (remove-if-not #'term~constant-p mapp-domain))
		   (mapp-constant-codomain (remove-if-not #'term~constant-p mapp-codomain)))
	      
	      (cons (list "equal"
			  (env~lookup-object '= (res~proof-environment spass*current-resolution-problem)))
		    (mapcar #'(lambda (dom codom)
				(let* ((name (keim~name dom)))
				  (list (if (stringp name)
					    name
					  (string name))
					codom)))					  
			    mapp-constant-domain mapp-constant-codomain))))
	nil))))

(defun spass=reorder-clauses! (res-proof clause-triples)
  (declare (edited  "28-JUN-2000")
	   (authors Ameier)
	   (input   "A resolution proof and a list of clause triples consiting of two clauses and a list of positions.")
	   (effect  "Because of the rest of the spass parse things it is necessary, that the input clauses in the"
		    "resolution proof are ordered in the same way as the clauses are used in the out-file."
		    "We compute in this function the order of the initial clauses of the resolution proof corresponding"
		    "to the order of the input clauses in the spass outfile and reorder the clauses accordingly"
		    "in the res~proof-clauses slot of the resolution proof.") 
	   (value   "The changed resolution proof."))
  (let* ((initial-clauses-in-right-order (do* ((rest-clause-triples clause-triples (rest rest-clause-triples))
					       (back-clauses nil))
					     ((null rest-clause-triples)
					      back-clauses)
					   (let* ((head-clause-triple (first rest-clause-triples))
						  (out-clause (first head-clause-triple))
						  (according-clause (second head-clause-triple))
						  (init-clause (spass=get-initial-clause according-clause))
						  (curr-number (keim~get out-clause 'spass-number))
						  (next-number (if (null (rest rest-clause-triples))
								   nil
								 (keim~get (first (first (rest rest-clause-triples))) 'spass-number))))
					     (if (null next-number)
						 (setf back-clauses (append back-clauses (list init-clause)))
					       (setf back-clauses (append back-clauses (spass=multiple init-clause
												       (- next-number curr-number))))))))
	 (all-clauses (res~proof-clauses res-proof))
	 (all-clauses-except-inits (remove-if #'(lambda (clause)
						  (find clause initial-clauses-in-right-order))
					      all-clauses)))
    
    (setf (res~proof-clauses res-proof) (append initial-clauses-in-right-order all-clauses-except-inits))
    ;;(format t "~%INITIAL CLAUSES: ~A" (res~proof-initial-clauses res-proof))
    res-proof))

;; Notice: The usage of this spass=multiple thing duplicates (NOT COPIES!) clauses in the res~proof steps.
;;         Conceptually this is bull shit!
;;         So, why did i add this?
;;         It is because the parse functions of spass use the number of the clause to detect the corresponding clause
;;         in the initial clause list. For instance, if a clause-line starts with number 1, the resulting clause is connected
;;         with the first clause of the initial clause list. If we parse back into the origianl resolution proof this is not
;;         a problem, since then the clauses in the initial-clause list are ordered in the right way. But if we parse back
;;         free from a file into another resolution proof, we have to order the initial clause list in this resolution proof
;;         accordingly. Now it can happen that in the output some numbers are missing (for intance, 1 ... then 3 ...) in this
;;         case i simply duplicate the clause for one to fill the gap.

(defun spass=multiple (item number)
  (do* ((rest number (- rest 1))
	(back-list nil (cons item back-list)))
      ((= rest 0)
       back-list)))

(defun spass=get-initial-clause (clause)
  (let* ((just (node~justification clause)))
    (if (res~initial-p just)
	clause
      (spass=get-initial-clause (first (res~justification-parents just))))))
	  

(defun spass=read-input-clauses (out-string)
  (declare (edited  "29-MAY-2000")
	   (authors Ameier)
	   (input   "A spass outfile.")
	   (effect  "During the parsing of the input clauses (see value) the spass*renamed-objects is changed.")
	   (value   "Reads the input clauses, without a convertion setting."
		    "Thereby, for each name that starts with a capital letter a new variable of type i is produced, and"
		    "for each other name a constant of type (i <- ...) is produced."))
  
  (setf spass*renamed-objects nil)
  
  (let* ((line-strings (atptop~divide-string out-string #\Newline))
	 (input-clauses (do* ((rest-string-lines line-strings (rest rest-string-lines))
			      (return-list nil))
			    ((null rest-string-lines)
			     return-list)
			  (let* ((string-line (first rest-string-lines)))
			    (multiple-value-bind
				(success literals-string number)
				(spass=input-clause-test-p string-line)
			      (when success
				(let* ((clause (spass=parse-free-input-clause-line literals-string)))
				  (keim~put clause 'spass-number number) 
				  (setf return-list (append return-list (list clause))))))))))
    ;; Note:
    ;; If a input clause is parsed, each name that starts with a capital letter is interpreted as variable
    ;; (e.g., X_7146, _7150)! -> a new local variable of type i is created (local = relevant only for the clause itself) 
    ;; Each other letter is interpreted as a constant. Hence, e new constant is created. The type of the constant
    ;; is (o <- i ...) or (i <- i ...) depending on whether the constant is the predicat of a literal or internal
    ;; and on the number of the arguments on which it is applied.
    ;; Since constants are not local for a clause, for each new created constant a new entry is made in the
    ;; spass*renamed-objects
    ;; Furthermore, each literal which is left of the '->' is a negativ literal, each one right of the '->' is a positive literal
    input-clauses))

(defun spass=input-clause-test-p (string-line)
  (declare (edited  "27-JUN-2000")
	   (authors Ameier)
	   (input   "A string, containing a line of a spass proof.")
	   (effect  "None.")
	   (value   "Tests whether the line represents a input clause"
		    "Multiple-value:"
		    "First: t if the line is a input clause line, this is, if the line has the following form:"
		    "       'number[0:Inp] literals'."
		    "Second: If First is t, a string consisting only of 'literals'."
		    "Third: If first is t, the parsed number."))
  (multiple-value-bind
      (prefix rest-word)
      (atptop~get-next-word string-line #\[ :ignore-char-list (list #\* #\+))
    
    (if (and (null (string= rest-word ""))
	     (atptop~parse-number prefix))
	(multiple-value-bind
	    (prefix2 rest-word2)
	    (atptop~get-next-word rest-word #\: :ignore-char-list (list #\*))
	  (if (and (null (string= rest-word2 ""))
		   (atptop~parse-number prefix2))
	      (multiple-value-bind
		  (prefix3 rest-word3)
		  (atptop~get-next-word rest-word2 #\] :ignore-char-list (list #\* #\+))
		(if (string= prefix3 "Inp")
		    (values 't
			    (atptop~cut-last-char rest-word3)
			    (atptop~parse-number prefix))
		  (values nil
			  nil
			  nil)))
	    (values nil
		    nil
		    nil)))
      (values nil
	      nil
	      nil))))


(defun spass=complete-factors (clause-list)
  (declare (edited  "28-JUN-2000")
	   (authors Ameier)
	   (input   "A list of clauses.")
	   (effect  "None.")
	   (value   "A clause list of all factors that can be computed (recursively) from these clauses."))
  (apply #'append (mapcar #'spass=produce-factors clause-list)))

(defun spass=produce-factors (clause)
  (declare (edited  "27-AUG-1998")
	   (authors Ameier)
	   (input   "A clause.")
	   (effect  "None.")
	   (value   "A list consisting of the clause and of all clauses that can be computed"
		    "from it with recursive application of factoring."))
  (let* ((factor-clauses (res~binary-factoring clause)))
    (append factor-clauses (apply 'append (mapcar #'spass=produce-factors factor-clauses)))))

(defun spass=complete-flipping (clause-list)
  (declare (edited  "03-NOV-1999")
	   (authors Ameier)
	   (input   "A clause-list.")
	   (effect  "None.")
	   (value   "A list of all clauses that can be derived by flipping some equality literals in the input clauses."))
  (apply #'append (mapcar #'spass=flip-all-positions clause-list)))

(defun spass=flip-all-positions (clause)
  (declare (edited  "03-NOV-1999")
	   (authors Ameier)
	   (input   "A clause.")
	   (effect  "None.")
	   (value   "A list of clauses, consisting of each clause that can be derived by flipping some"
		    "equality literals of the input clause."))
  (let* ((literals (cl~literals clause))
	 (equality-literal-positions (data~positions literals #'atptop~equation-p))
	 (all-subsets (rest (spass=all-subsets equality-literal-positions))))
    (mapcar #'(lambda (pos-list)
		(spass=flip-clause-at-positions clause pos-list))
	    all-subsets)))

(defun spass=all-subsets (listi)
  (declare (edited  "28-JUN-2000")
	   (authors Ameier)
	   (input   "A list.")
	   (effect  "None.")
	   (value   "A list with all sublists of the list. The first element is the empty list."))
  (let* ((all-sublists-with-one-element (mapcar #'list listi)))
    (do* ((rest-one-elements (reverse all-sublists-with-one-element) (rest rest-one-elements))
	  (current-list (list nil)))
	((null rest-one-elements)
	 current-list)
      (let* ((head-one (first rest-one-elements))
	     (mult-with-head-one (mapcar #'(lambda (listii)
					     (append head-one listii))
					 current-list)))
	(setf current-list (append current-list mult-with-head-one))))))

  

(defun spass=flip-clause-at-positions (clause pos-list)
  (declare (edited  "28-AUG-1998")
	   (authors Ameier)
	   (input   "A clasue and a list of positions.")
	   (effect  "NOne.")
	   (value   "A clause with flipped literals at each position of the list."))
  (if (null pos-list)
      clause
    (let* ((curr-pos (first pos-list))
	   (pos-num (pos~first curr-pos))
	   (new-literals (do* ((rest-literals (cl~literals clause) (rest rest-literals))
			       (i 0 (+ i 1))
			       (back-literals nil))
			     ((null rest-literals) back-literals)
			   (let* ((head-literal (first rest-literals)))
			     (if (= i pos-num) ;; -> flippen
				 (let* ((atom (lit~atom head-literal)))
				   (when (null (atptop~equation-p atom))
				     (error "~% PROBLEM IN FUNCTION spass=flip-clause-at-positions LITERAL SELECTED FOR FLIPPING IS NOT AN EQUATION !!. ~%"))
				   (setq back-literals (append back-literals
							       (list (lit~literal-create
								      (term~appl-create (data~appl-function atom)
											(list (data~copy (second (data~appl-arguments atom))
													 :downto '(data+primitive))
											      (data~copy (first (data~appl-arguments atom))
													 :downto '(data+primitive))))
								      (lit~positive-p head-literal))))))
			       (setq back-literals (append back-literals
							   (list (data~copy head-literal :downto '(data+primitive)))))))))
	   (new-just (res~flip-create clause curr-pos))
	   (new-clause (cl~create new-literals :justification new-just)))
      (spass=flip-clause-at-positions new-clause (rest pos-list)))))

(defun spass=parse-free-input-clause-line (input-clause-line)
  (declare (edited  "29-MAY-2000")
	   (authors Ameier)
	   (input   "A string representing a input clause.")
	   (effect  "The variables spass*local-clause-vars and spass*renamed-objects can be changed"
		    "(spass*local-clause-vars is set to nil at the beginning, then new local variables"
		    " are added. For spass*renamed-objects see spass=read-input-clauses.")
	   (value   "The new created clause."))
  
  (setf spass*local-clause-vars nil)

  (let* ((parts (atptop~divide-string input-clause-line #\space))
	 (literals (do* ((rest-parts parts (rest rest-parts))
			 (pos-sign nil)
			 (back-literals nil))
		       ((null rest-parts)
			back-literals)
		     (let* ((head-part (first rest-parts)))
		       (cond ((or (string= head-part "||")
				  (string= head-part ""))
			      nil)
			     ((string= head-part "->")
			      (setf pos-sign 't))
			     (t
			      (setf back-literals
				    (append back-literals 
					    (list (spass=parse-free-literal head-part :polarity pos-sign))))))))))
    (cl~create literals)))

(defun spass=parse-free-literal (literal-string &key (polarity nil))
  (declare (edited  "29-MAY-2000")
	   (authors Ameier)
	   (input   "A string representing a literal.")
	   (effect  "See spass=parse-free-input-clause-line.")
	   (value   "A literal."))
  (let* ((atom (spass=parse-free-term literal-string :predicat 't)))
    (lit~literal-create atom polarity)))


(defun spass=parse-free-term (term-string &key (predicat nil))
   (multiple-value-bind
      (functor-string rest-string)
      (atptop~get-next-word term-string #\()
     ;; reads till a "(" is reached 
     ;; if rest-string = "" (empty) -> term isn't an application -> otherwise term is an application
     
     (if (string= rest-string "")
	 (spass=free-string2object functor-string :predicat predicat :number-of-args 0)
	(let* ((args (mapcar #'(lambda (term-string)
				 (spass=parse-free-term term-string))
			     (spass=parse-term-list (atptop~cut-last-char rest-string))))
	       (functor (spass=free-string2object functor-string :predicat predicat :number-of-args (length args))))
	  (term~appl-create functor args)))))

(defun spass=free-string2object (string &key (predicat nil) (number-of-args 0))
  (declare (edited  "29-MAY-2000")
	   (authors Ameier)
	   (input   "A string, and as keywords predicat (to sign whether the string should be intrpreted as"
		    "predicat or as function) and number-of-args (to sign how many arguments the premitive"
		    "corresponding to the string should have.")
	   (effect  "The spass*renamed-objects and spass*local-clause-vars can be changed:"
		    "1.) If the string starts with a capital letter and it is not alsready conatined in an entry"
		    "    in the spass*local-clause-vars, a new variable with type i is created and a corresponding"
		    "    entry is made in the spass*local-clause-vars."
		    "2.) Othwewise: if the string is not contained in an entry in the spass*renamed-objects, a new"
		    "    constant (whose type depends on the keywords predicat and number-of-args) is created"
		    "    and a corresponding entry is added to spass*renamed-objects.")		    
	   (value   "The object corresponding wrt. spass*renamed-objects or spass*local-clause-vars to the string."))
  (let ((member-convert-list (first (find string spass*renamed-objects
					  :test #'(lambda (string pair)
						    (string= string (second pair))))))
	(member-local-clause (first (find string spass*local-clause-vars
					      :test #'(lambda (string pair) (string= string (second pair))))))
	(env (res~proof-environment spass*current-resolution-problem)))

    (cond ((string-equal string "equal")
	   (env~lookup-object '= env))

	  (member-convert-list
	   ;; -> string is already in spass*renamed-objects -> give back the corresponding object
	   
	   member-convert-list)
	  
	  (member-local-clause
	   ;; -> string is already in spass*local-clause-vars -> return it

	   member-local-clause)

	  (;; string neither in spass*renamed-objects nor spass*local-clause-vars
	   ;; -> create new object and add entry to spass*renamed-objects or spass*local-clause-vars
	   
	   (if (find (char string 0) otter*capital-letters)
	       ;; -> first letter of string is a capital letter
	       ;; -> produce new variable and add it to spass*local-clause-vars
	       
	       (let* ((new-var (term~generate-term-primitive-with-new-name 'orv- (type~i) 'term+variable env)))
		 
		 (setq spass*local-clause-vars (cons (list new-var string) spass*local-clause-vars))
		 
		 new-var)

	     ;; first letter of string is not a capital letter
	     ;; -> produce new constant and add it to spass*renamed-objects
	     
	     (let* ((type (if predicat 
			      (type~predicate-create number-of-args)
			    (type~function-create number-of-args)))
		    (new-constant (term~constant-create string type)))
	       
	       (setq spass*renamed-objects (cons (list new-constant string) spass*renamed-objects))
	       
	       new-constant))))))




