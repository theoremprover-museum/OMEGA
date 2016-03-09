;;; -*- syntax: common-lisp; package: OMEGA; base: 10; mode: LISP -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     PROVERB Project                                                      ;;
;;     AG Siekmann/FB Informatik                                            ;;
;;     Universitaet des Saarlandes                                          ;;
;;     Bau 36, 4. Stock                                                     ;;
;;     D-66041 Saarbruecken                                                 ;;
;;     Germany                                                              ;;
;;   electronic mail: proverb@cs.uni-sb.de                                  ;;
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


(mod~defmod WALD 
            :uses (atpprb atptop cl data delta env hocnf just keim lit node omega otter p2f pos res sys term)
            :documentation "Handles the use of WALDMEISTER."
            :exports (
                      
                      wald~add-in-string!
                      wald~call-wald
                      wald~complete-wald-problem!
		      wald~generate-wald-problem
                      wald~generate-wald-problem-default!
                      wald~program
		      wald~read-wald-output
		      wald~complete-wald-problem-from-file!
                      ))



(defun wald~program ()
  (let* ((prog (sys~getenv "WALDMEISTERHOME")))
    
    (when (or (null prog) (null (probe-file prog)))
      (error "There is no WALDMEISTER-executable at ~A, please check your path to the WALDMEISTER-executable." prog))
    
    prog))

(defvar wald*convert-counter nil)
;; counter fuer neue Namen

(defvar wald*current-problem nil)
;; momentanes problem

(defvar wald*current-environment nil)
;; momentanes environment

(defvar wald*convert-list nil)
;; die convertierungs liste: liste von paaren von objecten und entsprechenden convertierten names

;; this are the symbols, allowed in a name:
(defvar wald*name-symbols '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L
			   #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X
			   #\Y #\Z #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j
			   #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v
			   #\w #\x #\y #\z #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7
			   #\8 #\9 #\_))

(defvar wald*equality-object nil)
;; zum speichern der Gleichheit

(defvar wald*temporary-variables nil)
;; zum speichern temporaerer Variablen !

(defvar wald*just-counter nil)
;; zum zaehlen neuer Justs

(defvar wald*in-string nil)
;; variable fuer den waldmeister-string !!

(defvar wald*label-clause-list nil)
;; a list to store during reading of proofs pairs of labels and clauses in

(defvar wald*rest-initial-clauses nil)
;; a list to store during reading of proofs the initial clauses not already used in

(defvar wald*local-clause-vars nil)
;; a list to store the local vars of an equation (clause) in

(defvar wald*reflexivity-item nil)
;; to store the reflexivty-item in

(defvar wald*narrow-operator nil)
;; a variable to store the narrow operator in 

(defvar wald*narrow-reflexivity-axiom nil)
;; a variable to store the narrow reflexivity axiom (eq(x,x)=true) in.


#| --------------------------------------------------- READ -> WALDMEISTER.OUT ------------------------------------------------------- |#

(defun wald=read (res-proof wald-out-string parse-back)
  (declare (edited  "14-OCT-1998")
	   (authors Ameier)
	   (input   "The current resolution proof, the waldmeister outstring and a flag signing whether"
		    "the out-string should only used to check whether a proof is found or whether to"
		    "parse back the proof.")
	   (effect  "The resolution proof is changed if the output is parsed.")
	   (value   "T if a proof was found by waldmeister, nil otherwise."))
  
  (omega~message "~% PARSING WALDMEISTER OUTPUT ... ~%")
  
  (let* ((line-strings (remove-if #'(lambda (stringi)
				      (string= stringi ""))
				  (atptop~divide-string wald-out-string #\Newline)))
	 (proof-flag (wald=read-direct line-strings)))
    
    (when (and proof-flag parse-back)
      (wald=read-proof line-strings res-proof)
      )
    
    proof-flag))

(defun wald=read-direct (strings)
  (declare (edited  "14-OCT-1998")
	   (authors Ameier)
	   (input   "The waldmeister output file as a list of strings.")
	   (effect  "None.")
	   (value   "T if a proof is found, this means if the line:"
		    "'Proof:' is contained, nil otherwise."))
  (find "Proof:" strings :test #'string=))

(defun wald=read-proof (line-strings res-proof)
  (declare (edited  "14-OCT-1998")
	   (authors Ameier)
	   (input   "A list of strings representing the output of waldmeister.")
	   (effect  "The resolution proof is updated by new readed steps.")
	   (value   "Undefined."))

  (setq wald*rest-initial-clauses (res~proof-initial-clauses res-proof))
  (setq wald*label-clause-list nil)
  (setq wald*narrow-operator nil)
  (setq wald*narrow-reflexivity-axiom nil)
  
  ;; fuegt narrow-operator ein, sowie true and false in wald*convert-list
  (wald=update-convert-list-by-narrow-op!)

  (multiple-value-bind
      (axioms theorems lemmas end-step)
      (wald=divide-input-line-strings line-strings)

    (mapcar #'wald=read-input-clause
	    (append axioms theorems))

    (let* ((lemma-clauses (progn ;; (format t "~%~% WE HAVE THE FOLLOWING LEMMAS: ~A ~%~%" lemmas)
				 
				 (do* ((rest-lemmas lemmas (rest rest-lemmas))
				       (produced-lemma-clauses nil))
				     ((null rest-lemmas)
				      produced-lemma-clauses)
				   
				   (let* ((new-clause (wald=parse-lemma (first rest-lemmas))))
				     
				     (setq produced-lemma-clauses (append produced-lemma-clauses (list new-clause)))))))
	   (last-lemma (first (last lemma-clauses)))
	   (empty-clause (progn ;; (format t "~%~%~%WE ARE STARTING NOW WITH THE END-STEP:~%~%")
			   
			   (let* ((produced-clause (wald=parse-series end-step)))

			     (if (wald=simple-refutation-p produced-clause)
				 (wald=get-empty-clause-from-simple-refutation! produced-clause res-proof)
			       (wald=parse-end-step end-step produced-clause res-proof))))))
      
      (wald=add-steps-to-proof! empty-clause res-proof)
      
      (mapcar #'(lambda (clause)
		  (keim~remprop clause 'local-clause-vars))
	      (res~proof-clauses res-proof))
      
      empty-clause)))

(defun wald=simple-refutation-p (clause)
  (declare (edited  "18-DEC-1998")
	   (authors Ameier)
	   (input   "A clause.")
	   (effect  "None.")
	   (value   "T if the clause has the form true=false."))
  (let* ((literal-atom-args (data~appl-arguments (first (cl~literals clause))))
	 (first-arg (first literal-atom-args))
	 (second-arg (second literal-atom-args))
	 (false-obj (env~lookup-object 'false wald*current-environment))
	 (true-obj (env~lookup-object 'true wald*current-environment)))
    
    (if (or (and (keim~equal first-arg false-obj)
		 (keim~equal second-arg true-obj))
	    (and (keim~equal first-arg true-obj)
		 (keim~equal second-arg false-obj)))
	't
      nil)))

(defun wald=add-steps-to-proof! (clause res-proof)
  (declare (edited  "27-AUG-1998")
	   (authors Ameier)
	   (input   "A clause and a res-proof.")
	   (effect  "Using the justification all successors of the clause are added to the res-proof"
		    "as long as they are not already in the res-proof.")
	   (value   "Undefined."))

  (if (find clause (res~proof-clauses res-proof))
      nil
    (progn
      (setf (res~proof-clauses res-proof) (cons clause (res~proof-clauses res-proof)))
      (mapcar #'(lambda (parent-clause)
		  (wald=add-steps-to-proof! parent-clause res-proof))
	      (res~justification-parents (node~justification clause))))))


(defun wald=divide-input-line-strings (line-strings)
  (declare (edited  "15-OCT-1998")
	   (authors Ameier)
	   (input   "A list of strings.")
	   (effect  "None.")
	   (value   "Multiple-value:"
		    "First: A list of strings, representing the axioms of the proof."
		    "Second: A list of strings, representing the theorems of the proof."
		    "Third: A list of list of strings, each list representing one lemma."
		    "Fourth: A list of strings, representing the end steps."))
  (let* ((axioms nil)
	 (theorems nil)
	 (lemmas nil)
	 (end-step nil))
    
    (do* ((rest-strings line-strings (rest rest-strings))
	  (lemma-list nil)
	  (mode 'start)
	  )
	((null rest-strings)
	 (values axioms theorems lemmas end-step))
      (let* ((head-string (first rest-strings)))

	(cond ((string= head-string "Consider the following set of axioms:")
	       
	       (setq mode 'axioms)

	       )

	      ((equal mode 'axioms)

	       (if (string= head-string "This theorem holds true:")
		   (setq mode 'theorems)
		 (setq axioms (append axioms (list head-string)))))
	      
	      ((equal mode 'theorems)
	       
	       (if (string= head-string "Proof:")
		   (setq mode 'lemmas)
		 (setq theorems (append theorems (list head-string)))))

	      ((equal mode 'lemmas)

	       (cond ((atptop~string-is-prefix-of-string-p "  Lemma" head-string)
		      
		      (when lemma-list
			(setq lemmas (append lemmas (list lemma-list))))

		      (setq lemma-list (list head-string)))

		     ((atptop~string-is-prefix-of-string-p "  Theorem" head-string)

		      (when lemma-list
			(setq lemmas (append lemmas (list lemma-list))))
		      
		      (setq end-step (list head-string))
		      (setq mode 'end-step))

		     (t

		      (setq lemma-list (append lemma-list (list head-string))))))

	      ((equal mode 'end-step)

	       (setq end-step (append end-step (list head-string))))

	      (t
	       nil))))))

(defun wald=read-input-clause (string-line)
  (declare (edited  "15-OCT-1998")
	   (authors Ameier)
	   (input   "A string-line, representing an input equation.")
	   (effect  "Reads the string as equation clause. Changes the wald*local-clause-vars."
		    "The found clause is compared with the input clauses. The wald*rest-initial-clauses and"
		    "wald*label-clause-list are changed.")
	   (value   "The according input clause."))
  
  (multiple-value-bind
      (label-string equation-string)
      (atptop~get-next-word string-line #\: :ignore-char-list (list #\space))
    
    (let* ((new-clause (wald=read-equation-as-clause string-line))
	   (according-clause (find new-clause wald*rest-initial-clauses
				   :test #'(lambda (clause1 clause2)
					     (atptop~clauses-equal-till-renaming-and-ordering-p clause1 clause2
												:flip 't)))))

      ;; if narrowing is in the proof it can be happens that there is a clause returned of the form
      ;; (narrow-op t1 t2) = true/false
      ;; in this case this clause has no corrspondation in the initial clauses and is as first used directly as
      ;; itself, and is replaced later !!!!!!
      
      (cond (according-clause
	     (multiple-value-bind
		 (flag var-pairs ordering flip)
		 (atptop~clauses-equal-till-renaming-and-ordering-p new-clause according-clause :flip 't)
	       
	       (let* ((back-clause (if (null flip)
				       according-clause
				     (wald=flip-clause according-clause))))
		 
		 (keim~put back-clause 'local-clause-vars (wald=apply-renaming wald*local-clause-vars var-pairs))
		 
		 (setq wald*rest-initial-clauses (remove back-clause wald*rest-initial-clauses))
		 (setq wald*label-clause-list (cons (list label-string back-clause) wald*label-clause-list))
		 
		 back-clause)))
	    (t
	     ;; -> narrowing clause see above !!

	     (cond ((keim~equal (second (data~appl-arguments (lit~atom (first (cl~literals new-clause)))))
	           		(env~lookup-object 'true wald*current-environment)) 
	            
	            ;; reflexivitaets Klausel eq(x1,x1) = true -> type-abschliessen
		    		    
		    (let* ((var-in-orig-parsed (first (data~appl-arguments 
						       (first (data~appl-arguments
							       (lit~atom (first (cl~literals new-clause))))))))
			   (type-of-var (term~type var-in-orig-parsed)))

		      (setf (node~justification new-clause) (res~initial-create (gensym)))

		      ;; Entfaellt ab Version 3.3
		      ;; setzt Kappa-Slot im Reflexivity-Item-ATOM
		      ;; (setf (data~kappa (lit~atom (first (cl~literals new-clause)))) (list type-of-var))
		      
		      ;; Speichern in wald*narrow-reflexivity-axiom
		      (setq wald*narrow-reflexivity-axiom new-clause)
		      
		      (keim~put wald*narrow-reflexivity-axiom 'local-clause-vars wald*local-clause-vars)
		      
		      (setq wald*label-clause-list (cons (list label-string new-clause) wald*label-clause-list))
		      
		      new-clause))
		   
		   ((and (keim~equal (second (data~appl-arguments (lit~atom (first (cl~literals new-clause)))))
				     (env~lookup-object 'false wald*current-environment))
			 (keim~equal (first (data~appl-arguments (lit~atom (first (cl~literals new-clause)))))
				     (env~lookup-object 'true wald*current-environment)))
		    
		    ;; true=false theorem clause -> nix machen 
		    
		    (setq wald*label-clause-list (cons (list label-string new-clause) wald*label-clause-list))
		    
		    new-clause)
		   
		   (t
		    ;; clause der form eq(t1,t2)=false
		    ;; unequality clause, taken from an theorem -> man muss dafuer sorgen, dass die narrow-clause aus genau den termen
		    ;; besteht, aus denen auch auch die original theorem clausel besteht -> sonst spaeter probleme beim ersetzen !!!
		    (let* ((uneq-clause (wald=uneq-clause-to-narrow-clause new-clause))
			   (according-clause (find uneq-clause wald*rest-initial-clauses
						   :test #'(lambda (clause1 clause2)
							     (atptop~clauses-equal-till-renaming-and-ordering-p clause1 clause2
														:flip 't)))))
		      
		      (multiple-value-bind
			  (flag var-pairs ordering flip)
			  (atptop~clauses-equal-till-renaming-and-ordering-p uneq-clause according-clause :flip 't)
			
			(let* ((narrow-clause-with-orig-args (if flip
								 (wald=narrow-clause-to-args
								  (reverse (data~appl-arguments (lit~atom (first (cl~literals
														  according-clause))))))
							       (wald=narrow-clause-to-args
								(data~appl-arguments (lit~atom (first (cl~literals according-clause))))))))
			  
			  (keim~put narrow-clause-with-orig-args 'local-clause-vars (wald=apply-renaming wald*local-clause-vars var-pairs))

			  (setq wald*label-clause-list (cons (list label-string narrow-clause-with-orig-args) wald*label-clause-list))
			  
			  narrow-clause-with-orig-args))))))))))


(defun wald=apply-renaming (local-clause-vars var-renaming)
  (declare (edited  "20-OCT-1998")
	   (authors Ameier)
	   (input   "A local-clause-vars list (a list of pairs of vars and strings) and a renaming"
		    "in form of a substitution or a list of var pairs.")
	   (effect  "None.")
	   (value   "The renaming is applied on the variables of the lical-clause-vars."))
  (let* ((renaming (if (typep var-renaming 'subst+substitution)
		       var-renaming
		     (subst~create (mapcar #'first var-renaming)
				   (mapcar #'second var-renaming)))))
    
    (mapcar #'(lambda (var-string-pair)
		(list (subst~apply renaming (first var-string-pair))
		      (second var-string-pair)))
	    local-clause-vars)))

(defun wald=uneq-clause-to-narrow-clause (clause)
  (declare (edited  "13-NOV-1998")
	   (authors Ameier)
	   (input   "A clause of the form narrow-op(t1,t2)=false.")
	   (effect  "None.")
	   (value   "A clause of the form t1!=t2."))

  (let* ((args-of-left-side (data~appl-arguments (first (data~appl-arguments (lit~atom (first (cl~literals clause))))))))
    (cl~create (list (lit~literal-create (term~appl-create (env~lookup-object '= wald*current-environment)
							   args-of-left-side)
					 nil)))))

(defun wald=narrow-clause-to-args (args)
  (declare (edited  "13-NOV-1998")
	   (authors Ameier)
	   (input   "A list of two terms t1 t2.")
	   (effect  "None.")
	   (value   "A new clause of the form narror-op(t1,t2)=false, t1 and t2 are copied downtp"
		    "term+primitive."))
  (let* ((narrow-op (wald=string2object "eq"))
	 (atom (term~appl-create (env~lookup-object '= wald*current-environment)
				 (list (term~appl-create narrow-op
							 (list (data~copy (first args) :downto '(data+primitive))
							       (data~copy (Second args) :downto '(data+primitive))))
				       (env~lookup-object 'false wald*current-environment)))
	       ))
    (cl~create (list (lit~literal-create atom 't)))))

(defun wald=update-convert-list-by-narrow-op! ()
  (declare (edited  "16-NOV-1998")
	   (authors Ameier)
	   (input   "Nothing.")
	   (effect  "A new narrow operator is produced in inserted with string eq into the"
		    "wald*convert-list, also true and false are inserted into this list with"
		    "according (equal) strings. The global variable wald*narrow-operator"
		    "is set to the new produced narrow-operator.")
	   (value   "Undefined."))
  (let* ((new-type-var (type~variable-create 'aa))
	 (narrow-op (term~generate-term-primitive-with-new-name
		     'narrow-op-
		     (type~func-create (list new-type-var new-type-var)
				       (env~lookup-object 'o wald*current-environment))
		     'term+constant
		     wald*current-environment))
	 (narrow-op-kappa (term~schema-close narrow-op)))
    
    ;; Entfaellt ab Version 3.3
    ;; (setf (data~kappa narrow-op) (list new-type-var))

    (env~remove (keim~name narrow-op) wald*current-environment) 

    (setq wald*narrow-operator narrow-op-kappa)
    
    (setq wald*convert-list (cons (list (env~lookup-object 'false wald*current-environment) "false")
				  (cons (list (env~lookup-object 'true wald*current-environment) "true")
					(cons (list narrow-op "eq") wald*convert-list))))

    narrow-op-kappa
    ))


(defun wald=parse-lemma (lemma-lines)
  (declare (edited  "15-OCT-1998")
	   (authors Ameier)
	   (input   "A list of strings, representing a lemma in a waldmeister proof.")
	   (effect  "None.")
	   (value   "The produced clause."))

  (let* ((lemma-string (first lemma-lines))
	 (produced-clause (wald=parse-series lemma-lines))
	 (needed-clause (wald=read-equation-as-clause lemma-string))
	 (needed-args (data~appl-arguments (lit~atom (first (cl~literals needed-clause)))))
	 (false-obj (env~lookup-object 'false wald*current-environment))
	 (true-obj (env~lookup-object 'true wald*current-environment)))
			

   ;; (if (or (and (keim~equal (first needed-args) false-obj)
   ;;		 (keim~equal (second needed-args) true-obj))
   ;;	    (and (keim~equal (first needed-args) true-obj)
   ;;		 (keim~equal (second needed-args) false-obj)))
   ;;	;; falls needed-clause ist false=true oder umgekehrt
   ;;	;; -> produced clause is +(= false (narrow-op t t)) -> der rest muss im end-step gemacht werden
   ;;	;; -> gib einfach nur die produced clause zurueck !!
   ;;
   ;;	produced-clause
    
    (multiple-value-bind
	(flag var-pairs ordering flip)
	(atptop~clauses-equal-till-renaming-and-ordering-p needed-clause produced-clause)
      
      (when (null flag)
	(error "In function wald=parse-lemma: clauses are not equal-till-renaming."))
      
      (keim~put produced-clause 'local-clause-vars (wald=apply-renaming wald*local-clause-vars var-pairs))
      
      (multiple-value-bind
	  (label-string equation-string)
	  (atptop~get-next-word lemma-string #\: :ignore-char-list (list #\space))
	
	(setq wald*label-clause-list (cons (list label-string produced-clause) wald*label-clause-list))
	
	produced-clause))))


(defun wald=parse-end-step (end-step-lines produced-clause res-proof)
  (declare (edited  "15-OCT-1998")
	   (authors Ameier)
	   (input   "A list of strings, representing the end-steps in a waldmeister proof, the"
		    "clause produced thereby and the resolution proof.")
	   (effect  "None.")
	   (value   "The produced empty clause."))
  
  (let* ((theorem-string (first end-step-lines))
	 (initial-string (third end-step-lines))
	 (produced-clauses (list produced-clause (wald=flip-clause produced-clause))))
    
    (multiple-value-bind
	(used-ax-lem direction position subst)
	(wald=divide-inf-string initial-string)
      
      (multiple-value-bind
	  (theorem-stringi equation-string)
	  (atptop~get-next-word theorem-string #\: :ignore-char-list (list #\space))
	
	(let* ((theorem-clause (wald=label2clause theorem-stringi))
	       (empty-clause (first (apply 'append (mapcar #'(lambda (produced-clause)
							       (res~binary-resolution produced-clause theorem-clause))
							   produced-clauses)))))
	  
	  empty-clause)))))

(defun wald=parse-series (lemma-lines)
  (declare (edited  "15-OCT-1998")
	   (authors Ameier)
	   (input   "A list of strings, representing a series of steps.")
	   (effect  "None.")
	   (value   "The produced clause by this series of steps."))

  ;; -> bei Lemma herstellung
  ;; -> lies linke Seite LS von Lemma und stelle als Anfang eine Reflexivity-clause her mit LS=LS
      
  (let* ((lemma-string (first lemma-lines))		 
	 (equation-clause (wald=read-equation-as-clause lemma-string))
	 (left-side (first (data~appl-arguments (lit~atom (first (cl~literals equation-clause))))))
	 (start-clause (wald=produce-special-ref-clause left-side)))
    
    (do* ((rest-strings (rest (rest lemma-lines)) (rest (rest rest-strings)))
	  (current-mother-local-clause-vars wald*local-clause-vars)
	  (current-mother-clause start-clause))
	((null rest-strings)	     
	 current-mother-clause)
      (let* ((head-string (first rest-strings)))
	
	(multiple-value-bind
	    (used-ax-lem direction position subst)
	    (wald=divide-inf-string head-string)
	  
	  (let* ((father-clause (wald=label2clause used-ax-lem))
		 (father-local-clause-vars (keim~get father-clause 'local-clause-vars)))
	    (multiple-value-bind
		(renamed-father father-renaming)
		(res~separate-clauses current-mother-clause father-clause)
	      (let* ((renamed-father-local-clause-vars (wald=apply-renaming father-local-clause-vars father-renaming)))
		
		(multiple-value-bind
		    (substitution new-mother-local-clause-vars)
		    (wald=read-subst subst
				     current-mother-local-clause-vars
				     renamed-father-local-clause-vars)
		  
		  (let* ((mother-position (wald=compute-position position))
			 (new-clause (wald=make-paramod-steps current-mother-clause
							      mother-position
							      father-clause
							      father-renaming
							      renamed-father
							      substitution
							      direction)))
		    
		    (setq current-mother-clause new-clause)
		    (setq current-mother-local-clause-vars new-mother-local-clause-vars)))))))))))

  
(defun wald=make-paramod-steps (mother-clause
				mother-position
				father-clause
				father-renaming
				renamed-father
				substitution
				direction)
  (declare (edited  "19-OCT-1998")
	   (authors Ameier)
	   (input   "The mother clause and position, the father clause and renaming and the renamed father-clause"
		    ", the substituion and the direction.")
	   (effect  "NOne.")
	   (value   "The new paramod-clause."))
  (let* ((complete-subst (if (eq father-clause wald*narrow-reflexivity-axiom)
			     (wald=complete-substitution substitution)
			   substitution))
	 (father-position (pos~list-position '(0)))
	 (mother-renaming (subst~create nil nil))
	 (new-just (res~paramod-create mother-clause
				       mother-position
				       mother-renaming
				       father-clause
				       father-position
				       father-renaming
				       (if (string= direction "LR")
					   'lr
					 'rl)
				       complete-subst))
	 (new-literal (lit~literal-create
		       (subst~apply complete-subst
				    (lit~atom (first (cl~literals
						      (data~replace-at-position (data~copy mother-clause :downto '(data+primitive))
										mother-position
										(if (string= direction "LR")
										    (second (data~appl-arguments (lit~atom (first (cl~literals renamed-father)))))
										  (first (data~appl-arguments (lit~atom (first (cl~literals renamed-father))))))
										:destructive 't
										:replacers-downto '(data+primitive))))))
		       't))
	 (new-clause (cl~create (list new-literal)
				:justification new-just)))
    
    new-clause))


(defun wald=complete-substitution (subst)
  (declare (edited  "07-JAN-1999")
	   (authors Ameier)
	   (input   "A substitution.")
	   (effect  "None.")
	   (value   "Father is the wald*narrow-reflexivity-axiom"
		    "-> In the substitution has to be inserted a entry for the type-variable"
		    "of the wald*narrow-reflexivity-axiom."))
  
  (let* ((var (first (data~appl-arguments
		      (first (data~appl-arguments (lit~atom (first (cl~literals wald*narrow-reflexivity-axiom))))))))
	 (type-var (term~type var))
	 (domain (subst~domain subst))
	 (codomain (subst~codomain subst))
	 (pair-list (mapcar #'(lambda (domain-term codomain-term)
				(list domain-term codomain-term))
			    domain
			    codomain))
	 (subst-term-of-var (second (assoc var pair-list)))
	 (type-of-subst-term-of-var (term~type subst-term-of-var)))

    (setf (data~annotation var) type-of-subst-term-of-var)
    
    ;;(format t "~%VAR IS: ~A" var)
    ;;(format t "~%TYPE_OF_VAR_IS: ~A" type-var)
    ;;(format t "~%SUBST_TERM IS: ~A" subst-term-of-var)
    ;;(format t "~%TYPE_OF_SUBST: ~A" type-of-subst-term-of-var)
        
    (subst~compose-substitution (subst~create (list type-var) (list type-of-subst-term-of-var))
				subst)))


(defun wald=read-subst (subst-string mother-local-clause-vars father-local-clause-vars)
  (declare (edited  "20-OCT-1998")
	   (authors Ameier)
	   (input   "A string representing a substitution, the mother-local-clause-vars and the father"
		    "local-clause vars.")
	   (effect  "None.")
	   (value   "Multiple-value-bind"
		    "First: The read substitution, the domain is read over the father-local-clause-vars,"
		    "       the codomain is read over the mother-local-clause-vars."
		    "Second: A local-clause-vars list, namely the updated mother-local-clause-vars"
		    "        (changes if new variables are found)."))
  (let* ((string-without-para (atptop~cut-first-char (atptop~cut-last-char subst-string)))
	 (pairs-strings (wald=divide-pairs string-without-para))
	 (divided-pairs (mapcar #'(lambda (pair-string)
				    (let* ((parts (atptop~divide-string-by-lists pair-string (list #\< #\-) (list #\space))))
				      (list (first parts)
					    (fifth parts))))
				pairs-strings))
	 (domain (progn
		   (setq wald*local-clause-vars father-local-clause-vars)
		   (mapcar #'(lambda (pair)
			       (wald=parse-term (first pair)))
			   divided-pairs)))
	 (codomain (progn
		     (setq wald*local-clause-vars mother-local-clause-vars)
		     (mapcar #'(lambda (pair)
				 (wald=parse-term (second pair)))
			     divided-pairs))))

    (values (subst~create domain codomain)
	    wald*local-clause-vars)))

(defun wald=divide-pairs (subst-string)	 
  (declare (edited  "19-OCT-1998")
	   (authors Ameier)
	   (input   "A string representing a substitution.")
	   (effect  "None.")
	   (value   "A list of strings, representing the pairs of a substitution."))

  ;; jedes Komma, das nicht innerhalb von klammern steh ist literal trenner

  (do* ((i 0 (+ i 1))
	(current-string "")
	(open-par 0)
	(pairs nil))
      ((= i (length subst-string))
       (append pairs (list current-string)))
    (let* ((current-char (char subst-string i)))
      (cond ((equal current-char #\()
	     (setq open-par (+ open-par 1))
	     (setq current-string (format nil "~A~A" current-string current-char)))
	    ((equal current-char #\))
	     (setq open-par (- open-par 1))
	     (setq current-string (format nil "~A~A" current-string current-char)))
	    ((and (equal current-char #\,) (= open-par 0))
	     (setq pairs (append pairs (list current-string)))
	     (setq current-string ""))
	    ((and (equal current-char #\,) (null (= open-par 0)))
	     (setq current-string (format nil "~A~A" current-string current-char)))
	    (t
	     (setq current-string (format nil "~A~A" current-string current-char)))))))


(defun wald=divide-inf-string (inf-in-string)
  (declare (edited  "15-OCT-1998")
	   (authors Ameier)
	   (input   "A information string line.")
	   (effect  "None.")
	   (value   "Multiple-value:"
		    "First: the used axiom or lemma."
		    "Second: the direction."
		    "Third: The position"
		    "Fourth: the substitution."))
  (multiple-value-bind
      (prefix inf-string)
      (atptop~get-next-word inf-in-string #\y)

    (let* ((infs (atptop~divide-string inf-string #\space)))

      (values (format nil "~A~A" (second infs) (third infs))
	      (fourth infs)
	      (sixth infs)
	      (do* ((subst-strings (rest (rest (rest (rest (rest (rest (rest infs))))))) (rest subst-strings))
		    (subst-string ""))
		  ((null subst-strings)
		   subst-string)
		(let* ((head-string (first subst-strings)))

		  (setq subst-string (format nil "~A~A" subst-string head-string))))))))


(defun wald=compute-position (pos-string)
  (declare (edited  "20-OCT-1998")
	   (authors Ameier)
	   (input   "A string representing a position.")
	   (effect  "None.")
	   (value   "The position."))
  (let* ((pos-list (atptop~divide-string pos-string #\.)))
    
    (do* ((rest-pos-list pos-list (rest rest-pos-list))
	  (current-pos (pos~list-position '(0 1 2))))
	((null rest-pos-list)
	 current-pos)
      (let* ((next-pos (first rest-pos-list))
	     (number (atptop~parse-number next-pos)))
	
	(setq current-pos (pos~add-end number current-pos))))))

		
(defun wald=flip-clause (clause)
  (declare (edited  "20-OCT-1998")
	   (authors Ameier)
	   (input   "A equation clause.")
	   (effect  "None.")
	   (value   "A clause with flipped literal."))
  (let* ((literals (cl~literals clause))
	 (lit (first literals))
	 (copy-lit (data~copy lit :downto '(term+primitive)))
	 (atom (lit~atom copy-lit))
	 (flipped-lit (lit~literal-create
		       (term~appl-create (data~appl-function atom)
					 (list (cadr (data~appl-arguments atom)) (car (data~appl-arguments atom)))
					 )
		       (lit~positive-p copy-lit))))
    
    (when (> (length literals) 1)
      (error "~% In function wald=flip-clause, clause has more as one literals."))
    
    (cl~create (list flipped-lit)
	       :justification (res~flip-create clause (pos~list-position '(0))))))



(defun wald=read-equation-as-clause (string-line)
  (declare (edited  "14-OCT-1998")
	   (authors Ameier)
	   (input   "A string-line, representing an equation.")
	   (effect  "Changes the wald*local-clause-vars.")
	   (value   "Reads the string as equation clause. The read clause."))
  
  (let* ((pol (if (atptop~string-is-prefix-of-string-p "  Theorem" string-line)
		  nil
		't)))
    
    (multiple-value-bind
	(label-string equation-string)
	(atptop~get-next-word string-line #\: :ignore-char-list (list #\space))
      
      (setq wald*local-clause-vars nil)
      
      (let* ((equation (wald=parse-term equation-string))
	     (new-clause (cl~create (list (lit~literal-create equation pol)))))

	new-clause))))


#| ---------------------------------------------------------- READ-OLD ------------------------------------------------------------- |# 

#|
(defun wald=read-input-clause (string-line)
  (declare (edited  "15-OCT-1998")
	   (authors Ameier)
	   (input   "A string-line, representing an input equation.")
	   (effect  "Reads the string as equation clause. Changes the wald*local-clause-vars."
		    "The found clause is compared with the input clauses. The wald*rest-initial-clauses and"
		    "wald*label-clause-list are changed.")
	   (value   "The according input clause."))

  (multiple-value-bind
      (label-string equation-string)
      (atptop~get-next-word string-line #\: :ignore-char-list (list #\space))
    
    (let* ((new-clause (wald=read-equation-as-clause string-line))
	   (according-clause (find new-clause wald*rest-initial-clauses
				      :test #'(lambda (clause1 clause2)
						(atptop~clauses-equal-till-renaming-and-ordering-p clause1 clause2
												   :flip 't)))))

      (multiple-value-bind
	  (flag var-pairs ordering flip)
	  (atptop~clauses-equal-till-renaming-and-ordering-p new-clause according-clause :flip 't)

	(let* ((back-clause (if (null flip)
				according-clause
			      (wald=flip-clause according-clause))))
	  
	  
	  (setq wald*rest-initial-clauses (remove back-clause wald*rest-initial-clauses))
	  (setq wald*label-clause-list (cons (list label-string back-clause) wald*label-clause-list))
	  
	  back-clause)))))



(defun wald=parse-lemma (lemma-lines)
  (declare (edited  "15-OCT-1998")
	   (authors Ameier)
	   (input   "A list of strings, representing a lemma in a waldmeister proof.")
	   (effect  "None.")
	   (value   "The produced clause."))

  (let* ((lemma-string (first lemma-lines))
	 (produced-clauses (wald=parse-series lemma-lines))
	 (needed-clause (wald=read-equation-as-clause lemma-string))
	 (produced-clause (find needed-clause produced-clauses
				:test #'(lambda (need-clause proc-clause)
					  (term~alpha-match (lit~atom (first (cl~literals proc-clause)))
							    (lit~atom (first (cl~literals need-clause))))))))
    
    (multiple-value-bind
	(label-string equation-string)
	(atptop~get-next-word lemma-string #\: :ignore-char-list (list #\space))

      (setq wald*label-clause-list (cons (list label-string produced-clause) wald*label-clause-list))

      produced-clause)))

(defun wald=parse-end-step (end-step-lines res-proof)
  (declare (edited  "15-OCT-1998")
	   (authors Ameier)
	   (input   "A list of strings, representing the end-steps in a waldmeister proof.")
	   (effect  "None.")
	   (value   "The produced empty clause."))

  (let* ((theorem-string (first end-step-lines))
	 (initial-string (third end-step-lines))
	 (produced-clauses (wald=parse-series end-step-lines)))
    
    (multiple-value-bind
	(used-ax-lem direction position subst)
	(wald=divide-inf-string initial-string)

      (if (string= position "e")
	  
	  (multiple-value-bind
	      (theorem-stringi equation-string)
	      (atptop~get-next-word theorem-string #\: :ignore-char-list (list #\space))
	    
	    (let* ((theorem-clause (wald=label2clause theorem-stringi))
		   (empty-clause (first (apply 'append (mapcar #'(lambda (produced-clause)
								   (res~binary-resolution produced-clause theorem-clause))
							       produced-clauses)))))
	      
	      empty-clause))

	(progn
	  (when (null wald*reflexivity-item)
	    (wald=produce-reflexivity-item res-proof))
	  
	  (let* ((empty-clause (first (apply 'append
					     (mapcar #'(lambda (produced-clause)
							 (res~binary-resolution produced-clause wald*reflexivity-item))
						     produced-clauses)))))
	    
	    empty-clause))))))

(defun wald=parse-series (lemma-lines)
  (declare (edited  "15-OCT-1998")
	   (authors Ameier)
	   (input   "A list of strings, representing a series of steps.")
	   (effect  "None.")
	   (value   "The produced clause by this series of steps."))
  
  (let* ((initial-string (third lemma-lines)))
    
    (multiple-value-bind
	(used-ax-lem direction position subst)
	(wald=divide-inf-string initial-string)
      
      (cond ((string= position "e")
	     
	     ;; -> axiom, lemma holen zum starten 
	     
	     (do* ((rest-strings (rest (rest (rest (rest lemma-lines)))) (rest (rest rest-strings)))
		   (current-clause-list (if (string= direction "LR")
					    (list (wald=label2clause used-ax-lem))
					  (list (wald=flip-clause (wald=label2clause used-ax-lem))))))
		 ((null rest-strings)	     
		  current-clause-list)
	       (let* ((head-string (first rest-strings)))
		 
		 (multiple-value-bind
		     (used-ax-lem direction position subst)
		     (wald=divide-inf-string head-string)
		   
		   (let* ((mother-position (wald=compute-position position))
			  (new-clause-list (apply 'append
						  (mapcar #'(lambda (current-clause)
							      (wald=make-paramod-steps current-clause 
										       (wald=label2clause used-ax-lem)
										       direction))
							  current-clause-list))))
		     (setq current-clause-list new-clause-list))))))
	    ((atptop~string-is-prefix-of-string-p "  Theorem" (first lemma-lines))

	     ;; -> das ganze ist end-step -> mache alles auf theorem selbst
	     ;; -> anschliessend im end-step dann noch ein resolution schritt ueber reflexivity-item
	     
	     (let* ((theorem-string (first lemma-lines)))
	       
	       (multiple-value-bind
		   (theorem-stringi equation-string)
		   (atptop~get-next-word theorem-string #\: :ignore-char-list (list #\space))
		 
		 
		 (do* ((rest-strings (rest (rest lemma-lines)) (rest (rest rest-strings)))
		       (current-clause-list (list (wald=flip-clause (wald=label2clause theorem-stringi)))))
		     ((null rest-strings)	     
		      current-clause-list)
		   (let* ((head-string (first rest-strings)))
		     
		     (multiple-value-bind
			 (used-ax-lem direction position subst)
			 (wald=divide-inf-string head-string)
		       
		       (let* ((mother-position (wald=compute-position position))
			      (new-clause-list (apply 'append
						      (mapcar #'(lambda (current-clause)
								  (wald=make-paramod-steps current-clause 
											   (wald=label2clause used-ax-lem)
											   direction))
							      current-clause-list))))
			 (setq current-clause-list new-clause-list))))))))
	    
	    (t

	     ;; -> bei Lemma herstellung
	     ;; -> lies linke Seite LS von Lemma und stelle als Anfang eine Reflexivity-clause her mit LS=LS

	     (let* ((lemma-string (first lemma-lines))		 
		    (equation-clause (wald=read-equation-as-clause lemma-string))
		    (left-side (first (data~appl-arguments (lit~atom (first (cl~literals equation-clause))))))
		    (start-clause (wald=produce-special-ref-clause left-side)))

	       (do* ((rest-strings (rest (rest lemma-lines)) (rest (rest rest-strings)))
		     (current-clause-list (list start-clause)))
		   ((null rest-strings)	     
		    current-clause-list)
		 (let* ((head-string (first rest-strings)))
		   
		   (multiple-value-bind
		       (used-ax-lem direction position subst)
		       (wald=divide-inf-string head-string)
		     
		     (let* ((mother-position (wald=compute-position position))
			    (new-clause-list  (apply 'append
						     (mapcar #'(lambda (current-clause)
								 (wald=make-paramod-steps current-clause 
											  (wald=label2clause used-ax-lem)
											  direction))
							     current-clause-list))))
		       (setq current-clause-list new-clause-list)))))))))))

(defun wald=make-paramod-steps (mother-clause father-clause direction)
  (declare (edited  "15-OCT-1998")
	   (authors Ameier)
	   (input   "The mother clause, the father clause and the direction.")
	   (effect  "None.")
	   (value   "A list of clauses, that are the results of applying paramodulation of father clause"
		    "to the right side of mother clause using direction."))

  (multiple-value-bind
      (renamed-father-clause father-renaming)
      (res~separate-clauses mother-clause father-clause)
    
    (let* ((father-position (pos~list-position '(0)))
	   (mother-renaming (subst~create nil nil))
	   (eq-args (data~appl-arguments (lit~atom (first (cl~literals renamed-father-clause)))))
	   (part-to-unify (if (string= direction "LR")
			      (first eq-args)
			    (second eq-args)))
	   (part-to-replace (if (string= direction "LR")
				(second eq-args)
			      (first eq-args))))

      (keim::res=unify-equation-part part-to-unify part-to-replace
				     father-clause father-position father-renaming
				     mother-clause mother-renaming (if (string= direction "LR")
								       'lr
								     'rl)
				     (second (data~appl-arguments (lit~atom (first (cl~literals mother-clause)))))
				     (pos~list-position '(0 1 2))))))

|#

#| --------------------------------------------------------- PARSING TOOLS --------------------------------------------------------- |#


(defun wald=label2clause (label)
  (declare (edited  "17-MAY-1996")
	   (authors Ameier)
	   (input   "A label.")
	   (effect  "None.")
	   (value   "If the label corresponds to a clause and is therefor standing in"
		    "the wald*label-clause-list the corresponding clause is returned."
		    "otherwise nil."))
  (second (first (member label wald*label-clause-list
			 :test #'(lambda (label pair)
				   (if (stringp label)
				       (string= label (first pair))
				     (string= (string label) (first pair))))))))
  
(defun wald=parse-term (term-string &key (type nil))
  
  (if (> (atptop~number-of-char-in-string #\= term-string) 0)
      ;; If the term is an equation:
      (let* ((args (atptop~divide-string term-string #\= :ignore-char-list (list #\space)))
	     (awaiting-types (data~n-domain (term~type (data~schema-range (wald=string2object "=")))))
	     (first-arg (wald=parse-term (first args) :type (first awaiting-types)))
	     (second-arg (wald=parse-term (second args) :type (second awaiting-types)))
	     (application (term~appl-create (wald=string2object "=") (list first-arg second-arg)
					    )))
	application)
	
    (multiple-value-bind
	(functor-string rest-string)
	(atptop~get-next-word term-string #\()
      ;; reads till a "(" is reached 
      ;; if rest-string = "" (empty) -> term isn't an application -> otherwise term is an application
      (if (string= rest-string "")
	  (wald=string2object functor-string :type type)
	(let* ((functor (wald=string2object functor-string))
	       (args (mapcar #'(lambda (term-string awaiting-type)
				 (wald=parse-term term-string :type awaiting-type))
			     (wald=parse-term-list (atptop~cut-last-char rest-string))
			     (data~n-domain (term~type (if (term~schema-p functor)
							   (data~schema-range functor)
							 functor))))))
	  (term~appl-create functor args
			    ))))))



(defun wald=parse-term-list (term-list-string)
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

(defun wald=string2object (string &key (type nil))
  (declare (edited  "17-MAY-1996")
	   (authors Ameier)
	   (input   "A string")
	   (effect  "None.")
	   (value   "Everything thats not a constant is interpreted as a variable."
		    "So first is checked whether something is already in the wald*convert-list, if this isn't truth"
		    "it is checked whether it is alrady in the wald*local-clause-vars. If this also does not hold"
		    "a new variable is created and added into the wald*local-clause-vars."))

  (let ((member-convert-list (first (first (member string wald*convert-list
						   :test #'(lambda (string pair) (string= string (second pair))))))))
    
    (if member-convert-list

	;; -> already in convert-list -> reconvert the string to an object
	;; member-convert-list

	(cond ((typep member-convert-list 'term+number)
	       member-convert-list)
	      ((string-equal (keim~name member-convert-list) "=")
	       (env~lookup-object '= wald*current-environment))
	      (t
	       member-convert-list))
      
      ;; -> variable
      (let ((member-local-clause (first (first (member string wald*local-clause-vars
						       :test #'(lambda (string pair) (string= string (second pair))))))))
	(if member-local-clause
	    member-local-clause   ;; -> already a new local clause var produced -> return it
	  ;; not yet a new var produced, produce it , add the pair (new-var "var-string") to  wald*local-clause-vars and
	  ;; return the new-var
	  (let* ((needed-type (cond ((null type)
				     (type~i))
				    ((null (type~variable-p type))
				     type)
				    (t ;; -> type ist type-variable
				     (if (env~lookup-object (keim~name type) wald*current-environment)
					 ;; ist im environment
					 type
				       ;; ist nicht im environment -> Kappa gebunden irgendwo her
				       (let* ((new-symbol (term~generate-new-name 'ntv wald*current-environment))
					      (new-type-var (type~variable-create new-symbol)))
					 (env~enter new-symbol new-type-var wald*current-environment)
					 new-type-var)))))
		 (new-var (term~generate-term-primitive-with-new-name 'orv- needed-type 'term+variable wald*current-environment)))
	    
	    (setq wald*temporary-variables (cons new-var wald*temporary-variables))
	    (setq wald*local-clause-vars (cons (list new-var string) wald*local-clause-vars))
	    
	    new-var))))))

(defun wald=produce-reflexivity-item (res-proof)
  (declare (edited  "27-AUG-1998")
	   (authors Ameier)
	   (input   "The current resolution proof.")
	   (effect  "A new reflexivity clause is produced."
		    "The global varibale wald*reflexivity-item is set on this new clause.")
	   (value   "Undefined.")) 
  
  (let* ((new-type-var (type~variable-create 'aa)) ;; der Name ist hier Scheiss egal !!!!!
	 (new-var (term~generate-term-primitive-with-new-name 'x- new-type-var 'term+variable wald*current-environment))
	 (new-term (term~appl-create wald*equality-object
				     (list new-var new-var)
				     ))
	 (new-clause (cl~create (list (lit~literal-create new-term
							  't))
				:justification (res~reflex-create (gensym)))))
    
    (env~remove (keim~name new-var) wald*current-environment)

    ;; Entfaellt ab VErsion 3.3
    ;; setzt Kappa-Slot im Reflexivity-Item-ATOM
    ;; (setf (data~kappa new-term) (list new-type-var))
        
    (setq wald*reflexivity-item new-clause)))

(defun wald=produce-special-ref-clause (term)
  (declare (edited  "15-OCT-1998")
	   (authors Ameier)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "Creates a new clause with literal term=term and justification reflexivity."))
  (let* ((new-term (term~appl-create wald*equality-object
				     (list (data~copy term :downto '(data+primitive))
					   (data~copy term :downto '(data+primitive)))
				     ))
	 (new-clause (cl~create (list (lit~literal-create new-term
							  't))
				:justification (res~reflex-create (gensym)))))

    new-clause))
    

#| --------------------------------------------- NARROW STUFF ------------------------------------------------ |#

(defun wald=clause-is-narrow-clause-p (clause)
  (declare (edited  "13-NOV-1998")
	   (authors Ameier)
	   (input   "A clause.")
	   (effect  "None.")
	   (value   "T if one of the args of the equality is an application with th narrow-operator as"
		    "function."))
  (let* ((lit-atom (lit~atom (first (cl~literals clause))))
	 (args (data~appl-arguments lit-atom)))

    (if (or (and (data~appl-p (first args))
		 (data~equal (data~appl-function (first args)) wald*narrow-operator))
	    (and (data~appl-p (second args))
		 (data~equal (data~appl-function (second args)) wald*narrow-operator)))
	't
      nil)))


(defun wald=get-empty-clause-from-simple-refutation! (curr-clause res-proof)
  (declare (edited  "06-JAN-1999")
	   (authors Ameier)
	   (input   "A clause thats literal is a simple refutation (true=false or vice versa) and the"
		    "current resolution proof.")
	   (effect  "The steps over this refutation are repaired by removing the occurences of the narrow_op."
		    "The result are two resolvable clauses, whose resolution produces an empty clause."
		    "The steps in the resolution proof can be changed thereby and also the justifications"
		    "in the clauses.")
	   (value   "An empty clause."))
  
  ;; schmeisse diese Klausel raus aus den steps
  (setf (res~proof-clauses res-proof) (remove curr-clause (res~proof-clauses res-proof)))

  ;; (format t "~%~%The justification of this clause is: ~A" (node~justification curr-clause))

  (let* ((just (node~justification curr-clause))
	 (parents (res~justification-parents just))
	 (mother (first parents))
	 (father (second parents)))

    (cond
     ;; Die folgenden beiden Faelle dienen nur dazu unnoetige Schritte am Schluss
     ;; wie das umdrehen von false=true -> true=false und aehnliches wegzuschmeissen

     ((wald=simple-refutation-p mother)
      (wald=get-empty-clause-from-simple-refutation! mother res-proof))
     
     ((wald=simple-refutation-p father)
      (wald=get-empty-clause-from-simple-refutation! father res-proof))

     ((wald=simple-refutation-p curr-clause)

      ;; die Momentane Clausel ist eine simple-refutation, aber keines ihrer Parents ist eine simple-refutation
      ;; -> die Parents der Momentanen Klausel sehen irgendwie so aus:
      ;;        true=eq(t1,t2) (oder umgekehrt rum)
      ;; bzw.   false=eq(t1,t2) (oder umgekehrt rum)
      ;;
      ;; uber beide Aeste muessen Reparatur Zykel laufengelassen werden
      ;; Die Ergebnisse beider Aeste werden schliesslich miteinander resolviert.

      ;; (format t "~%THE PARENTS ARE: MOTHER: ~A, FATHER: ~A" mother father)
      
      (let* ((mother-equation-args (data~appl-arguments (lit~atom (first (cl~literals mother)))))
	     (father-equation-args (data~appl-arguments (lit~atom (first (cl~literals father)))))
	     (false-obj (env~lookup-object 'false wald*current-environment))
	     (false-parent (if (find false-obj mother-equation-args :test #'data~equal)
			       mother
			     father))
	     (true-parent (if (find false-obj mother-equation-args :test #'data~equal)
			      father
			    mother))
	     (false-parent-directly (wald=narrow-clause-is-from-input-directly false-parent))
	     (true-parent-directly (wald=narrow-clause-is-from-input-directly true-parent))
	     (repaired-false-parent (if false-parent-directly
					false-parent-directly
				      (wald=repair-narrow-clause-recursive! false-parent res-proof)))
	     (repaired-true-parent (if true-parent-directly
				       true-parent-directly
				     (wald=repair-narrow-clause-recursive! true-parent res-proof)))
	     ;;(flipped-repaired-true-parent (wald=flip-clause repaired-true-parent))
	     (resolution-directly (res~binary-resolution repaired-false-parent repaired-true-parent))	
	     ;;(resolution-flipped (res~binary-resolution repaired-false-parent flipped-repaired-true-parent)))
	     )
	
	;; es kann sein, dass es Probleme gibt mit flipps -> zur Sicherheit auch noch einmal Resolution mit geflipptem
	;; true-parent -> probleme beim Flippen -> echt alles Scheisse !
	
	(if resolution-directly
	    (first resolution-directly)
	  (first resolution-flipped)))))))

(defun wald=narrow-clause-is-from-input-directly (clause)
  (declare (edited  "08-APR-1999")
	   (authors Ameier)
	   (input   "A clause.")
	   (effect  "NOne.")
	   (value   "If the clause is directly a narrow clause to a clause in the input clause set, this"
		    "clause from the input clause set is returned, otherwise nil."))
  (let* ((false-obj (env~lookup-object 'false wald*current-environment))
	 (atom (lit~atom (first (cl~literals clause))))
	 (clause-left-side (if (data~appl-p atom)
			       (first (data~appl-arguments atom))
			     nil))
	 (args-of-left-side (if (and clause-left-side
				     (data~appl-p clause-left-side)
				     (null (keim~equal clause-left-side false-obj)))
				(data~appl-arguments clause-left-side)
			      nil))
	 (new-uneq-or-eq-clause (if args-of-left-side
				    (cl~create (list (lit~literal-create
						      (term~appl-create (env~lookup-object '= wald*current-environment)
									args-of-left-side)
						      nil)))
				  nil))
	 (according-clause (if new-uneq-or-eq-clause
			       (find new-uneq-or-eq-clause wald*rest-initial-clauses
				     :test #'(lambda (clause1 clause2)
					       (multiple-value-bind
						   (flag var-pairs ordering flip)
						   (atptop~clauses-equal-till-renaming-and-ordering-p clause1 clause2
												      :flip 't)
						 (and flag (every #'(lambda (item)
								      (eq (first item)
									  (second item)))
								  var-pairs))))) ;; Variablen muessen gleich sein
			     nil)))

    according-clause
    ;; DIe narrow-clause ist direkt ein 'input' narrow clause aus einer anderen mitgelieferten clause
    ))


(defun wald=repair-narrow-clause-recursive! (curr-clause res-proof)
  (declare (edited  "17-DEC-1998")
	   (authors Ameier)
	   (input   "A clause containing the narrow_op (true=narrow_op(t1,t2) or false=narrow_op(t1,t2))."
		    "The clauses are repaired by deleting the narrow_op. From a true-clause we get then t1=t2"
		    "and from a false-clause we get t1!=t2. The second argument of the function is the current resolution"
		    "proof.")
	   (effect  "The old-clauses are removed from the steps of the resolution proof, the new ones are inserted.")
	   (value   "Multiple-value:"
		    "First: A clause of the form [- = t1 t2] or [+ = t1 t2] respectively."
		    "Second: A possibly still to apply substitution on this clause."))
  
  (cond ((eq curr-clause wald*narrow-reflexivity-axiom)
	 ;; gib einfach das reflexivity-item zurueck

	 (when (null wald*reflexivity-item)
	   (wald=produce-reflexivity-item res-proof))
	 
	 (values wald*reflexivity-item
		 (subst~create nil nil)))
	
	(t
	 ;; Idee: Narrow-Ketten beginnen immer mit false=false oder true=true
	 ;; -> gehe hoch bis diese Mother Klauses mit einer Reflexiviaets Justification erreicht sind
	 ;; repariere dann abwaerts an ihnen entlang
	 
	 ;; (format t "~%~%JUstification of curr-clause ~A is: ~A" curr-clause (node~justification curr-clause))
	 ;;(format t "~%Thereby the parents are ~A and the unifier is ~A"
	 ;;	 (res~justification-parents (node~justification curr-clause))
	 ;;	 (res~justification-renamings (node~justification curr-clause))
	 ;;	 (res~justification-unifier (node~justification curr-clause)))
		 
	 (let* ((just (node~justification curr-clause))
		(parents (res~justification-parents just))
		(mother (first parents))
		(father (second parents))
		(false-obj (env~lookup-object 'false wald*current-environment))
		(true-obj (env~lookup-object 'false wald*current-environment))
		(curr-clause-equation-args (data~appl-arguments (lit~atom (first (cl~literals curr-clause)))))
		(kind (if (find false-obj curr-clause-equation-args)
			  'false
			'true))
		(father-renaming (second (res~justification-renamings just)))
		(unifier (res~justification-unifier just))
		(mother-just (node~justification mother))
		(father-args (data~appl-arguments (lit~atom (first (cl~literals father))))))
	   
	   (cond ((res~reflex-p mother-just)
		  
		  ;;(format t "~%~% MOTHER-JUST oF MOTHER ~A is REFLEX, FATHER iS ~A ~%~%, RENAMINGS: ~A, UNIFIER: ~A"
		  ;;             mother father (res~justification-renamings just) (res~justification-unifier just))
		  
		  ;; mother ist false=false (falls kind=false oder true=true reflexivitaet fals kind=true
		  ;; father ist dann dementsprechend von der Form +false=narrow-op(t1 t2) oder +true=narrow-op(t1 t2).
		  
		  ;; -> falls father ist +true=narrow-op(x x)
		  ;;    gib reflexivitaets-Axiom x=x als Mother zurueck
		  ;; -> Andernfalls:
		  ;;    Schreibe father als normalse Ungleichung bzw. Gleichung um
		  ;;    -> falls umgeschriebener Father eine Klausel in der Inputmenge
		  ;;       gib diese Klausel als mother zurueck
		  ;;    -> Andernfalls mache auf father weiter mit dem Repairing
		  
		  (if (eq father wald*narrow-reflexivity-axiom)
		      
		      (progn

			;; (format t "~%~% WE ARE HERE!!! FATHER IS wald*narrow-reflexivity-axiom")
			
			(when (null wald*reflexivity-item)
			  (wald=produce-reflexivity-item res-proof))
			
			(let* ((reflexivity-item-variable (first
							   (data~appl-arguments
							    (lit~atom (first (cl~literals wald*reflexivity-item))))))
			       (reflexivity-item-variable-type (term~type reflexivity-item-variable))
			       (narrow-reflexivity-axiom-variable 
				(first (data~appl-arguments
					(first (data~appl-arguments
						(lit~atom (first (cl~literals wald*narrow-reflexivity-axiom))))))))
			       (subst-pairs (mapcar #'(lambda (domain-term codomain-term)
							(list domain-term codomain-term))
						    (subst~domain unifier)
						    (subst~codomain unifier)))
			       (father-renaming-pairs (mapcar #'(lambda (domain-term codomain-term)
							(list domain-term codomain-term))
						    (subst~domain father-renaming)
						    (subst~codomain father-renaming)))
			       (renaming-of-narrow-reflexivity-axiom-variable
				(if (assoc narrow-reflexivity-axiom-variable father-renaming-pairs)
				    (second (assoc narrow-reflexivity-axiom-variable father-renaming-pairs))
				  narrow-reflexivity-axiom-variable))
			       (subst-of-narrow-reflexivity-axiom-variable (second (assoc renaming-of-narrow-reflexivity-axiom-variable
											  subst-pairs)))
			       (new-unifier (subst~create (list reflexivity-item-variable
								reflexivity-item-variable-type)
							  (list subst-of-narrow-reflexivity-axiom-variable
								(term~type subst-of-narrow-reflexivity-axiom-variable)))))
			  			  
			  (values wald*reflexivity-item
				  new-unifier)))
		    
		    (let* ((father-left-side (if (data~appl-p (lit~atom (first (cl~literals father))))
						 (first (data~appl-arguments (lit~atom (first (cl~literals father)))))
					       nil))
			   (args-of-left-side (if (and father-left-side
						       (data~appl-p father-left-side)
						       (null (keim~equal father-left-side false-obj)))
						  (data~appl-arguments father-left-side)
						nil))
			   (new-uneq-or-eq-clause (if args-of-left-side
						      (cl~create (list (lit~literal-create
									(term~appl-create (env~lookup-object '= wald*current-environment)
											  args-of-left-side)
									(if (equal kind 'false)
									    nil
									  't))))
						    nil))
			   (according-clause (if new-uneq-or-eq-clause
						 (find new-uneq-or-eq-clause wald*rest-initial-clauses
						       :test #'(lambda (clause1 clause2)
								 (multiple-value-bind
								     (flag var-pairs ordering flip)
								     (atptop~clauses-equal-till-renaming-and-ordering-p clause1 clause2
															:flip 't)
								   (and flag (every #'(lambda (item)
											(eq (first item)
											    (second item)))
										    var-pairs))))) ;; Variablen muessen gleich sein
					       nil)))
		      
		      (if according-clause
			  ;; -> falls father als Ungleichung bzw. Gleichung in der Input clausel Menge
			  ;;    benutze diese Klausel als mother
			  ;;    beachte, dass moegliche father renamings and unifier als pre-subst mitzurueckgegeben werden muessen
			  (multiple-value-bind
			      (flag var-pairs ordering flip)
			      (atptop~clauses-equal-till-renaming-and-ordering-p new-uneq-or-eq-clause according-clause :flip 't)
			    
			    (let* ((back-clause (if (null flip)
						    according-clause
						  (wald=flip-clause according-clause))))
			      
			      ;; Man beachte: Die Narrow-clause (father) und die tatsaechliche clause (according-clause) muessen in
			      ;; den untertermen uebereinstimmen, damit die Variablen die gleichen sind -> dann kann man alle substitutionen
			      ;; usw. uebernehmen und muss nur die positions anpassen !!!!!!
			      ;; Dies sollte im Normalfall auch so sein, siehe wald=read-input-clause.
			      
			      ;; (format t "~%~%We found the following clause to our narrow-father-clause ~A: ~A" father back-clause) 
			      
			      (values back-clause
				      (subst~compose-substitution unifier father-renaming)
				      )))
			;; -> Andernfalls
			;;    mache auf father weiter mit dem narrow-repairing
			;;    gib die reparierte Klausel zurueck aber integrierre in den pre-subst das father-renaming und den unifier
			;;    dieses Schrittes
			(multiple-value-bind
			    (clause pre-subst)
			    (wald=repair-narrow-clause-recursive! father res-proof)
			  (values clause (subst~compose-substitution unifier
								     (subst~compose-substitution father-renaming pre-subst))))))))
		 
		 
		 (t ;; mother ist gewoehnlicher paramod step in serie !!
		  
		  ;; (format t "~%~% MOTHER-JUST IS STANDARD ~%~%")
		  
		  (multiple-value-bind
		      (new-mother pre-subst)
		      (wald=repair-narrow-clause-recursive! mother res-proof)
		    
		    (let* ((new-clause (wald=repair-narrow-paramodulation-step! new-mother just pre-subst :kind kind)))
		      
		      (values new-clause
			      (subst~create nil nil))))))))))

     
(defun wald=repair-narrow-paramodulation-step! (repaired-mother just pre-subst &key kind)
  (declare (edited  "13-NOV-1998")
	   (authors Ameier)
	   (input   "A clause (a repaired mother clause) and a justification using the narrowed clause to"
		    "this clause as mother. As third a substitution pre-subst that has to be applied"
		    "additionally on the mother-clause and is integrated into the unifier of the justification.")
	   (effect  "The mother-position in the justification is changed in way that it is correct for this"
		    "mother, also the mother itself is changed to the repaired-mother.")
	   (value   "The new clause resulting from applying the corrected justification on the repaired-mother."))

  (wald=repair-mother-position-and-mother! just repaired-mother)

  ;; (format t "~%THE FATHER: ~A" (second (res~justification-parents just)))
  
  (let* ((parents (res~justification-parents just))
	 (positions (res~justification-positions just))
	 (renamings (res~justification-renamings just))
	 (substitution (res~justification-unifier just))
	 (direction (res~paramod-direction just))
	 (mother-clause repaired-mother)
	 (father-clause (second parents))
	 (mother-position (first positions))
	 (father-position (second positions))
	 (mother-renaming (first renamings))
	 (father-renaming (second renamings))
	 (renamed-father (subst~apply father-renaming father-clause))
	 ;; (copied-mother (data~copy mother-clause :downto '(data+primitive)))
	 (mother-with-pre-subst (subst~apply pre-subst mother-clause))
	 ;; Remark: Mother-renaming sollte eigentlich immer {} sein -> daher zu vernachlaessigen
	 (new-literal (lit~literal-create
		       (subst~apply substitution
				    (lit~atom (first (cl~literals
						      (data~replace-at-position (data~copy mother-with-pre-subst
											   :downto '(data+primitive))
										mother-position
										(if (equal direction 'lr)
										    (second (data~appl-arguments (lit~atom (first (cl~literals renamed-father)))))
										  (first (data~appl-arguments (lit~atom (first (cl~literals renamed-father))))))
										:destructive 't
										:replacers-downto '(data+primitive))))))
		       (if (equal kind 'false)
			   nil
			 't)))
	 (new-clause (cl~create (list new-literal)
				:justification just)))
    
    (setf (res~justification-unifier just) (subst~compose-substitution substitution pre-subst))
    
    new-clause))

(defun wald=repair-mother-position-and-mother! (just repaired-mother)
  (declare (edited  "13-NOV-1998")
	   (authors Ameier)
	   (input   "A paramodulation justification.")
	   (effect  "The mother position is changed from (0 1 2 rest) to (0 1 rest), and the mother is changed from"
		    "to repaired-mother.")
	   (value   "The just itself."))
  (when (null (res~paramodulation-p just))
    (error "~% In function wald=repair-mother-position!: Justification has to be of type paramodulation."))

  (let* ((positions (res~justification-positions just))
	 (mother-pos (first positions))
	 (mother-pos-list (pos~number-list mother-pos))
	 (corrected-list (cons '0 (cons '1 (rest (rest (rest mother-pos-list))))))
	 (mother-pos (pos~list-position corrected-list)))

    (when (null (and (= (first mother-pos-list) 0)
		     (= (second mother-pos-list) 1)
		     (= (third mother-pos-list) 2)))
      (error "~% In function wald=repair-mother-position!: mother position has to start with (0 1 2 ...), but is: ~A !!"
	     mother-pos-list))
    
    (setf (res~justification-positions just)
	  (list mother-pos
		(second positions)))
    (setf (res~justification-parents just)
	  (list repaired-mother
		(second (res~justification-parents just))))
    
    just))


#| --------------------------------------------------- PRINT -> WALDMEISTER.IN ------------------------------------------------------- |#

(defun wald=add-string-to-in-string (string)
  (declare (edited  "25-MAR-1998")
	   (authors Ameier)
	   (input   "A string.")
	   (effect  "Adds the string to the wald*in-string.")
	   (value   "Undefined."))
  (setq wald*in-string (format nil "~A~A" wald*in-string string)))

(defun wald=print (clauses command-string)
  (declare (edited  "29-SEP-1998")
	   (authors Ameier)
	   (input   "A set of clauses and a command-string.")
	   (effect  "Writes the waldmeister.in file as string into the wald*in-string variable.")
	   (value   "Undefined."))

  (let* ((name-string (format nil "~A" (keim~name wald*current-problem)))
	 (new-name-string (do* ((i 0 (+ i 1))
				(new-string ""))
			      ((>= i (length name-string))
			       new-string)
			    (when (member (char name-string i) wald*name-symbols)
			      (setq new-string (format nil "~A~A" new-string (char name-string i)))))))
    
    
    (setq wald*in-string "")
    
    (wald=add-string-to-in-string (format nil "NAME         ~A" new-name-string))
    (wald=add-string-to-in-string #\Newline)
    (wald=add-string-to-in-string "MODE         PROOF")
    (wald=add-string-to-in-string #\Newline)
    (wald=add-string-to-in-string "SORTS        ANY")
    (wald=add-string-to-in-string #\Newline)
    (wald=print-signature! clauses)

    ;; check: Ordering ??
    ;; einbau command-string ?
    ;;(if (string= command-string "")
    ;;	(progn
    	  (wald=add-string-to-in-string (format nil "ORDERING     LPO" command-string))
	  

    ;;	  (wald=print-ordering!))
    ;;  (wald=add-string-to-in-string (format nil "ORDERING     ~A" command-string)))
      
    (wald=add-string-to-in-string #\Newline)

    (wald=print-variables!)
    (wald=print-clauses! clauses)
    
    wald*in-string))

(defun wald=print-ordering! ()
  (wald=add-string-to-in-string #\Newline)
  (let* ((all-signiture-pairs (remove-if-not #'(lambda (pair)
						 (and (null (eq (first pair) wald*equality-object))
						      (null (term~variable-p (first pair)))))
					     wald*convert-list))
	 (ordered-pairs (sort all-signiture-pairs #'(lambda (pair1 pair2)
						      (> (length (data~n-domain (term~type (first pair1))))
							 (length (data~n-domain (term~type (first pair2)))))))))
    
    (wald=add-string-to-in-string (format nil "             ~A" (second (first ordered-pairs))))
    (mapcar #'(lambda (pair)
		(wald=add-string-to-in-string (format nil " > ~A" (second pair))))
	    (rest ordered-pairs))
    ;;(wald=add-string-to-in-string #\Newline)
    ))
    
(defun wald=print-signature! (clauses)
  (declare (edited  "29-SEP-1998")
	   (authors Ameier)
	   (input   "A list of clauses.")
	   (effect  "Prints the signature (all function apearing in the clauses with their used"
		    "arity) into yhe wald*in-string.")
	   (value   "Undefined."))
  (wald=add-string-to-in-string "SIGNATURE")
  (wald=add-string-to-in-string #\Newline)

  (let* ((function-pairs (remove-duplicates (apply 'append (mapcar #'(lambda (clause)
								       ;; clauses sind nur noch einliteralige equation clauses
								       (let* ((terms (data~appl-arguments (lit~atom (first (cl~literals clause))))))
									 (append (wald=get-functions-pairs (first terms))
										 (wald=get-functions-pairs (second terms)))))
								   clauses))
					    :test #'(lambda (pair1 pair2)
						      (keim~equal (first pair1) (first pair2)))))
	 (functions (mapcar #'first function-pairs))
	 (function-names (mapcar #'wald=get-checked-name-to-object functions))
	 (arities (mapcar #'second function-pairs)))
    
	 
    (mapcar #'(lambda (name arity)
		(let* ((sort-string (do* ((rest-length arity (- rest-length 1))
					  (curr-string ""))
					((= 0 rest-length)
					 curr-string)
				      (setq curr-string (format nil "ANY ~A" curr-string)))))
		  (wald=add-string-to-in-string (format nil "             ~A: ~A-> ANY" name sort-string))
		  (wald=add-string-to-in-string #\Newline)))
	    function-names arities)))

(defgeneric wald=get-functions-pairs (term)
  (declare (edited  "23-OCT-1998")
	   (authors Ameier)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "A list of pairs of functions used in this term with their arities."))
  (:method ((term term+appl))
	   (cons (list (data~appl-function term) (length (data~appl-arguments term)))
		 (apply 'append (mapcar #'wald=get-functions-pairs (data~appl-arguments term)))))
  (:method ((term term+variable))
	   nil)
  (:method ((term term+constant))
	   (list (list term '0))))
 

(defun wald=print-variables! ()
  (declare (edited  "29-SEP-1998")
	   (authors Ameier)
	   (input   "Nothing.")
	   (effect  "Prints the variables into the wald*in-string.")
	   (value   "Undefined."))
  (wald=add-string-to-in-string "VARIABLES    ")
  (do* ((rest-pairs (remove-if-not #'(lambda (pair)
				       (term~variable-p (first pair)))
				   wald*convert-list)
		    (rest rest-pairs)))
      ((null rest-pairs)
       nil)
    (let* ((pair (first rest-pairs))
	   (object (first pair))
	   (name (second pair)))
      (when (term~variable-p object)
	(if (= (length rest-pairs) 1)
	    (wald=add-string-to-in-string (format nil "~A" name))
	  (wald=add-string-to-in-string (format nil "~A," name))))))
  
  (wald=add-string-to-in-string " : ANY")
  (wald=add-string-to-in-string #\Newline)
  
  wald*in-string)


(defun wald=print-clauses! (clauses)
  (declare (edited  "29-SEP-1998")
	   (authors Ameier)
	   (input   "A list of clauses.")
	   (effect  "Prints the clauses into the wald*in-string.")
	   (value   "Undefined."))
  (let* ((pos-clauses (remove-if-not #'(lambda (clause)
					 (lit~positive-p (first (cl~literals clause))))
				     clauses))
	 (neg-clauses (remove-if #'(lambda (clause)
				     (lit~positive-p (first (cl~literals clause))))
				 clauses)))
    (wald=add-string-to-in-string "EQUATIONS")
    (wald=print-equations! (mapcar #'(lambda (clause)
				       (lit~atom (first (cl~literals clause))))
				   pos-clauses))
    (wald=add-string-to-in-string #\Newline)
    (wald=add-string-to-in-string "CONCLUSION")
    (wald=print-equations! (mapcar #'(lambda (clause)
				       (lit~atom (first (cl~literals clause))))
				   neg-clauses))
    ))

(defun wald=print-equations! (list-of-equations)
  (declare (edited  "29-SEP-1998")
	   (authors Ameier)
	   (input   "A list of equations.")
	   (effect  "Prints the equations into the wald*in-string.")
	   (value   "None."))
  (mapcar #'(lambda (equation)
	      (wald=add-string-to-in-string #\Newline)
	      (wald=add-string-to-in-string "             ")
	      (wald=print-term (first (data~appl-arguments equation)))
	      (wald=add-string-to-in-string " = ")
	      (wald=print-term (second (data~appl-arguments equation))))
	  list-of-equations))

(defgeneric wald=print-term (object)
  (declare (edited  "29-SEP-1998")
	   (authors Ameier)
	   (input   "An term object (REMARK: NO EQUATIONS, BECAUSE EQUATIONS ARE ALREADY ELIMINATED !!.")
	   (effect  "Prints the object into the wald*in-string.")
	   (value   "None."))
  (:method ((term term+appl))
	   (wald=print-term (data~appl-function term))
	   (let* ((args (data~appl-arguments term)))
	     (wald=add-string-to-in-string "(")
	     (wald=print-term (first args))
	     (mapc #'(lambda (term)
		       (wald=add-string-to-in-string ",")
		       (wald=print-term term))
		   (rest args))
	     (wald=add-string-to-in-string ")")))
  (:method ((var term+variable))
	   (wald=add-string-to-in-string (wald=get-checked-name-to-object var)))
  (:method ((const term+constant))
	   (wald=add-string-to-in-string (wald=get-checked-name-to-object const))))
  
  
#| --------------------------------------------- Handling the convertion of names -------------------------------------- |#

#|
   You can't use all symbols in names for predicates, functions and symbols as input for wald. So the existing names
   has to be converted in a wald acceptable form.
   In the variable wald*convert-list a list of pairs (lists) of objects and their converted name-strings is stored
|#

(defun wald=compute-convert-list (clauses)
  (declare (edited  "21-AUG-1997")
	   (authors Ameier)
	   (input   "A list of clauses.")
	   (effect  "All names of Constants and variables of the clauses are converted"
		    "into names, that are usable by wald."
		    "A list of these objects and their corresponding new names (strings)"
		    "is stored in wald*convert-list.")
	   (value   "Undifined."))
  (setq wald*convert-list nil)
  (mapcar #'wald=convert-object clauses))

(defgeneric wald=convert-object (object)
  (declare (edited  "21-AUG-1997")
	   (authors Ameier)
	   (input   "An object.")
	   (effect  "The wald*convert-list is updated. by converting the parts of this"
		    "object.")
	   (value   "Undifined."))
  (:method ((object cl+clause))
	   (mapcar #'wald=convert-object (cl~literals object)))
  (:method ((object lit+literal))
	   (wald=convert-object (lit~atom object)))
  (:method ((object term+appl))
	   (mapcar #'wald=convert-object
		   (cons (data~appl-function object)
			 (data~appl-arguments object))))
  (:method ((object term+primitive))
	   (wald=convert-name object)))

(defun wald=convert-name (object)
  (declare (edited  "14-MAY-1996")
	   (authors Ameier)
	   (input   "An object, that can be of type term+variable, term+constant or term+number.")
	   (effect  "If a new name-string is produced, a pair of old-name-string"
		    "and new-name-string is added to the wald*convert-list.")
	   (value   "From the name is a wald-compatible name produced."
		    "That means all symbols till alphabetics,numbers and _ are"
		    "deleted from the name and a counter-number is added."
		    "If var is set the resulting string is upcased, otherwise"
		    "it is downcased. If the name was attached before the"
		    "before produced new-string is taken from the wald*convert-list"
		    "and is returned otherwise a new string is produced in the way"
		    "descibed before."))
  (if (and (or (keim~equal object wald*equality-object)
	       (and (term~constant-p object)
		    (null (typep object 'term+number))
		    (string= (string (keim~name object)) "=")))
	   (null (find "=" wald*convert-list :test #'(lambda (str pair)
						       (string= str (second pair))))))
      (setq wald*convert-list (cons (list (if (term~schema-p wald*equality-object)
					      (data~schema-range wald*equality-object)
					    wald*equality-object)
					  "=")
				    wald*convert-list))
    
    (let* ((name (keim~name object))
	   (name-string (if (stringp name)
			    name
			  (format nil "~A" name)))
	   ;; Compute whether the element is already in the wald*convert-list
	   ;;(partner-string (second (first (member object wald*convert-list
	   ;;					  :test #'(lambda (thing pair)
	   ;;						    (keim~equal thing (first pair))))))))
	   (partner-string (second (first (member object wald*convert-list
	   					  :test #'(lambda (thing pair)
	   						    (let* ((thing2 (first pair)))
	   						      (or (eq thing thing2)
								  (and (data~equal thing thing2)
								       (data~equal (term~type thing) (term~type thing2)))))))))))
      
      ;; If element already in the wald*convert-list give back the ob-string already used in the wald*cnvert-list
      (if partner-string
	  partner-string
	;; If not a new string is computed and is together with the input object, or, if existing its representation in the
	;; environment, added to the wald*convert-list (list object string).
	;; If polymorphie is used, it is necessary to use the object from the environment.
	;; For example set with Type (aa -> o), but in the application (set a) with a of type i, set is of type (i -> o)
	;; If we would save in the wald*convert-list this set, we couldn't create a term (set 1) with 1 of type num.
	(let ((new-string ""))
	  (do ((i 0 (+ i 1)))
	      ((>= i (length name-string)) nil)
	    (when (member (char name-string i) wald*name-symbols)
	      (setq new-string (format nil "~A~A" new-string (char name-string i)))))
	  (setq new-string (format nil "ob_~A_~A" new-string (incf wald*convert-counter)))
	  (let* ((checked-one (wald=cut-length-20 new-string))
		 (checked-new-string (if (term~variable-p object)
					 (string-upcase checked-one)
				       (string-downcase checked-one)))
		 ;;(env-element (env~lookup-object (intern (string-upcase name-string) (find-package :omega))
		 ;;				 (res~proof-environment wald*current-problem))))
		 )
	    
	    (setq wald*convert-list (cons (list object
						;;(if (and env-element (not (typep object 'term+number)))
						;;    env-element
						;;   object)
						checked-new-string) wald*convert-list))
	    checked-new-string)))))) 

(defun wald=cut-length-20 (stringi)
  (declare (edited  "29-OCT-1998")
	   (authors Ameier)
	   (input   "A string.")
	   (effect  "None.")
	   (value   "Cuts as long chars from the front of the name as the string is bigger then 20."))
  (do* ((current-string stringi (atptop~cut-first-char current-string)))
      ((< (length current-string) 21)
       current-string)))

  
(defun wald=get-checked-name-to-object (object)
  (declare (edited  "22-AUG-1997")
	   (authors Ameier)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "The checked name string to the object."
		    "If the object is in the wald*convert-list, this string is taken"
		    "otherwise the keim~name slot of the object is returned as string."))
  (let* ((name (keim~name object))
	 (name-string (if (stringp name)
			  name
			(format nil "~A" name)))
	 ;; Compute whether the element is already in the wald*convert-list
	 ;;(partner-string (second (first (member object wald*convert-list
	 ;;					:test #'(lambda (thing pair)
	 ;;						  (keim~equal thing (first pair))))))))
	 (partner-string (second (first (member object wald*convert-list
						:test #'(lambda (thing pair)
							  (let* ((thing2 (first pair)))
							    (or (eq thing thing2)
								(and (data~equal thing thing2)
								     (data~equal (term~type thing) (term~type thing2)))))))))))
    
    (if partner-string
	partner-string
      name-string))) 


#| ------------------------------------------------------- AUX ----------------------------------------------------------------------- |#

#|

Entfaellt ab Version 3.3

(defun wald=set-current-type-var-subst! ()
  (declare (edited  "27-MAR-1998")
	   (authors Ameier)
	   (input   "None.")
	   (effect  "If the wald*current-environment contains a type-var-subst, the global"
		    "variable wald*current-type-var-subst is set to this value, otherwise"
		    "a new substitution is created, added with key type-var-subst in the"
		    "environment and wald*current-type-var-subst is set to this new substitution.")
	   (value   "Undefined."))
  (let* ((type-var-subst (env~lookup-object 'type-var-subst wald*current-environment)))
    (if type-var-subst
	(setq wald*current-type-var-subst type-var-subst)
      (let ((new-subst (subst~create nil nil)))
	(setq wald*current-type-var-subst new-subst)
	(env~enter 'type-var-subst new-subst wald*current-environment)))))

|#

#| --------------------------------------------------- CALL WALDMEISTER MAIN --------------------------------------------------------- |#


(defun wald~generate-wald-problem (conclusion-node assumption-nodes ho-pds)
  
  (declare (edited  "26-MAR-1998")
	   (authors Ameier)
	   (input   "A conclusion-node, a set of assumption-nodes and the pds the nodes are from.")
	   (effect  "None.")
	   (value   "A atp-problem with type wald, a partial resolution proof and partial settet"
		    "global vars."
		    "Remark: atp-in-string is NOT-SET, and global vars are not set completly !!"
		    "        As input clauses only such clauses of the CNF are allowed that contain"
		    "        only equality-literals."
		    ))
  
  (setq wald*convert-counter 0)       ;; setzt counter fuer neue Namen auf 0
  
  (let* ((res-proof (atptop~resolution-proof-create-from-nodes conclusion-node assumption-nodes ho-pds)))

    (setq wald*current-problem res-proof)
    (setq wald*current-environment (res~proof-environment res-proof))
    (setq wald*equality-object (env~lookup-object '= wald*current-environment))

    ;; Entfaellt ab Version 3.3
    ;; (wald=set-current-type-var-subst!)   ;; setzt variable wald*current-type-var-subst
   				       
    ;; translate the initial resolution proof res-proof to f.o. and normalize it
    (p2f~translate res-proof)
    (omega~message "~% Normalizing ...")
    (hocnf~normalize-res-proof! res-proof)

    ;; removes all clauses that are not pure unit-equality-clauses
    (atptop~filter-resolution-proof res-proof) 

    (when (null (res~proof-clauses res-proof))
      (omega~message "~%REMARK: This problem seems not to be convinient for WALDMEISTER, because it contains NO unit-equation clauses!"))

    ;; Remove clauses that contain abstractions
    ;; After p2f translation it can happen that there are clauses that contain lambda-abstraction
    ;; Such clauses can not be handled by OTTER (or any f.o. ATP and are therefore removed from the
    ;; clauses list
    (atptop~remove-clauses-with-abstractions! res-proof)
        
    ;; Compute the convertion of names
    (wald=compute-convert-list (res~proof-clauses res-proof))
    
    (atpprb~create-fo-problem (gensym "wald-problem-")
			      'waldmeister
			      nil      ;; wald-in-file kommt erst spaeter dazu: -> wald~add-in-string! 
			      nil
			      res-proof
			      (list wald*convert-list)
			      (list 'p2f p2f*domain p2f*codomain))
    
    ;; in die atpprb~problem-translation-settings kommt eine Liste mit:
    ;; 'p2f p2f*domain p2f*codomain 
    
    ;; in the global-var-list ist folgende Ordnung:
    ;; 1.  otter*convert-list
        
    ;; nicht in der global-var-liste sind andere Dingens, siehe otter !
    ))

(defun wald~complete-wald-problem! (wald-problem &key (parse-back 't))
  (declare (edited  "25-MAR-1998")
	   (authors Ameier)
	   (input   "A wald problem and as keyword parse-back, a flag whether the may found proof"
		    "should be parsed back from the string.")
	   (effect  "The atp-out-string is read and then the resolution proof is parsed"
		    "from this string to complete the partial resolution proof.")
	   (value   "If there was a atp-out-string that represents a resolution proof (if wald has found a"
		    "proof) the complete resolution proof if flag parse-back was T or T if parse-bacl was"
		    "nil. If there is no atp-out-string (wald has failed to find a proof) nil."))
  
  (if (null (atpprb~problem-atp-out-string wald-problem))
      nil
    (let* ((wald-out-string (atpprb~problem-atp-out-string wald-problem))
	   (res-proof (atpprb~problem-part-res-proof wald-problem))
	   (global-vars (atpprb~problem-global-vars wald-problem))
	   (translation-settings (atpprb~problem-translation-settings wald-problem)))

      (setq wald*convert-list (first global-vars))
            
      (setq wald*current-problem res-proof)
      (setq wald*equality-object (env~lookup-object '= (res~proof-environment res-proof)))
      (setq wald*current-environment (res~proof-environment res-proof))

      ;; Entfaellt ab Version 3.3
      ;; (wald=set-current-type-var-subst!)   ;; setzt wald*current-type-var-subst

      (setq wald*temporary-variables nil) ;; setzt temporaeri variablen auf nil
      (setq wald*just-counter 0)      ;; setzt justification counter fuer neue justification auf 0
      
      (setq p2f*domain (second translation-settings))            ;; stellt Uebersetzungsinformation pl2p wieder her
      (setq p2f*codomain (third translation-settings))

      ;; ;; -> beim end-step eventuell reflex-item benoetigt 
      ;; (otter=handle-equality res-proof :insert nil)  ----> handling of REFLEXIVITY !!!! CHECK !!
      
      ;; read waldmeister.out file
      (let* ((proof-flag (wald=read res-proof wald-out-string parse-back)))
	
	;; output
	(if proof-flag
	    (omega~message "~% WALDMEISTER HAS FOUND A PROOF ~%")
	  (omega~message "~% WALDMEISTER HAS FAILED TO FIND A PROOF ~%"))

	;; remove temporary-variables
	(mapcar #'(lambda (var)
		    (env~remove (keim~name var) wald*current-environment))
		wald*temporary-variables)
	
	;; values
	(if proof-flag
	    (if parse-back
		(progn
		  (setf (res~proof-empty-clause res-proof)
			(otter=find-empty-clause (res~proof-step-clauses res-proof)))
		  (setq omega*current-resolution-proof res-proof)
		  (res~add-proof-in-hash-table res-proof)		  
		  (atptop~order-resolution-steps! res-proof)
		  (omega~message "~% OMEGA*CURRENT-RESOLUTION-PROOF IS SET TO THE FOUND RESOLUTION PROOF")
		  res-proof)
	      't)
	  nil))))) 

(defun wald~add-in-string! (wald-problem clauses command-string)
  (declare (edited  "26-MAR-1998")
	   (authors Ameier)
	   (input   "An wald-problem, the clauses and the commadn-string.")
	   (effect  "The wald*in-string is produced and added to the wald-problem.")
	   (value   "Undefined."))

  (let* ((res-proof (atpprb~problem-part-res-proof wald-problem))
	 )
    
    ;; konstruiert das wald.in file im wald*in-string
    (wald=print clauses
		command-string)
    
    ;; setzt atp-in-string im wald-problem
    (setf (atpprb~problem-atp-in-string wald-problem) wald*in-string)
    
    ))

(defun wald~call-wald (open-node ho-pds dir ressource command-string parse-back)
  (declare (edited  "21-AUG-1997")
	   (authors Ameier)
	   (input   "An open node and the according pds, that contains this node. A directory in that"
		    "the new files should stand, the time-ressource, the command-string for settings for wald"
		    "and whether the given proof should be parsed-back or not.")
	   (effect  "None.")
	   (value   "If WALD has found a proof t or the according resolution proof (if parse-back was t)"
		    "nil otherwise"))

  (let* ((problem-name (keim~name ho-pds))
	 (wald-problem-dir (merge-pathnames (format nil "~A/" (string-downcase problem-name)) dir))
	 (in-file (merge-pathnames "waldmeister.in" wald-problem-dir))
	 (out-file (merge-pathnames "waldmeister.out" wald-problem-dir))
	 (wald-problem (wald~generate-wald-problem open-node
						   (remove open-node (pds~node-supports open-node))
						   ho-pds))
	 (res-proof (atpprb~problem-part-res-proof wald-problem)))
    
    ;; erzeugt directory falls es nicht existiert
    (unless (probe-file wald-problem-dir)
      (sys~call-system (format nil "mkdir ~A" (string-right-trim '(#\/) (namestring wald-problem-dir)))))
    
    ;; erzeuge waldmeister.in file in wald*in-string, fuege wald*in-string zum otter-problem hinzu (einschlieslich proof-object)
    (wald~add-in-string! wald-problem
			 (res~proof-clauses res-proof)
			 command-string)
        
    
    ;; call-wald vor Ort -> schreibt wald.out file in den out-string des otter-problems
    (wald=call-wald! wald-problem wald-problem-dir ressource)

    ;; parsen des wald-beweises
    (wald~complete-wald-problem! wald-problem :parse-back parse-back)
    
    ))
;; parsen des wald-beweises
;;(wald~complete-wald-problem! wald-problem :parse-back parse-back)))

    
(defun wald=call-wald! (wald-problem wald-problem-dir ressource)
  (declare (edited  "26-MAR-1998")
	   (authors Ameier)
	   (input   "An wald-problem, the directory for input, output and a time-ressource.")
	   (effect  "Writes the in-string of the wald-problem to the file wald.in in the"
		    "directory, calls wald on it, reads the file wald.out from the directory"
		    "and writes it into the out-string of the wald-problem.")
	   (value   "Undefined."))
  
  (let* ((in-file (merge-pathnames "waldmeister.in" wald-problem-dir))
	 (temp-out-file (merge-pathnames "tmp.out" wald-problem-dir))
	 (out-file (merge-pathnames "waldmeister.out" wald-problem-dir)))

    (atptop~print-string-in-file (atpprb~problem-atp-in-string wald-problem) in-file)

    (let* ((call-flag (atptop~call-with-time-ressource (format nil "~A --auto --details ~A >! ~A;mv ~A ~A &"
							       (wald~program) in-file temp-out-file 
							       temp-out-file out-file)
						       out-file
						       "WALDMEISTER"
						       wald-problem-dir
						       ressource)))      
      
      (if (null call-flag)
	  (omega~message "~% WALDMEISTER was not able to find a proof in the given time ressource. ~%")
	
	;; read waldmeister.out file as string ans set atp-out-string of the wald-problem
	(setf (atpprb~problem-atp-out-string wald-problem)
	      (atptop~read-file-as-string out-file))))))

(defun wald~generate-wald-problem-default! (conclusion-node assumption-nodes ho-pds)
  (declare (edited  "26-MAR-1998")
	   (authors Ameier)
	   (input   "A conclusion-node, a set of assumption-nodes and the pds the nodes are from.")
	   (effect  "The created wald-problem is inserted into the list in the plist entry"
		    "'atp-problems of the conclusion node.")
	   (value   "A atp-problem with type wald, a partial resolution proof."
		    "The global-vars are set completely: proof-object -> t"
		    "The atp-in-string is also already set, using the standart auto-mode"
		    "settings without additional information."))
  (let* ((wald-problem (wald~generate-wald-problem conclusion-node assumption-nodes ho-pds)))
    
    (wald~add-in-string! wald-problem
			(res~proof-clauses (atpprb~problem-part-res-proof wald-problem))
			"")
    
    (keim~put conclusion-node 'atp-problems (cons wald-problem (keim~get conclusion-node 'atp-problems)))
    
    wald-problem))


#| -------------------------------------------------- Read free stuff for WALDMEISTER ----------------------------------------------- |#


(defun wald~read-wald-output (open-node file)
  (declare (edited  "02-JUN-2000")
	   (authors Ameier)
	   (input   "An open node and a file.")
	   (effect  "Mayby changes the waldmeister global variables.")
	   (value   "1. For the open node a new resolution proof is created."
		    "2. The file is tried to read as a waldmeister proof for this resolution proof"
		    "   In particular, a mapping is computed from initial clauses of the waldmeister"
		    "   file and the initial clauses of the new reslution proof."
		    "If it was possible to read the file as a waldmeister proof file, the resolution"
		    "proof is completed (an empty clauses is dreived according to the proof in"
		    "the file) and this complete resolution proof is returned."
		    "If it was not possible to read the file as a waldmeister proof file nil is"
		    "returned."))
  (let* ((waldmeister-problem (wald~generate-wald-problem open-node
								 (remove open-node (pds~node-supports open-node))
								 omega*current-proof-plan))
	 (res-proof (wald~complete-wald-problem-from-file! waldmeister-problem file)))
    
    res-proof))

(defun wald~complete-wald-problem-from-file! (atp-problem file)
  (let* ((out-string (atptop~read-file-as-string file))
	 (res-proof (atpprb~problem-part-res-proof atp-problem))
	 (global-vars (atpprb~problem-global-vars atp-problem)))
    
    ;; was ist mit translation settings? -> braucht man zumindest nicht fuer reine first order probleme

    (setf (atpprb~problem-atp-out-string atp-problem) out-string)

    ;; Note: in the atp-problem we have in the global-var-list:
    ;; 1.  wald*convert-list       -> we have to reconstruct by matching the clauses!
        
    (setq wald*convert-list (wald=reconstruct-convert-list res-proof out-string))
       
    ;; Other necessary settings
    (setq wald*current-problem res-proof)
    (setq wald*equality-object (env~lookup-object '= (res~proof-environment res-proof)))
    (setq wald*current-environment (res~proof-environment res-proof))
    (setq wald*temporary-variables nil)                      ;; setzt temporaeri variablen auf nil
    (setq wald*just-counter 0)
    
    (if (null wald*convert-list)
	(progn
	  (omega~message "~% Could not match the out-file to the problem.")
	  nil)
      (let* ((proof-flag (wald=read res-proof out-string 't)))

	;; remove temporary-variables
	(mapcar #'(lambda (var)
		    (env~remove (keim~name var) wald*current-environment))
		wald*temporary-variables)
	
	(if proof-flag
	    (progn
	      (omega~message "~% WALDMEISTER HAS FOUND A PROOF ~%")
	      (setf (res~proof-empty-clause res-proof)
		    (otter=find-empty-clause (res~proof-step-clauses res-proof)))
	      (setq omega*current-resolution-proof res-proof)
	      (res~add-proof-in-hash-table res-proof)		  
	      (atptop~order-resolution-steps! res-proof)
	      (omega~message "~% OMEGA*CURRENT-RESOLUTION-PROOF IS SET TO THE FOUND RESOLUTION PROOF")
	      res-proof)
	  (progn
	    (omega~message "~% WALDMEISTER HAS FAILED TO FIND A PROOF ~%")
	    nil))))))


;; NOTICE: IN CONTRAST TO THE FULL F.O. PROVERS LIKE OTTER,BLIKSEM,...
;; WALDMEISTER AND EQP HANDLE ONLY PURE EQUALITY CLAUSES! CONSISTING ONLY OF ONE EQUATION!
;; HEMCE, FOR WALDMEISTER AND EQP WE DON'T HAVE THE PROBLEM THAT IN THE OUTPUT THE ORDER OF THE
;; LITERALS IS MAYBY CHANGED! SO, WE DO NOT NEED TO REORDER THE LITERALS!
(defun wald=reconstruct-convert-list (res-proof out-string)
  (declare (edited  "29-MAY-2000")
	   (authors Ameier)
	   (input   "The current resolution proof and the out-string of a waldmeister call.")
	   (effect  "The content of wald*convert-list and wald*local-clause-vars can be changed.")
	   (value   "First the input clauses of the proof in the out-string are read. Thereby, the new"
		    "created constants have as names exactly the names they have in the out-file."
		    "Then these read clauses are matched against the input clauses in the resolution proof."
		    "This results (if successfull) in a mapping, that mapps each constant in the"
		    "read clauses to a constant in the input clauses. From this mapping we compute"
		    "a list of pairs of constants and strings (where the constant is taken from the"
		    "new resolutiomn proof whereas the string is from the out-file."))

  (let* ((res-proof-input-clauses (res~proof-clauses res-proof))
	 (out-file-input-clauses (wald=read-input-clauses out-string)))
    
    (multiple-value-bind
	(success mapping clauses-pairs)
	(atptop~subset-by-equality-except-names-p out-file-input-clauses (append res-proof-input-clauses
										 (wald=complete-flipping res-proof-input-clauses)))
      
      (if success
	  (let* ((mapp-domain (mapp~domain mapping))
		 (mapp-codomain (mapp~codomain mapping))
		 (mapp-constant-domain (remove-if-not #'term~constant-p mapp-domain))
		 (mapp-constant-codomain (remove-if-not #'term~constant-p mapp-codomain)))
	    
	    (mapcar #'(lambda (dom codom)
			(let* ((name (keim~name dom)))
			  (list codom (if (stringp name)
					  name
					(string name)))))
		    mapp-constant-domain mapp-constant-codomain))
	nil))))

(defun wald=complete-flipping (clause-list)
  (apply #'append (mapcar #'blik=complete-flipping clause-list)))

(defun wald=read-input-clauses (out-string)
  (declare (edited  "29-MAY-2000")
	   (authors Ameier)
	   (input   "A waldmeister outfile.")
	   (effect  "During the parsing of the input clauses (see value) the wald*convert-list is changed.")
	   (value   "Reads the input clauses, without a convertion setting."
		    "Thereby, for each name that starts with a capital letter a new variable of type i is produced, and"
		    "for each other name a constant of type (i <- ...) is produced."))
  
  (setf wald*convert-list nil)
  
  (let* ((line-strings (atptop~divide-string out-string #\Newline)))

    (multiple-value-bind
	(initial-equation-lines goal-equation-lines)
	(wald=get-initial-and-goal-lines line-strings)
      
      ;; Note:
      ;; If a input clause is parsed, each name that starts with x,y,z is interpreted as variable!
      ;; -> a new local variable of type i is created (local = relevant only for the clause itself) 
      ;; Each other letter is interpreted as a constant. Hence, a new constant is created. The type of the constant
      ;; is (o <- i ...) or (i <- i ...) depending on whether the constant is the predicat of a literal or internal
      ;; and on the number of the arguments on which it is applied.
      ;; Since constants are not local for a clause, for each new created constant a new entry is made in the
      ;; wald*convert-list

      (append (mapcar #'(lambda (initial-line)
			  (wald=parse-free-input-clause-line initial-line :polarity 't))
		      initial-equation-lines)
	      (mapcar #'(lambda (goal-line)
			  (wald=parse-free-input-clause-line goal-line :polarity nil))
		      goal-equation-lines)))))
	      
(defun wald=get-initial-and-goal-lines (line-strings)
  (declare (edited  "01-JUN-2000")
	   (authors Ameier)
	   (input   "A list of strings, the lines of the waldmeister out-file.")
	   (effect  "None.")
	   (value   "Multiple-value:"
		    "First: A list of all lines under initial equations."
		    "Second: A list of all lines under goal equations."))
  (do* ((rest-string-lines line-strings (rest rest-string-lines))
	(current-reader nil)
	(break-flag nil)
	(initial-list nil)
	(goal-list nil))
      ((or (null rest-string-lines)
	   break-flag)
       (values initial-list
	       goal-list))
    (let* ((string-line (first rest-string-lines)))
      (cond ((string= string-line "Initial equations:")
	     (setf current-reader 'initial)
	     (setf rest-string-lines (rest (rest rest-string-lines))))
	    ((string= string-line "Goals:")
	     (setf current-reader 'goal)
	     (setf rest-string-lines (rest (rest rest-string-lines))))
	    ((and (string= string-line "")
		  (equal current-reader 'goal))
	     (setf break-flag 't))
	    ((and (string= string-line "")
		  (equal current-reader 'initial))
	     nil)
	    ((equal current-reader 'goal)
	     (setf goal-list (append goal-list (list string-line))))
	    ((equal current-reader 'initial)
	     (setf initial-list (append initial-list (list string-line))))
	    (t
	     nil)))))
    

(defun wald=parse-free-input-clause-line (input-clause-line &key (polarity nil))
  (declare (edited  "29-MAY-2000")
	   (authors Ameier)
	   (input   "A string representing a input clause and as keyword a polarity for the resulting clause"
		    "the clause consists only of one literal, the polarity determines the polarity of this"
		    "literal).")
	   (effect  "The variables wald*local-clause-vars and wald*convert-list can be changed"
		    "(wald*local-clause-vars is set to nil at the beginning, then new local variables"
		    " are added. For wald*convert-list see wald=read-input-clauses.")
	   (value   "The new clause."))
  
  (setf wald*local-clause-vars nil)

  (let* ((cleared-clause-string (atptop~filter-chars
				 (atptop~cut-x-first-chars input-clause-line 6)
				 :ignore-char-list (list #\space #\?)))
	 ;; cuts the first six letter which are counters like "(   1)" and removes all spaces
	 (literal (wald=parse-free-literal cleared-clause-string :polarity polarity)))
    (cl~create (list literal))))

(defun wald=parse-free-literal (literal-string &key (polarity nil))
  (declare (edited  "29-MAY-2000")
	   (authors Ameier)
	   (input   "A string representing a literal and as keyword the polarity of the literal.")
	   (effect  "See wald=parse-free-input-clause-line.")
	   (value   "A literal."))
  (if polarity
      (lit~literal-create (wald=parse-free-term literal-string) 't)
    (lit~literal-create (wald=parse-free-term literal-string) nil)))

(defun wald=parse-free-term (term-string)
  (if (> (atptop~number-of-char-in-string #\= term-string) 0)
      ;; If the term is an equation:
      (let* ((args (atptop~divide-string term-string #\= :ignore-char-list (list #\space)))
	     (first-arg (wald=parse-free-term (first args)))
	     (second-arg (wald=parse-free-term (second args)))
	     (application (term~appl-create (wald=free-string2object "=") (list first-arg second-arg))))
	application)
    ;; if the term is not an equation
    (multiple-value-bind
	(functor-string rest-string)
	(atptop~get-next-word term-string #\()
      ;; reads till a "(" is reached 
      ;; if rest-string = "" (empty) -> term isn't an application -> otherwise term is an application
      
      (if (string= rest-string "")
	  (wald=free-string2object functor-string :predicat nil :number-of-args 0)
	(let* ((args (mapcar #'(lambda (term-string)
				 (wald=parse-free-term term-string))
			     (wald=parse-term-list (atptop~cut-last-char rest-string))))
	       (functor (wald=free-string2object functor-string :predicat nil :number-of-args (length args))))
	  (term~appl-create functor args))))))

(defun wald=free-string2object (string &key (predicat nil) (number-of-args 0))
  (declare (edited  "29-MAY-2000")
	   (authors Ameier)
	   (input   "A string, and as keywords predicat (to sign whether the string should be intrpreted as"
		    "predicat or as function) and number-of-args (to sign how many arguments the premitive"
		    "corresponding to the string should have.")
	   (effect  "The wald*convert-list and wald*local-clause-vars can be changed:"
		    "1.) If the string starts with x,y,zand it is not alsready conatined in an entry"
		    "    in the wald*local-clause-vars, a new variable with type i is created and a corresponding"
		    "    entry is made in the wald*local-clause-vars."
		    "2.) Othwewise: if the string is not contained in an entry in the wald*convert-list, a new"
		    "    constant (whose type depends on the keywords predicat and number-of-args) is created"
		    "    and a corresponding entry is added to wald*convert-list.")		    
	   (value   "The object corresponding wrt. wald*convert-list or wald*local-clause-vars to the string."))
  (let ((member-convert-list (first (find string wald*convert-list
					  :test #'(lambda (string pair)
						    (string= string (second pair))))))
	(member-local-clause (first (find string wald*local-clause-vars
					      :test #'(lambda (string pair) (string= string (second pair)))))))

    (cond ((string-equal string "=")
	   (env~lookup-object '= wald*current-environment))

	  (member-convert-list
	   ;; -> string is already in wald*convert-list -> give back the corresponding object
	   
	   member-convert-list)
	  
	  (member-local-clause
	   ;; -> string is already in wald*local-clause-vars -> return it

	   member-local-clause)

	  (;; string neither in wald*convert-list nor wald*local-clause-vars
	   ;; -> create new object and add entry to wald*convert-list or wald*local-clause-vars
	   
	   (if (find (char string 0) (list #\x #\y #\z))
	       ;; -> first letter of string is x,y,z
	       ;; -> produce new variable and add it to wald*local-clause-vars
	       
	       (let* ((new-var (term~generate-term-primitive-with-new-name 'orv- (type~i) 'term+variable wald*current-environment)))
		 
		 (setq wald*temporary-variables (cons new-var wald*temporary-variables))
		 (setq wald*local-clause-vars (cons (list new-var string) wald*local-clause-vars))
		 
		 new-var)
	     
	     ;; first letter of string is not x,y,z
	     ;; -> produce new constant and add it to wald*convert-list
	     
	     (let* ((type (if predicat 
			      (type~predicate-create number-of-args)
			    (type~function-create number-of-args)))
		    (new-constant (term~constant-create string type)))
	       
	       (setq wald*convert-list (cons (list new-constant string) wald*convert-list))
	       
	       new-constant))))))
