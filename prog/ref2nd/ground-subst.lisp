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




(mod~defmod GSUB 
            :uses (cl data keim lit node pos r2ntop res subst term)
            :documentation "Computing the ground subtsitution of a resolution proof."
            :exports (
                      
                      gsub~check-ground-subst
                      gsub~proof2ground-sub-proof
                      
                      gsub*initial-clause-list))



		      
(defvar gsub*initial-clause-list nil)
;; a list in which triples
;; (new-ground-initial-clause according-original-initial-clause ground-substitution)
;; are stored

(defvar gsub*initial-tnd-clauses nil)
;; a list of clauses of the kind: [+ X, - X], used in the proof to replace equality factoring steps.
;;

(defun gsub~proof2ground-sub-proof (clause curr-ground-sub justification)
  (declare (edited  "22-FEB-1996")
	   (authors Ameier)
	   (input   "A clause, the current-ground-substitution and the"
		    "justification of the clause.")
	   (effect  "If needed new constants are created and added to the environment of"
		    "omega*current-proof-plan.")
	   (value   "The ground-substitution is recursively computed and applied at"
		    "the clause and at the resolution-tree above the"
		    "justification of the clause. If clause is an initial-clause,"
		    "a list of the new-ground-subst-initial-clause and this old"
		    "initial-clause is added to the gsub*initial-clause-list,(who is"
		    "used later to update the extdelta-relation in res2ref)."
		    "The ground-clause with the ground-resolution-tree"
		    "in justification is returned."))
  (let* ((possible-referenzes (keim~get clause 'ground-referenzes)))
    ;; wurde dieser Ast bereits einmal abgearbeitet ?
    ;; -> falls ja, und falls die Klausel eine Unit-Klausel ist: checke zuerst ob eine der bereits erzeugten Grund-Klauseln der
    ;;    Momentanen (mit der momentanen Grund-Substitution) entspricht, wenn dem so ist, muss der gesammte Ast nicht noch einmal
    ;;    abgearbeitet werden, sondern man kann direkt diese bereits erzeugte Grund-Klausel nehmen.
    ;;    falls keine bereits erzeugte Grundklausel mehr passt, muss der Ast neu abgearbeitet werden
    ;;    -> TODO: Lemmas einfuehren !!!
    ;; -> falls diese Klausel keine Unit ist geht es nicht, weil man sonst nicht mehr garantieren kann, dass keine Zykel entstehen !
    ;;    dann muss hier tatsaechlich der gesammte Ast jedesmal verdoppelt werden !!!!!!!!!
    ;;    -> TODO: Lemmas einfuehren !!!
    (if (and possible-referenzes (= (length (cl~literals clause)) 1))
	(let* ((ground-clause (subst~apply curr-ground-sub clause
					   :downto '(data+primitive)
					   :destructive nil
					   :replacers-downto '(data+struct)
					   ))
	       (clause-ref (first (member ground-clause possible-referenzes :test 'gsub=clause-equal))))
	  (if clause-ref
	      clause-ref
	    (let* ((new-ground-instance (gsub=create-new-ground-instance clause curr-ground-sub justification)))
	      (keim~put clause 'ground-referenzes (cons new-ground-instance possible-referenzes))
	      new-ground-instance)))
      (let* ((new-ground-instance (gsub=create-new-ground-instance clause curr-ground-sub justification)))
	(keim~put clause  'ground-referenzes (list new-ground-instance))
	new-ground-instance))))




(defun gsub=create-new-ground-instance (clause curr-ground-sub justification)
  (declare (edited  "17-SEP-1996")
	   (authors Ameier)
	   (input   "A clause, the current ground substitution and the justification of"
		    "the clause.")
	   (effect  "If needed new constants are created and added to the environment of"
		    "omega*current-proof-plan.")
	   (value   "The ground-substitution algorithm is applied recursivvely"
		    "on the parents of the clause. This returns new ground-parents."
		    "With this ground-parents a new justification is created."
		    "The current ground-substitution is apllied on the literals"
		    "of clause and with this literals and the new justification are"
		    "new clause (ground clause) is produced and returned."))
  (let* ((ground-subst-parents (gsub=get-ground-subst-parents (res~justification-parents justification)
							      (res~justification-renamings justification)
							      (res~justification-unifier justification)
							      curr-ground-sub))
	 (positions (res~justification-positions justification))
	 (new-literals (mapcar #'(lambda (x) 
				   (lit~literal-create
				    (keim::subst~apply-separate curr-ground-sub (lit~atom x)
								:downto '(data+primitive)
								:destructive nil
								:replacers-downto '(data+struct)
								;; :test #'(lambda (it1 it2)
								;; 	   (equal (keim~name it1) (keim~name it2)))
								;; AENDERUNG ZUM LADEN ???
								;; Sonst lassen sich wieder geladene Resolutions BEweise nicht
								;; uebersetzen!!!!!!!
								)
				    ;; BITTE IM AUGE BAHLTEN OB DIESES DINGENS FUNKTIONIERT!!!!!!
				    ;; DAS MIT keim::subst~apply-separate !!!!!!!!!!!!!!!!!!!!!!!
				    ;; Problem war folgendes: in multi-arity-test.pos
				    ;; Man braucht zwei Instanzen von [= X-1:aa X-1:aa] einmal mit Typ i einmal mit (i i)
				    ;; wendet man die erste Ground-Substitution an auf [= X-1 X-1]
				    ;; Sagen wir mal {a -> i, X-1 -> c:i}, dann wird das original [= X-1:aa X-1:aa]
				    ;; bei der Ground substitution leider zu [= X-1:i X-1:i], womit sich die zweite
				    ;; Substitution nicht mehr machen laesst!!
				    ;; Da subst~apply-separate sowieso einfach subst~apply aufrauft, wenn keine Variablen vorkommen
				    ;; mit Typvariablen in der Klausel! Das heisst im Prinzip sollte die ganze Sache eh auf
				    ;; Reflexivity Items beschraenkt sein und da scheint sie zu klappen ...
				    (lit~positive-p x)))
			       (cl~literals clause)))
	 (new-just-name (intern (string-upcase (format nil "~A-ground-subst" (keim~name justification)))
				(find-package :omega))))
    (cond ((res~paramodulation-p justification)
	   (gsub=replace-paramodulation clause new-literals
					(first ground-subst-parents) (first positions)
					(second ground-subst-parents) (second positions)))
	  ((res~flip-p justification)
	   (gsub=replace-flip clause new-literals
			      (first ground-subst-parents) (first positions)))
	  ((res~equality-factoring-p justification)
	   (gsub=replace-equality-factoring clause new-literals (first ground-subst-parents))) 
	  ((res~reflex-p justification)
	   (let* ((new-clause (r2ntop~create-reflex-clause new-literals :justification (res~initial-create))))
	     (when res2ref*debug
	       (omega~message "~%~%res2ref*debug: Adding the following triple to gsub*initial-clause-list:")
	       (omega~message "~%     New-Clause: ~A, Old-Clause: ~A, G-Subst: ~A"  new-clause new-clause (subst~create () ())))
	     (setq gsub*initial-clause-list (cons (list new-clause new-clause (subst~create () ()))
						  gsub*initial-clause-list))
	     new-clause))
	  (t
	   (let* ((new-justification (cond ((res~initial-p justification)
					    (res~initial-create))
					   (t
					    (funcall (cond ((res~hyper-resolution-p justification) 'res~hyper-resolution-create)
							   ((res~ur-resolution-p justification) 'res~ur-resolution-create)
							   ((res~resolution-p justification) 'res~resolution-create)
							   ((res~factoring-p justification) 'gsub=factoring-create-spezial)
							   ((res~paramodulation-p justification) 'res~paramod-create-spezial))
						     ground-subst-parents
						     positions
						     (mapcar #'(lambda (parent)
								 (declare (ignore parent))
								 (subst~create () ()))
							     ground-subst-parents)
						     (subst~create () ())
						     new-just-name))))
		  (new-clause (cond ((typep clause 'r2ntop+paramod-clause)
				     (r2ntop~create-paramod-clause new-literals
								   :justification new-justification
								   :paramod-position (keim~copy
										      (r2ntop~paramod-clause-paramod-position clause))
								   )
				     )
				    ((typep clause 'r2ntop+flip-clause)
				     (r2ntop~create-flip-clause new-literals :justification new-justification))
				    ((typep clause 'r2ntop+transitivity-clause)
				     (r2ntop~create-transitivity-clause new-literals :justification new-justification))
				    (t
				     (cl~create new-literals :justification new-justification)))))
	     
	     (when (res~initial-p justification)
	       (let ((checked-ground-subst (gsub~check-ground-subst clause curr-ground-sub)))
		 (when res2ref*debug
		   (omega~message "~%~%res2ref*debug: Adding the following triple to gsub*initial-clause-list:")
		   (omega~message "~%New-Clause: ~A, Old-Clause: ~A, G-Subst: ~A"  new-clause clause checked-ground-subst))
		 (setq gsub*initial-clause-list (cons (list new-clause clause checked-ground-subst)
						      gsub*initial-clause-list))))

	     new-clause)))))

#| ----------------------------------------------------- Auxiliaries --------------------------------------------------- |#

(defun gsub=clause-equal (clause1 clause2)
  (declare (edited  "17-SEP-1996")
	   (authors Ameier)
	   (input   "Two clauses.")
	   (effect  "None.")
	   (value   "T if the clauses are equal in the sence that all"
		    "there literals are keim~equal pair by pair." ))
  (let ((literal-equals-list (mapcar #'(lambda (literal1 literal2)
					 (keim~equal literal1 literal2))
				     (cl~literals clause1) (cl~literals clause2))))
    (if (member nil literal-equals-list)
	nil
      t)))



(defun gsub~check-ground-subst (clause ground-subst)
  (declare (edited  "29-MAY-1996")
	   (authors Ameier)
	   (input   "A clause, and a ground-subst.")
	   (effect  "None.")
	   (value   "The input ground-subst is cleared from double settings,"
		    "and from variables in domain, who doesn't appear in"
		    "clause."))
  (let ((free-variables (remove-duplicates (apply 'append (mapcar #'(lambda (literal)
								      (data~free-variables (lit~atom literal)))
								  (cl~literals clause)))))
	(domain (subst~domain ground-subst))
	(codomain (subst~codomain ground-subst)))
    (do* ((rest-domain domain (rest rest-domain))
	  (rest-codomain codomain (rest rest-codomain))
	  (new-domain)
	  (new-codomain))
	((null rest-domain) (subst~create new-domain new-codomain))
      (let* ((according-free-var  (find (first rest-domain) free-variables
					:test #'keim~equal)
				  ;; #'(lambda (it1 it2)
				  ;;    (equal (keim~name it1) (keim~name it2))))
				  ;; ;; AENDERUNG zum LADEN ???
				  ;; sonst lassen sich wieder reingeladene Res-Proofs nicht uebersetzen!!
				  ))
	(when according-free-var
	  (setq new-domain (cons (first rest-domain) new-domain))
	  ;; (setq new-domain (cons according-free-var new-domain)) ;; AENDERUNG zum LADEN ???
	  (setq new-codomain (cons (first rest-codomain) new-codomain))
	  )))))


(defun gsub=get-ground-subst-parents (list-of-parents list-of-renamings unifier curr-ground-sub)
  (declare (edited  "29-MAY-1996")
	   (authors Ameier)
	   (input   "A list of clauses (the parents), a list of according renamings"
		    "the unifier for all this parents and the current-ground-subst"
		    "for the resulting-clause of this parents.")
	   (effect  "If needed new constant are created and added in the environment"
		    "of omega*current-proof-plan.")
	   (value   "Computes recursive the ground-substituted parents."))
  (let ((ground-unifier (if unifier
			    (gsub=set-substitution-to-ground (subst~compose-substitution curr-ground-sub unifier
											 :downto '(data+primitive)
											 :destructive nil
											 :replacers-downto '(data+struct)
											 ))
			  nil)))
    (do* ((rest-parents list-of-parents (rest rest-parents))
	  (rest-renamings list-of-renamings (rest rest-renamings))
	  (back-parents nil))
	((null rest-parents) back-parents)
      (let ((new-ground-subst (gsub=new-ground-subst curr-ground-sub ground-unifier (first rest-renamings)))
	    (parent (first rest-parents)))
	(setq back-parents (append back-parents
				   (list (gsub~proof2ground-sub-proof parent
								      new-ground-subst
								      (node~justification parent)))))))))

(defun gsub=new-ground-subst (curr-ground-sub ground-unifier renaming)
  (declare (edited  "21-MAR-1996")
	   (authors Ameier)
	   (input   "The current-ground-sub for a clause, the unifier and the renaming"
		    "for a parent of the clause.")
	   (effect  "By using renamings and unifiers the ground-substs for the parent"
		    "are computed, if necessary new constants are created and added"
		    "to the environment of omega*current-proof-plan.")
	   (value   "The new ground-subst for the parent of clause."))
  (declare (ignore curr-ground-sub))
  (let* ((new-ground-subst (subst~compose-substitution ground-unifier renaming
						       :downto '(data+primitive)
						       :destructive nil
						       :replacers-downto '(data+struct)
						       ))
	 (vars (remove-if-not #'(lambda (x)
				  (typep x 'term+variable))
			      (subst~codomain new-ground-subst)))
	 (new-consts (mapcar #'r2ntop~new-const vars))
	 (additional-subst (subst~create vars new-consts)))
    (subst~compose-substitution additional-subst new-ground-subst
				:downto '(data+primitive)
				:destructive nil
				)))

(defun gsub=set-substitution-to-ground (substitution)
  (declare (edited  "02-JUL-1996")
	   (authors Ameier)
	   (input   "A substitution.")
	   (effect  "Possibly new created constants are added in the environment"
		    "of omega*current-proof-plan.")
	   (value   "The substitution is made to a ground Substitution by"
		    "replacing each variable in the codomain by a constant"
		    "and adding these pairs at the substitution too."))
  (let* ((vars (remove-duplicates (remove-if-not #'(lambda (x)
						     (typep x 'term+variable))
						 (apply 'append
							(mapcar #'data~free-variables
								(subst~codomain substitution))))
				  :test 'keim~equal))
	 (new-consts (mapcar #'r2ntop~new-const vars))
	 (additional-subst (subst~create vars new-consts)))
    (subst~compose-substitution additional-subst substitution
				:downto '(data+primitive)
				:destructive nil
				:replacers-downto '(data+struct)
				)))

(defun gsub=factoring-create-spezial (list-parent list-positions list-renaming unifier name)
  (res~factoring-create (first list-parent) list-positions (first list-renaming) unifier name))

#| --------------------------------------------- REPLACE PARAMODULATION ---------------------------------------------- |#

(defun gsub=copy-literal (literal)
  (declare (edited  "13-SEP-1996")
	   (authors Ameier)
	   (input   "A literal.")
	   (effect  "None.")
	   (value   "A literal, which is a copy of the input."))
  (lit~literal-create (lit~atom literal)
		      (lit~positive-p literal)))

(defun gsub=negate-literal (literal)
  (declare (edited  "10-SEP-1996")
	   (authors Ameier)
	   (input   "A literal.")
	   (effect  "None.")
	   (value   "A new literal which same atom as input literal and contrary polarity."))
  (lit~literal-create (lit~atom literal) (not (lit~positive-p literal))))

(defun gsub=replace-paramodulation (clause new-literals
					   ground-mother mother-position
					   ground-father father-position)
  (declare (edited  "13-SEP-1996")
	   (authors Ameier)
	   (input   "A clause with a paramodulation justification, the ground-substituted literals"
		    "of this clause and the ground-substituated mother of the clause and the"
		    "mother position and the ground-substituated father of the clause and the"
		    "father position.")
	   (effect  "The new generated paramod-clause is added to gsub*initial-clause-list.")
	   (value   "There are two cases:"
		    "First: The equation of the paramodulation (from the father-clause) consists of equal terms"
		    "       (of the kind t=t) and this is the only literal of the father clause."
		    "       In this case the ground-substituated mother clause is returned"
		    "Second: The paramodulation is replaced by a resolution/permutation sequenze with"
		    "        a new generated paramod-clause."
		    "        The output is a clause with the input ground-sibstituted literals, in"
		    "        the correct order, but with other justification."))
  (declare (ignore clause))
  (let* ((father-terms (data~appl-arguments (lit~atom (r2ntop~term-at-position (cl~literals ground-father) father-position)))))
    (if (and (cl~unit-p ground-father)
	     (data~equal (first father-terms) (second father-terms)))
	(progn
	  ;;            ground-mother ground-father mother-position)
	  ground-mother)  ;; paramodulation changes nothing -> return ground-mother-clause
      ;; paramodulation changes anything ->
      (let* ((mother-literal-position (pos~list-position (list (pos~first mother-position))))
	     (equation-literal (gsub=copy-literal (r2ntop~term-at-position (cl~literals ground-father) father-position)))
	     (mother-literal (gsub=copy-literal (r2ntop~term-at-position (cl~literals ground-mother) mother-literal-position)))
	     (result-literal (gsub=copy-literal (r2ntop~term-at-position new-literals mother-literal-position)))
	     (paramod-clause (r2ntop~create-paramod-clause (list (gsub=negate-literal mother-literal)
								 (gsub=negate-literal equation-literal)
								 result-literal)
							   :justification (res~initial-create)
							   :paramod-position (pos~rest (pos~rest mother-position))))
	     (resol-mother-paramod (gsub=resolution ground-mother mother-literal-position
						    paramod-clause (pos~list-position (list '0))))
	     (special-position-list (list (- (length (cl~literals resol-mother-paramod)) 2)))
	     (resol-result-father (gsub=resolution resol-mother-paramod (pos~list-position special-position-list)
						   ground-father father-position))
	     (new-justification (r2ntop~make-permutation-literals resol-result-father
								  (pos~list-position special-position-list)
								  mother-literal-position)))
	;; (keim~put paramod-clause 'paramoded-mother resol-result-father)
	;; -> korrekt paramods !!
	
	(setq gsub*initial-clause-list (cons (list paramod-clause paramod-clause (subst~create () ()))
					     gsub*initial-clause-list))
	(cl~create new-literals :justification new-justification)))))


(defun gsub=resolution (clause1 position1 clause2 position2)
  (declare (edited  "10-SEP-1996")
	   (authors Ameier)
	   (input   "A clause a position in this clause ,a second clause and a position in"
		    "this second clause.")
	   (effect  "None.")
	   (value   "A clause ,who is the result of the binary resolution between this two clauses"
		    "at the according position."
		    "Remark: there is no check wether the two literals are contrary !!"))
  (let* ((new-literals (mapcar #'gsub=copy-literal
			       (remove (r2ntop~term-at-position (cl~literals clause1) position1)
				       (remove (r2ntop~term-at-position (cl~literals clause2) position2)
					       (append (cl~literals clause1) (cl~literals clause2))))))
	 (new-just (res~resolution-create (list clause1 clause2)
					  (list position1 position2)
					  (list (subst~create () ()) (subst~create () ()))
					  (subst~create () ())
					  (gensym))))
    (cl~create new-literals :justification new-just)))

#| ------------------------------------------------------ Replace flip -------------------------------------------- |#

(defun gsub=replace-flip (clause new-literals ground-parent flip-position)
  (declare (edited  "13-SEP-1996")
	   (authors Ameier)
	   (input   "A clause, the literals of this clause already ground-substituted,"
		    "The parents of the clause already ground-substituted, and the"
		    "position of the literal who is flipped from the parent.")
	   (effect  "The gsub*initial-clause-list is updated by a new created flipped-clasue.")
	   (value   "A new ground clause, with the same literals as new-liteals,"
		    "but with other justification: The flipped-just is replaced by using the"
		    "flipped-clause [-x=y,y=x] by a resolution/permutation constraint."))
  (declare (ignore clause))
  (let* ((literal-to-flip (r2ntop~term-at-position (cl~literals ground-parent) flip-position))
	 (literal-after-flipping (r2ntop~term-at-position new-literals flip-position))
	 (flip-clause (r2ntop~create-flip-clause (list (gsub=negate-literal literal-to-flip)
							(gsub=copy-literal literal-after-flipping))
						  :justification (res~initial-create)))
	 (clause1 (gsub=resolution ground-parent flip-position flip-clause (pos~list-position (list '0))))
	 (new-justification (r2ntop~make-permutation-literals clause1
							    (pos~list-position (list (- (length new-literals) 1)))
							    flip-position)))
    (setq gsub*initial-clause-list (cons (list flip-clause flip-clause (subst~create () ()))
					 gsub*initial-clause-list))
    (cl~create new-literals :justification new-justification)))

#| ----------------------------------------------------- Replace EQuality Factoring ------------------------------------------ |#

(defun gsub=replace-equality-factoring (clause new-literals ground-parent)
  (declare (edited  "30-OCT-1999")
	   (authors Ameier)
	   (input   "A clause and the list of its ground-substituted literals and the already"
		    "ground substituted parent of the clausse.")
	   (effect  "Introduces (if not already done) the tertium-non-datur axiom and sets the"
		    "r2ntop*equality-factoring entry.")
	   (value   "A clause, consisting of the input literals, but with another justification as the input clause."))


  (omega~message "~%~%FOUND AN EQUALITY FACTORING STEP !!!!!!!")
  
  ;; 1. Falls Tertium-Non-Datur noch nicht im Beweis -> erzeugen:
  (when (null r2ntop*tertium-non-datur)
    (setq r2ntop*tertium-non-datur
	  (pds~add-thy-assertion (ot~read-thy-assumption 'tertium-non-datur)
				 omega*current-proof-plan)))
    
  (let* ((just (node~justification clause))
	 (remaining-position (res~equality-factoring-just-remaining-literal-position just))
	 (positions (res~justification-positions just))
	 (pos1 (first positions))
	 (pos2 (second positions))
	 (last-pos1 (first (last (pos~number-list pos1))))
	 (last-pos2 (first (last (pos~number-list pos2))))
	 (contra-term1-pos (if (= last-pos1 1)
			       (pos~list-position (append (butlast (pos~number-list pos1)) (list 2)))
			     (pos~list-position (append (butlast (pos~number-list pos1)) (list 1)))))
	 (contra-term2-pos (if (= last-pos2 1)
			       (pos~list-position (append (butlast (pos~number-list pos2)) (list 2)))
			     (pos~list-position (append (butlast (pos~number-list pos2)) (list 1)))))
	 (ground-contra-term1 (data~struct-at-position (cl~literals ground-parent) contra-term1-pos))
	 (ground-contra-term2 (data~struct-at-position (cl~literals ground-parent) contra-term2-pos))
	 (tnd-ground-clause (cl~create (list (lit~literal-create (term~appl-create
								  (env~lookup-object '= (pds~environment omega*current-proof-plan))
								  (mapcar #'(lambda (term)
									      (data~copy term :downto '(data+primitive)))
									  (list ground-contra-term1 ground-contra-term2)))
								 't)
					     (lit~literal-create (term~appl-create
								  (env~lookup-object '= (pds~environment omega*current-proof-plan))
								  (mapcar #'(lambda (term)
									      (data~copy term :downto '(data+primitive)))
									  (list ground-contra-term1 ground-contra-term2)))
								 nil))
				       :justification (res~initial-create)))
	 (new-var (term~generate-term-primitive-with-new-name 'x- (type~o) 'term+variable
							      (pds~environment omega*current-proof-plan)))
	 (tnd-nonground-clause (cl~create (list (lit~literal-create new-var 't)
						(lit~literal-create new-var nil))
					  :justification (res~initial-create)))
	 (tnd-ground-subst (subst~create (list new-var) (list (term~appl-create
							       (env~lookup-object '= (pds~environment omega*current-proof-plan))
							       (mapcar #'(lambda (term)
									   (data~copy term :downto '(data+primitive)))
								       (list ground-contra-term1 ground-contra-term2))))))
	 ;; 2. Hiermit haben wir eine neue ground-tnd-clause, eine non-ground-tnd-clause und eine ground-substitution zwischen diesen
	 ;;    beiden berechnet.
	 ;; 3. Jetzt wir die neue clausel berechnet, die der ground-substituierten input clausel entspricht (insbesondre sind die
	 ;;    literale in genau dieser Reihenfolge, bzw. die new-literals werden direkt als Literale benutzt), deren justification
	 ;;    jedoch nicht mehr auf equality-factoring, sondern auf Resolution + factoring beruht.
	 (new-clause (gsub=compute-new-not-equality-factoring-clause new-literals ground-parent tnd-ground-clause just))
	 )

    ;;(format t "~%Contra-term1-pos: ~A" contra-term1-pos)
    ;;(format t "~%Contra-term2-pos: ~A" contra-term2-pos)
    
    (env~remove (keim~name new-var) (pds~environment omega*current-proof-plan))

    (setq gsub*initial-tnd-clauses (append gsub*initial-tnd-clauses (list tnd-nonground-clause)))

    (setq gsub*initial-clause-list (cons (list tnd-ground-clause tnd-nonground-clause tnd-ground-subst)
					 gsub*initial-clause-list))
    
    new-clause))
    

(defun gsub=compute-new-not-equality-factoring-clause (new-literals ground-parent tnd-ground-clause just)
  (declare (edited  "30-OCT-1999")
	   (authors Ameier)
	   (input   "A list of new literals, the ground-parent, the tnd-ground-clause and the"
		    "justification of the original clause.")
	   (effect  "None.")
	   (value   "A new clause, with new literals as literals, created from resolution steps from"
		    "ground-parent and tnd-ground-clause.")) 

  (let* ((positions (res~justification-positions just))
	 (pos1 (first positions))
	 (pos2 (second positions))
	 (pos11 (first (pos~number-list pos1)))
	 (pos21 (first (pos~number-list pos2)))
	 (pos1last (first (last (pos~number-list pos1))))
	 (pos2last (first (last (pos~number-list pos2)))))

    ;; 1. Berechne die Positionen an denen Paramoduliert wird und wie die Literale aussehen, die dabei entstehrn
    ;;    Dies geht so: Falls das 1. Literal uebrig bleibt, paramoduliere im 2. Literal die Gegenseite
    ;;                  Falls das 2. Literal uberig bleibt, paramoduliere im 1. Literal die Gegenseite
    (multiple-value-bind
	(paramod-resulting-ground-literals mother-position father-position)
	(gsub=compute-paramodulation-step-parameters ground-parent tnd-ground-clause just)

      ;; 2. Erzuege damit uber gsub=replace-paramodulation die Clausel, die das Resultat dieser Paramodulation ist
      (let* ((clause1 (gsub=replace-paramodulation nil paramod-resulting-ground-literals
						   ground-parent mother-position 
						   tnd-ground-clause father-position)))

	;; 3. Checke ob ein Flip Schritt vonoeten ist
	;;    Das ist dann der Fall, falls nicht die gleichen Seiten unifiziert wurden
	;;    Dann berechne die Ergebniss literare nach Flippen und die Flip-position (flippe immer das 1. Literal)
	;; 4. Falls notwendig: fuhre Flipping mittels gsub=replace-flip aus -> clause2
	(multiple-value-bind
	    (flip-necessary-flag flip-resulting-ground-literals flip-position)
	    (gsub=compute-flip-step-parameters clause1 just)

	  (let* ((clause2 (if flip-necessary-flag
			      (gsub=replace-flip nil flip-resulting-ground-literals clause1 flip-position)
			    clause1))
		 ;; 5. Erzuege Fac-justification 
		 (fac-justification (res~factoring-create clause2
							  (list (pos~list-position (list pos11))
								(pos~list-position (list pos21)))
							  (subst~create nil nil)
							  (subst~create nil nil))))

	    ;; 6. Erzeuge korrekte Clausel
	    (cl~create new-literals :justification fac-justification)))))))

(defun gsub=compute-paramodulation-step-parameters (ground-parent tnd-ground-clause just)
  (let* ((remaining-position (res~equality-factoring-just-remaining-literal-position just))
	 (positions (res~justification-positions just))
	 (pos1 (first positions))
	 (pos2 (second positions))
	 (number-list1 (pos~number-list pos1))
	 (number-list2 (pos~number-list pos2))
	 (last1 (first (last number-list1)))
	 (last2 (first (last number-list2)))
	 (mother-position (if (equal (first (pos~number-list remaining-position))
				     (first number-list1))
			      ;; -> remaining-literal ist das 1. Literal -> paramoduliere im 2. Literal die Gegenseite
			      (pos~list-position (append (butlast number-list2)
							 (if (equal last2 1)
							     (list 2)
							   (list 1))))
			    ;; -> remaining-literal ist das 2. Literal -> paramoduliere im 1. Literal die Gegenseite
			    (pos~list-position (append (butlast number-list1)
						       (if (equal last1 1)
							   (list 2)
							 (list 1))))))
	 (contra-position (if (equal (first (pos~number-list remaining-position))
				     (first number-list1))
			      ;; -> remaining-literal ist das 1. Literal -> paramoduliere im 2. Literal die Gegenseite
			      ;;                                            mit der Gegenseite des 1. Literals
			      (pos~list-position (append (butlast number-list1)
							 (if (equal last1 1)
							     (list 2)
							   (list 1))))
			    ;; -> remaining-literal ist das 2. Literal -> paramoduliere im 1. Literal die Gegenseite
			    ;;                                            mit der Gegenseite des 2. Literals
			    (pos~list-position (append (butlast number-list2)
						       (if (equal last2 1)
							   (list 2)
							 (list 1))))))
	 (contra-struct (data~struct-at-position (cl~literals ground-parent) contra-position))
	 (father-position (pos~list-position '(0)))
	 (new-literals (mapcar #'(lambda (lit)
				   (data~copy lit :downto '(data+primitive)))
			       (append
				;; Die Mother Literale nach Replacement
				(data~replace-at-position (cl~literals ground-parent) mother-position contra-struct)
				;; Das negations Literal 
				(list (second (cl~literals tnd-ground-clause)))))))

    (values new-literals
	    mother-position
	    father-position)))


			    
(defun gsub=compute-flip-step-parameters (clause just)

  (let* ((positions (res~justification-positions just))
	 (pos1 (first positions))
	 (pos2 (second positions))
	 (remaining-pos (res~equality-factoring-just-remaining-literal-position just))
	 (remaining-number (first (pos~number-list remaining-pos)))
	 (number-list1 (pos~number-list pos1))
	 (number-list2 (pos~number-list pos2))
	 (last1 (first (last number-list1)))
	 (last2 (first (last number-list2))))

    (if (equal last1 last2)
	;; die beiden Seiten auf den unifiziert wurde waren gleich
	;; -> kein Flipping notwendig
	(values nil nil nil)
      ;; die beiden Seiten waren unterschiedlich -> flippe an der Position des Literals, das nicht uebrig bleibt!
      (let* ((flip-position-number (if (equal remaining-number (first number-list1))
				       (first number-list2)
				     (first number-list1)))
	     (literals (cl~literals clause))
	     (flip-position (pos~list-position (list flip-position-number)))
	     (new-literals (do* ((i 0 (+ i 1))
				 (back-literals nil))
			       ((= i (length literals))
				back-literals)
			     (let* ((curr-lit (data~struct-at-position literals (pos~list-position (list i)))))

			       (if (= i flip-position-number)
				   (let* ((flip-atom (lit~atom curr-lit))
					  (new-atom (data~copy (term~appl-create (data~appl-function flip-atom)
										 (reverse (data~appl-arguments flip-atom)))
							       ))
					  (flipped-literal (lit~literal-create new-atom
									       (lit~positive-p curr-lit))))
				     (setq back-literals (append back-literals (list flipped-literal))))
				 (setq back-literals (append back-literals (list (data~copy curr-lit :downto '(data+primitive))))))))))
	
	(values 't
		new-literals
		flip-position)))))

	     

	     
