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



(mod~defmod BLIK 
            :uses (atpprb atptop cl data env hocnf just keim lit node omega p2f res subst sys term)
            :documentation "Handling call of BLIKSEM"
            :exports (
                      
                      blik~add-in-string!
                      blik~call-bliksem
                      blik~complete-bliksem-problem!
                      blik~generate-bliksem-problem
		      blik~generate-bliksem-problem-default!
                      blik~program
		      blik~read-bliksem-output
		      blik~complete-bliksem-problem-from-file!
                      
                      blik*convert-counter
                      blik*convert-list
                      blik*current-environment
                      blik*current-problem
		      blik*equality-object
                      blik*in-string
                      blik*name-symbols))


(defun blik~program ()
  (let* ((prog (sys~getenv "BLIKSEMHOME")))

    (when (or (null prog) (null (probe-file prog)))
      (error "There is no bliksem-executable at ~A, please check your path to the bliksem-executable." prog))

    prog))

(defvar blik*convert-list nil) ;; stores the list of pairs of objects and converted names

(defvar blik*equality-object nil) ;; stores the current equality object

(defvar blik*name-symbols '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L
			    #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X
			    #\Y #\Z #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j
			    #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v
			    #\w #\x #\y #\z #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7
			    #\8 #\9 #\_)) ;; the allowed symbols in names

(defvar blik*convert-counter nil) ;; the counter for conversation names

(defvar blik*current-problem nil) ;; the current problem

(defvar blik*current-environment nil) ;; the current environment

(defvar blik*in-string nil) ;; to store the blik in-string in

(defvar blik*number-clause-list nil) ;; a list to store pairs of clauses and their corresponding numbers in the proof
                                     ;; gebraucht nur beim lesen des BEweises

(defvar blik*rest-initial-clauses nil) ;; a list to store always the initial clauses in, that are not already used in a proof
                                       ;; ebenfalls nur beim lesen eines Beweises benoetigt

(defvar blik*local-clause-vars nil) ;; a list to store local variables in, created during parsing of clauses
                                    ;; ebenfalls nur beim lesen eines Beweises benoetigt

(defvar blik*temporary-variables nil) ;; a list to store temporaery used variables in, to remove them later again from the environment
                                      ;; ebenfalls nur beim lesen eines Beweises benoetigt

(defvar blik*reflexivity-item nil) ;; if the rule Equality-Reflexivty is applied by BLIKSEM the new clause: { = x x } added to the
                                   ;; current resolution proof
                                   ;; ebenfalls nur beim lesen eines Beweises benoetigt

#| --------------------------------------------- Handling the convertion of names -------------------------------------- |#

#|
   You can't use all symbols in names for predicates, functions and symbols as input for Bliksem. So the existing names
   has to be converted in a bliksem acceptable form.
   In the variable blik*convert-list a list of pairs (lists) of objects and their converted name-strings is stored
|#

(defun blik=compute-convert-list (clauses)
  (declare (edited  "21-AUG-1997")
	   (authors Ameier)
	   (input   "A list of clauses.")
	   (effect  "All names of Constants and variables of the clauses are converted"
		    "into names, that are usable by bliksem."
		    "A list of these objects and their corresponding new names (strings)"
		    "is stored in blik*convert-list.")
	   (value   "Undifined."))
  (setq blik*convert-list nil)
  (mapcar #'blik=convert-object clauses))

(defgeneric blik=convert-object (object)
  (declare (edited  "21-AUG-1997")
	   (authors Ameier)
	   (input   "An object.")
	   (effect  "The blik*convert-list is updated. by converting the parts of this"
		    "object.")
	   (value   "Undifined."))
  (:method ((object cl+clause))
	   (mapcar #'blik=convert-object (cl~literals object)))
  (:method ((object lit+literal))
	   (blik=convert-object (lit~atom object)))
  (:method ((object term+appl))
	   (mapcar #'blik=convert-object
		   (cons (data~appl-function object)
			 (data~appl-arguments object))))
  (:method ((object term+primitive))
	   (blik=convert-name object)))

(defun blik=convert-name (object)
  (declare (edited  "14-MAY-1996")
	   (authors Ameier)
	   (input   "An object, that can be of type term+variable, term+constant or term+number.")
	   (effect  "If a new name-string is produced, a pair of old-name-string"
		    "and new-name-string is added to the blik*convert-list.")
	   (value   "From the name is a bliksem-compatible name produced."
		    "That means all symbols till alphabetics,numbers and _ are"
		    "deleted from the name and a counter-number is added."
		    "If var is set the resulting string is upcased, otherwise"
		    "it is downcased. If the name was attached before the"
		    "before produced new-string is taken from the blik*convert-list"
		    "and is returned otherwise a new string is produced in the way"
		    "descibed before."))
  (if (and (or (keim~equal object blik*equality-object)
	       (and (term~constant-p object)
		    (null (typep object 'term+number))
		    (string= (string (keim~name object)) "=")))
	   (null (find "=" blik*convert-list :test #'(lambda (str pair)
						       (string= str (second pair))))))
      
      ;;(setq blik*convert-list (cons (list blik*equality-object "=") blik*convert-list))

      (setq blik*convert-list (cons (list (if (term~schema-p blik*equality-object)
					      (data~schema-range blik*equality-object)
					    blik*equality-object)
					  "=")
				    blik*convert-list))
    
    (let* ((name (keim~name object))
	   (name-string (if (stringp name)
			    name
			  (format nil "~A" name)))
	   ;; Compute whether the element is already in the blik*convert-list
	   ;;(partner-string (second (first (member object blik*convert-list
	   ;;					  :test #'(lambda (thing pair)
	   ;;						    (let* ((thing2 (first pair)))
	   ;;						      (if (term~schema-p thing2)
	   ;;							  (data~equal-p thing (data~schema-range thing2))
	   ;;							(keim~equal thing thing2)))))))))
	   (partner-string (second (first (member object blik*convert-list
						  :test #'(lambda (thing pair)
	   						    (let* ((thing2 (first pair)))
	   						      (or (eq thing thing2)
								  (and (data~equal thing thing2)
								       (data~equal (term~type thing) (term~type thing2)))))))))))
      
      ;; If element already in the blik*convert-list give back the ob-string already used in the blik*convert-list
      (if partner-string
	  partner-string
	;; If not a new string is computed and is together with the input object, or, if existing its representation in the
	;; environment, added to the blik*convert-list (list object string).
	;; If polymorphie is used, it is necessary to use the object from the environment.
	;; For example set with Type (aa -> o), but in the application (set a) with a of type i, set is of type (i -> o)
	;; If we would save in the blik*convert-list this set, we couldn't create a term (set 1) with 1 of type num.
	(let ((new-string ""))
	  (do ((i 0 (+ i 1)))
	      ((>= i (length name-string)) nil)
	    (when (member (char name-string i) blik*name-symbols)
	      (setq new-string (format nil "~A~A" new-string (char name-string i)))))
	  (setq new-string (format nil "ob_~A_~A" new-string (incf blik*convert-counter)))
	  (let ((checked-new-string (if (term~variable-p object)
					(string-upcase new-string)
				      (string-downcase new-string)))
		(env-element (env~lookup-object (intern (string-upcase name-string) (find-package :omega))
						(res~proof-environment blik*current-problem))))
	    
	    (setq blik*convert-list (cons (list object
						;;(if (and env-element (not (typep object 'term+number)))
						;;     env-element
						;;   object)
						checked-new-string) blik*convert-list))
	    checked-new-string)))))) 

(defun blik=get-checked-name-to-object (object)
  (declare (edited  "22-AUG-1997")
	   (authors Ameier)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "The checked name string to the object."
		    "If the object is in the blik*convert-list, this string is taken"
		    "otherwise the keim~name slot of the object is returned as string."))
  (let* ((name (keim~name object))
	 (name-string (if (stringp name)
			  name
			(format nil "~A" name)))
	 ;; Compute whether the element is already in the blik*convert-list
	 ;;(partner-string (second (first (member object blik*convert-list
	 ;;					:test #'(lambda (thing pair)
	 ;;						  (let* ((thing2 (first pair)))
	 ;;						    (if (term~schema-p thing2)
	 ;;							(data~equal-p thing (data~schema-range thing2))
	 ;;                                                   (keim~equal thing thing2)))))))))
	 (partner-string (second (first (member object blik*convert-list
						:test #'(lambda (thing pair)
							  (let* ((thing2 (first pair)))
							    (or (eq thing thing2)
								(and (data~equal thing thing2)
								     (data~equal (term~type thing) (term~type thing2)))))))))))
    
    
    (if partner-string
	partner-string
      name-string))) 

#| -------------------------------------------- END: Handling the convertion of names ------------------------------------- |#

#| -------------------------------------------- HANDLE EQUALITY ----------------------------------------------------------- |#

(defun blik=handle-equality (res-proof)
  (declare (edited  "26-AUG-1998")
	   (authors Ameier)
	   (input   "A resolution proof.")
	   (effect  "blik*equality-object is set to the equality in the environment.")
	   (value   "undefined."))

  (setq blik*equality-object (env~lookup-object '= (res~proof-environment res-proof))))

;; Vielleicht noch weitere Sachen notwendig. -> Siehe Otter (RELEXIVITY CLAUSE )!!
;; NOE -> SCHEINTS NICHT !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;; ODER ?

#| ------------------------------------------------- ENDE: Handle Equality ------------------------------------------------ |#

#| -------------------------------------------------- PARSE BLIKSEM OUTPUT ------------------------------------------------- |#

;; ----------------------------------------------> Bliksem Matchings:

(eval-when (load compile eval)
  (defclass blik+matching (res+justification)
    ()
    (:documentation "Justification for matching literals.")))

(defmethod print-object ((just blik+matching) stream)
  (let* ((parent (first (res~justification-parents just))))
    (format stream "MATCHING of ~S" parent)))

(defun blik=matching-create (parent &optional (name (intern (format nil "MATCH-just-~A"
								    (incf res*justification-counter))
							    (find-package :keim))))
  (make-instance 'blik+matching
		 :parents (list parent)
		 :positions nil
		 :unifier (subst~create nil nil)
		 :renamings nil
		 :name name
		 :method 'matching))

(defgeneric blik=replace-matching-just! (clause just)
  (declare (edited  "26-OCT-1998")
	   (authors Ameier)
	   (input   "A clause and its justification.")
	   (effect  "If the justification is blik+matching:"
		    "The justifications slot of the clause is changed in the way that it is replaced"
		    "by a factoring a clause. This clause itself has to be also produced by factoring"
		    "and so on ...")
	   (value   "The clause itself."))
  (:method (clause (just blik+matching))
	   (let* ((parent (first (res~justification-parents just)))
		  (all-factors-of-parent (blik=produce-factors parent))
		  (corresponding-clause (find clause all-factors-of-parent :test #'blik=equal-clauses-p)))
	     (setf (node~justification clause)
		   (node~justification corresponding-clause))
	     clause))
  (:method (clause just)
	   clause))

(defun blik=equal-clauses-p (clause1 clause2)
  (declare (edited  "26-OCT-1998")
	   (authors Ameier)
	   (input   "Two clauses.")
	   (effect  "None.")
	   (value   "T if the clauses are equal in the sense that their literals a pairwise"
		    "equal, nil otherwise."))
  (if (null (= (length (cl~literals clause1)) (length (cl~literals clause2))))
      nil
    (every #'(lambda (pair)
	       (keim~equal (first pair) (second pair)))
	   (mapcar #'(lambda (lit1 lit2)
		       (list lit1 lit2))
		   (cl~literals clause1)
		   (cl~literals clause2)))))

(defun blik=replace-matching-justs-in-resolution-proof! (res-proof)
  (declare (edited  "26-OCT-1998")
	   (authors Ameier)
	   (input   "A resolution proof.")
	   (effect  "All occurences of blik+matching justifications are replaced by factoring"
		    "justifications. New steps can be introduced into the resolution proof.")
	   (value   "The resolution proof itself."))
  (mapcar #'(lambda (clause)
	      (let* ((just (node~justification clause)))
		(blik=replace-matching-just! clause just)
		(when (null (eq (node~justification clause) just))
		  ;; -> new just -> neue Schritte einfuegen
		  (blik=add-steps-to-proof! (first (res~justification-parents (node~justification clause)))
					    res-proof))))
	  (res~proof-clauses res-proof)))


;; ---------------------------------------------------> Read


(defun blik=read (out-string res-proof parse-back)
  (declare (edited  "23-JUL-1998")
	   (authors Ameier)
	   (input   "The bliksem output string, the current partial resolution proof and a boolean"
		    "to sign whether the output should be parsed if it represents a proof.")
	   (effect  "If parse-back is true and the out-string contains a proof this proof"
		    "is read and added into the res-proof.")
	   (value   "'proof if out-string contains the line 'found a proof!',"
		    "'sat if out-string contains the line 'found a saturation!',"
		    "nil elsewhere."))

  (omega~message "~% PARSING BLIKSEM OUTPUT ... ~%")
  
  (let* ((line-strings (atptop~divide-string out-string #\Newline))
	 (proof-flag (blik=read-direct line-strings)))

    (when (and (equal proof-flag 'proof) parse-back)
      (blik=read-proof line-strings res-proof)
      )

    proof-flag))
	 
(defun blik=read-direct (strings)
  (declare (edited  "23-JUL-1998")
	   (authors Ameier)
	   (input   "The bliksem output as a list of line strings.")
	   (effect  "None.")
	   (value   "'proof if it contains the line 'found a proof!',"
		    "'sat if it contains the line 'found a saturation!',"
		    "nil elsewhere."))
  (let* ((line-strings (reverse strings)))
    ;; die entscheidenden Worte stehen gaaaaaaaannnnnnnnnnnzzzzzzzz unten -> reverse
    (cond ((find "found a proof!" line-strings :test 'string=)
	   'proof)
	  ((find "found a saturation!" line-strings :test 'string=)
	   'sat)
	  (t
	   nil))))

(defun blik=add-steps-to-proof! (clause res-proof)
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
		  (blik=add-steps-to-proof! parent-clause res-proof))
	      (res~justification-parents (node~justification clause))))))

(defun blik=flip-clause-at-positions (clause pos-list)
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
				     (error "~% PROBLEM IN FUNCTION blik=flip-clause-at-positions LITERAL SELECTED FOR FLIPPING IS NOT AN EQUATION !!. ~%"))
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
      (blik=flip-clause-at-positions new-clause (rest pos-list)))))
				 
	
(defun blik=produce-reflexivity-item (res-proof)
  (declare (edited  "27-AUG-1998")
	   (authors Ameier)
	   (input   "The current resolution proof.")
	   (effect  "A new reflexivity clause is prodiced and added to the clauses of the res-proof."
		    "The global varibale blik*reflexivity-item is set on this new clause.")
	   (value   "Undefined.")) 
  
  (let* ((new-type-var (type~variable-create 'aa)) ;; der Name ist hier Scheiss egal !!!!!
	 (new-var (term~generate-term-primitive-with-new-name 'x- new-type-var 'term+variable blik*current-environment))
	 ;;
	 ;;	 (data~c-domain (term~type blik*equality-object))))   ;; hier ist es Strunz egal welche Typ-Variable
	 ;;                                                                ;; hier hin kommt, aber vielleicht sollte es nicht
	 ;;                                                                ;; gerade die des = sein !!! HACK BY AMEIER
	 (new-term (term~appl-create blik*equality-object
				     (list new-var new-var)
				     ))
	 (new-clause (cl~create (list (lit~literal-create new-term
							  't))
				:justification (res~reflex-create (gensym)))))
    
    (env~remove (keim~name new-var) blik*current-environment)

    ;; Entfaellt ab Version 3.3
    ;; setzt Kappa-Slot im Reflexivity-Item-ATOM
    ;; (setf (data~kappa new-term) (list new-type-var))
    ;; (list (data~c-domain (term~type blik*equality-object))))
    
    (setf blik*reflexivity-item new-clause)
    (setf (res~proof-clauses res-proof) (cons new-clause (res~proof-clauses res-proof)))))

(defun blik=complete-initial-clauses! ()
  (declare (edited  "27-AUG-1998")
	   (authors Ameier)
	   (input   "Nothing.")
	   (effect  "In the blik*rest-initial-clauses on the clauses is as long binary-factoring tried"
		    "as possible. All factors are added to the blik*rest-initial-clauses.")
	   (value   "Undefined."))
  (setq blik*rest-initial-clauses
	(apply 'append (mapcar #'blik=produce-factors blik*rest-initial-clauses))))

(defun blik=separate-literals (clause-string)
  (declare (edited  "27-AUG-1998")
	   (authors Ameier)
	   (input   "A string, representing a clause string.")
	   (effect  "None.")
	   (value   "A lsit of strings, representing the single literals."))
  
  ;; jedes Komma, das nicht innerhalb von klammern steh ist literal trenner

  (do* ((i 0 (+ i 1))
	(current-string "")
	(open-par 0)
	(literals nil))
      ((= i (length clause-string))
       (append literals (list current-string)))
    (let* ((current-char (char clause-string i)))
      (cond ((equal current-char #\()
	     (setq open-par (+ open-par 1))
	     (setq current-string (format nil "~A~A" current-string current-char)))
	    ((equal current-char #\))
	     (setq open-par (- open-par 1))
	     (setq current-string (format nil "~A~A" current-string current-char)))
	    ((and (equal current-char #\,) (= open-par 0))
	     (setq literals (append literals (list current-string)))
	     (setq current-string ""))
	    ((and (equal current-char #\,) (null (= open-par 0)))
	     (setq current-string (format nil "~A~A" current-string current-char)))
	    (t
	     (setq current-string (format nil "~A~A" current-string current-char)))))))

(defun blik=produce-factors (clause)
  (declare (edited  "27-AUG-1998")
	   (authors Ameier)
	   (input   "A clause.")
	   (effect  "None.")
	   (value   "A list consisting of the clause and of all clauses that can be computed"
		    "from it with recursive application of factoring."))
  (let* ((factor-clauses (res~binary-factoring clause)))
    (cons clause (apply 'append (mapcar #'blik=produce-factors factor-clauses))))) 

(defun blik=complete-equality-factoring (clauses)
  (declare (edited  "03-NOV-1999")
	   (authors Ameier)
	   (input   "A list of clauses, all justified by equality-factoring.")
	   (effect  "None.")
	   (value   "A list of clauses consisting of the input clauses and"
		    "clauses arising from the input clauses if the remaining literal"
		    "and/or the inequality-literal of the equality-factoring step"
		    "are flipped."))
  (apply #'append (mapcar #'(lambda (clause)
			      (let* ((just (node~justification clause))
				     (positions (res~justification-positions just))
				     (pos1f (pos~list-position (list (pos~first (first positions)))))
				     (poslast (pos~list-position (list (- (length (cl~literals clause)) 1))))
				     
				     ;; pos1f ist die Position des Equality Literals das uebriug bleibt,
				     ;; poslast ist die Position des Unequality-Literals

				     (clause-flip-first (blik=flip-clause-at-positions clause (list pos1f)))
				     (clause-flip-last (blik=flip-clause-at-positions clause (list poslast)))
				     (clause-flip-both (blik=flip-clause-at-positions clause-flip-first (list poslast))))
				
				(list clause
				      clause-flip-first
				      clause-flip-last
				      clause-flip-both)))
			  clauses)))

  

#|

;; ----------------------------------------------------------->
;; Das nunkommende ist auskommentiert, seit wir auf den Prolog-Style Verbose Bliksem Output umgestigen sind!!!!!

(defun blik=read-proof (line-strings res-proof)
  (declare (edited  "27-AUG-1998")
	   (authors Ameier)
	   (input   "A list of strings representing the output of BLIKSEM.")
	   (effect  "The resolution proof res-proof is updated by new readed steps.")
	   (value   "Undefined.")) 

  ;; setting der initial clauses
  (setq blik*rest-initial-clauses (res~proof-initial-clauses res-proof))
  (blik=complete-initial-clauses!)
  
  (setq blik*number-clause-list nil)

  (let* ((proof-lines (do* ((rest-string-lines line-strings (rest rest-string-lines))
			    (return-list nil)
			    (proof-part-flag 'start))
			  ((or (null rest-string-lines) (equal proof-part-flag 'end))
			   (when (null rest-string-lines)
			     (error "Something went wrong in blik=read-proof !, the case that the end of file is reached shouldn't be happens."))
			   return-list)
			(let* ((string-line (first rest-string-lines)))
			  (if (equal proof-part-flag 'proof)
			      (if (string= string-line "found a proof!")
				  (setq proof-part-flag 'end)
				(if (not (string= string-line ""))
				    (setq return-list (append return-list (list string-line)))))
			    (if (string= string-line "Bliksems!, er is een bewijs:")
				(setq proof-part-flag 'proof))))))
	 (composed-proof-lines (do* ((rest-proof-lines proof-lines (rest rest-proof-lines))
				     (current-string "")
				     (back-strings nil))
				   ((null rest-proof-lines)
				    back-strings)
				 (let* ((head-line (first rest-proof-lines)))
				   (if (equal (char head-line (- (length head-line) 1)) #\.)
				       (progn
					 (setq back-strings (append back-strings (list (format nil "~A~A" current-string head-line))))
					 (setq current-string ""))
				     (setq current-string (format nil "~A~A" current-string head-line))))))
	 ;; das Problem ist, dass die PROOF-lines gebrochen sind -> immer solange zusammenfassen bis . am ENDE !

	 ;;(ordered-proof-lines (blik=order-proof-lines composed-proof-lines))
	 ;; Problem ist, dass proof lines vielleicht nicht in der richtigen Reihenfolge sind -> ordnen !!
	 )
    
    
    (mapcar #'(lambda (proof-line)
		(blik=parse-line proof-line res-proof))
	    ;; ordered-proof-lines)))
	    composed-proof-lines)

    (blik=replace-matching-justs-in-resolution-proof! res-proof)))


;; Kleine Neuerung fuer V1.10A
;; Diese Funktionmacht folgendes: Gleiche Literale in Klauseln (und zwar wirklich Gleiche!, nicht unifizierende!) werden
;; zusammengeworfen. Leider stellte ich bei V1.10A fest, dass dies nicht immer der Fall ist! Tatsaechlich gab es faelle, wo
;; gleiche Literale nach einer Resolution 'R' mittels einer Factorisierung 'f' zusammengezogen wurden. Dies fuehrte zu einem
;; Fehler, da die Literale bereits zusammengezogen waren, also nicht mehr Fakrorisiert werden konnten.
;; Daher folgende Neuerung V1.10A: blik=produce-matchings erzeugt nicht nur die zusammengezogenen Klauseln, sondern gibt auch
;; noch die original Klauseln mit. Das vergroessert zwar etwas den Klauselraum, scheint aber den Fehler auszumaerzen ...
;; AMEIER 20.10.99

(defun blik=produce-matchings (clauses)
  (declare (edited  "26-OCT-1998")
	   (authors Ameier)
	   (input   "A list of clauses.")
	   (effect  "None.")
	   (value   "A list of clauses, containing the clauses of the input set and also the"
		    "clauses resulting from matching (of equal literals) of the clauses."))
  (apply #'append (mapcar #'(lambda (clause)
			      (let* ((new-literals (remove-duplicates (data~copy (cl~literals clause) :downto '(data+primitive))
								      :test #'keim~equal
								      :from-end 't)))
				(if (< (length new-literals) (length (cl~literals clause)))
				    (list clause (cl~create new-literals
							    :justification (blik=matching-create clause)))
				  (list clause))))
			  clauses)))

(defun blik=parse-line (proof-line res-proof)
  (declare (edited  "27-AUG-1998")
	   (authors Ameier)
	   (input   "A string that consists of a proof line of bliksem and the current res-proof.")
	   (effect  "The clause/step of this line is produced and if not an initial clause added to"
		    "the steps of the res-proof (Remark: It is possible that one step of BLIKSEM"
		    "corresponds to several steps by us !). The blik*number-clause-list is updated"
		    "by a new number/clause-pair. If the found clause is an initial it is removed from"
		    "the list of blik*rest-initial-clause-list.")
	   (value   "The new produced clause from this line."))
  (multiple-value-bind
      (number-string justification-string clause-string)
      (blik=split-line-string proof-line)

    (let* ((number (atptop~parse-number number-string))
	   (clause (blik=parse-clause clause-string justification-string res-proof)))

      (setq blik*number-clause-list (cons (list number clause) blik*number-clause-list))
      
      clause)))

(defun blik=split-line-string (line-string)
  (declare (edited  "27-AUG-1998")
	   (authors Ameier)
	   (input   "A string,representing a proof line of bliksem.")
	   (effect  "None.")
	   (value   "Multiple-value:"
		    "First: The number of the presented clause (as string)."
		    "Second: The justification of the made step (as string)."
		    "Third: The clause (as string)."))
  (let* ((string-list (atptop~divide-string-by-lists line-string (list #\{ #\}) (list #\space)))
	 (number-string (third (atptop~divide-string-by-lists (first string-list) (list #\( #\, #\)) nil)))
	 )

    (values number-string
	    (fifth string-list)
	    (seventh string-list))))

(defun blik=parse-clause (clause-string justification-string res-proof)
  (declare (edited  "17-SEP-1996")
	   (authors Ameier)
	   (input   "A string (representing a clause in bliksem.out), the justification-string of this"
		    "step and the current res-proof.")
	   (effect  "The blik*local-clause-vars are changed.")
	   (value   "The instance of a clause,that is a renamed and reordered copy of the clause"
		    "from input string, justified by omega-rules."))

  (setq blik*local-clause-vars nil)
  
  (let* ((literal-list (if (string= "" clause-string)
			   nil
			 (mapcar #'blik=parse-literal
				 (blik=separate-literals clause-string))))
	 (bliksem-clause (cl~create literal-list))
	 (new-clause (blik=use-justification justification-string bliksem-clause res-proof)))
    (blik=add-steps-to-proof! new-clause res-proof)
    new-clause))


(defun blik=use-justification (justification-string bliksem-clause res-proof)
  (declare (edited  "27-AUG-1998")
	   (authors Ameier)
	   (input   "The justification string and the clause given by bliksem.")
	   (effect  "None.")
	   (value   "We have two problems:"
		    "First: Implicit Factorings are not given !"
		    "Second: The literals are ordered by blixem in a special way !"
		    "So we do the following: We take the major justification:"
		    "Resolution,Paramodulation and Reflexsivity and apply it"
		    "on the given parent clauses. This application gives as mayby a set"
		    "of possible results. On all results we apply as long as possible factoring"
		    "and seek among all clauses we get in this way the clause that consists"
		    "of the same literals as the bliksem clause (modulo another ordering of"
		    "the literals and modulo a variable renaming). This clause is returned"
		    "(if several match the first found is returned). If the clause is justified by"
		    "initial it is searched in the blik*rest-initial-clauses. Remark that this list"
		    "is already closed under factoring."))
  (do* ((rest-string justification-string)
	(current-clause-list nil))
      ((string= rest-string "")

       (let* ((clause (find bliksem-clause current-clause-list :test #'(lambda (clause1 clause2)
									 (atptop~clauses-equal-till-renaming-and-ordering-p
									  clause1 clause2 :flip 't)))))
	 
	 (multiple-value-bind
	     (flag ren pos flip-list)
	     (atptop~clauses-equal-till-renaming-and-ordering-p bliksem-clause clause :flip 't)
	   (let* ((flipped-clause (blik=flip-clause-at-positions clause flip-list)))
	     
	     flipped-clause))))
    
    (multiple-value-bind
	(next-just rest-rest-string)
	(atptop~get-next-word rest-string #\;)
      
      (setq rest-string rest-rest-string)
      
      ;;(format t "~% CURRENT-CLAUSE-LIST:~A" current-clause-list)
      ;;(format t "~% THE REST_STRING: ~A" rest-string)
            
      (cond ((equal (char next-just 0) #\I)
	     
	     ;; -> Input clause; immer first -> setzten von curren-clause-list !!!
	     
	     (setq current-clause-list blik*rest-initial-clauses) ;; initial-clauses bereits bzgl. factorisierung abgeschlossen !!
	     
	     )	
	    ((equal (char next-just 0) #\R)

	     ;;(format t "~%Entering R")
	     ;; -> Resolution; immer first -> setzten von curren-clause-list  !!!
	     
	     (let* ((res-strings (atptop~divide-string-by-lists next-just (list #\; #\, #\( #\)) nil))
		    (first-number (atptop~parse-number (third res-strings)))
		    (second-number (atptop~parse-number (fifth res-strings)))
		    (parent1 (blik=number2clause first-number))
		    (parent2 (blik=number2clause second-number))
		    (resolvents (res~binary-resolution parent1 parent2))
		    (res+facts (blik=produce-matchings resolvents)))

	       ;;(format t "~%Setting the current clauses to: ~A" res+facts)
	       
	       (setq current-clause-list res+facts)))	
	    
	    ((equal (char next-just 0) #\P)
	     ;; -> Paramodulation; immer first -> setzen von current-clause-list
	     
	     (let* ((par-strings (atptop~divide-string-by-lists next-just (list #\; #\, #\( #\)) nil))
		    (first-number (atptop~parse-number (third par-strings)))
		    (second-number (atptop~parse-number (fifth par-strings)))
		    (father-parent (blik=number2clause first-number))
		    (mother-parent (blik=number2clause second-number))
		    (paramods (res~paramodulation-mother-father mother-parent father-parent))
		    (all-paramods (apply 'append (mapcar #'blik=complete-paramod paramods)))
		    (pars+facts (blik=produce-matchings all-paramods)))
	       
	       (setq current-clause-list pars+facts)))
	    
	    ((equal (char next-just 0) #\D)
	     ;; -> Demodulation; immer first -> setzen von current-clause-list
	     
	     (let* ((par-strings (atptop~divide-string-by-lists next-just (list #\; #\, #\( #\)) nil))
		    (first-number (atptop~parse-number (third par-strings)))
		    (second-number (atptop~parse-number (fifth par-strings)))
		    (father-parent (blik=number2clause first-number))
		    (mother-parent (blik=number2clause second-number))
		    (paramods (res~binary-demodulation mother-parent father-parent))
		    (all-paramods (apply 'append (mapcar #'blik=complete-paramod paramods)))
		    (pars+facts (blik=produce-matchings all-paramods)))
	       
	       (setq current-clause-list pars+facts)))
	    
	    ((equal (char next-just 0) #\E)
	     
	     ;; /* Equality factoring.
	     ;; Hans beschreibt die Regel so:
	     ;; From { t1 = t2, t1 = t3, R } derive 
             ;; { ! t2 = t3, t1 = t3, R }.
	     ;; immer zuerst gesetzt -> setzen von current-clause-list
	     	     
	     (let* ((ref-strings (atptop~divide-string-by-lists next-just (list #\; #\, #\( #\)) nil))
		    (number (atptop~parse-number (third ref-strings)))
		    (parent (blik=number2clause number))
		    (eq-facs (keim::res~equality-factoring parent))
		    (eq-facII (blik=complete-equality-factoring eq-facs))
		    (eq-facs+matchs (blik=produce-matchings eq-facII)))
	       
	       (setq current-clause-list eq-facs+matchs)))
	    
	    ((equal (char next-just 0) #\Q)
	     ;; -> Reflexivitaet; immer first ->setzen von current-clause-list
	     
	     (when (null blik*reflexivity-item)
	       (blik=produce-reflexivity-item res-proof))
	     
	     (let* ((ref-strings (atptop~divide-string-by-lists next-just (list #\; #\, #\( #\)) nil))
		    (number (atptop~parse-number (third ref-strings)))
		    (parent (blik=number2clause number))
		    (resolvents (res~binary-resolution parent blik*reflexivity-item))
		    (res+facts (blik=produce-matchings resolvents)))
	       
	       (setq current-clause-list res+facts)))
	    
	    ((equal (char next-just 0) #\F)

	     ;; Factorisierung; immer First gestetz -> setzten von current-clause-list

	     (let* ((par-strings (atptop~divide-string-by-lists next-just (list #\; #\, #\( #\)) nil))
		    (number (atptop~parse-number (third par-strings)))
		    (parent (blik=number2clause number))
		    (all-facs (res~binary-factoring parent))
		    (facs (blik=produce-matchings all-facs)))
	       
	       (setq current-clause-list facs)))
	    
	    ((equal (char next-just 0) #\S)

	     ;; Resimplification, macht selbst nix, nimmt aber Klausel auf ...,
	     ;; immer first -> setzten current-clause-list

	     (let* ((par-strings (atptop~divide-string-by-lists next-just (list #\; #\, #\( #\)) nil))
		    (number (atptop~parse-number (third par-strings)))
		    (parent (blik=number2clause number)))

	       (setq current-clause-list (list parent))))
	    

	    ((equal (char next-just 0) #\f)

	     ;; -> factorisierung; nachgesetzt -> aufnehemen von current-clause-list

	     ;;(format t "~%Entering f")
	     
	     (let* ((facs (apply 'append (mapcar #'res~binary-factoring current-clause-list)))
		    (facs+facts (blik=produce-matchings facs)))

	       ;;(format t "~%Setting current-clause-list to ~A" facs+facts)
	       
	       (setq current-clause-list facs+facts)))
	    
	    ((or (equal (char next-just 0) #\u)
		 (equal (char next-just 0) #\r))
	     
	     ;; unit-resolution; immer nachgesetzt -> aufnehmen von current-clause-list

	     (multiple-value-bind
		 (next-word rest-words)
		 (atptop~get-next-word next-just #\()
	       (let* ((number-string (atptop~get-next-word rest-words #\)))
		      (number (atptop~parse-number number-string))
		      (unit-parent (blik=number2clause number))
		      (resolvents (apply 'append (mapcar #'(lambda (clause)
							     (res~binary-resolution unit-parent clause))
							 current-clause-list)))
		      (res+facts (blik=produce-matchings resolvents)))
		 (setq current-clause-list res+facts))))
	    
	    ((equal (char next-just 0) #\d)

	     ;; demodulation; immer nachgesetzt -> aufnehmen von current-clause-list

	     (multiple-value-bind
		 (next-word rest-words)
		 (atptop~get-next-word next-just #\()
	       (let* ((number-string (atptop~get-next-word rest-words #\)))
		      (number (atptop~parse-number number-string))
		      (demodulator-parent (blik=number2clause number))
		      (demods (apply 'append (mapcar #'(lambda (clause)
							 (res~binary-demodulation clause demodulator-parent))
						     current-clause-list)))
		      (all-demods (apply 'append (mapcar #'blik=complete-paramod demods)))
		      (demods+facts (blik=produce-matchings all-demods)))
		 (setq current-clause-list demods+facts))))

	    ((equal (char next-just 0) #\q)
	     
	     ;; nachgesetzte equality ref action ! -> aufnehmen von current-clause-list
	     
	     (when (null blik*reflexivity-item)
	       (blik=produce-reflexivity-item res-proof))
	     
	     (let* ((resolvents (apply 'append (mapcar #'(lambda (clause)
							   (res~binary-resolution clause blik*reflexivity-item))
						       current-clause-list)))
		    (res+facts (blik=produce-matchings resolvents)))
	       
	       (setq current-clause-list res+facts)))
	    	    
	    (t
	     
	     (error "~% Ueberaschendes Ereignis in blik=use-justification! Gibts hier noch mehr justs als F,R,P,I,E,D,Q,q,f,u,d,r ????? ~%"))))))


(defun blik=parse-literal (literal-string)
  (declare (edited  "17-SEP-1996")
	   (authors Ameier)
	   (input   "A string representing a literal.")
	   (effect  "Perhaps blik*local-vars is changed (if new vars needed).")
	   (value   "A literal instance of this string."))
  (if (equal (char literal-string 0) #\!)
      (lit~literal-create (blik=parse-term (atptop~cut-first-char literal-string)) nil)
    (lit~literal-create (blik=parse-term literal-string) 't)))

(defun blik=parse-term (term-string &key (type nil))
  (declare (edited  "17-SEP-1996")
	   (authors Ameier)
	   (input   "A string representing a term and keyword type.")
	   (effect  "Perhaps the blik*local-vars is changed (if new vars needed).")
	   (value   "A term instance of the input string."))
  (if (> (atptop~number-of-char-in-string #\= term-string) 0)

      ;; If the term is an equation:
      (let* ((args (multiple-value-bind
		       (arg1 arg2)
		       (atptop~get-next-word term-string #\= :ignore-char-list (list #\= #\>))
		     (list arg1 arg2)))
	     (awaiting-types (data~n-domain (term~type (data~schema-range (blik=string2object "=")))))
	     (first-arg (blik=parse-term (first args) :type (first awaiting-types)))
	     (second-arg (blik=parse-term (second args) :type (second awaiting-types)))
	     (application (term~appl-create (blik=string2object "=") (list first-arg second-arg)
					    )))
	application)
    ;; If the term isn't an equation:
    (multiple-value-bind
	(functor-string rest-string)
	(atptop~get-next-word term-string #\()
      ;; reads till a "(" is reached 
      ;; if rest-string = "" (empty) -> term isn't an application -> otherwise term is an application
      (if (string= rest-string "")
	  (blik=string2object functor-string :type type)
	(let* ((functor (blik=string2object functor-string))
	       (args (mapcar #'(lambda (term-string awaiting-type)
				 (blik=parse-term term-string :type awaiting-type))
			     (blik=parse-term-list (atptop~cut-last-char rest-string))
			     (data~n-domain (if (term~schema-p functor)
						(data~schema-range functor)
					      functor)))))
	  (term~appl-create functor args
			    ))))))


|#

;; --------------------------------------------------------> Parse NEW
;; Nun kommt das Zeug, das seit BliksemV1.10A an den Prolog-Output angepasst ist!

(defun blik=read-proof (line-strings res-proof)
  (declare (edited  "27-AUG-1998")
	   (authors Ameier)
	   (input   "A list of strings representing the output of BLIKSEM.")
	   (effect  "The resolution proof res-proof is updated by new readed steps.")
	   (value   "Undefined.")) 

  ;; Ruecksetzten des reflexivity-item
  (setf blik*reflexivity-item nil)
  
  ;; setting der initial clauses
  (setq blik*rest-initial-clauses (res~proof-initial-clauses res-proof))
  (blik=complete-initial-clauses!)
  
  (setq blik*number-clause-list nil)
  
  (let* ((proof-lines (do* ((rest-string-lines line-strings (rest rest-string-lines))
			    (return-list nil)
			    (proof-part-flag 'start))
			  ((or (null rest-string-lines) (equal proof-part-flag 'end))
			   (when (null rest-string-lines)
			     (error "Something went wrong in blik=read-proof !, the case that the end of file is reached shouldn't be happens."))
			   return-list)
			(let* ((string-line (first rest-string-lines)))
			  (if (equal proof-part-flag 'proof)
			      (if (string= string-line "end.")
				  (setq proof-part-flag 'end)
				(if (not (string= string-line ""))
				    (setq return-list (append return-list (list string-line)))))
			    (if (string= string-line "found a proof!")
				(setq proof-part-flag 'proof))))))
	
	 (proof-linesII (do* ((rest-proof-lines (rest proof-lines) (rest rest-proof-lines))
			      (current-string nil)
			      (back-lines nil))
			    ((null rest-proof-lines)
			     back-lines)
			  (let* ((head-line (first rest-proof-lines)))
			    
			    (cond ((and (or (atptop~string-is-prefix-of-string-p "initialclauses" head-line)
					    (atptop~string-is-prefix-of-string-p "resolution" head-line)
					    (atptop~string-is-prefix-of-string-p "paramod" head-line)
					    (atptop~string-is-prefix-of-string-p "eqswap" head-line)
					    (atptop~string-is-prefix-of-string-p "factor" head-line)
					    (atptop~string-is-prefix-of-string-p "subsumption" head-line)
					    (atptop~string-is-prefix-of-string-p "eqfact" head-line)
					    (atptop~string-is-prefix-of-string-p "eqrefl" head-line))
					(equal (char head-line (- (length head-line) 1)) #\.))
				   
				   ;; neuer Step + Step schon mit dieser Zeile zu Ende
				   
				   (setq back-lines (append back-lines
							    (if current-string
								(list current-string head-line)
							      (list head-line)))))
				  
				  ((or (atptop~string-is-prefix-of-string-p "initialclauses" head-line)
				       (atptop~string-is-prefix-of-string-p "resolution" head-line)
				       (atptop~string-is-prefix-of-string-p "paramod" head-line)
				       (atptop~string-is-prefix-of-string-p "eqswap" head-line)
				       (atptop~string-is-prefix-of-string-p "factor" head-line)
				       (atptop~string-is-prefix-of-string-p "subsumption" head-line)
				       (atptop~string-is-prefix-of-string-p "eqfact" head-line)
				       (atptop~string-is-prefix-of-string-p "eqrefl" head-line))
				   
				   ;; neuer step beginnt hier
				   (setq back-lines (append back-lines
							    (if current-string
								(list current-string)
							      nil)))
				   (setq current-string head-line))
				  
				  (t
				   
				   ;; weiter fuehren eines Schrittes
				   
				   (setq current-string (format nil "~A~A" current-string head-line))))))))
    
    ;; (format t "~%~%THE PROOF LINES ARE:")
    ;; (mapcar #'(lambda (line)
    ;;		(format t "~%~A" line))
    ;;	    proof-linesII)
    ;;
    ;; (error "NU IS ABER GUT!")
    
    (mapcar #'(lambda (proof-line)
    		(blik=parse-proof-line proof-line res-proof))
	    proof-linesII)
    
    (blik=replace-matching-justs-in-resolution-proof! res-proof)))

(defun blik=parse-proof-line (proof-line res-proof)
  (declare (edited  "04-NOV-1999")
	   (authors Ameier)
	   (input   "A bliksem proof line and the current resolution proof.")
	   (effect  "blik*number-clause-list can be changed, resolution proof is updated"
		    "by new steps.")
	   (value   "If the step is the initial clauses step: a list of clauses, corresponding to the"
		    "initial clauses, otherwise one clause corresponding to the step."))
  
  (if (atptop~string-is-prefix-of-string-p "initialclauses" proof-line)
      
      ;; die initial clauses
      (blik=read-initial-clauses proof-line res-proof)
    
    ;; Ein richtiger Step
    (multiple-value-bind
	(justification-string number-string clause-string rest-string-list)
	(blik=split-step-line-string proof-line)
      
      (let* ((number (atptop~parse-number number-string))
	     (clause (blik=parse-step-clause clause-string justification-string res-proof rest-string-list)))
	
	(setq blik*number-clause-list (cons (list number clause) blik*number-clause-list))
	
	clause))))


;; ---------------------------------------> Handling of a step

(defun blik=split-step-line-string (proof-line)
  (declare (edited  "05-NOV-1999")
	   (authors Ameier)
	   (input   "A proof line with a step.")
	   (effect  "None.")
	   (value   "Multiple-value"
		    "First: The string with the justification of the step."
		    "Second: The string with the number of the new clause."
		    "Third: The string with the new clause."
		    "Fourth: A list with all other strings holding other informations of the step."))

  (multiple-value-bind
      (justification-string rest-string1)
      (atptop~get-next-word proof-line #\( :ignore-char-list (list #\space #\'))

    (let* ((rest-string2 (atptop~cut-x-last-chars rest-string1 2))
	   ;; Jedes Komma das nun nicht innerhalb von Klammer (...) auftaucht ist clauses trenner!
	   ;; Diese Funktionalitaet erfauellt blik=separate-literals !! Der Name der Funktion ist vielleicht etwas bloede gewaehlt!
	   (parts-strings (blik=separate-literals rest-string2)))

      (multiple-value-bind
	  (number-string clause-string)
	  (blik=divide-clause (first parts-strings))

	(values justification-string
		number-string
		clause-string
		(rest parts-strings))))))


(defun blik=parse-step-clause (clause-string justification-string res-proof rest-string-list)
  (declare (edited  "05-NOV-1999")
	   (authors Ameier)
	   (input   "A string (representing a clause in bliksem.out), the justification-string of this"
		    "step, the current res-proof and the rest list of strings relaevant for this step.")
	   (effect  "The blik*local-clause-vars are changed.")
	   (value   "The instance of a clause,that is a renamed and reordered copy of the clause"
		    "from input string, justified by omega-rules."))

  (setq blik*local-clause-vars nil)
  
  (let* ((literal-list (if (string= "" clause-string)
			   nil
			 (mapcar #'blik=parse-literal
				 (blik=separate-literals clause-string))))
	 (bliksem-clause (cl~create literal-list))
	 (omega-clause (blik=use-step-justification justification-string bliksem-clause res-proof rest-string-list)))

    (blik=add-steps-to-proof! omega-clause res-proof)
    
    omega-clause))

(defun blik=use-step-justification (justification-string bliksem-clause res-proof rest-string-list)
  (declare (edited  "04-NOV-1999")
	   (authors Ameier)
	   (input   "A string - giving the justification of bliksem -, the clause created by bliksem"
		    "the current resolution proof and the rest strings of the bliksem step.")
	   (effect  "None.")
	   (value   "The omega clause - justified by omega-justifications to the bliksem-clause."))
    
  (cond ((string= justification-string "subsumption")
	 
	 ;; im Fall von Justification interessiert uns nur die Parent clause, um dann
	 ;; die Number dieses Schrittes ebenfalls an die Clause jenes Schrittes zu binden
	 ;; Die Parent Information steht in der naechsten Line drin!!
	 
	 (let* ((parent (blik=read-parent (first rest-string-list))))
	   
	   parent))
	
	((string= justification-string "eqswap")
	 
	 ;; Flipping
	 ;; -> Wir probieren jedes moegliche Flipping aus

	 (let* ((parent (blik=read-parent (first rest-string-list)))
		(flips (blik=complete-flipping parent))
		(omega-clause (find bliksem-clause flips
				    :test #'(lambda (clause1 clause2)
					      (atptop~clauses-equal-till-renaming-and-ordering-p
					       clause1 clause2 :flip nil)))))
	   
	   omega-clause))
	
	((string= justification-string "factor")
	 
	 ;; Factorisierung

	 (let* ((parent (blik=read-parent (first rest-string-list)))
		(factors (res~binary-factoring parent))
		(omega-clause (find bliksem-clause factors
				    :test #'(lambda (clause1 clause2)
					      (atptop~clauses-equal-till-renaming-and-ordering-p
					       clause1 clause2 :flip nil)))))
	   
	   omega-clause))
	
	((string= justification-string "resolution")
	 
	 ;; Resolution 
	 
	 (let* ((parent1 (blik=read-parent (first rest-string-list)))
		(parent2 (blik=read-parent (third rest-string-list)))
		(resolvents (res~binary-resolution parent1 parent2))
		(omega-clause (find bliksem-clause resolvents
				    :test #'(lambda (clause1 clause2)
					      (atptop~clauses-equal-till-renaming-and-ordering-p
					       clause1 clause2 :flip nil)))))
	   
	   omega-clause))
	
	((string= justification-string "paramod")
	 
	 ;; Paramodulation
	 
	 (let* ((father (blik=read-parent (first rest-string-list)))
		(mother (blik=read-parent (third rest-string-list)))
		(paramods (res~paramodulation-mother-father mother father))
		(omega-clause (find bliksem-clause paramods
				    :test #'(lambda (clause1 clause2)
					      (atptop~clauses-equal-till-renaming-and-ordering-p
					       clause1 clause2 :flip nil)))))
	   
	   omega-clause))

	((string= justification-string "eqfact")

	 ;; equality factoring

	 (let* ((parent (blik=read-parent (first rest-string-list)))
		(eqfacts (res~equality-factoring parent))
		(eqfactsII (blik=complete-equality-factoring eqfacts))
		(omega-clause (find bliksem-clause eqfactsII
				    :test #'(lambda (clause1 clause2)
					      (atptop~clauses-equal-till-renaming-and-ordering-p
					       clause1 clause2 :flip nil)))))
	   
	   omega-clause))

	
	((string= justification-string "eqrefl")
	 
	 ;; equality reflexivity

	 (when (null blik*reflexivity-item)
	   (blik=produce-reflexivity-item res-proof))

	 (let* ((parent (blik=read-parent (first rest-string-list)))
		(resolvents (res~binary-resolution parent blik*reflexivity-item))
		(omega-clause (find bliksem-clause resolvents
				    :test #'(lambda (clause1 clause2)
					      (atptop~clauses-equal-till-renaming-and-ordering-p
					       clause1 clause2 :flip nil)))))
	   
	   omega-clause))
	
	(t
	 
	 (omega~error "~%Ein ueberaschender Schritt: ~A,der uns noch nicht bekannt war!" justification-string))))


(defun blik=read-parent (clause-string)
  (declare (edited  "03-NOV-1999")
	   (authors Ameier)
	   (input   "A line, containing a parent information.")
	   (effect  "None.")
	   (value   "The number of the parent is read and then the parent clause is determined and returned."))

  (multiple-value-bind
      (number-string literals-string)
      (blik=divide-clause clause-string) 
    
    (blik=number2clause (atptop~parse-number number-string))))
			
;; ---------------------------------------> Read initial clauses

(defun blik=read-initial-clauses (proof-line res-proof)
  (declare (edited  "04-NOV-1999")
	   (authors Ameier)
	   (input   "The proof line corresponing to the initial clauses of the proof, and the"
		    "resolution proof.")
	   (effect  "blik*number-clause-list is updated.")
	   (value   "The list of initial clauses."))
  
  (let* ((pairs-of-numbers-and-clauses (blik=split-initial-clauses-string proof-line)))

    (mapcar #'(lambda (pair-of-number-and-clause)

		(setq blik*local-clause-vars nil)
		
		(let* ((number-string (first pair-of-number-and-clause))
		       (number (atptop~parse-number number-string))
		       (clause-string (second pair-of-number-and-clause))
		       (literal-list (if (string= "" clause-string)
					 nil
				       (mapcar #'blik=parse-literal
					       (blik=separate-literals clause-string))))
		       (bliksem-clause (cl~create literal-list))
		       (omega-clause (find bliksem-clause blik*rest-initial-clauses
					   :test #'(lambda (clause1 clause2)
						     (atptop~clauses-equal-till-renaming-and-ordering-p
						      clause1 clause2 :flip nil)))))

		  (blik=add-steps-to-proof! omega-clause res-proof)
		  
		  (setq blik*number-clause-list (cons (list number omega-clause) blik*number-clause-list))
		  (setq blik*rest-initial-clauses (remove omega-clause blik*rest-initial-clauses))
		  
		  omega-clause))

	    pairs-of-numbers-and-clauses)))

(defun blik=split-initial-clauses-string (proof-line)
  (declare (edited  "04-NOV-1999")
	   (authors Ameier)
	   (input   "A bliksem line, expressing the initial clauses.")
	   (effect  "NOne.")
	   (value   "A list of pairs of strings, the first always giving the number of a clause"
		    "and the second giving the clause itself."))

  (let* (;; Als erstes schneiden wir hinten und vorne ein bisserl was weg
	 (cutted-line1 (atptop~cut-x-first-chars proof-line 17))
	 (cutted-line2 (atptop~cut-x-last-chars cutted-line1 4))
	 ;; Jedes Komma das nun nicht innerhalb von Klammer (...) auftaucht ist clauses trenner!
	 ;; Diese Funktionalitaet erfauellt blik=separate-literals !! Der Name der Funktion ist vielleicht etwas bloede gewaehlt!
	 (clauses-strings (blik=separate-literals cutted-line2)))

    
    (mapcar #'(lambda (clause-string)
		(multiple-value-bind
		    (number-string literals-string)
		    (blik=divide-clause clause-string)

		  (list number-string literals-string)))
	    
	    clauses-strings)))

;; ----------------------------------------------> allgemeine Parse Sachen:

(defun blik=divide-clause (clause-string)
  (declare (edited  "05-NOV-1999")
	   (authors Ameier)
	   (input   "A string for a clause: 'clause(number,[clause-string])'.")
	   (effect  "None.")
	   (value   "Multiple-value:"
		    "First: 'number' (as string)"
		    "Second: 'clause-string'."))

  (multiple-value-bind
      (clause-prefix rest-string1)
      (atptop~get-next-word clause-string #\(
			    :ignore-char-list (list #\space #\'))
    
    (multiple-value-bind
	(number-string rest-string2)
	(atptop~get-next-word rest-string1 #\,)
      
      (values number-string
	      (atptop~cut-x-first-chars 
	       (atptop~cut-x-last-chars rest-string2 2)
	       1)))))

(defun blik=parse-literal (literal-string)
  (declare (edited  "17-SEP-1996")
	   (authors Ameier)
	   (input   "A string representing a literal.")
	   (effect  "Perhaps blik*local-vars is changed (if new vars needed).")
	   (value   "A literal instance of this string."))
  (if (equal (char literal-string 0) #\~)
      (lit~literal-create (blik=parse-term (atptop~cut-last-char (atptop~cut-x-first-chars literal-string 2))) nil)
    (lit~literal-create (blik=parse-term literal-string) 't)))

		  
(defun blik=parse-term (term-string &key (type nil))
  (declare (edited  "17-SEP-1996")
	   (authors Ameier)
	   (input   "A string representing a term and keyword type.")
	   (effect  "Perhaps the blik*local-vars is changed (if new vars needed).")
	   (value   "A term instance of the input string."))

  (multiple-value-bind
      (functor-string rest-string)
      (atptop~get-next-word term-string #\()
    ;; reads till a "(" is reached 
    ;; if rest-string = "" (empty) -> term isn't an application -> otherwise term is an application
    (if (string= rest-string "")
	(blik=string2object functor-string :type type)
      (let* ((functor (cond ((or (string= functor-string "=")
				 (string= functor-string "==>"))
			     (blik=string2object "="))
			    (t
			     (blik=string2object functor-string))))
	     (args (mapcar #'(lambda (term-string awaiting-type)
			       (blik=parse-term term-string :type awaiting-type))
			   (blik=parse-term-list (atptop~cut-last-char rest-string))
			   (data~n-domain (term~type (if (term~schema-p functor)
							   (data~schema-range functor)
							 functor))))))
	(term~appl-create functor args
			  )))))

(defun blik=parse-term-list (term-list-string)
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


(defun blik=complete-flipping (clause)
  (declare (edited  "03-NOV-1999")
	   (authors Ameier)
	   (input   "A clause.")
	   (effect  "None.")
	   (value   "A list of clauses, each created by flipping ONE (exactly ONE) equality (or inequality)"
		    "literal in input clause."))

  (let* ((literals (cl~literals clause))
	 (lengthl (length literals)))
    
    (do* ((i 0 (+ i 1))
	  (rest-literals literals (rest rest-literals))
	  (back-clauses nil))
	((null rest-literals)
	 back-clauses)
      
      (let* ((head-literal (first rest-literals))
	     (atom (lit~atom head-literal)))

	(when (atptop~equation-p atom)
	  (setq back-clauses (append back-clauses (list (blik=flip-clause-at-positions clause (list (pos~list-position (list i))))))))))))


#| ----------------------------------------------- ENDE: PARSE BLIKSEM OUTPUT ----------------------------------------------- |#

#| ----------------------------------------------- BLIKSEM COMPLETE PARAMOD ------------------------------------------------- |#

#| Wir haben ein kleines PRoblem mit der Paramodulation von BLIKSEM:
   Es kann passieren, dass er mehr als eine Ersetzung zurselben Zeit durchfuehrt !

   [A=B,Q] + [P(A,A)] -> [P(B,B),Q] In einem Paramodschritt

   Um dies zu garantieren muessen wir die Paramodulation abschliessen, d.h. nacj jeder Paramodulation muessen auf die resultierende
   binaere Clauses solange wietere mit dem gleichen Vater-literal in gleicher Richtung moegliche Paramodulationen ausgefuehrt werden
   wie moeglich !

   [A=B,Q] + [P(A,A),Q] -> [P(A,B),Q]
                           + [A=B,Q] -> [P(B,B),Q,Q] -> [P(B,B),Q]

   Auf die anderen Literale der Vaterklausel muss dann noch factoring gemacht werden (bzw. Abschluss bzgl. factoring -> eh eingebaut
   im paramodulation Schritt).
|#


(defun blik=complete-paramod (clause)
  (declare (edited  "28-AUG-1998")
	   (authors Ameier)
	   (input   "A clause.")
	   (effect  "None.")
	   (value   "A list of clauses (siehe oben)."))
  (let* ((just (node~justification clause)))

    (when (null (res~paramodulation-p just))
      (error "~% Something went wrong in function blik=complete-paramod, should only be used on paramod-clauses. ~%"))

    (let* ((mother-clause (res~paramod-mother just))
	   (orig-mother-length (length (cl~literals mother-clause)))
	   (literals-to-check (do* ((rest-literals (cl~literals clause) (rest rest-literals))
				    (i orig-mother-length (- i 1))
				    (back-literals nil))
				  ((= 0 i)
				   back-literals)
				(let* ((head-literal (first rest-literals)))
				  (setq back-literals (append back-literals (list head-literal))))))
	   
	   (father-clause (res~paramod-father just))
	   (father-position (res~paramod-father-position just))
	   (father-renaming (second (res~justification-renamings just)))
	   (direction (res~paramod-direction just))
	   (mgu (res~justification-unifier just))
	   (eq-literal (data~struct-at-position father-clause father-position))
	   (eq-side (if (or (string= (string direction) "lr")
			    (string= (string direction) "LR"))
			(first (data~appl-arguments (lit~atom eq-literal)))
		      (second (data~appl-arguments (lit~atom eq-literal)))))
	   (unified-eq-side (subst~apply mgu (subst~apply father-renaming eq-side)))
	   (unified-eq-side-occ (data~substruct-positions unified-eq-side literals-to-check))
	   (all-pos-lists (blik=compute-pot-set unified-eq-side-occ)))

      (cons clause (mapcar #'(lambda (pos-list)
			       (blik=paramod-at-all-positions pos-list
							      clause
							      father-clause father-position
							      direction))
			   all-pos-lists)))))

(defun blik=compute-pot-set (list-of-pos)
  (declare (edited  "31-AUG-1998")
	   (authors Ameier)
	   (input   "A list of positions.")
	   (effect  "None.")
	   (value   "A list of all possible sublists of the input-list."))	   
  (if (null list-of-pos)
      nil
    (let* ((head-pos (first list-of-pos))
	   (rest-pot-set (blik=compute-pot-set (rest list-of-pos))))
      (append (list (list head-pos))
	      rest-pot-set
	      (mapcar #'(lambda (pos-list)
			  (cons head-pos pos-list))
		      rest-pot-set)))))


(defun blik=paramod-at-all-positions (pos-list mother-clause
					       father-clause father-position
					       direction)
  (declare (edited  "31-AUG-1998")
	   (authors Ameier)
	   (input   "A list of positions, a mother-clause, a father-clause, father-position and a direction.")
	   (effect  "None.")
	   (value   "The clause that arises if one applies paramodulation on all positions of the clause."))
  (do* ((rest-positions pos-list (rest rest-positions))
	(current-mother-clause mother-clause))
      ((null rest-positions)
       current-mother-clause)
    (let* ((next-pos (first rest-positions)))
      (multiple-value-bind
	  (renamed-father renaming)
	  (res~separate-clauses current-mother-clause father-clause)
	(let* ((ren-eq-literal (data~struct-at-position renamed-father father-position))
	       (ren-eq-side (if (or (string= (string direction) "lr")
				    (string= (string direction) "LR"))
				(first (data~appl-arguments (lit~atom ren-eq-literal)))
			      (second (data~appl-arguments (lit~atom ren-eq-literal)))))
	       (ren-replace-side (if (or (string= (string direction) "lr")
					 (string= (string direction) "LR"))
				     (second (data~appl-arguments (lit~atom ren-eq-literal)))
				   (first (data~appl-arguments (lit~atom ren-eq-literal)))))
	       (mother-term-at-pos (data~struct-at-position current-mother-clause next-pos))
	       (matcher (term~alpha-match ren-eq-side mother-term-at-pos)))
	  
	  (when (null matcher)
	    (error "~% Something wrong in function blik=paramod-at-all-positions, match should always work!"))
	  
	  (let* ((replace-term (subst~apply matcher ren-replace-side))
		 (mother-pos-num (pos~first next-pos))
		 (new-literals (mapcar #'(lambda (lit)
					   (data~copy lit :downto '(data+primitive)))
				       (append
					(data~replace-at-position (cl~literals current-mother-clause)
								  next-pos
								  replace-term)
					(mapcar #'(lambda (lit)
						    (subst~apply matcher lit))
						(remove ren-eq-literal (cl~literals renamed-father))))))
		 (new-just (res~paramod-create current-mother-clause next-pos (subst~create nil nil)
					       father-clause father-position renaming
					       direction matcher
					       
					       (intern (format nil "HIHI-just-~A" (incf res*justification-counter))
						       (find-package :keim))
					       
					       ))
		 (new-clause (cl~create new-literals :justification new-just)))
	    (setq current-mother-clause new-clause)))))))

#| ----------------------------------------------- PARSE UTILITIES ---------------------------------------------------------- |#
   
(defun blik=string2object (string &key (type nil))
  (declare (edited  "17-MAY-1996")
	   (authors Ameier)
	   (input   "A string")
	   (effect  "None.")
	   (value   "If the string corresponds to an object in the blik*convert-list"
		    "or the blik*local-clause-vars list the corresponding object is returned."
		    "otherwise the string corresponds to a new renaming var, so this"
		    "new renaming-var is created, the blik*local-clause-vars list is updated"
		    "by the pair new-var/string and the new-var is returned."))
  (let ((member-convert-list (first (first (member string blik*convert-list
						   :test #'(lambda (string pair) (string= string (second pair))))))))
    (if member-convert-list
	;; -> already in convert-list -> reconvert the string to an object
	;;member-convert-list
	
	(cond ((typep member-convert-list 'term+number)
	       member-convert-list)
	      ((string-equal (keim~name member-convert-list) "=")
	       (env~lookup-object '= blik*current-environment))
	      (t
	       member-convert-list))
	  
      ;; not yet in convert-list -> look at blik*local-clause-vars
      (let ((member-local-clause (first (first (member string blik*local-clause-vars
						       :test #'(lambda (string pair) (string= string (second pair))))))))
	(if member-local-clause
	    member-local-clause   ;; -> already a new local clause var produced -> return it
	  ;; not yet a new var produced, produce it , add the pair (new-var "var-string") to  blik*local-clause-vars and
	  ;; return the new-var
	  (let* ((needed-type (cond ((null type)
				     (type~i))
				    ((null (type~variable-p type))
				     type)
				    (t ;; -> type ist type-variable
				     (if (env~lookup-object (keim~name type) blik*current-environment)
					 ;; ist im environment
					 type
				       ;; ist nicht im environment -> Kappa gebunden irgendwo her
				       (let* ((new-symbol (term~generate-new-name 'ntv blik*current-environment))
					      (new-type-var (type~variable-create new-symbol)))
					 (env~enter new-symbol new-type-var blik*current-environment)
					 new-type-var)))))
		 (new-var (term~generate-term-primitive-with-new-name 'brv- needed-type 'term+variable blik*current-environment)))
	    
	    (setq blik*temporary-variables (cons new-var blik*temporary-variables))
	    (setq blik*local-clause-vars (cons (list new-var string) blik*local-clause-vars))
	    
	    new-var))))))

(defun blik=number2clause (number)
  (declare (edited  "17-MAY-1996")
	   (authors Ameier)
	   (input   "A number.")
	   (effect  "None.")
	   (value   "If the number corresponds to a clause and is therefor standing in"
		    "the blik*number-clause-list the corresponding clause is returned."
		    "otherwise nil."))
  (second (first (member number blik*number-clause-list
			 :test #'(lambda (number pair) (= number (first pair)))))))



#| ----------------------------------------------- ENDE: PARSE UTILITIES ---------------------------------------------------- |#

#| ----------------------------------------------- GENERATE BLIKSEM INPUT --------------------------------------------------- |#

(defun blik=add-string-to-in-string (string)
  (declare (edited  "25-MAR-1998")
	   (authors Ameier)
	   (input   "A string.")
	   (effect  "Adds the string to the blik*in-string.")
	   (value   "Undefined."))
  (setq blik*in-string (format nil "~A~A" blik*in-string string)))

(defun blik=print (clauses command-string)
  (declare (edited  "23-JUL-1998")
	   (authors Ameier)
	   (input   "A set of clauses and the input-command-string.")
	   (effect  "None.")
	   (value   "Creates the input file in blik*in-string."))

  (setq blik*in-string "")

  ;; fuegt den string mit den entsprechenden Steuerkommandos ein.
  (blik=add-string-to-in-string command-string)

  (blik=add-string-to-in-string #\Newline)
  (blik=add-string-to-in-string #\Newline)

  ;; fuegt die klauseln ein
  (mapc #'(lambda (clause)
	    (blik=print-clause clause)
	    (blik=add-string-to-in-string #\Newline))
	clauses))

(defun blik=print-clause (clause)
  (declare (edited  "22-AUG-1997")
	   (authors Ameier)
	   (input   "A clause.")
	   (effect  "A BLIKSEM-input representation of the CLAUSE is added to the blik*in-string.")
	   (value   "Undefined." ))
  ;; (blik=add-string-to-in-string #\Newline)

  ;; forall ueber freie Variablen vornedran
  (let* ((free-variables (data~free-variables clause))
	 (con-name-strings-of-vars (mapcar #'blik=get-checked-name-to-object free-variables)))
    (when con-name-strings-of-vars
      (blik=add-string-to-in-string "[ ")
      (blik=add-string-to-in-string (first con-name-strings-of-vars))
      (mapcar #'(lambda (var-string)
		  (blik=add-string-to-in-string ", ")
		  (blik=add-string-to-in-string var-string))
	      (rest con-name-strings-of-vars))
      (blik=add-string-to-in-string " ] ")))
  
  (blik=print-term (first (cl~literals clause)))
  (mapc #'(lambda (lit)
	    (blik=add-string-to-in-string " | ")
	    (blik=print-term lit))
	(rest (cl~literals clause)))
  (blik=add-string-to-in-string "."))


(defgeneric blik=print-term (object)
  (declare (edited  "21-APR-1993 09:14")
	   (authors AMEIER)
	   (input   "An term object.")
	   (effect  "A BLIKSEM-input representation of OBJECT is added to the blik*in-string.")
	   (value   "Undefined."))
  (:method ((literal lit+literal))
	   (when (and (not (lit~positive-p literal)) (not (atptop~equation-p literal)))
	     (blik=add-string-to-in-string "!"))
	   (when (atptop~equation-p literal)
	     (if (lit~positive-p literal)
		 (keim~put (lit~atom literal) :polarity 't)
	       (keim~put (lit~atom literal) :polarity nil)))
	   (blik=print-term (lit~atom literal)))
  (:method ((var term+variable))
	   (blik=add-string-to-in-string (blik=get-checked-name-to-object var)))
  (:method ((const term+constant))
	   (blik=add-string-to-in-string (blik=get-checked-name-to-object const)))
  (:method ((term term+appl))
	   (if (atptop~equation-p term)
	       (progn
		 (blik=print-term (first (data~appl-arguments term)))
		 (if (keim~get term :polarity)
		     (blik=add-string-to-in-string "=")
		   (blik=add-string-to-in-string "!="))
		 (keim~remprop term :polarity)
		 (blik=print-term (second (data~appl-arguments term))))
	     (progn
	       (blik=print-term (data~appl-function term))
	       (let ((args (data~appl-arguments term)))
		 (blik=add-string-to-in-string "(")
		 (blik=print-term (first args))
		 (mapc #'(lambda (term)
			   (blik=add-string-to-in-string ",")
			   (blik=print-term term))
		       (rest args))
		 (blik=add-string-to-in-string ")"))))))

#| ----------------------------------------------- ENDE: Generate Bliksem Input --------------------------------------------- |#

#| ----------------------------------------------- MAIN --------------------------------------------------------------------- |#

(defun blik~add-in-string! (bliksem-problem clauses command-string)
  (declare (edited  "23-JUL-1998")
	   (authors Ameier)
	   (input   "The bliksem-problem, the clauses and the command-string.")
	   (effect  "The blik*in-string is produced and added to the bliksem-problem.")
	   (value   "Undefined."))

  ;; konstruiert das bliksem.in file im blik*in-string
  (blik=print clauses command-string)

  ;; setzt atp-in-string im bliksem-problem
  (setf (atpprb~problem-atp-in-string bliksem-problem) blik*in-string)
  )

#|

Fliegt raus ab Versin 3.3

(defun blik=se-current-type-var-subst! ()
  (declare (edited  "27-MAR-1998")
	   (authors Ameier)
	   (input   "None.")
	   (effect  "If the blik*current-environment contains a type-var-subst, the global"
		    "variable blik*current-type-var-subst is set to this value, otherwise"
		    "a new substitution is created, added with key type-var-subst in the"
		    "environment and blik*current-type-var-subst is set to this new substitution.")
	   (value   "Undefined."))
  (let* ((type-var-subst (env~lookup-object 'type-var-subst blik*current-environment)))
    (if type-var-subst
	(setq blik*current-type-var-subst type-var-subst)
      (let ((new-subst (subst~create nil nil)))
	(setq blik*current-type-var-subst new-subst)
	(env~enter 'type-var-subst new-subst blik*current-environment)))))

|#

(defun blik~generate-bliksem-problem (conclusion-node assumption-nodes ho-pds)
  (declare (edited  "23-JUL-1998")
	   (authors Ameier)
	   (input   "A conclusion node, a set of assumption nodes and the pds the nodes are from.")
	   (effect  "None.")
	   (value   "A atp-problem of type bliksem, with a partial resolution proof"
		    "Since at the moment we have no parsing back of bliksem outputs we"
		    "need no storage of global vars and."
		    "Remark: atp-in-sting is not set !!"))

  (setq blik*convert-counter 0)       ;; setzt counter fuer neue Namen auf 0
     
  (let* ((res-proof (atptop~resolution-proof-create-from-nodes conclusion-node assumption-nodes ho-pds)))
    
    (setq blik*current-problem res-proof)
    (setq blik*current-environment (res~proof-environment res-proof))

    ;; Fliegt raus ab VErsion 3.3.
    ;; (blik=set-current-type-var-subst!)   ;; setzt variable blik*current-type-var-subst
    
    ;; translate the initial resolution proof res-proof to f.o. and normalize it
    (p2f~translate res-proof)

    ;; Normalisierung 
    (omega~message "~% Normalizing ...")
    (hocnf~normalize-res-proof! res-proof)

    ;; beachte behandlung der Gleichheit !
    (blik=handle-equality res-proof)

    ;; Remove clauses that contain abstractions
    ;; After p2f translation it can happen that there are clauses that contain lambda-abstraction
    ;; Such clauses can not be handled by OTTER (or any f.o. ATP and are therefore removed from the
    ;; clauses list
    (atptop~remove-clauses-with-abstractions! res-proof)
    
    ;; Compute the convertion of names
    (blik=compute-convert-list (res~proof-clauses res-proof))

    (atpprb~create-fo-problem (gensym "bliksem-problem-")
			      'bliksem
			      nil    ;; bliksem-in-file kommt erst spaeter dazu: -> blik~add-in-string!
			      nil
			      res-proof
			      (list blik*convert-list)
			      (list 'p2f p2f*domain p2f*codomain))))

    
(defun blik~call-bliksem (open-node ho-pds dir ressource command-string parse-back)
  (declare (edited  "23-JUL-1998")
	   (authors Ameier)
	   (input   "An open node and the according pds, that contains this node. A directory where"
		    "the new files should stand and the time ressource.")
	   (effect  "None.")
	   (value   "If bliksem finds a proof the message 'proof found' is printed and t is returned, otherwise"
		    "nil is returned. If bliksem finds a saturation the message 'saturation found' is printed."))
  
  (let* ((problem-name (keim~name ho-pds))
	 (bliksem-problem-dir (merge-pathnames (format nil "~A/" (string-downcase problem-name)) dir))
	 (in-file (merge-pathnames "bliksem.in" bliksem-problem-dir))
	 (out-file (merge-pathnames "bliksem.out" bliksem-problem-dir))
	 (bliksem-problem (blik~generate-bliksem-problem open-node
							 (remove open-node (pds~node-supports open-node))
							 ho-pds))
	 (res-proof (atpprb~problem-part-res-proof bliksem-problem)))

    ;; erzeugt directory falls es nicht existiert
    (unless (probe-file bliksem-problem-dir)
      (sys~call-system (format nil "mkdir ~A" (string-right-trim '(#\/) (namestring bliksem-problem-dir)))))
;;;    (format t "~A" (tstp~prob2tstp res-proof))
    ;; erzeuge bliksem.in file in blik*in-string, fuege blik*in-string zum bliksem-problem hinzu 
    (blik~add-in-string! bliksem-problem
			 (res~proof-clauses res-proof)
			 (if (null (string= command-string ""))
			     command-string
			       
			   (format nil "~A~A~A~A~A"
    				   "Set( totalproof, 1 )."
    				   #\Newline
    				   "Set( prologoutput, 1 )."
    				   #\Newline
    				   "Auto."
				   ))) ;; -> auto-mode
    
;;    (format t "~A" (atpprb~problem-part-res-proof bliksem-problem))
    ;; call-bliksem vot Ort -> schreibt bliksem.out file in den out-string des bliksem-problems
    (blik=call-bliksem! bliksem-problem bliksem-problem-dir ressource)
    
    ;; parsen des bliksem-beweises
    (blik~complete-bliksem-problem! bliksem-problem :parse-back parse-back)))

(defun blik=call-bliksem! (bliksem-problem bliksem-problem-dir ressource)
  (declare (edited  "26-MAR-1998")
	   (authors Ameier)
	   (input   "A bliksem-problem, the directory for input, output and a time-ressource.")
	   (effect  "Writes the in-string of the bliksem-problem to the file bliksem.in in the"
		    "directory, calls bliksem on it, reads the file bliksem.out from the directory"
		    "and writes it into the out-string of the bliksem-problem.")
	   (value   "Undefined."))

  (let* ((in-file (merge-pathnames "bliksem.in" bliksem-problem-dir))
	 (temp-out-file (merge-pathnames "tmp.out" bliksem-problem-dir))
	 (out-file (merge-pathnames "bliksem.out" bliksem-problem-dir)))

    (atptop~print-string-in-file (atpprb~problem-atp-in-string bliksem-problem) in-file)
    (atptop~print-string-in-file (atpprb~problem-atp-in-string bliksem-problem) "/home/jzimmer/blik.in")

    (let* ((call-flag (atptop~call-with-time-ressource (format nil "~A < ~A >! ~A; mv ~A ~A &"
							       (blik~program) in-file temp-out-file
							       temp-out-file out-file)
						       out-file
						       "bliksem"
						       bliksem-problem-dir
						       ressource)))

      (if (null call-flag)
	  (omega~message "~% Bliksem was not able to find a proof in the given time resource.")
	
	;; read bliksem.out file as string ans set atp-out-string of the bliksem-problem
	(setf (atpprb~problem-atp-out-string bliksem-problem)
	      (atptop~read-file-as-string out-file))))))

(defun blik~complete-bliksem-problem! (bliksem-problem &key (parse-back 't))
  (declare (edited  "23-JUL-1998")
	   (authors Ameier)
	   (input   "A bliksem problem.")
	   (effect  "None.")
	   (value   "If there is an atp-out-string that contains the line 'found a proof!' an"
		    "according message is printed and t returned. If a line 'found a saturation!'"
		    "is found, an according message is printed and nil returned. If none of both is"
		    "found the message 'no proof found' is printed and nil is returned."))
  (if (null (atpprb~problem-atp-out-string bliksem-problem))
      nil
    (let* ((bliksem-out-string (atpprb~problem-atp-out-string bliksem-problem))
	   (res-proof (atpprb~problem-part-res-proof bliksem-problem))
	   (global-vars (atpprb~problem-global-vars bliksem-problem))
	   (translation-settings (atpprb~problem-translation-settings bliksem-problem)))

      (setq blik*convert-list (first global-vars))

      (setq blik*current-problem res-proof)
      (setq blik*equality-object (env~lookup-object '= (res~proof-environment res-proof)))
      (setq blik*current-environment (res~proof-environment res-proof))

      ;; Fliegt raus ab VErsion 3.3
      ;; (blik=set-current-type-var-subst!)   ;; setzt blik*current-type-var-subst

      (setq blik*temporary-variables nil) ;; setzt temporaeri variablen auf nil

      (setq p2f*domain (second translation-settings))            ;; stellt Uebersetzungsinformation pl2p wieder her
      (setq p2f*codomain (third translation-settings))

      ;;read bliksem out:
      (let* ((proof-flag (blik=read bliksem-out-string res-proof parse-back)))
	
	;; remove temporary-variables
	(mapcar #'(lambda (var)
		    (env~remove (keim~name var) blik*current-environment))
		blik*temporary-variables)
	
	(cond ((equal proof-flag 'proof)
	       (omega~message "~% Bliksem has found a proof.~%")
	       (if parse-back
		   (progn
		     (setf (res~proof-empty-clause res-proof)
			   (otter=find-empty-clause (res~proof-step-clauses res-proof)))
		     (setq omega*current-resolution-proof res-proof)
		     (res~add-proof-in-hash-table res-proof)		  
		     (atptop~order-resolution-steps! res-proof)
		     (omega~message "~% OMEGA*CURRENT-RESOLUTION-PROOF IS SET TO THE FOUND RESOLUTION PROOF")
		     res-proof)
		 't))
	      ((equal proof-flag 'sat)
	       (omega~message "~% Bliksem has found a saturation.~%")
	       nil)
	      (t
	       (omega~message "~% Bliksem has not found a proof.~%")
	       nil))))))


(defun blik~generate-bliksem-problem-default! (conclusion-node assumption-nodes ho-pds)
  (declare (edited  "26-MAR-1998")
	   (authors Ameier)
	   (input   "A conclusion-node, a set of assumption-nodes and the pds the nodes are from.")
	   (effect  "The created bliksem-problem is inserted into the list in the plist entry"
		    "'atp-problems of the conclusion node.")
	   (value   "A atp-problem with type bliksem, a partial resolution proof."
		    "The global-vars are set completely: proof-object -> t"
		    "The atp-in-string is also already set, using the standart auto-mode"
		    "settings without additional information."))
  (let* ((bliksem-problem (blik~generate-bliksem-problem conclusion-node assumption-nodes ho-pds)))
    
    (blik~add-in-string! bliksem-problem
			 (res~proof-clauses (atpprb~problem-part-res-proof bliksem-problem))
	
    
			 (format nil "~A~A~A~A~A"
				 "Set( totalproof, 1 )."
				 #\Newline
				 "Set( prologoutput, 1 )."
				 #\Newline
				 "Auto.")) ;; -> auto-mode
    
    (keim~put conclusion-node 'atp-problems (cons bliksem-problem (keim~get conclusion-node 'atp-problems)))
    
    bliksem-problem))


#| ---------------------------------------------------- ENDE: MAIN ------------------------------------------------------- |#

#| ------------------------------------------- Read Output free for Bliksem ---------------------------------------------- |#

(defun blik~read-bliksem-output (open-node file)
  (declare (edited  "02-JUN-2000")
	   (authors Ameier)
	   (input   "An open node and a file.")
	   (effect  "Mayby changes the bliksem global variables.")
	   (value   "1. For the open node a new resolution proof is created."
		    "2. The file is tried to read as a bliksem proof for this resolution proof"
		    "   In particular, a mapping is computed from initial clauses of the bliksem"
		    "   file and the initial clauses of the new reslution proof."
		    "If it was possible to read the file as a bliksem proof file, the resolution"
		    "proof is completed (an empty clauses is dreived according to the proof in"
		    "the file) and this complete resolution proof is returned."
		    "If it was not possible to read the file as a bliksem proof file nil is"
		    "returned."))
  (let* ((bliksem-problem (blik~generate-bliksem-problem open-node
							 (remove open-node (pds~node-supports open-node))
							 omega*current-proof-plan))
	 (res-proof (blik~complete-bliksem-problem-from-file! bliksem-problem file)))
    
    res-proof))

(defun blik~complete-bliksem-problem-from-file! (atp-problem file)
  (let* ((out-string (atptop~read-file-as-string file))
	 (res-proof (atpprb~problem-part-res-proof atp-problem))
	 (global-vars (atpprb~problem-global-vars atp-problem)))
    
    ;; was ist mit translation settings? -> braucht man zumindest nicht fuer reine first order probleme

    (setf (atpprb~problem-atp-out-string atp-problem) out-string)
      
    ;; Note: in the atp-problem we have in the global-var-list:
    ;; 1.  blik*convert-list       -> we have to reconstruct by matching the clauses!
        
    (setq blik*convert-list (blik=reconstruct-convert-list! res-proof out-string))
       
    ;; Other necessary settings
    (setq blik*current-problem res-proof)
    (setq blik*equality-object (env~lookup-object '= (res~proof-environment res-proof)))
    (setq blik*current-environment (res~proof-environment res-proof))
    (setq blik*temporary-variables nil)                      ;; setzt temporaeri variablen auf nil
   
    (if (null blik*convert-list)
	(progn
	  (omega~message "~% Could not match the out-file to the problem.")
	  nil)
      (let* ((proof-flag (blik=read out-string res-proof 't)))

	;; remove temporary-variables
	(mapcar #'(lambda (var)
		    (env~remove (keim~name var) blik*current-environment))
		blik*temporary-variables)

	(cond ((equal proof-flag 'proof)
	       (omega~message "~% Bliksem has found a proof.~%")
	       (setf (res~proof-empty-clause res-proof)
		     (otter=find-empty-clause (res~proof-step-clauses res-proof)))
	       (setq omega*current-resolution-proof res-proof)
	       (res~add-proof-in-hash-table res-proof)		  
	       (atptop~order-resolution-steps! res-proof)
	       (omega~message "~% OMEGA*CURRENT-RESOLUTION-PROOF IS SET TO THE FOUND RESOLUTION PROOF")
	       res-proof)
	      ((equal proof-flag 'sat)
	       (omega~message "~% Bliksem has found a saturation.~%")
	       nil)
	      (t
	       (omega~message "~% Bliksem has not found a proof.~%")
	       nil))))))

(defun blik=reconstruct-convert-list! (res-proof out-string)
  (declare (edited  "29-MAY-2000")
	   (authors Ameier)
	   (input   "The current resolution proof and the out-string of a bliksem call.")
	   (effect  "The content of blik*convert-list and blik*local-clause-vars can be changed.")
	   (value   "First the input clauses of the proof in the out-string are read. Thereby, the new"
		    "created constants have as names exactly the names they have in the out-file."
		    "Then these read clauses are matched against the input clauses in the resolution proof."
		    "This results (if successfull) in a mapping, that mapps each constant in the"
		    "read clauses to a constant in the input clauses. From this mapping we compute"
		    "a list of pairs of constants and strings (where the constant is taken from the"
		    "new resolutiomn proof whereas the string is from the out-file."))

  (blik=produce-reflexivity-item res-proof)

  
  
  (let* ((res-proof-input-clauses (res~proof-clauses res-proof))
	 (out-file-input-clauses (blik=read-input-clauses out-string)))
    
    (multiple-value-bind
	(success mapping info-triples)
	(atptop~subset-by-equality-except-names-p out-file-input-clauses
						  (cons blik*reflexivity-item
							(apply 'append (mapcar #'blik=produce-factors res-proof-input-clauses))))
      ;; It can happen that bliksem uses directly some factors of input clauses -> complete the set by all possible
      ;; factors!
      
      (if success

	  (progn
	    (atptop~change-resolution-proof! res-proof info-triples)
	    
	    (let* ((mapp-domain (mapp~domain mapping))
		   (mapp-codomain (mapp~codomain mapping))
		   (mapp-constant-domain (remove-if-not #'term~constant-p mapp-domain))
		   (mapp-constant-codomain (remove-if-not #'term~constant-p mapp-codomain)))
	      
	      (mapcar #'(lambda (dom codom)
			  (let* ((name (keim~name dom)))
			    (list codom (if (stringp name)
					    name
					  (string name)))))
		      mapp-constant-domain mapp-constant-codomain)))
	nil))))

(defun blik=read-input-clauses (out-string)
  (declare (edited  "29-MAY-2000")
	   (authors Ameier)
	   (input   "A bliksem outfile.")
	   (effect  "During the parsing of the input clauses (see value) the blik*convert-list is changed.")
	   (value   "Reads the input clauses, without a convertion setting."
		    "Thereby, for each name that starts with a capital letter a new variable of type i is produced, and"
		    "for each other name a constant of type (i <- ...) is produced."))
  
  (setf blik*convert-list nil)
  
  (let* ((line-strings (atptop~divide-string out-string #\Newline))
	 (proof-lines (do* ((rest-string-lines line-strings (rest rest-string-lines))
			    (break-flag nil)
			    (read-flag nil)
			    (return-list nil))
			  ((or (null rest-string-lines)
			       break-flag)
			   return-list)
			(let* ((string-line (first rest-string-lines)))
			  (cond ((string= string-line "] .")
				 (setf break-flag 't))
				((string= string-line "Clauses:")
				 (setf rest-string-lines (rest rest-string-lines)) ;; skip one line (containing "[" only)
				 (setf read-flag 't))
				(read-flag
				 (setf return-list (append return-list (list string-line))))))))
	 ;; Unfortunately, clauses can be broken in the bliskem out file, that means, a line does not necessaryly contain a clause
	 ;; completely, it can also be happen, that the clause continues in the next line!
	 ;; Hence, we put together as net the clauses, such that one string contains exactly one clause!
	 (initial-clauses-string (do* ((rest-string-list proof-lines (rest rest-string-list))
				       (back-string ""))
				     ((null rest-string-list)
				      back-string)
				   (setf back-string (format nil "~A~A" back-string (first rest-string-list)))))

	 (lines-of-initial-clauses (do* ((rest-string initial-clauses-string)
					 (back-list nil))
				       ((string= rest-string "")
					back-list)
				     (multiple-value-bind
					 (clause-string new-rest-string)
					 (atptop~get-next-word rest-string #\] :ignore-char-list (list #\[ #\' #\space))
				       (setf back-list (append back-list (list clause-string)))
				       (setf rest-string new-rest-string))))
	 ;; Note:
	 ;; If a input clause is parsed, each name that starts with a capital letter is interpreted as variable!
	 ;; -> a new local variable of type i is created (local = relevant only for the clause itself) 
	 ;; Each other letter is interpreted as a constant. Hence, e new constant is created. The type of the constant
	 ;; is (o <- i ...) or (i <- i ...) depending on whether the constant is the predicat of a literal or internal
	 ;; and on the number of the arguments on which it is applied.
	 ;; Since constants are not local for a clause, for each new created constant a new entry is made in the
	 ;; blik*convert-list
	 (input-clauses (apply #'append (mapcar #'(lambda (line)
						    (blik=parse-free-input-clause-line line))
						lines-of-initial-clauses))))
    input-clauses))

(defun blik=parse-free-input-clause-line (input-clause-line)
  (declare (edited  "29-MAY-2000")
	   (authors Ameier)
	   (input   "A string representing a input clause.")
	   (effect  "The variables blik*local-clause-vars and blik*convert-list can be changed"
		    "(blik*local-clause-vars is set to nil at the beginning, then new local variables"
		    " are added. For blik*convert-list see blik=read-input-clauses.")
	   (value   "A list containing the new created clause."))
  
  (setf blik*local-clause-vars nil)

  (let* ((cleared-clause-string (if (equal (char input-clause-line 0) #\,)
				    (atptop~cut-first-char input-clause-line)
				  input-clause-line))
	 ;; cuts the first four letters (which are "   [") and the last two or the last letter (which are "]" or "],")
	 (literal-list (do* ((rest-literal-strings (blik=separate-literals cleared-clause-string) (rest rest-literal-strings))
			     (literal-list nil))
			   ((null rest-literal-strings)
			    literal-list)
			 (let* ((head-literal (first rest-literal-strings)))
			   (setf literal-list (append literal-list
						      (list (blik=parse-free-literal
							     (atptop~filter-chars head-literal :ignore-char-list (list #\space #\'))))))))))
    (list (cl~create literal-list))))

(defun blik=parse-free-literal (literal-string)
  (declare (edited  "29-MAY-2000")
	   (authors Ameier)
	   (input   "A string representing a literal.")
	   (effect  "See blik=parse-free-input-clause-line.")
	   (value   "A literal."))
  (if (equal (char literal-string 0) #\~)
      (lit~literal-create (blik=parse-free-term (atptop~cut-last-char (atptop~cut-x-first-chars literal-string 2))
						:predicat 't) nil)
    (lit~literal-create (blik=parse-free-term literal-string :predicat 't) 't)))

(defun blik=parse-free-term (term-string &key (predicat nil))
   (multiple-value-bind
      (functor-string rest-string)
      (atptop~get-next-word term-string #\()
     ;; reads till a "(" is reached 
     ;; if rest-string = "" (empty) -> term isn't an application -> otherwise term is an application
     
     (if (string= rest-string "")
	 (blik=free-string2object functor-string :predicat predicat :number-of-args 0)
	(let* ((args (mapcar #'(lambda (term-string)
				 (blik=parse-free-term term-string))
			     (blik=parse-term-list (atptop~cut-last-char rest-string))))
	       (functor (blik=free-string2object functor-string :predicat predicat :number-of-args (length args))))
	  (term~appl-create functor args)))))

(defun blik=free-string2object (string &key (predicat nil) (number-of-args 0))
  (declare (edited  "29-MAY-2000")
	   (authors Ameier)
	   (input   "A string, and as keywords predicat (to sign whether the string should be intrpreted as"
		    "predicat or as function) and number-of-args (to sign how many arguments the premitive"
		    "corresponding to the string should have.")
	   (effect  "The blik*convert-list and blik*local-clause-vars can be changed:"
		    "1.) If the string starts with a capital letter and it is not alsready conatined in an entry"
		    "    in the blik*local-clause-vars, a new variable with type i is created and a corresponding"
		    "    entry is made in the blik*local-clause-vars."
		    "2.) Othwewise: if the string is not contained in an entry in the blik*convert-list, a new"
		    "    constant (whose type depends on the keywords predicat and number-of-args) is created"
		    "    and a corresponding entry is added to blik*convert-list.")		    
	   (value   "The object corresponding wrt. blik*convert-list or blik*local-clause-vars to the string."))
  (let ((member-convert-list (first (find string blik*convert-list
					  :test #'(lambda (string pair)
						    (string= string (second pair))))))
	(member-local-clause (first (find string blik*local-clause-vars
					      :test #'(lambda (string pair) (string= string (second pair)))))))

    (cond ((string-equal string "=")
	   (env~lookup-object '= blik*current-environment))

	  (member-convert-list
	   ;; -> string is already in blik*convert-list -> give back the corresponding object
	   
	   member-convert-list)
	  
	  (member-local-clause
	   ;; -> string is already in blik*local-clause-vars -> return it

	   member-local-clause)

	  (;; string neither in blik*convert-list nor blik*local-clause-vars
	   ;; -> create new object and add entry to blik*convert-list or blik*local-clause-vars
	   
	   (if (find (char string 0) otter*capital-letters)
	       ;; -> first letter of string is a capital letter
	       ;; -> produce new variable and add it to blik*local-clause-vars
	       
	       (let* ((new-var (term~generate-term-primitive-with-new-name 'orv- (type~i) 'term+variable blik*current-environment)))
		 
		 (setq blik*temporary-variables (cons new-var blik*temporary-variables))
		 (setq blik*local-clause-vars (cons (list new-var string) blik*local-clause-vars))
		 
		 new-var)

	     ;; first letter of string is not a capital letter
	     ;; -> produce new constant and add it to blik*convert-list
	     
	     (let* ((type (if predicat 
			      (type~predicate-create number-of-args)
			    (type~function-create number-of-args)))
		    (new-constant (term~constant-create string type)))
	       
	       (setq blik*convert-list (cons (list new-constant string) blik*convert-list))
	       
	       new-constant))))))


