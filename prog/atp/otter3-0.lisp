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


(mod~defmod otter
	    :uses (keim post env pos data subst term type res cl lit delta hocnf node just pds pdsj pdsn prob f2p p2f inter comint arg p2pl mixin
			)
	    :documentation "Calling otter in omega"
	    :exports (
		      otter~program
		      
		      otter+instantiation
		      otter~instantiation-create
		      otter~instantiation-p

		      otter~generate-otter-problem-default!
		      otter~add-in-string!
		      otter~complete-otter-problem!
		      otter~generate-otter-problem
		      otter~call-otter
		      otter~use-otter-out
		      otter~read-otter-output
		      otter~complete-otter-problem-from-file!
		      )
	    )





#| #{\section {Using The Otter Theorem Prover}

If the problems get easy, by dividing it interactively or with the planner in small and easy subparts that can be solved by an ATP, the
subproblems can be solved by OTTER. If OTTER succeeds a resolution proof can be produced, that can be translated into an ND-PROOF at
the assertion level and this ND-Proof can be inserted into the supperior ND-Proof.

#} |#


#| ------------------------------------------------ Global Variables ---------------------------------------------------- |#

(defvar otter*in-string nil)

(defvar otter*equality-object nil) ;; an variable to the = instance form problem in

(defvar otter*current-problem nil)

(defvar otter*current-environment nil) ;; to store the current environment in

(defun otter~program ()
  (let* ((prog (sys~getenv "OTTERHOME")))

     (when (or (null prog) (null (probe-file prog)))
      (error "There is no otter-executable at ~A, please check your path to the otter-executable." prog))

    prog))


;; this are the flags we need to parse the result:
(defvar otter*needed-flags (list (format nil "~A%%%% NEEDED FLAGS FOR PARSING %%%%~A" #\Newline #\Newline)
				 "set(detailed_history)." "set(order_history)."
				 "set(prolog_style_variables)." "set(factor)."
				 "set(demod_history)."
				 "clear(sort_literals)."
				 "clear(propositional)."
				 "clear(delete_identical_nested_skolem)." 
				 "clear(symbol_elim)." "clear(lex_order_vars)."

				 ))

;; this are the flags otter always sets in auto mode:
(defvar otter*auto-standart-flags (list "set(process_input)." "clear(print_kept)."
					"clear(print_new_demod)." "clear(print_back_demod)."
					"clear(print_back_sub)." "set(control_memory)."
					"assign(max_mem, 12000)." "assign(pick_given_ratio, 4)."
					"assign(stats_level, 1)." "assign(max_seconds, 10800)."))

(defvar otter*convert-list nil)
;; this list contains pairs (=lists) of original problem variable/constants names-strings and convert names-strings
;; (convert by otter=convert-name), cause otter has problems if a variable/constant name contains other
;; symbols than the alpabetics ,numbers and _


;; a counter to count the produced new names, the number is added to the name : checkedname_number:
(defvar otter*convert-counter nil)


;; this are the symbols, allowed in a name:
(defvar otter*name-symbols '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L
			   #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X
			   #\Y #\Z #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j
			   #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v
			   #\w #\x #\y #\z #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7
			   #\8 #\9 #\_))


(defvar otter*number-clause-list nil)
;; a list of pairs number - clause, the number is the number of clause in otter-proof

(defvar otter*just-counter 0)
;; a list to count the new generated justifications.

(defvar otter*temporary-variables nil)  

(defvar otter*local-clause-vars nil)

(defvar otter*rest-initial-clauses nil)
;; This list contains the current initial clauses not used yet

(defvar otter*reflexivity-item nil)
;; if equality is needed there is a new clause: { = x x } added.

(defvar otter*already-set nil)
;; A variable for temporary storing whether the input is set in Loui or emacs

(defvar otter*capital-letters (list #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N
				    #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))

#| ---------------------------------------------- ADDITIONAL JUSTIFICATIONS -------------------------------------------- |#

;; These are justification that otter use, but that are not needed in general, so steps (clauses) are only justified
;; with such justification during the process of reading a otter proof. In the resulting resolution proof no such
;; steps are contained.

;; instantiation
(defvar otter*instantiation-counter 0)

(eval-when (load compile eval)
  (defclass otter+instantiation (res+justification)
    ()))

(defun otter~instantiation-create (parent instantiation &optional (name (format nil "Instantiation-just-~A"
										(incf otter*instantiation-counter))))
  (make-instance 'otter+instantiation
		 :parents (list parent)
		 :positions nil
		 :unifier instantiation
		 :renamings nil 
		 :name name
		 :method 'instantiation))

(defmethod print-object ((resolution-just otter+instantiation) stream)
  (format stream "Instantiation of ~A"
	  (first (res~justification-parents resolution-just))))

(defgeneric otter~instantiation-p (object)
  (:method ((object cl+clause))
	   (otter~instantiation-p (node~justification object)))
  (:method (object)
	   (typep object 'otter+instantiation)))

#| -------------------------------------------------- PRINTING ---------------------------------------------------------- |#

(defun otter=print (mode declarations usable-clauses sos-clauses &key (docu 't))
  (declare (edited  "22-AUG-1997")
	   (authors Ameier)
	   (input   "The mode, choosen by the user, the declarations to control otter"
		    "and two sets of clauses, first the set of usable clauses and second"
		    "the set of sos-clauses. The keyword docu signs, whether the convert-list"
		    "should be added to the otter.in string or not.")
	   (effect  "None.")
	   (value   "The otter.in string"))

  (setq otter*in-string "")
  
  (otter=write-options declarations)
  (when docu
    (otter=write-convert-list))
  (cond ((eq mode 'auto)
	 (let* ((avoid-prop-clause (otter=non-prop-clause)))	   
	   (otter=print-usable-clauses (cons avoid-prop-clause usable-clauses))))
	(t
	 (otter=print-usable-clauses usable-clauses)
	 (otter=print-sos-clauses sos-clauses)))
  otter*in-string)


(defun otter=add-string-to-in-string (string)
  (declare (edited  "25-MAR-1998")
	   (authors Ameier)
	   (input   "A string.")
	   (effect  "Adds the string to the otter*in-string.")
	   (value   "Undefined."))
  (setq otter*in-string (format nil "~A~A" otter*in-string string)))

(defun otter=write-convert-list ()
  (declare (edited  "28-AUG-1997")
	   (authors Ameier)
	   (input   "None.")
	   (effect  "Adds the convert-list to the otter*in-string.")
	   (value   "Undefined."))
  (otter=add-string-to-in-string #\Newline)
  (otter=add-string-to-in-string #\Newline)
  (otter=add-string-to-in-string "%%%% THE NAME CONVERSIONS %%%%")
  (otter=add-string-to-in-string #\Newline)
  (mapcar #'(lambda (pair)
	      (otter=add-string-to-in-string (format nil "%%%%    ~A  -->  ~A"
						     (if (term~schema-p (first pair))
							 (keim~name (data~schema-range (first pair)))
						       (keim~name (first pair)))
						     (second pair)))
	      (otter=add-string-to-in-string #\Newline))
	  otter*convert-list)
  (otter=add-string-to-in-string #\Newline)
  (otter=add-string-to-in-string "%%%% THE CLAUSE LISTS %%%%")
  (otter=add-string-to-in-string #\Newline))


(defun otter=write-options (declarations)
  (declare (edited  "22-AUG-1997")
	   (authors Ameier)
	   (input   "The declarations for otter.in.")
	   (effect  "Adds the declarations to the otter*in-string.")
	   (value   "Undefined.")) 
  (mapcar #'(lambda (string)
	      (otter=add-string-to-in-string string)
	      (otter=add-string-to-in-string #\Newline))
	  declarations))


(defun otter=print-usable-clauses (ax-clauses)
  (declare (edited  "22-AUG-1997")
	   (authors Ameier)
	   (input   "A set of clauses.")
	   (effect  "First list(usable).  then the clauses and then end_of_list. are"
		    "written to otter*in-string.")
	   (value   "Undefined."))
  (otter=add-string-to-in-string #\Newline)
  (otter=add-string-to-in-string "list(usable).")
  (mapc #'(lambda (clause)
	    (otter=print-clause clause)
	    (otter=add-string-to-in-string #\Newline))
	ax-clauses)
  (otter=add-string-to-in-string #\Newline)
  (otter=add-string-to-in-string "end_of_list.")
  (otter=add-string-to-in-string #\Newline))

(defun otter=print-sos-clauses (th-clauses)
  (declare (edited  "22-AUG-1997")
	   (authors Ameier)
	   (input   "A set of clauses.")
	   (effect  "First list(sos).  then the clauses and then end_of_list. are"
		    "written to the otter*in-string.")
	   (value   "Undefined."))
  (otter=add-string-to-in-string #\Newline)
  (otter=add-string-to-in-string "list(sos).")
  (mapc #'(lambda (clause)
	    (otter=print-clause clause)
	    (otter=add-string-to-in-string #\Newline))
	th-clauses)
  (otter=add-string-to-in-string #\Newline)
  (otter=add-string-to-in-string "end_of_list.")
  (otter=add-string-to-in-string #\Newline))


(defun otter=print-clause (clause)
  (declare (edited  "22-AUG-1997")
	   (authors Ameier)
	   (input   "A clause.")
	   (effect  "A OTTER-input representation of the CLAUSE is added to the otter*in-string.")
	   (value   "Undefined." ))
  (otter=add-string-to-in-string #\Newline)
  (otter=print-term (first (cl~literals clause)))
  (mapc #'(lambda (lit)
	    (otter=add-string-to-in-string " | ")
	    (otter=print-term lit))
	(rest (cl~literals clause)))
  (otter=add-string-to-in-string "."))


(defgeneric otter=print-term (object)
  (declare (edited  "21-APR-1993 09:14")
	   (authors RICHTS)
	   (input   "An term object.")
	   (effect  "A OTTER-input representation of OBJECT is added to the otter*in-string.")
	   (value   "Undefined."))
  (:method ((literal lit+literal))
	   (if (and (not (lit~positive-p literal)) (not (atptop~equation-p literal)))
	       (otter=add-string-to-in-string "-"))
	   (if (atptop~equation-p literal)
	       (if (lit~positive-p literal)
		   (keim~put (lit~atom literal) :polarity 't)
		 (keim~put (lit~atom literal) :polarity nil)))
	   (otter=print-term (lit~atom literal)))
  (:method ((var term+variable))
	   (otter=add-string-to-in-string (otter=get-checked-name-to-object var)))
  (:method ((const term+constant))
	   (otter=add-string-to-in-string (otter=get-checked-name-to-object const)))
  (:method ((term term+appl))
	   (cond ((atptop~equation-p term)
		  (otter=print-term (first (data~appl-arguments term)))
		  (if (keim~get term :polarity)
		      (otter=add-string-to-in-string "=")
		    (otter=add-string-to-in-string "!="))
		  (keim~remprop term :polarity)
		  (otter=print-term (second (data~appl-arguments term))))
		 (t
		  (otter=print-term (data~appl-function term))
		  (let ((args (data~appl-arguments term)))
		    (otter=add-string-to-in-string "(")
		    (otter=print-term (first args))
		    (mapc #'(lambda (term)
			      (otter=add-string-to-in-string ",")
			      (otter=print-term term))
			  (rest args))
		    (otter=add-string-to-in-string ")"))))))


#| ----------------------------------------------- READING without parsing ---------------------------------------------- |#

(defun otter=read-without-parsing (res-proof otter-out-string)
  (declare (edited  "07-AUG-1996")
	   (authors Ameier)
	   (input   "The partial resolution proof and the otter-out-string.")
	   (effect  "None.")
	   (value   "Nil if the otter has failed to find a proof, t otherwise."))
  (let* ((line-strings (atptop~divide-string otter-out-string #\Newline)))
    (or (find "---------------- PROOF ----------------" line-strings :test #'string=)
	(find ";; BEGINNING OF PROOF OBJECT" line-strings :test #'string=)
	(find "Proof object:" line-strings :test #'string=))))



#| ----------------------------------------------- Reading without proof-object ---------------------------------------- |#

(defun otter=read-without-proof-object (res-proof otter-out-string)
  (declare (edited  "31-MAY-1996")
	   (authors Ameier)
	   (input   "The partial resolution proof and the otter-out-string.")
	   (effect  "The steps (clauses) of the proof are parsed from the otter-out-string"
		    "and are added to the partial res-proof. By this process"
		    "otter*rest-initial-clauses and otter*number-clause-list are changed"
		    "by removing used initial clauses and adding new number clause pairs.")
	   (value   "Nil if the otter has failed to find a proof, t otherwise."))

  (setq otter*rest-initial-clauses (if otter*reflexivity-item
				       (cons otter*reflexivity-item (res~proof-initial-clauses res-proof))
				     (res~proof-initial-clauses res-proof)))
  (setq otter*number-clause-list nil)

  (let* ((line-strings (atptop~divide-string otter-out-string #\Newline)))
    (multiple-value-bind
	(proof-flag file-lines)
	(do* ((rest-string-lines line-strings (rest rest-string-lines))
	      (return-list nil)
	      (proof-part-flag 'start))
	    ((or (null rest-string-lines) (equal proof-part-flag 'end)) (values proof-part-flag return-list))
	  (let* ((string-line (first rest-string-lines)))
	    (if (equal proof-part-flag 'proof)
		(if (string= string-line "------------ end of proof -------------")
		    (setq proof-part-flag 'end)
		  (if (not (string= string-line ""))
		      (setq return-list (append return-list (list string-line)))))
	      (if (string= string-line "---------------- PROOF ----------------")
		  (setq proof-part-flag 'proof)))))
      
      (if (equal proof-flag 'end) ;; proof was found
	  (let* ((check (do* ((rest-lines file-lines (rest rest-lines))
			      (check-flag 't))
			    ((or (null check-flag)
				 (null rest-lines))
			     check-flag)
			  (let* ((line (first rest-lines))
				 (clause (otter=parse-line line res-proof)))
			    (when (null clause)
			      (setq check-flag nil))))))
	    (if check
		't
	      (progn
		(omega~message "~% WARNING: OTTER PROOF can not be parsed !! Sorry !!")
		(omega~message "~% Try it again and guarant, that PROOF-OBJECT is SET. ~%")
		nil)))
	nil))))

(defun otter=parse-line (line-string res-proof &key (ignore-justification nil))
  (declare (edited  "15-MAY-1996")
	   (authors Ameier)
	   (input   "A string (representing a proof-line of an otter output file,"
		    "consisting of Number Justification Clause) ,the current"
		    "res-proof and a keyword ignore-justification (default nil).")
	   (effect  "The clause according to this line is computed and"
		    "if not a initial-clause added to the proof-steps of the"
		    "res-proof. The otter*number-clause-list is updated by"
		    "the new number clause pair. If the clause is a initial"
		    "clause it is removed from the otter*rest-initial-clause-list.")
	   (value   "The new produced clause from the line."))
  (multiple-value-bind
      (numbers-string justification-string clause-string)
      (otter=split-line-string line-string)

    (if (atptop~string-is-prefix-of-string-p "-ADDITIONALNONPROPPRED_" clause-string)
	't    ;; checkt ob es die Zusaetzliche-Klausel ist -> einfach nur 't zurueck 

      (let* ((numbers (mapcar #'atptop~parse-number
			      (atptop~divide-string numbers-string #\,)))
	     (otter-clause (otter=parse-clause clause-string))
	     (new-clause (otter=use-justification otter-clause (atptop~cut-first-char
								(atptop~cut-last-char justification-string))
						  res-proof)))
	
	(setq otter*number-clause-list (append (mapcar #'(lambda (number)
							   (list number new-clause))
						       numbers)
					       otter*number-clause-list))
	
	(otter=add-steps-to-proof! new-clause res-proof)
	
	new-clause))))

(defun otter=split-line-string (line-string)
  (declare (edited  "15-MAY-1996")
	   (authors Ameier)
	   (input   "A string, containing a proof-line of a otter output.")
	   (effect  "None.")
	   (value   "The string is departet in 3 parts who are returned as"
		    "multiple values."
		    "First: a string containing the numbers of the step/clause."
		    "Second: a string with the justification"
		    "Third: a string with the clause."))
  (multiple-value-bind
      (numbers-string rest-string)
      (atptop~get-next-word line-string #\space)
    (multiple-value-bind
	(justification-string clause-string)
	(atptop~get-next-word rest-string #\space)
      (values numbers-string justification-string clause-string))))

(defun otter=parse-while-number (string-list)
  (declare (edited  "28-MAY-1996")
	   (authors Ameier)
	   (input   "A list of strings.")
	   (effect  "None.")
	   (value   "If sring list beginns with number-strings these are parsed"
		    "till the first non-number-string is reached."
		    "Multiple-values:"
		    "First: The list of starting number-strings."
		    "Second: The rest string-list."))
  (do* ((last-head (first string-list) (first rest-string-list))
	(rest-string-list string-list (rest rest-string-list))
	(number-list nil)
	(flag t))
      ((or (null rest-string-list) (null flag)) (if (null flag)
						    (values number-list (cons last-head rest-string-list))
						  (values number-list nil)))
    (let ((number (atptop~parse-number (first rest-string-list))))
      (if number
	  (setq number-list (append number-list (list number)))
	(setq flag nil)))))
						      	    
(defun otter=parse-clause (clause-string)
  (declare (edited  "17-SEP-1996")
	   (authors Ameier)
	   (input   "A string (representing a clause in otter.out.")
	   (effect  "The otter*local-clause-vars is changed.")
	   (value   "The represented clause."))
  
  (setq otter*local-clause-vars nil)
  (let* ((literal-list (if (string= "$F." clause-string)
			   nil
			 (do* ((rest-literal-strings (atptop~divide-string clause-string #\|
									   :ignore-char-list '(#\. #\space))
						     (rest rest-literal-strings))
			       (literal-list nil))
			     ((null rest-literal-strings) literal-list)
			   (setq literal-list (append literal-list
						      (list (otter=parse-literal (first rest-literal-strings)))))))))
    (cl~create literal-list)))

(defun otter=use-justification (otter-clause justification-string res-proof)
  (declare (edited  "11-SEP-1996")
	   (authors Ameier)
	   (input   "The produced otter-clause, the justification-string and the resolution proof.")
	   (effect  "A clause is build from the just-string with our justifications, that is"
		    "equal to the eqp-clause modulo renaming and ordering and flip steps.")
	   (value   "The new clause."))
  (cond ((string= justification-string "")
	 ;; -> initial clause
	 (let* ((clause (find otter-clause otter*rest-initial-clauses  :test #'(lambda (clause1 clause2)
										 (atptop~clauses-equal-till-renaming-and-ordering-p
										  clause1 clause2)))))
	   
	   (setq otter*rest-initial-clauses (remove clause otter*rest-initial-clauses))
	   
	   clause))
	(t
	 (do* ((just-list (atptop~divide-string justification-string #\, :ignore-char-list '(#\[ #\])))
	       (first-just 't)
	       (current-clause-list nil))
	     ((null just-list)
	      (find otter-clause current-clause-list :test #'(lambda (clause1 clause2)
							       (atptop~clauses-equal-till-renaming-and-ordering-p
								clause1 clause2))))
	   (let* ((head (first just-list)))

	     (cond ((string= head "copy")

		    ;; always first just -> setzen der current-clause-list !!
		    
		    (let* ((copy-clause (otter=number2clause (atptop~parse-number (second just-list)))))

		      (setq current-clause-list (list copy-clause))
		      
		      (setq just-list (rest (rest just-list)))
		      
		      ))

		   ((string= head "back_demod")
		    
		    ;; always first just -> setzen der current-clause-list !!

		    (let* ((back-demod-clause (otter=number2clause (atptop~parse-number (second just-list)))))

		      (setq current-clause-list (list back-demod-clause))
		      
		      (setq just-list (rest (rest just-list)))

		      ))

		   ((string= head "unit_del")

		    ;; always nachgesetzt -> benutzen der current-clause-list !!
		    
		    (multiple-value-bind
			(numbers rest-just-list)
			(otter=parse-while-number (rest just-list))

		      (let* ((units (mapcar #'otter=number2clause numbers)))

			(setq current-clause-list
			      (apply 'append (mapcar #'(lambda (clause)
							 (do* ((rest-units units (rest rest-units))
							       (return-clause-set (list clause)))
							     ((null rest-units) return-clause-set)
							   (let* ((head-unit (first rest-units)))
							     (setq return-clause-set
								   (apply 'append (mapcar #'(lambda (current-return-clause)
											      (res~binary-resolution
											       current-return-clause head-unit))
											  return-clause-set))))))
						     current-clause-list)))
			
			(setq just-list rest-just-list))))
		   
		   ((string= head "demod")

		    ;; always nachgesetzt -> benutzen der current-clause-list !!
		    
		    (multiple-value-bind
			(numbers rest-just-list)
			(otter=parse-while-number (rest just-list))

		      (let* ((demod-clauses (mapcar #'otter=number2clause numbers)))

			(setq current-clause-list (apply 'append (mapcar #'(lambda (clause)
									      (do* ((rest-demods demod-clauses (rest rest-demods))
										    (return-clauses (list clause)))
										  ((null rest-demods) return-clauses)
										(let* ((head (first rest-demods)))
										  (setq return-clauses (apply 'append (mapcar #'(lambda (return-clause)
																  (res~binary-demodulation return-clause head))
															      return-clauses))))))
									 current-clause-list)))
			
			(setq just-list rest-just-list))))
		   
		   ((atptop~string-is-prefix-of-string-p "flip" head)

		    ;; flip ist in Version 3.0.3 etwas anders !!
		    ;; always nachgesetzt -> benutzen der current-clause-list !!
		    
		    (let* ((elements (atptop~divide-string head #\.))
			   (literal-position (pos~list-position (list (- (atptop~parse-number (second elements)) 1)))))

		      (setq current-clause-list (apply 'append
						       (mapcar #'(lambda (clause)
								   (let* ((clause-literals (cl~literals clause))
									  (literal-to-flip (data~struct-at-position clause-literals literal-position))
									  (atom (lit~atom literal-to-flip)))
								     (if (null (atptop~equation-p atom))
									 nil
								       (let* ((flipped-literal (lit~literal-create
												(term~appl-create (data~appl-function atom)
														  (list (cadr (data~appl-arguments atom)) (car (data~appl-arguments atom)))
														  )
												(lit~positive-p literal-to-flip))))
									 (list (cl~create (mapcar #'(lambda (literal)
												      (if (not (eq literal literal-to-flip))
													  (lit~literal-create (lit~atom literal)
															      (lit~positive-p literal))
													flipped-literal))
												  (cl~literals clause))
											  :justification (res~flip-create clause literal-position)))))))
							       current-clause-list)))
		      
		      (setq just-list (rest just-list))))
		   
		   ((string= head "binary")

		    ;; always first just -> setzen der current-clause-list !!
		    
		    (let* ((clause-and-pos1 (atptop~divide-string (second just-list) #\.))
			   (clause-and-pos2 (atptop~divide-string (third just-list) #\.))
			   (parent1 (otter=number2clause (atptop~parse-number (first clause-and-pos1))))
			   (parent2 (otter=number2clause (atptop~parse-number (first clause-and-pos2))))
			   (position1 (pos~list-position (list (- (atptop~parse-number (second clause-and-pos1)) 1))))
			   (position2 (pos~list-position (list (- (atptop~parse-number (second clause-and-pos2)) 1)))))

		      (multiple-value-bind
			  (renamed-parent2 renaming-parent2)
			  (res~separate-clauses parent1 parent2)
			(let* ((literal-to-unify1 (data~struct-at-position (cl~literals parent1) position1))
			       (literal-to-unify2 (data~struct-at-position (cl~literals renamed-parent2) position2))
			       (mgu (term~unify (lit~atom literal-to-unify1) (lit~atom literal-to-unify2)))
			       (new-just (res~resolution-create (list parent1 parent2)
								(list position1 position2)
								(list (subst~create nil nil) renaming-parent2)
								mgu))
			       (new-clause (cl~create (mapcar #'(lambda (literal)
								  (lit~literal-create (subst~apply mgu (lit~atom literal))
										      (lit~positive-p literal)))
							      (append (remove literal-to-unify1 (cl~literals parent1))
								      (remove literal-to-unify2 (cl~literals renamed-parent2))))
						      :justification new-just)))

			  (setq current-clause-list (list new-clause))
			  
			  (setq just-list (rest (rest (rest just-list))))))))
		   
		   ((string= head "factor")

		    ;; always first just -> setzen der current-clause-list !!
		    
		    (let* ((parent (otter=number2clause (atptop~parse-number (second just-list))))
			   (parent-literals (cl~literals parent))
			   (position1 (pos~list-position (list (- (atptop~parse-number (third just-list)) 1))))
			   (position2 (pos~list-position (list (- (atptop~parse-number (fourth just-list)) 1))))
			   (literal-to-unify1 (data~struct-at-position parent-literals position1))
			   (literal-to-unify2 (data~struct-at-position parent-literals position2))
			   (mgu (term~unify (lit~atom literal-to-unify1) (lit~atom literal-to-unify2)))
			   (new-just (res~factoring-create parent
							   (list position1 position2)
							   (subst~create nil nil)
							   mgu))
			   (new-clause (cl~create (mapcar #'(lambda (literal)
							      (lit~literal-create (subst~apply mgu (lit~atom literal))
										  (lit~positive-p literal)))
							  (remove literal-to-unify2 parent-literals))
						  :justification new-just)))

		      (setq current-clause-list (list new-clause))
		      
		      (setq just-list (rest (rest (rest (rest just-list)))))))
		   
		   ((or (string= head "hyper") (string= head "neg_hyper"))

		    ;; always first just -> setzen der current-clause-list !!
		    
		    (multiple-value-bind
			(numbers rest-just-list)
			(otter=parse-while-number (rest just-list))

		      (let* ((parents (mapcar #'otter=number2clause numbers)))

			(setq current-clause-list (res~hyper-resolution (first parents) (rest parents)))
			
			(setq just-list rest-just-list))))
		   
		   ((string= head "ur")

		    ;; always first just -> setzen der current-clause-list !!
		    
		    (multiple-value-bind
			(numbers rest-just-list)
			(otter=parse-while-number (rest just-list))
		      (let* ((parents (mapcar #'otter=number2clause numbers)))

			(setq current-clause-list (res~ur-resolution (first parents) (rest parents)))
			
			(setq just-list rest-just-list))))
		   
		   ((string= head "factor_simp")

		    ;; nicht first -> anwenden auf current-clause-list
		    
		    (setq current-clause-list (apply 'append (mapcar #'res~binary-factoring current-clause-list)))
		    
		    (setq just-list (rest just-list)))

		   ((or (string= head "para_from") (string= head "para_into"))
		    (let* ((mother-clause-and-pos (if (string= head "para_into")
						      (atptop~divide-string (second just-list) #\.)
						    (atptop~divide-string (third just-list) #\.)))
			   (father-clause-and-pos (if (string= head "para_into")
						      (atptop~divide-string (third just-list) #\.)
						    (atptop~divide-string (second just-list) #\.)))
			   (mother (otter=number2clause (atptop~parse-number (first mother-clause-and-pos))))
			   (father (otter=number2clause (atptop~parse-number (first father-clause-and-pos))))
			   (father-position (pos~list-position
					     (list (- (atptop~parse-number (second father-clause-and-pos)) 1))))
			   (father-direction (atptop~parse-number (third father-clause-and-pos)))
			   (direction (if (= father-direction 1)
					  'LR
					'RL))
			   (mother-numbers (mapcar #'atptop~parse-number (rest mother-clause-and-pos)))
			   (mother-position (pos~list-position (cons (- (first mother-numbers) 1) (cons 1 (rest mother-numbers))))))
		      
		      (multiple-value-bind
			  (renamed-father renaming-father)
			  (res~separate-clauses mother father)
			
			(let* ((mother-literal (data~struct-at-position (cl~literals mother)
									(pos~list-position (list (pos~first mother-position)))))
			       (mother-term-position (pos~rest (pos~rest mother-position)))
			       ;; 2* pos~rest wegen referenzierung des atoms im literal
			       (mother-term (data~struct-at-position (lit~atom mother-literal) mother-term-position))
			       (father-literal (data~struct-at-position renamed-father father-position))
			       (eq-args (data~appl-arguments (lit~atom father-literal)))
			       (father-unify-term (if (equal direction 'lr)
						      (first eq-args)
						    (second eq-args))) 
			       (father-new-term (if (equal direction 'lr)
						    (second eq-args) 
						  (first eq-args)))		      
			       (mgu (term~unify mother-term father-unify-term))
			       (new-just (res~paramod-create mother mother-position (subst~create nil nil)
							     father father-position renaming-father
							     direction mgu))
			       (new-clause (cl~create (mapcar #'(lambda (literal)
								  (lit~literal-create
								   (subst~apply mgu
										(if (eq literal mother-literal)
										    (data~replace-at-position (lit~atom literal)
													      mother-term-position
													      father-new-term
													      :downto '(data+primitive))
										  (lit~atom literal)))
								   (lit~positive-p literal)))
							      (append (cl~literals mother)
								      (remove father-literal (cl~literals renamed-father))))
						      :justification new-just)))

			  (setq current-clause-list (list new-clause))
			  
			  (setq just-list (rest (rest (rest just-list))))))))))))))


(defun otter=add-steps-to-proof! (new-clause res-proof)
  (declare (edited  "17-SEP-1996")
	   (authors Ameier)
	   (input   "A clause and the resolution-proof.")
	   (effect  "Recursive about the parents of the clause all clauses"
		    "in the tree over the clause are added to the step slot"
		    "of the resolution proof if not yet in.")
	   (value   "Undefined."))
  (let ((steps (res~proof-step-clauses res-proof))
	(clauses (res~proof-initial-clauses res-proof)))
    (cond ((res~reflex-p new-clause)
	   (if (not (member new-clause clauses))
	       (setf (res~proof-clauses res-proof) (cons new-clause (res~proof-clauses res-proof)))))
	  ((res~initial-p new-clause)
	   nil)
	  ((res~resolution-p new-clause)
	   (if (not (member new-clause steps))
	       (progn
		 (setf (res~proof-clauses res-proof) (cons new-clause (res~proof-clauses res-proof)))
		 (mapcar #'(lambda (clause)
			     (otter=add-steps-to-proof! clause res-proof))
			 (res~resolution-clauses new-clause)))))
	  ((res~flip-p new-clause)
	   (if (not (member new-clause steps))
	       (progn
		 (setf (res~proof-clauses res-proof) (cons new-clause (res~proof-clauses res-proof)))
		 (otter=add-steps-to-proof!
		  (first (res~justification-parents (node~justification new-clause))) res-proof))))
	  ((res~factoring-p new-clause)
	   (if (not (member new-clause steps))
	       (progn
		 (setf (res~proof-clauses res-proof) (cons new-clause (res~proof-clauses res-proof)))
		 (otter=add-steps-to-proof! (res~factoring-clause new-clause) res-proof))))
	  ((res~hyper-resolution-p new-clause)
	   (if (not (member new-clause steps))
	       (progn
		 (setf (res~proof-clauses res-proof) (cons new-clause (res~proof-clauses res-proof)))
		 (mapcar #'(lambda (clause)
			     (otter=add-steps-to-proof! clause res-proof))
			 (res~justification-parents (node~justification new-clause))))))
	  ((res~ur-resolution-p new-clause)
	   (if (not (member new-clause steps))
	       (progn
		 (setf (res~proof-clauses res-proof) (cons new-clause (res~proof-clauses res-proof)))
		 (mapcar #'(lambda (clause)
			     (otter=add-steps-to-proof! clause res-proof))
			 (res~justification-parents (node~justification new-clause))))))
	  ((res~paramodulation-p new-clause)
	   (if (not (member new-clause steps))
	       (progn
		 (setf (res~proof-clauses res-proof) (cons new-clause (res~proof-clauses res-proof)))
		 (mapcar #'(lambda (clause)
			     (otter=add-steps-to-proof! clause res-proof))
			 (res~justification-parents (node~justification new-clause)))))))))


(defun otter=parse-literal (literal-string)
  (declare (edited  "17-SEP-1996")
	   (authors Ameier)
	   (input   "A string representing a literal.")
	   (effect  "Perhaps otter*local-vars is changed (if new vars needed).")
	   (value   "A literal instance of this string."))
  (if (equal (char literal-string 0) #\-)
      (lit~literal-create (otter=parse-term (atptop~cut-first-char literal-string)) nil)
    (let ((lit-atom (otter=parse-term literal-string)))
      (if (atptop~equation-p lit-atom)
	  (let* ((literal (lit~literal-create lit-atom (keim~get lit-atom :polarity))))
	    (keim~remprop lit-atom :polarity)
	    literal)
	(lit~literal-create lit-atom 't)))))

(defun otter=parse-term (term-string &key (type nil))
  (declare (edited  "17-SEP-1996")
	   (authors Ameier)
	   (input   "A string representing a term and keyword type.")
	   (effect  "Perhaps the otter*local-vars is changed (if new vars needed).")
	   (value   "A term instance of the input string."))
  (if (> (atptop~number-of-char-in-string #\= term-string) 0)
      ;; If the term is an equation:
      (let* ((args (atptop~divide-string term-string #\=))
	     (first-string (first args)))
	(if (equal #\! (char first-string (- (length first-string) 1))) ;; -> ungleichung
	    (let* ((awaiting-types (data~n-domain (term~type (data~schema-range (otter=string2object "=")))))
		   (first-arg (otter=parse-term (atptop~cut-last-char first-string) :type (first awaiting-types)))
		   (second-arg (otter=parse-term (second args) :type (second awaiting-types)))
		   (application (term~appl-create (otter=string2object "=") (list first-arg second-arg)
						  )))
	      (keim~put application :polarity nil)
	      application)
	  (let* ((awaiting-types (data~n-domain (term~type (data~schema-range (otter=string2object "=")))))
		 (first-arg (otter=parse-term first-string :type (first awaiting-types)))
		 (second-arg (otter=parse-term (second args) :type (second awaiting-types)))
		 (application (term~appl-create (otter=string2object "=") (list first-arg second-arg)
						)))
	    (keim~put application :polarity t)
	    application)))
    ;; If the term isn't an equation:
    (multiple-value-bind
	(functor-string rest-string)
	(atptop~get-next-word term-string #\()
      ;; reads till a "(" is reached 
      ;; if rest-string = "" (empty) -> term isn't an application -> otherwise term is an application
      (if (string= rest-string "")
	  (otter=string2object functor-string :type type)
	(let* ((functor (otter=string2object functor-string))
	       (args (mapcar #'(lambda (term-string awaiting-type)
				 (otter=parse-term term-string :type awaiting-type))
			     (otter=parse-term-list (atptop~cut-last-char rest-string))
			     (data~n-domain (term~type (if (term~schema-p functor)
							   (data~schema-range functor)
							 functor))))))
	  (term~appl-create functor args
			    ))))))

(defun otter=parse-term-list (term-list-string)
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

#| ----------------------------------------------- Reading with proof-object ------------------------------------------- |#

;; Idee: Die Schritte werden immer parallel zu dem Otter proof object nachgebaut, dabei erhalten wir immer Klauseln, die
;; matchbar sind mit den Klauseln, die geparst werden, d.h., wenn cl1 usere erzeugte und cl2 und gepartse Klausel ist, dann\
;; gibt es eine Substitution S derart, dass cl1S=cl2 ist. 

(defun otter=read-with-proof-object (res-proof otter-out-string)
  (declare (edited  "22-AUG-1997")
	   (authors Ameier)
	   (input   "The partial resolution proof and the otter-out-string.")
	   (effect  "The steps (clauses) of the proof-object are parsed from the otter.out"
		    "file and are added to the initial problem. During this process"
		    "otter*rest-initial-clauses and otter*number-clause-list are changed"
		    "by removing used initial clauses and adding new number clause pairs."
		    "Additionally the steps of the resolution proof are changed, by parsing"
		    "the otter-out-string.")
	   (value   "Nil if the otter has failed to find a proof, t otherwise."))
  
  (setq otter*rest-initial-clauses (if otter*reflexivity-item
				       (cons otter*reflexivity-item (res~proof-initial-clauses res-proof))
				     (res~proof-initial-clauses res-proof)))
  (setq otter*number-clause-list nil)
  (setq otter*local-clause-vars nil)

  (let* ((line-strings (atptop~divide-string otter-out-string #\Newline)))
    (multiple-value-bind
	(proof-flag file-lines)
	(do* ((rest-string-lines line-strings (rest rest-string-lines))
	      (return-list nil)
	      (proof-part-flag 'start))
	    ((or (null rest-string-lines) (equal proof-part-flag 'end)) (values proof-part-flag return-list))
	  (let ((string-line (first rest-string-lines)))
	    (if (equal proof-part-flag 'proof)
		(if (or (string= string-line "End of proof object.")           ;; Version 3.0.5
			(string= string-line ";; END OF PROOF OBJECT"))        ;; Version 3.0.6
		    (setq proof-part-flag 'end)
		  (if (not (or (string= string-line "")
			       (string= string-line "(")
			       (string= string-line ")")))
		      (setq return-list (append return-list (list string-line)))))
	      (if (or (string= string-line "Proof object:")                    ;; Version 3.0.5
		      (string= string-line ";; BEGINNING OF PROOF OBJECT"))    ;; Version 3.0.6
		  (setq proof-part-flag 'proof)))))
      
      (if (equal proof-flag 'end) ;; proof object was found and read
	  (let* ((check (do* ((rest-lines file-lines (rest rest-lines))
			      (check-flag 't))
			    ((or (null check-flag)
				 (null rest-lines))
			     check-flag)
			  (let* ((line (first rest-lines))
				 (clause (otter=parse-line-with-proof-object line res-proof)))
			    (when (null clause)
			      (setq check-flag nil))))))
	    (if check
		't
	      (progn
		(omega~message "~% WARNING: OTTER PROOF can not be parsed !! Sorry !!")
		(omega~message "~% Try it again and guarant, that PROOF-OBJECT is SET. ~%")
		nil)))
	nil))))


#|(defun otter=parse-line-with-proof-object (line-string res-proof)
   (declare (edited  "22-AUG-1997")
	   (authors Ameier)
	   (input   "A string (representing a proof-line of an otter proof-object,"
		    "consisting of Number Justification Clause) and the current"
		    "res-proof.")
	   (effect  "The clause according to"
		    "this line is computed and if not a initial-clause added to the"
		    "proof-steps of the res-proof. The otter*number-clause-list is updated"
		    "by the new number clause pair. If the clause is a initial"
		    "clause it is removed from the otter*rest-initial-clause-list.")
	   (value   "Undefined."))
   (multiple-value-bind
       (number-string justification-string clause-string)
       (otter=split-line-string-with-proof-object line-string)

     (if (atptop~string-is-prefix-of-string-p "((not (ADDITIONALNONPROPPRED_" clause-string)
	 't   ;; checkt ob es die Zusaetzliche-Klausel ist -> einfach nur 't zurueck  
       
       (let* ((number (atptop~parse-number number-string))
	      (clause-or-just (otter=parse-justification-with-proof-object justification-string))
	      (clause (cond ((cl~clause-p clause-or-just)
			     ;; falls flip,res,paramod ->  fertige clause bereits da -> in steps
			     (setf (res~proof-clauses res-proof) (cons clause-or-just (res~proof-clauses res-proof)))
			     clause-or-just)
			    ((otter~instantiation-p clause-or-just)
			     ;; falls instantiation -> einfach clause nil, nicht in steps
			     (let* ((new-clause (cl~create nil)))
			       (setf (node~justification new-clause) clause-or-just)
			       new-clause))
			    ((res~initial-p clause-or-just)
			     ;; falls initial -> suche passend original clause
			     (let* ((parse-clause (otter=parse-clause-with-proof-object clause-string))
				    (original-clause (find parse-clause otter*rest-initial-clauses
							   :test #'atptop~clauses-equal-till-renaming-p)))
			       ;; Vorsicht: (setq otter*rest-initial-clauses (remove original-clause otter*rest-initial-clauses))
			       original-clause))
			    (t
			     ;; liste von clauseln -> factoring -> suche passende fact-clause -> in steps
			     (let* ((parse-clause (otter=parse-clause-with-proof-object clause-string))
				    #|  (new-clause (find parse-clause clause-or-just
						    :test #'atptop~clauses-equal-till-renaming-p))) |#
				    ;; ACHTUNG: EVENTUELL iST matcher-p nicht stark genug ??
				    (new-clause (find parse-clause clause-or-just
						      :test #'(lambda (cl1 cl2)
								;;(otter=matcher-p (res~separate-clauses cl1 cl2) cl1)))))
								(atptop~clauses-equal-till-renaming-and-ordering-p
								 (res~separate-clauses cl1 cl2) cl1)))))
			       (setf (res~proof-clauses res-proof) (cons new-clause (res~proof-clauses res-proof)))
			       new-clause)))))
	 
	 ;; updaten der number-clause list
	 (setq otter*number-clause-list (cons (list number clause) otter*number-clause-list))
	 
	 clause))))|#

;; My hope was that i could use all information of the proof object, for instance, the positions for resolution, paramodulation
;; and so on ...
;; Unfortunately, i found that there is one step that endangers everything: factoring!
;; UNfortunately, it can happen that the second literal is deleted when factoring, not the first one!
;; Since i'm not able (did not want to do !) to make a second sort of factoring, i returned to the assumption, that
;; my clauses are always only equal to the otter clauses modulo reordering of the literals!
;; Hence, i can not use anymore informations as resolution positions and so on ...
;;
;; AMEIER


(defun otter=parse-line-with-proof-object (line-string res-proof)
   (declare (edited  "22-AUG-1997")
	   (authors Ameier)
	   (input   "A string (representing a proof-line of an otter proof-object,"
		    "consisting of Number Justification Clause) and the current"
		    "res-proof.")
	   (effect  "The clause according to"
		    "this line is computed and if not a initial-clause added to the"
		    "proof-steps of the res-proof. The otter*number-clause-list is updated"
		    "by the new number clause pair. If the clause is a initial"
		    "clause it is removed from the otter*rest-initial-clause-list.")
	   (value   "Undefined."))
   (multiple-value-bind
       (number-string justification-string clause-string)
       (otter=split-line-string-with-proof-object line-string)

     (if (atptop~string-is-prefix-of-string-p "((not (ADDITIONALNONPROPPRED_" clause-string)
	 't   ;; checkt ob es die Zusaetzliche-Klausel ist -> einfach nur 't zurueck  
       
       (let* ((number (atptop~parse-number number-string))
	      (clauselist-or-just (otter=parse-justification-with-proof-object justification-string))
	      (clause (cond ((otter~instantiation-p clauselist-or-just)
			     ;; falls instantiation -> einfach clause nil, nicht in steps
			     (let* ((new-clause (cl~create nil)))
			       (setf (node~justification new-clause) clauselist-or-just)
			       new-clause))
			    ((res~initial-p clauselist-or-just)
			     ;; falls initial -> suche passend original clause
			     (let* ((parse-clause (otter=parse-clause-with-proof-object clause-string))
				    (original-clause (find parse-clause otter*rest-initial-clauses
							   :test #'atptop~clauses-equal-till-renaming-p)))
			       ;; Vorsicht: (setq otter*rest-initial-clauses (remove original-clause otter*rest-initial-clauses))
			       original-clause))
			    (t
			     ;; liste von clauseln -> factoring -> suche passende fact-clause -> in steps
			     (let* ((parse-clause (otter=parse-clause-with-proof-object clause-string))
				    (new-clause (find parse-clause clauselist-or-just
						      :test #'(lambda (cl1 cl2)
								(atptop~clauses-equal-till-renaming-and-ordering-p
								 (res~separate-clauses cl1 cl2) cl1)))))
			       (setf (res~proof-clauses res-proof) (cons new-clause (res~proof-clauses res-proof)))
			       new-clause)))))
	 
	 ;; updaten der number-clause list
	 (setq otter*number-clause-list (cons (list number clause) otter*number-clause-list))
	 
	 clause))))

(defun otter=split-line-string-with-proof-object (line-string)
   (declare (edited  "22-AUG-1997")
	   (authors Ameier)
	   (input   "A string, containing a proof-line of a otter proof-object.")
	   (effect  "None.")
	   (value   "The string is departet in 3 parts who are returned as"
		    "multiple values."
		    "First: a string containing the number of the step/clause."
		    "Second: a string with the justification"
		    "Third: a string with the clause."))
   (let* ((number-string "")
	  (just-string "")
	  (clause-string "")
	  (check-flag 'first)
	  (open-kl 0)
	  (closed-kl 0))
     (do* ((i 1 (+ i 1)))               ;; 1 und -1, weil der erste und letzte Buchstabe sind ( bzw. )
	 ((= i (- (length line-string) 1))  ;; 
	  (values number-string just-string clause-string))
       (let* ((current-char (char line-string i)))
	 (cond ((eq check-flag 'first)
		(if (eq current-char #\space)
		    (setq check-flag 'second)
		  (setq number-string (format nil "~A~A" number-string current-char))))
	       ((eq check-flag 'second)
		(cond ((eq current-char #\()
		       (setq open-kl (+ open-kl 1)))
		      ((eq current-char #\))
		       (setq closed-kl (+ closed-kl 1))))
		(setq just-string (format nil "~A~A" just-string current-char))
		(when (and (= open-kl closed-kl) (not (= open-kl 0)))
		  (setq check-flag 'third)
		  (setq i (+ i 1))))
	       (t
		(setq clause-string (format nil "~A~A" clause-string current-char))))))))

(defun otter=parse-clause-with-proof-object (clause-string)
  (declare (edited  "25-AUG-1997")
	   (authors Ameier)
	   (input   "A string, that represents a clause in a otter proof-object.")
	   (effect  "If the clause contains new renaming variables (the input clauses)"
		    "a renamed from the really inputed clauses) the otter*convert-list is"
		    "updated by pairs (new-var var-string).")
	   (value   "A corresponding clause."))
  (setq otter*local-clause-vars nil)
  (let* ((literal-string-list (do* ((i 1 (+ i 1))
				    (string-list nil)
				    (current-string "")
				    (open-kl 0)
				    (closed-kl 0))
				  ;; We start with i = 1 since the first letter is a laeding ( for the literal list
				  ((and (= open-kl closed-kl)
					(eq (char clause-string i) #\)))
				   string-list)
				(let* ((current-char (char clause-string i)))
				  
				  (when (eq current-char #\()
				    (setq open-kl (+ open-kl 1)))
				  (when (eq current-char #\))
				    (setq closed-kl (+ closed-kl 1)))

				  (setq current-string (format nil "~A~A" current-string current-char))
				  
				  (when (and (= open-kl closed-kl)
					     (eq current-char #\)))
				    (setq string-list (append string-list (list current-string)))
				    (setq current-string "")
				    (when (null (eq (char clause-string (+ i 1)) #\)))
				      (setf i (+ i 1))))
				  )))	 
	 (literals (mapcar #'otter=parse-literal-with-proof-object literal-string-list)))
    (cl~create literals)))

(defun otter=parse-literal-with-proof-object (literal-string)
  (declare (edited  "25-AUG-1997")
	   (authors Ameier)
	   (input   "A string representing a literal in a otter proof-object.")
	   (effect  "If the literal contains new renaming variables (the input clauses)"
		    "a renamed from the really inputed clauses) the otter*convert-list is"
		    "updated by pairs (new-var var-string).")
	   (value   "The corresponing literal."))			      
  (let* ((polarity (if (string= (format nil "~A~A~A~A"
					(char literal-string 0)
					(char literal-string 1)
					(char literal-string 2)
					(char literal-string 3))
				"(not")
		       nil
		     't))
	 (atom-string (if polarity
			  literal-string
			(do* ((i 4 (+ i 1))
			      (new-string "" (format nil "~A~A" new-string (char literal-string i))))
			    ((= i (- (length literal-string) 2))
			     new-string))))
	 (atom (otter=parse-term-with-proof-object atom-string)))
    (lit~literal-create atom polarity)))

(defun otter=parse-term-with-proof-object (term-string &key (type nil))
  (declare (edited  "25-AUG-1997")
	   (authors Ameier)
	   (input   "A string, that represents a term in a otter proof-object and possibly"
		    "with a keyword a needed type. That means the string is interpreted"
		    "as term of this type.")
	   (effect  "If the literal contains new renaming variables (the input clauses)"
		    "a renamed from the really inputed clauses) the otter*convert-list is"
		    "updated by pairs (new-var var-string).")
	   (value   "The according term."))
  (cond ((eq (char term-string 0) #\()
	 (let* ((subterm-string-list (do* ((i 1 (+ i 1))
					   (string-list nil)
					   (current-string "")
					   (open-kl 0)
					   (closed-kl 0))
					 ;; 1 und -1, weil der erste und letzte Buchstabe sind ( bzw. )
					 ((= i (- (length term-string) 1))  ;; 
					  (append string-list (list current-string)))
				       (let* ((current-char (char term-string i)))
					 (cond ((eq current-char #\()
						(setq open-kl (+ open-kl 1)))
					       ((eq current-char #\))
						(setq closed-kl (+ closed-kl 1))))
					 (if (and (= open-kl closed-kl)
						  (eq current-char #\space))
					     (progn
					       (setq string-list (append string-list (list current-string)))
					       (setq current-string "")
					       (setq open-kl 0)
					       (setq closed-kl 0))
					   (setq current-string (format nil "~A~A" current-string current-char))))))
		(functor-string (first subterm-string-list))
		(arg-strings (rest subterm-string-list)))
	   (cond (arg-strings
		  (let* ((functor (otter=parse-term-with-proof-object functor-string :type type))
			 (args (mapcar #'(lambda (term-string awaiting-type)
					   (otter=parse-term-with-proof-object term-string :type awaiting-type))
				       arg-strings
				       (data~n-domain (term~type (if (term~schema-p functor)
								     (data~schema-range functor)
								   functor))))))
		    (term~appl-create functor args
				      )))
		 (t
		  (otter=parse-term-with-proof-object functor-string :type type)))))
	
	(t
	 (otter=string2object term-string :type type))))

(defun otter=parse-justification-with-proof-object (just-string)
  (declare (edited  "25-AUG-1997")
	   (authors Ameier)
	   (input   "A string, representing the justification of a clause in a"
		    "otter proof object.")
	   (effect  "None.")
	   (value   "Nil if just string is (input), otherwise an according justification"))
  (let* ((without-klammern-string (atptop~cut-first-char (atptop~cut-last-char just-string))))
    (multiple-value-bind
	(prefix rest-string)
	(atptop~get-next-word without-klammern-string #\space)
      (cond ((string= prefix "input")
	     (res~initial-create (format nil "JUST-~A" (incf otter*just-counter))))
	    ((string= prefix "instantiate")
	     (multiple-value-bind
		 (number-string rest-str)
		 (atptop~get-next-word rest-string #\space)
	       (declare (ignore rest-str))
	       (let* ((parent-clause (otter=number2clause (atptop~parse-number number-string))))
		 (otter~instantiation-create parent-clause (subst~create nil nil)
					     (format nil "JUST-~A" (incf otter*just-counter))))))
	    ((string= prefix "merge")
	     (multiple-value-bind
		 (clause-number-string position-string)
		 (atptop~get-next-word rest-string #\space)
	       (let* ((parent-clause (otter=number2clause (atptop~parse-number clause-number-string)))
		      (real-parent (first (otter=get-instantiation-clauses parent-clause)))
		      (position1 (otter=parse-position-with-proof-object position-string))
		      (fac-clauses (res~binary-factoring real-parent)))

		 ;; (remove-if-not #'(lambda (fac-cl)
		 ;;		    (find position1 (res~factoring-positions fac-cl) :test 'keim~equal))
		 ;;		fac-clauses))))

		 ;; UNFORTUNATELY WE HAVE TO RETURN TO THE MORE GENERAL FUNCTION res~binary-factoring
		 fac-clauses)))
	    ((string= prefix "propositional")
	     (let* ((parent-clause (otter=number2clause (atptop~parse-number rest-string)))
		    (real-parent (first (otter=get-instantiation-clauses parent-clause)))
		    (fac-clauses (res~binary-factoring real-parent)))
	       fac-clauses))	     
	    ((string= prefix "flip")
	     (multiple-value-bind
		 (clause-number-string position-string)
		 (atptop~get-next-word rest-string #\space)
	       (let* ((parent-clause (otter=number2clause (atptop~parse-number clause-number-string)))
		      (position (otter=parse-position-with-proof-object position-string))
		      (real-parent (first (otter=get-instantiation-clauses parent-clause))))

		 ;; ACHTUNG: Kann passieren !!!
		 ;; -> ignorieren
		 ;; Kann es passieren, dass real-parent != parent-clause ??
		 ;; (when  (not (eq parent-clause real-parent))
		 ;;  (omega~error "~%By flip the real parent is unequal to parent"))
		 
		 ;;(otter=make-flip-step real-parent position)

		 ;; Unfortunately we have to return to a more general Funktion:
		 (otter=make-flips real-parent))))
		 
	    ((string= prefix "resolve")
	     (multiple-value-bind
		 (number-string1 rest-str1)
		 (atptop~get-next-word rest-string #\space)
	       (multiple-value-bind
		   (pos-string1 rest-str2)
		   (atptop~get-next-word rest-str1 #\) :handle-break-char 'pre)
		 (multiple-value-bind
		     (number-string2 pos-string2)
		     (atptop~get-next-word (atptop~cut-first-char rest-str2) #\space)
		   (let* ((parent-clause1 (otter=number2clause (atptop~parse-number number-string1)))
			  (parent-clause2 (otter=number2clause (atptop~parse-number number-string2)))
			  (real-parent1 (first (otter=get-instantiation-clauses parent-clause1)))
			  (real-parent2 (first (otter=get-instantiation-clauses parent-clause2)))
			  (position1 (otter=parse-position-with-proof-object pos-string1))
			  (position2 (otter=parse-position-with-proof-object pos-string2)))
		     ;;(otter=make-resolution-step real-parent1 real-parent2 position1 position2)

		     ;; UNFORTUNATELY SOME PROBLEMS CAUSED US TO GO BACK TO THE MORE GENERAL FUNCTION res~binary-resolution!!!
		     (res~binary-resolution real-parent1 real-parent2))))))
	    ((string= prefix "paramod")
	     (multiple-value-bind
		 (number-string1 rest-str1)
		 (atptop~get-next-word rest-string #\space)
	       (multiple-value-bind
		   (pos-string1 rest-str2)
		   (atptop~get-next-word rest-str1 #\) :handle-break-char 'pre)
		 (multiple-value-bind
		     (number-string2 pos-string2)
		     (atptop~get-next-word (atptop~cut-first-char rest-str2) #\space)
		   (let* ((father-clause (otter=number2clause (atptop~parse-number number-string1)))
			  (mother-clause (otter=number2clause (atptop~parse-number number-string2)))
			  (real-father (first (otter=get-instantiation-clauses father-clause)))
			  (real-mother (first (otter=get-instantiation-clauses mother-clause)))
			  ;; Achtung: bisher nur father position (1 1) gefunden
			  ;; Vermutung: erste 1 steht fuer literal position
			  ;;            zweite 1 steht fuer richtung lr
			  (pre-father-position (otter=parse-position-with-proof-object pos-string1))
			  (father-position (pos~butlast pre-father-position 1))
			  (mother-position (otter=parse-position-with-proof-object pos-string2)))

		     (when (not (= (first (last (pos~number-list pre-father-position))) 1))
		       (omega~error "In function otter=parse-justification-with-proof-object the father-position is unequal (x 1)"))

		     ;;(otter=make-paramod-step real-mother (otter=correct-position-with-proof-object mother-position real-mother)
		     ;;    			      real-father father-position
		     ;;			      (if (= (first (last (pos~number-list pre-father-position))) 1)
		     ;;				  'LR
		     ;;    				'RL)))

		     ;; UNFORTUNATELY SOME PROBLEMS CAUSED US TO GO BACK TO THE MORE GENERAL FUNCTION res~binary-paramodulation
		     (res~binary-paramodulation real-father real-mother))))))))))


(defun otter=make-paramod-step (mother-clause mother-position father-clause father-position direction)
  (declare (edited  "02-SEP-1997")
	   (authors Ameier)
	   (input   "A mother-clause and a position in it, a father-clause and a position in it"
		    "and a direction.")
	   (effect  "None.")
	   (value   "A clause, that is the result of a paramodulation of the input clauses"
		    "at the input position and direction."))
  (multiple-value-bind
      (new-mother mother-renaming)
      (res~separate-clauses father-clause mother-clause)
    (let* ((mother-term (data~struct-at-position new-mother mother-position))
	   (father-literals (cl~literals father-clause))
	   (father-lit (data~struct-at-position father-literals father-position))
	   (father-eq-term (if (eq direction 'LR)
			       (first (data~appl-arguments (lit~atom father-lit)))
			     (second (data~appl-arguments (lit~atom father-lit)))))
	   (father-replace-term (if (eq direction 'LR)
				    (second (data~appl-arguments (lit~atom father-lit)))
				  (first (data~appl-arguments (lit~atom father-lit)))))
	   (unifier (term~unify father-eq-term mother-term)))
      (cl~create (mapcar #'(lambda (lit)
			     (lit~literal-create (subst~apply unifier (lit~atom lit))
			 			 (lit~positive-p lit)))
			 (append (remove father-lit father-literals)
			 	 (cl~literals (data~replace-at-position new-mother mother-position father-replace-term
									:downto '(data+primitive)))))
		 ;; Die Reihenfolge father-literals <-> mother-literals entspricht nicht dem omega/keim Format
		 ;; Korrektur in Correct paramod with proof object
		 :justification	(res~paramod-create mother-clause mother-position mother-renaming 
						    father-clause father-position (subst~create nil nil)
						    direction
						    unifier
						    (format nil "JUST-~A" (incf otter*just-counter)))))))

(defun otter=make-resolution-step (parent1 parent2 position1 position2)
  (declare (edited  "02-SEP-1997")
	   (authors Ameier)
	   (input   "Two clauses and two positions.")
	   (effect  "None.")
	   (value   "A clause, that is the result of the resolution of the two input clauses"
		    "at the two input positiions."))
  (multiple-value-bind
      (new-parent2 renaming2)
      (res~separate-clauses parent1 parent2)
    (let* ((literals1 (cl~literals parent1))
	   (literals2 (cl~literals new-parent2))
	   (res-lit1 (data~struct-at-position literals1 position1))
	   (res-lit2 (data~struct-at-position literals2 position2))
	   (unifier (term~unify (lit~atom res-lit1) (lit~atom res-lit2))))
      (cl~create (mapcar #'(lambda (lit)
			     (lit~literal-create
			      (subst~apply unifier (lit~atom lit))
			      (lit~positive-p lit)))
			 (append (remove res-lit1 literals1)
				 (remove res-lit2 literals2)))
		 :justification (res~resolution-create (list parent1 parent2)
						       (list position1 position2)
						       (list (subst~create nil nil) renaming2)
						       unifier
						       (format nil "JUST-~A" (incf otter*just-counter)))))))

(defun otter=make-flips (clause)
  (declare (edited  "11-AUG-2000")
	   (authors Ameier)
	   (input   "A clause.")
	   (effect  "None.")
	   (value   "A list of clauses, consisting of clauses that arise if one equation literal in the"
		    "input clause is flipped."))
  (do* ((rest-literals (cl~literals clause) (rest rest-literals))
	(i 0 (+ i 1))
	(back-clauses nil))
      ((null rest-literals)
       back-clauses)
    (let* ((head-lit (first rest-literals)))
      (when (atptop~equation-p head-lit)
	(setf back-clauses (append back-clauses (list (otter=make-flip-step clause (pos~add-front i (pos~empty))))))))))

(defun otter=make-flip-step (clause position)
  (declare (edited  "02-SEP-1997")
	   (authors Ameier)
	   (input   "A clause and a position.")
	   (effect  "None.")
	   (value   "A new clause, thats justification is a flipping of the input clause"
		    "at position position."))
  (let* ((literals (cl~literals clause))
	 (lit-at-pos (data~struct-at-position literals position)))
    (cl~create (mapcar #'(lambda (lit)
			   (if (eq lit lit-at-pos)
			       (let* ((lit-atom (lit~atom lit)))
				 (lit~literal-create (term~appl-create (data~appl-function lit-atom)
								       (reverse (data~appl-arguments lit-atom))
								       )
						     (lit~positive-p lit)))
			     (data~copy lit :downto '(data+primitive))))
		       literals)
	       :justification (res~flip-create clause position  (format nil "JUST-~A" (incf otter*just-counter))))))


(defun otter=correct-position-with-proof-object (position clause)
  (declare (edited  "27-AUG-1997")
	   (authors Ameier)
	   (input   "A position and a clause")
	   (effect  "None.")
	   (value   "A position with a addition 1 add second pos."))
  (let* ((literal-at-position (data~struct-at-position (cl~literals clause) (pos~list-position (list (pos~first position))))))
    (if (lit~positive-p literal-at-position)
	(pos~list-position (cons (pos~first position) (cons 1 (pos~number-list (pos~rest position)))))
      (pos~list-position (cons (pos~first position) (cons 1 (pos~number-list (pos~rest (pos~rest position)))))))))

(defun otter=get-instantiation-clauses (parent-clause)
  (declare (edited  "25-AUG-1997")
	   (authors Ameier)
	   (input   "A clause.")
	   (effect  "None.")
	   (value   "A list of all ancestors clauses of the input clause"
		    "till the first clause, that is not justified by instantiation,"
		    "possibly the input clause itself, if it isn't justified by"
		    "instantiation."
		    "(Remark: the first element of the list the clause not justified,"
		    " by instantiation, the last element is the input-clause itself."))
  (cond ((otter~instantiation-p parent-clause)
	 (let* ((parent (first (res~justification-parents (node~justification parent-clause)))))
	   (append (otter=get-instantiation-clauses parent) (list parent-clause))))
	(t
	 (list parent-clause))))

(defun otter=parse-position-with-proof-object (position-string)
  (declare (edited  "25-AUG-1997")
	   (authors Ameier)
	   (input   "A string, that represent a position in a otter proof-object.")
	   (effect  "None.")
	   (value   "The corresponding keim position."))
  (let* ((parts-list (do* ((i 1 (+ i 1))
			   (string-list nil)
			   (current-string ""))  ;; 1 und -1, weil der erste und letzte Buchstabe sind ( bzw. )
			 ((= i (- (length position-string) 1))  ;; 
			  (append string-list (list current-string)))
		       (let* ((current-char (char position-string i)))
			 (cond ((or (eq current-char #\()
				    (eq current-char #\)))           ;; ueberlesen von (,)
				nil)
			       ((eq current-char #\space)
				(when (not (string= current-string ""))
				  (setq string-list (append string-list (list current-string))))
				(setq current-string ""))
			       (t
				(setq current-string (format nil "~A~A" current-string current-char)))))))
	 (number-list (mapcar #'(lambda (number-string)
				  (atptop~parse-number number-string))
			      parts-list)))
    (pos~list-position (cons (- (first number-list) 1) (rest number-list)))))

#| ----------------------------------------------------- Matcher ------------------------------------------------------- |#

#| Problem: Gegeben zwei Klauseln c1 und c2, gibt es eine Substitution S, so das c1S=c2 ? |#

(defun otter=matcher-p (clause1 clause2)
  (declare (edited  "04-SEP-1997")
	   (authors Ameier)
	   (input   "Two clauses.")
	   (effect  "None.")
	   (value   "If there exists a substitution S of a kind, that clause clause1S=clause2, this"
		    "substitution is returned, otherwise nil."))
  (let* ((lits1 (cl~literals clause1))
	 (lits2 (cl~literals clause2)))
    ;; have both literal lists the same length ?
    (when (not (= (length lits1) (length lits2)))
      nil)
    ;; have corresponding literals the same polarity ?
    (when (not (eval (cons  'and (mapcar #'(lambda (lit1 lit2)
					     (equal (lit~positive-p lit1) (lit~positive-p lit2)))
					 lits1 lits2))))
      nil)
    ;; match the literal atoms
    (otter=compute-matcher (mapcar #'(lambda (lit1 lit2)
				       (list (lit~atom lit1) (lit~atom lit2)))
				   lits1 lits2))))

(defun otter=compute-matcher (equation-list)
  (declare (edited  "04-SEP-1997")
	   (authors Ameier)
	   (input   "A list of term pairs ((s1 t1) ... (sn tn)), interpreted as a list of equations"
		    "s1=t1 ... sn=tn.")
	   (effect  "None.")
	   (value   "If existing a Substitution S, so that for all i holds siS=ti."))
  (let* ((result (otter=compute-required-replacements equation-list)))
    (if (equal result 'clash)
	nil
      (otter=compute-matcher-II result))))

(defun otter=compute-matcher-II (required-replacements)
  (declare (edited  "04-SEP-1997")
	   (authors Ameier)
	   (input   )
	   (effect  )
	   (value   ))
  (do* ((rest-required-rep required-replacements)
	(failure-flag nil)
	(checked-pairs nil))
      ((or (null rest-required-rep)
	   failure-flag)
       (if failure-flag
	   nil
	 (subst~create (mapcar #'first checked-pairs)
		       (mapcar #'second checked-pairs))))
    (let* ((head-req-rep (first rest-required-rep))
	   (current-var (first head-req-rep))
	   (rep-pairs-of-var (remove-if-not #'(lambda (pair)
						(keim~equal (first pair) current-var))
					    rest-required-rep))
	   (rep-pairs-not-of-var (remove-if #'(lambda (pair)
						(keim~equal (first pair) current-var))
					    rest-required-rep))
	   (rep-terms-of-var (mapcar #'second rep-pairs-of-var))
	   (head-term (first rep-terms-of-var)))
      (setq rest-required-rep rep-pairs-not-of-var)
      (if (null (remove-if #'(lambda (term)
			       (keim~equal head-term term))
			   rep-terms-of-var))
	  ;; alle keim~equal
	  (setq checked-pairs (cons (first rep-pairs-of-var) checked-pairs))
	(setq failure-flag 't)))))
	  

(defun otter=compute-required-replacements (equation-list)
  (declare (edited  "04-SEP-1997")
	   (authors Ameier)
	   (input   "A list of term pairs ((s1 t1) ... (sn tn)), interpreted as a list of equations"
		    "s1=t1 ... sn=tn.")
	   (effect  "None.")
	   (value   "A list of needed replacemnet of variables of s1 ... sn or 'clash if a clash is"
		    "detected."))
  (let* ((rep-list (apply 'append (mapcar #'(lambda (equation)
					      (otter=compute-req-rep (first equation) (second equation)))
					  equation-list))))
    (if (find 'clash rep-list)
	'clash
      rep-list)))

(defun otter=compute-req-rep (left-side right-side)
  (declare (edited  "04-SEP-1997")
	   (authors Ameier)
	   (input   )
	   (effect  )
	   (value   ))
  (cond ((term~variable-p left-side)
	 (if (keim~equal left-side right-side)
	     nil
	   (list (list left-side right-side))))
	((term~constant-p left-side)
	 (if (keim~equal left-side right-side)
	     nil
	   (list 'clash)))
	(t ;; term~appl-p
	 (if (or (not (term~appl-p right-side))
		 (not (keim~equal (data~appl-function left-side) (data~appl-function right-side))))
	     (list 'clash)
	   (apply 'append (mapcar #'(lambda (arg1 arg2)
				      (otter=compute-req-rep arg1 arg2))
				  (data~appl-arguments left-side)
				  (data~appl-arguments right-side)))))))




#| ------------------------------------------------ Correct paramod with proof object ---------------------------------- |#

#| Folgendes Problem:

   Im Proof Object setzt sich bei der Paramodulation die entstehende Klausel so zusammen, dass zuerst die (restlichen) father
   literals kommen, und dann erst die Mother literals. In keim bzw. Omega ist dies jedoch genau umgekehrt. Daher muessen nach dem
   Parsen der Schritte die Paramodulation auch wieder an omega/keim format angepasst werden, was leider ein Veraenderung aller
   positions und literalreihenfolgen im ganzen Beweis nach sich ziehen kann.

   FUer jede clauses wird liste von listen von alten-pos und neuen-pos angegeben, die fuer jedes literal, das seine Position veraendert
   seine alte und seine neue Position enthaelt.
|#

(defun otter=first-is-keim-equal (item list)
  (keim~equal item (first list)))

(defun otter=second-is-keim-equal (item list)
  (keim~equal item (second list)))

(defun otter=second-is-eq (item list)
  (eq item (second list)))

(defun otter=second-or-third-is-eq (item list)
  (or (eq item (second list))
      (eq item (third list))))

(defun otter=correct-paramods! (clause just)
  (let* ((pospos-list (keim~get clause 'pospos-list)))
    (if pospos-list
	pospos-list
      (let* ((new-pospos-list (otter=correct-paramods-II! clause just)))
	(keim~put clause 'pospos-list new-pospos-list)
	new-pospos-list))))

(defgeneric otter=correct-paramods-II! (clause just)
  (:method (clause (just res+initial))
	   ;; initial -> weder reihenfolge der literale, noch positions in justification aendern sich -> kein old-pos <-> new-pos list
	   ;; ist identische Liste
	   (do* ((i 0 (+ i 1))
		 (pospos-list nil))
	       ((= i (length (cl~literals clause)))
		pospos-list)
	     (setq pospos-list (cons (list (pos~list-position (list i))
					   (pos~list-position (list i)))
				     pospos-list))))
  (:method (clause (just res+reflex))
	   (declare (ignore clause))
	   ;; selbe wie initial
	   (list (list (pos~list-position (list 0))
		       (pos~list-position (list 0)))))
  (:method (clause (just res+resolution))
	   (let* ((old-literals (cl~literals clause))
		  (parents (res~resolution-clauses just))
		  (positions (res~resolution-positions just))
		  (parent1 (first parents))
		  (parent2 (second parents))
		  (old-pos1 (first positions))
		  (old-pos2 (second positions))
		  (lit-parent-lit-pairs (mapcar #'(lambda (lit parent-lit)
						    (list lit parent-lit))
						old-literals
						(append (remove (data~struct-at-position (otter=literals-in-original-order parent1)
											 old-pos1)
								(otter=literals-in-original-order parent1))
							(remove (data~struct-at-position (otter=literals-in-original-order parent2)
											 old-pos2)
								(otter=literals-in-original-order parent2)))))
		  
		  ;; Jetzt wird die Reihenfolge der Literale in den Parent Klauses ge"andert
		  
		  (pospos-list1 (otter=correct-paramods! parent1 (node~justification parent1)))
		  (pospos-list2 (otter=correct-paramods! parent2 (node~justification parent2)))
		  (new-pos1 (second (find old-pos1 pospos-list1 :test 'otter=first-is-keim-equal)))
		  (new-pos2 (second (find old-pos2 pospos-list2 :test 'otter=first-is-keim-equal)))
		  
		  ;; Berechnung der Neuen Positionen
		  ;; Von der Parent1 Tauchlist + der Parent2 Tauschliste bekommt man die neue Tauchlist, indem:
		  ;; In Parent1: Aus der Tauschliste wird dasjenige Paar, das die ALTER resolvierte Position als erstes hat gestrichen
		  ;;             In den Heads wird 1 subtrahiert, wenn sie groesser als das ALTE geloeschte Resolve-Literal sind
		  ;;             In den Seconds wird 1 subtrahiert, wenn sie groesser als das NEUE geloeschte Resove-Literal sind
		  ;; In Parent2: Analog
		  ;;             +
		  ;;             In den Heads wie in den Seconds wird die Restlaenge von Parent1 addiert

		  (length-of-clause (length old-literals))
		  (length-rest-parent1 (- (length (cl~literals parent1)) 1))
		  (old-pos1-number (first (pos~number-list old-pos1)))
		  (old-pos2-number (first (pos~number-list old-pos2)))
		  (new-pos1-number (first (pos~number-list new-pos1)))
		  (new-pos2-number (first (pos~number-list new-pos2)))
		  (parent1-pos-pos-list (apply #'append
					       (mapcar #'(lambda (pair)
							   (let* ((head-pos (first pair))
								  (second-pos (second pair)))

							     (cond ((keim~equal head-pos old-pos1)
								    ;; -> resolviertes Literal -> fliegt raus
								    nil)
								   (t
								    (let* ((number1 (first (pos~number-list head-pos)))
									   (number2 (first (pos~number-list second-pos))))
								      (list (list (pos~list-position (list (if (< number1 old-pos1-number)
													       number1
													     (- number1 1))))
										  (pos~list-position (list (if (< number2 new-pos1-number)
													       number2
													     (- number2 1)))))))))))
						       pospos-list1)))
		  (parent2-pos-pos-list (apply #'append
					       (mapcar #'(lambda (pair)
							   (let* ((head-pos (first pair))
								  (second-pos (second pair)))

							     (cond ((keim~equal head-pos old-pos2)
								    ;; -> resolviertes Literal -> fliegt raus
								    nil)
								   (t
								    (let* ((number1 (first (pos~number-list head-pos)))
									   (number2 (first (pos~number-list second-pos))))
								      (list (list (pos~list-position (list (if (< number1 old-pos2-number)
													       (+ number1
														  length-rest-parent1)
													     (+ (- number1 1)
														length-rest-parent1))))
										  (pos~list-position (list (if (< number2 new-pos2-number)
													       (+ number2
														  length-rest-parent1)
													     (+ (- number2 1)
														length-rest-parent1)
													     ))))))))))
						       pospos-list2)))
		  (new-pos-pos-list (append parent1-pos-pos-list parent2-pos-pos-list)))
	     
	     ;; 1. KORREKT PARENT POSITIONS:
	     (setf (res~justification-positions just)
		   (list new-pos1 new-pos2))
	     
	     ;; 2. Aendern der Reihenfolge der Literale
	     (let* (;; (new-literals (mapcar #'(lambda (parent-lit)
		    ;;			      (first (find parent-lit lit-parent-lit-pairs :test 'otter=second-is-eq)))
		    ;;			  (append (remove (data~struct-at-position parent1 new-pos1) (cl~literals parent1))
		    ;;				  (remove (data~struct-at-position parent2 new-pos2) (cl~literals parent2))))))
		    (new-literals (do* ((i 0 (+ i 1))
					(back-list nil))
				      ((= i length-of-clause)
				       back-list)
				    (let* ((posi (pos~list-position (list i)))
					   (pair-with-posi-as-second (find posi new-pos-pos-list
									   :test #'(lambda (it pair)
										     (keim~equal it (second pair)))))
					   (head-of-pair (first pair-with-posi-as-second))
					   (according-literal (data~struct-at-position old-literals head-of-pair)))

				      (setq back-list (append back-list (list according-literal)))))))
		    
	       (setf (cl~literals clause) new-literals)
	       
	       ;; 3. Rueckgabe der geanderten Positions
	       
	       ;; (otter=compute-changed-positions old-literals new-literals))))

	       ;; (format t "~%I WAS USED!")
	       
	       new-pos-pos-list)))

  (:method (clause (just res+factoring))
	   (let* ((old-literals (cl~literals clause))
		  (parent (res~factoring-clause just))
		  (positions (res~factoring-positions just))
		  (old-pos1 (first positions))
		  (old-pos2 (second positions))
		  (lit-fac1 (data~struct-at-position (otter=literals-in-original-order parent) old-pos1))
		  (lit-fac2 (data~struct-at-position (otter=literals-in-original-order parent) old-pos2))
		  
		  (lit-parent-lit-pairs (mapcar #'(lambda (lit parent-lit)
						    (if (eq parent-lit lit-fac1)
							(list lit parent-lit lit-fac2)
						      (list lit parent-lit)))
						old-literals
						(remove lit-fac2 (otter=literals-in-original-order parent))))
		  
		  (pospos-list (otter=correct-paramods! parent (node~justification parent)))
		  (new-pos1 (second (find old-pos1 pospos-list :test 'otter=first-is-keim-equal)))
		  (new-pos2 (second (find old-pos2 pospos-list :test 'otter=first-is-keim-equal)))
		  (new-num1 (first (pos~number-list new-pos1)))
		  (new-num2 (first (pos~number-list new-pos2))))
	     
	     ;; 1. KORREKT PARENT POSITIONS:
	     (setf (res~justification-positions just)
		   (if (< new-num1 new-num2)
		       (list new-pos1 new-pos2)
		     (list new-pos1 new-pos2)))
	     
	     ;; 2. Aendern der Reihenfolge der Literale
	     (let* ((new-literals (mapcar #'(lambda (parent-lit)
					      (first (find parent-lit lit-parent-lit-pairs :test 'otter=second-or-third-is-eq)))
					  (remove (data~struct-at-position parent (if (< new-num1 new-num2)
									       new-pos2
									     new-pos1))
						  (cl~literals parent)))))
	       (setf (cl~literals clause) new-literals)
	       
	       ;; 3. Rueckgabe der geanderten Positions
	       
	       (otter=compute-changed-positions old-literals new-literals))))
  (:method (clause (just res+flip))
	   (let* ((old-literals (cl~literals clause))
		  (old-pos (first (res~justification-positions just)))
		  (parent (first (res~justification-parents just)))
		  (pospos-list (otter=correct-paramods! parent (node~justification parent))))
	     
	     ;; 1. Korrekt parent positions:
	     (setf (res~justification-positions just)
		   (list (second (find old-pos pospos-list :test 'otter=first-is-keim-equal))))
	     
	     ;; 2. Korrekt literal reihenfolge:
	     (let* ((new-literals (do* ((i 0 (+ i 1))
					(back-literals nil))
				      ((= (length old-literals) i)
				       back-literals)
				    (let* ((orig-pos (first (find (pos~list-position (list i)) pospos-list
								  :test 'otter=second-is-keim-equal))))
				      (setq back-literals (append back-literals
								  (list (data~struct-at-position old-literals orig-pos))))))))
	       (setf (cl~literals clause) new-literals)
	       
	       ;; 3. Rueckgabe der geanderten Positions
	       pospos-list)))
  (:method (clause (just res+paramodulation))

	   ;; (format t "~%WITH THE FOLLOWING RENAMINGS: ~A" (res~justification-renamings just))
	   
	   (let* ((old-literals (cl~literals clause))
		  (mother (res~paramod-mother just))
		  (father (res~paramod-father just))
		  (old-mother-pos (res~paramod-mother-position just))
		  (old-father-pos (res~paramod-father-position just))
		  (lit-parent-lit-pairs (mapcar #'(lambda (lit parent-lit)
						    (list lit parent-lit))
						old-literals
						(append (remove (data~struct-at-position (otter=literals-in-original-order father)
											 old-father-pos)
								(otter=literals-in-original-order father))
							(otter=literals-in-original-order mother))))
				  
		  ;; noch in falscher reihenfolge: father vor mother
		  ;; Jetzt wird die Reihenfolge der Literale in den Parent Klauses ge"andert
		  
		  (mother-pospos-list (otter=correct-paramods! mother (node~justification mother)))
		  (father-pospos-list (otter=correct-paramods! father (node~justification father)))
		  (old-mother-first (pos~list-position (list (pos~first old-mother-pos))))
		  (new-mother-first (second (find old-mother-first mother-pospos-list :test 'otter=first-is-keim-equal)))
		  (new-mother-pos (pos~add-front (first (pos~number-list new-mother-first)) (pos~rest old-mother-pos)))
		  (new-father-pos (second (find old-father-pos father-pospos-list :test 'otter=first-is-keim-equal)))
		  (length-of-clause (length (cl~literals clause)))
		  (rest-length-of-father (- (length (cl~literals father)) 1))
		  (length-of-mother (length (cl~literals mother)))
		  (old-father-number (first (pos~number-list old-father-pos)))
		  (new-father-number (first (pos~number-list new-father-pos)))
		  
		  ;; Von der Mutter Tauchlist + der Vater- Tauschliste bekommt man die neue Tauchlist, indem:
		  ;; Man streiche aus Vater - Tauschliste dasjenige  Paar, das die alte Father-position als erstes hat
		  ;; (oder das, das die neue Father-position als zweites hat), dann muss man eigentlich man nur noch
		  ;; die Vater-Tauchliste folgender massen veraendern:
		  ;; 1.) In den Heads der Paare wird 1 subtrahiert, wenn sie groesser als das ALTE geloeschte Equality-Literal sind
		  ;; 2.) In den Seconds der Paare wird 1 subtrahirt, wenn sie groesser als das NEUE geloeschte Equality-Literal sind
		  ;; 3.) Father Position muss man im Second jeweils die mother-laenge dazuaddiren 
		  ;; Mother Positions-pairs muss man im First jeweils die rest-father-laenge dazuaddieren

		  (new-mother-pos-list (mapcar #'(lambda (mother-pair)
						   (let* ((head-pos (first mother-pair))
							  (second-pos (second mother-pair)))
						     (list (pos~list-position (list (+ (first (pos~number-list head-pos))
										       rest-length-of-father)))
							   (pos~list-position (pos~number-list second-pos)))))
					       mother-pospos-list))

		  (new-father-pos-list (apply #'append
					      (mapcar #'(lambda (father-pair)
							  (let* ((head-pos (first father-pair))
								 (second-pos (second father-pair)))
							    
							    (cond ((keim~equal head-pos old-father-pos)
								   ;; Pair gehoert zum Gleichungsliteral -> ist nachher nichtmher da
								   nil)
								  (t
								   (let* ((number1 (first (pos~number-list head-pos)))
									  (number2 (first (pos~number-list second-pos))))
								     (list (list (pos~list-position (list (if (< number1 old-father-number)
													      number1
													    (- number1 1))))
										 (pos~list-position (list (if (< number2 new-father-number)
													      (+ number2 length-of-mother)
													    (+ (- number2 1) length-of-mother)))))))))))
						      father-pospos-list)))
		  (new-pos-pos-list (append new-mother-pos-list new-father-pos-list)))
	     
	     ;;(format t "~%father-pospos-list: ~A" father-pospos-list)
	     ;;(format t "~%new-mother-pos-list: ~A" new-mother-pos-list)
	     ;;(format t "~%new-father-pos-list: ~A" new-father-pos-list)
	     ;;(format t "~%new-pos-pos-list: ~A" new-pos-pos-list)
	     
	     ;; 1. KORREKT PARENT POSITIONS:
	     (setf (res~justification-positions just)
		   (list new-mother-pos new-father-pos))
	     
	     ;; 2. Aendern der Reihenfolge der Literale
	     (let* (;;(new-literals (mapcar #'(lambda (parent-lit)
		    ;;			      (first (find parent-lit lit-parent-lit-pairs :test 'otter=second-is-eq)))
		    ;;			  (append (cl~literals mother)
		    ;;				  (remove (data~struct-at-position father new-father-pos) (cl~literals father))))))
		    (new-literals (do* ((i 0 (+ i 1))
					(back-list nil))
				      ((= i length-of-clause)
				       back-list)
				    (let* ((posi (pos~list-position (list i)))
					   (pair-with-posi-as-second (find posi new-pos-pos-list
									   :test #'(lambda (it pair)
										     (keim~equal it (second pair)))))
					   (head-of-pair (first pair-with-posi-as-second))
					   (according-literal (data~struct-at-position old-literals head-of-pair)))

				      (setq back-list (append back-list (list according-literal)))))))
	       
	       ;; jetzt in richtiger Reihenfolge
	       
	       (setf (cl~literals clause) new-literals)
	       
	       ;; 3. Rueckgabe der geanderten Positions
	       
	       ;;(otter=compute-changed-positions old-literals new-literals)))))

	       new-pos-pos-list))))
  
(defun otter=literals-in-original-order (clause)
  (declare (edited  "21-OCT-1998")
	   (authors Ameier)
	   (input   "A clause.")
	   (effect  "None.")
	   (value   "The literals of the clause in the original ordering. This means if the clause was already"
		    "reordered the reordering in undone and a list with the literals in the correct ordering is"
		    "returned. If the clause was not already reordered simpleway the current ordering of the"
		    "literals is returned."))
  (let* ((pospos-list (keim~get clause 'pospos-list))
	 (literals (cl~literals clause)))
    (if (null pospos-list)
	literals
      (do* ((i 0 (+ i 1))
	    (back-list nil))
	  ((= i (length literals))
	   back-list)
	(let* ((pos-i (pos~list-position (list i)))
	       (pair-with-old-pos=i (find pos-i pospos-list :test #'otter=first-is-keim-equal))
	       (new-pos (second pair-with-old-pos=i))
	       (acc-literal (data~struct-at-position literals new-pos)))
	  (setq back-list (append back-list (list acc-literal))))))))

(defun otter=compute-changed-positions (old-literals new-literals)
  (declare (edited  "02-SEP-1997")
	   (authors Ameier)
	   (input   "Two lists of literals. The second has to be a permutation of the first.") 
	   (effect  "None.")
	   (value   "A list of position pairs, who both describe the same literal, the first position the literal in"
		    "the first list and the second position the same literal in the second list."))
  (do* ((i 0 (+ i 1))
	(rest-old-literals old-literals (rest rest-old-literals))
	(pospos-list nil))
      ((null rest-old-literals)
       pospos-list)
    (let* ((head-old-lit (first rest-old-literals))
	   (old-pos (pos~list-position (list i)))
	   (new-pos (first (data~positions new-literals #'(lambda (lit)
							    (eq lit head-old-lit))))))
      (setq pospos-list (cons (list old-pos new-pos)
			      pospos-list)))))

#| ------------------------------------------------- Parse Utilities --------------------------------------------------- |#
   
(defun otter=string2object (string &key (type nil))
  (declare (edited  "17-MAY-1996")
	   (authors Ameier)
	   (input   "A string")
	   (effect  "None.")
	   (value   "If the string corresponds to an object in the otter*convert-list"
		    "or the otter*local-clause-vars list the corresponding object is returned."
		    "otherwise the string corresponds to a new renaming var, so this"
		    "new renaming-var is created, the otter*local-clause-vars list is updated"
		    "by the pair new-var/string and the new-var is returned."))
  (let ((member-convert-list (first (first (member string otter*convert-list
						   :test #'(lambda (string pair) (string= string (second pair))))))))
    (if member-convert-list
	;; -> already in convert-list -> reconvert the string to an object
	;;member-convert-list

	(cond ((typep member-convert-list 'term+number)
	       member-convert-list)
	      ((string-equal (keim~name member-convert-list) "=")
	       (env~lookup-object '= otter*current-environment))
	      (t
	       member-convert-list))
      
      ;; not yet in convert-list -> look at otter*local-clause-vars
      (let ((member-local-clause (first (first (member string otter*local-clause-vars
						       :test #'(lambda (string pair) (string= string (second pair))))))))
	(if member-local-clause
	    member-local-clause   ;; -> already a new local clause var produced -> return it
	  ;; not yet a new var produced, produce it , add the pair (new-var "var-string") to  otter*local-clause-vars and
	  ;; return the new-var
	  (let* ((needed-type (cond ((null type)
				     (type~i))
				    ((null (type~variable-p type))
				     type)
				    (t ;; -> type ist type-variable
				     (if (env~lookup-object (keim~name type) otter*current-environment)
					 ;; ist im environment
					 type
				       ;; ist nicht im environment -> Kappa gebunden irgendwo her
				       (let* ((new-symbol (term~generate-new-name 'ntv otter*current-environment))
					      (new-type-var (type~variable-create new-symbol)))
					 (env~enter new-symbol new-type-var otter*current-environment)
					 new-type-var)))))
		 (new-var (term~generate-term-primitive-with-new-name 'orv- needed-type 'term+variable otter*current-environment)))
	    
	    (setq otter*temporary-variables (cons new-var otter*temporary-variables))
	    (setq otter*local-clause-vars (cons (list new-var string) otter*local-clause-vars))
	    
	    new-var))))))

(defun otter=number2clause (number)
  (declare (edited  "17-MAY-1996")
	   (authors Ameier)
	   (input   "A number.")
	   (effect  "None.")
	   (value   "If the number corresponds to a clause and is therefor standing in"
		    "the otter*number-clause-list the corresponding clause is returned."
		    "otherwise nil."))
  (second (first (member number otter*number-clause-list
			 :test #'(lambda (number pair) (= number (first pair)))))))

#| ----------------------------------------------------- Auxiliaries --------------------------------------------------- |#

(defun otter=find-empty-clause (list-of-clauses)
  (declare (edited  "07-MAY-1997")
	   (authors Ameier)
	   (input   "A list of clauses.")
	   (effect  "None.")
	   (value   "The first clause in the list with 0 literals, nil if no such one exists."))
  (first (remove-if-not #'(lambda (clause)
			    (null (cl~literals clause)))
			list-of-clauses)))

(defun otter=equality-in-clauses-p (clauses)
  (declare (edited  "17-SEP-1996")
	   (authors Ameier)
	   (input   "A set of clauses.")
	   (effect  "None.")
	   (value   "T if a literal of a clause contains = as predicat. Nil otherwise."))
  (do* ((rest-literals (apply 'append (mapcar #'cl~literals clauses)) (rest rest-literals))
	(flag nil))
      ((or flag (null rest-literals)) flag)
    (if (atptop~equation-p (first rest-literals))
	(setq flag 't))))

#| --------------------------------------------- Handling the convertion of names -------------------------------------- |#

#|
   You can't use all symbols in names for predicates, functions and symbols as input for otter. So the existing names
   has to be converted in a otter acceptable form.
   In the variable otter*convert-list a list of pairs (lists) of objects and their converted name-strings is stored
|#

(defun otter=compute-convert-list (clauses)
  (declare (edited  "21-AUG-1997")
	   (authors Ameier)
	   (input   "A list of clauses.")
	   (effect  "All names of Constants and variables of the clauses are converted"
		    "into names, that are usable by otter."
		    "A list of these objects and their corresponding new names (strings)"
		    "is stored in otter*convert-list.")
	   (value   "Undifined."))
  (setq otter*convert-list nil)
  (mapcar #'otter=convert-object clauses))

(defgeneric otter=convert-object (object)
  (declare (edited  "21-AUG-1997")
	   (authors Ameier)
	   (input   "An object.")
	   (effect  "The otter*convert-list is updated. by converting the parts of this"
		    "object.")
	   (value   "Undifined."))
  (:method ((object cl+clause))
	   (mapcar #'otter=convert-object (cl~literals object)))
  (:method ((object lit+literal))
	   (otter=convert-object (lit~atom object)))
  (:method ((object term+appl))
	   (mapcar #'otter=convert-object
		   (cons (data~appl-function object)
			 (data~appl-arguments object))))
  (:method ((object term+primitive))
	   (otter=convert-name object)))

(defun otter=convert-name (object)
  (declare (edited  "14-MAY-1996")
	   (authors Ameier)
	   (input   "An object, that can be of type term+variable, term+constant or term+number.")
	   (effect  "If a new name-string is produced, a pair of old-name-string"
		    "and new-name-string is added to the otter*convert-list.")
	   (value   "From the name is a otter-compatible name produced."
		    "That means all symbols till alphabetics,numbers and _ are"
		    "deleted from the name and a counter-number is added."
		    "If var is set the resulting string is upcased, otherwise"
		    "it is downcased. If the name was attached before the"
		    "before produced new-string is taken from the otter*convert-list"
		    "and is returned otherwise a new string is produced in the way"
		    "descibed before."))
  (if (and (or (keim~equal object otter*equality-object)
	       (and (term~constant-p object)
		    (null (typep object 'term+number))
		    (string= (string (keim~name object)) "=")))
	   (null (find "=" otter*convert-list :test #'(lambda (str pair)
							(string= str (second pair))))))
      ;;(setq otter*convert-list (cons (list otter*equality-object "=") otter*convert-list))

      (setq otter*convert-list (cons (list (if (term~schema-p otter*equality-object)
					       (data~schema-range otter*equality-object)
					     otter*equality-object)
					   "=")
				     otter*convert-list))
    
    (let* ((name (keim~name object))
	   (name-string (if (stringp name)
			    name
			  (format nil "~A" name)))

	   ;; Compute whether the element is already in the otter*convert-list
	   ;;(partner-string (second (first (member object otter*convert-list
	   ;;					  :test #'(lambda (thing pair)
	   ;;						    (let* ((thing2 (first pair)))
	   ;;						      (if (term~schema-p thing2)
	   ;;							  (data~equal-p thing (data~schema-range thing2))
	   ;;							(keim~equal thing thing2)))))))))
	   (partner-string (second (first (member object otter*convert-list
	   					  :test #'(lambda (thing pair)
	   						    (let* ((thing2 (first pair)))
	   						      (or (eq thing thing2)
								  (and (data~equal thing thing2)
								       (data~equal (term~type thing) (term~type thing2)))))))))))
      
      
      ;; If element already in the otter*convert-list give back the ob-string already used in the otter*cnvert-list
      (if partner-string
	  partner-string
	;; If not a new string is computed and is together with the input object, or, if existing its representation in the
	;; environment, added to the otter*convert-list (list object string).
	;; If polymorphie is used, it is necessary to use the object from the environment.
	;; For example set with Type (aa -> o), but in the application (set a) with a of type i, set is of type (i -> o)
	;; If we would save in the otter*convert-list this set, we couldn't create a term (set 1) with 1 of type num.
	(let ((new-string ""))
	  (do ((i 0 (+ i 1)))
	      ((>= i (length name-string)) nil)
	    (when (member (char name-string i) otter*name-symbols)
	      (setq new-string (format nil "~A~A" new-string (char name-string i)))))
	  (setq new-string (format nil "ob_~A_~A" new-string (incf otter*convert-counter)))
	  (let ((checked-new-string (if (term~variable-p object)
					(string-upcase new-string)
				      (string-downcase new-string)))
		;;(env-element (env~lookup-object (intern (string-upcase name-string) (find-package :omega))
		;;	     			(res~proof-environment otter*current-problem)))
		)
	    
	    (setq otter*convert-list (cons (list object
						 ;;(if (and env-element (not (typep object 'term+number)))
						 ;;    env-element
						 ;;  object)
						 checked-new-string) otter*convert-list))
	    checked-new-string)))))) 

(defun otter=get-checked-name-to-object (object)
  (declare (edited  "22-AUG-1997")
	   (authors Ameier)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "The checked name string to the object."
		    "If the object is in the otter*convert-list, this string is taken"
		    "otherwise the keim~name slot of the object is returned as string."))
  (let* ((name (keim~name object))
	 (name-string (if (stringp name)
			  name
			(format nil "~A" name)))
	 ;; Compute whether the element is already in the otter*convert-list
	 ;;(partner-string (second (first (member object otter*convert-list
	 ;; 					:test #'(lambda (thing pair)
	 ;;						  (let* ((thing2 (first pair)))
	 ;;						    (if (term~schema-p thing2)
	 ;;							(data~equal-p thing (data~schema-range thing2))
	 ;;						      (keim~equal thing thing2))))))))
	 (partner-string (second (first (member object otter*convert-list
	  					:test #'(lambda (thing pair)
							  (let* ((thing2 (first pair)))
							    (or (eq thing thing2)
								(and (data~equal thing thing2)
								     (data~equal (term~type thing) (term~type thing2)))))))))))
    
    
    (if partner-string
	partner-string
      name-string))) 


#| ---------------------------------------------- GETTING OTTER-informations ------------------------------------------- |#

(defun otter=get-otter-informations (res-proof otter-problem-dir)
  (declare (edited  "17-SEP-1996")
	   (authors Ameier)
	   (input   "A resolution proof, an existing directory in which the  otter.in"
		    "and otter.out file should be stored and this two files.")
	   (effect  "The otter*rest-initial-clauses are set to the initial clauses of"
		    "resolution proof, and the otter*number-clause-list is set to nil."
		    "Otter is called in AUTO mode on the both files (otter.in file"
		    "has to cotain the input file for otter) the result is written"
		    "in the otter.out file.")
	   (value   "Multiple-value:"
		    "First: a set of strings, representing all flags otter has choosen"
		    "       in auto mode without the forbidden, +  the standart falgs"
		    "       + needed-flags."
		    "Second: A list of all clauses otter has selected for usable list."
		    "Third: A list of all clauses otter has selected for sos list."))

  (let* ((in-file (merge-pathnames "otter.in" otter-problem-dir))
	 (out-file (merge-pathnames "otter.out" otter-problem-dir)))
    
    (setq otter*rest-initial-clauses (if otter*reflexivity-item
					 (cons otter*reflexivity-item (res~proof-initial-clauses res-proof))
				       (res~proof-initial-clauses res-proof)))
    (setq otter*number-clause-list nil)

    ;; schreibt otter.in file in otter*in-string
    (otter=print 'auto
		 (append (list (format nil "%%%% MODE FLAGS %%%%~A" #\Newline)
			       "set(auto)."
			       "assign(max_gen,0).")
			 otter*needed-flags)
		 (res~proof-clauses res-proof)
		 nil)

    ;; schreibt otter*in-string nach in-file
    (atptop~print-string-in-file otter*in-string in-file)
  
    (omega~message "~%Getting information from OTTER autonomous mode ... ")

    ;; ruft otter auf
    (sys~call-system (format nil "~A < ~A >! ~A" (otter~program) in-file out-file))

    ;; parsed otter-information
    (otter=parse-otter-informations res-proof otter-problem-dir)))

(defun otter=parse-otter-informations (res-proof directory)
  (declare (edited  "17-SEP-1996")
	   (authors Ameier)
	   (input   "A resolution proof and a directory ,in which the otter.out file"
		    "is stored, produced by starting otter in the auto-mode.")
	   (effect  "None.")
	   (value   "Multiple-value:"
		    "First: a set of strings, representing all flags otter has choosen"
		    "       in auto mode."
		    "Second: A list of all clauses otter has selected for usable list."
		    "Third: A list of all clauses otter has selected for sos list."))
  (let ((file (format nil "~A~A" directory "/otter.out")))
    (multiple-value-bind
	(inf-lines usable-clause-lines)
	(with-open-file (stream file
				:direction :input)
			(do* ((eof-flag nil)			      
			      (control-flag 'start)
			      (information-list nil)
			      (usable-clause-list nil))
			    ((or eof-flag (equal control-flag 'end))
			     (values information-list usable-clause-list))
			  (let ((string-line (read-line stream nil '+-*/)))
			    ;; +-*/ is a value never will be in the file, that's the only sence of +-*/
			    (if (equal string-line '+-*/) 
				(setq eof-flag 't)
			      (cond ((equal control-flag 'start)
				     (let ((test-string (atptop~string-is-prefix-of-string-p
							 "SCAN INPUT"
							 string-line)))
				       (if test-string
					   (setq control-flag 'inf))))
				    ((equal control-flag 'inf)
				     (let ((test-string (atptop~string-is-prefix-of-string-p
							 "   dependent: "
							 string-line)))
				       (if test-string
					   (setq information-list (append information-list (list test-string)))))
				     (if (string= string-line "------------> process usable:")
					 (setq control-flag 'usable)))
				    ((equal control-flag 'usable)
				     (if (string= string-line "------------> process sos:")
					 (setq control-flag 'end)
				       (if (not (string= string-line ""))
					   (setq usable-clause-list
						 (append usable-clause-list (list string-line)))))))))))
      (multiple-value-bind
	  (usable-clauses sos-clauses)
	  (otter=get-clauses usable-clause-lines res-proof)
	(values (otter=get-inf-lines inf-lines) usable-clauses sos-clauses)))))

(defun otter=get-inf-lines (inf-lines)
  (declare (edited  "17-SEP-1996")
	   (authors Ameier)
	   (input   "A set of strings representing the lines of flags otter has"
		    "choosen by using it in auto mode.")
	   (effect  "None.")
	   (value   "A set of strings, each representing a otter-flag setting or clearing."
		    "In this set are contained all flags from the input strings, if not"
		    "explicit forbidden in the otter*forbidden-flags, and the flags standing"
		    "in otter*auto-standart-flags otter*needed-flags."))
  (append (mapcar #'(lambda (string)
		      (format nil "~A~A" #\Newline string))
		  inf-lines) 
	  otter*auto-standart-flags))

(defun otter=get-clauses (usable-clause-lines res-proof)
  (declare (edited  "17-SEP-1996")
	   (authors Ameier)
	   (input   "A set of strings, representing the lines of input clauses, otter"
		    "has choose in auto to usable clauses, the resolution proof.")
	   (effect  "None.")
	   (value   "The set of initial clauses (taken from resolution proof by"
		    "res~proof-clauses) according to the string-lines."))
  (do* ((rest-usable-clause-lines usable-clause-lines (rest rest-usable-clause-lines))
	(rest-clauses (if otter*reflexivity-item
			  (cons otter*reflexivity-item (res~proof-initial-clauses res-proof))
			(res~proof-initial-clauses res-proof)))
	(usable-clauses nil))
      ((null rest-usable-clause-lines) (values usable-clauses rest-clauses))
    (let* ((clause-line (atptop~cut-first-char (second (atptop~divide-string (first rest-usable-clause-lines) #\:))))
	   (clause (otter=parse-line clause-line res-proof :ignore-justification 't)))
      (when (and clause (not (equal clause 't))) ;; t kann passieren bei additional clause !!!!!!!!!
	(progn
	  (setq usable-clauses (cons clause usable-clauses))
	  (setq rest-clauses (remove clause rest-clauses)))))))

#| --------------------------------------------------- Behandlung der Gleichheit ------------------------------------------- |#


(defun otter=handle-equality (res-proof &key (insert 't))
  (declare (edited  "25-MAR-1998")
	   (authors Ameier)
	   (input   "A resolution proof.")
	   (effect  "Handles the use of the equality."
		    "If the initial clauses of the resolution proof contain equality literals t"
		    "blablablablablablabl")
	   (value   "Undefined."))
  
  ;; Behandlung der Gleichheit
  ;; falls in den Klauseln = vorkommt (=> equality-flag true) wird das reflexsivitaets axiom x=x erzeugt und eine
  ;; entsprechende Klausel [+ x=x] zu Klauseln des initialen Resolutionsbeweises hinzuaddiert.

  (let* ((equality-flag (otter=equality-in-clauses-p (res~proof-initial-clauses res-proof))))
    
    (setq otter*equality-object (env~lookup-object '= (res~proof-environment res-proof)))
    
    (if equality-flag
	(let* ((new-type-var (type~variable-create 'aa)) ;; der Name ist hier Scheiss egal !!!!!
	       (new-var (term~generate-term-primitive-with-new-name 'x- new-type-var 'term+variable otter*current-environment))
	       ;;
	       ;;	 (data~c-domain (term~type otter*equality-object))))   ;; hier ist es Strunz egal welche Typ-Variable
	       ;;                                                                ;; hier hin kommt, aber vielleicht sollte es nicht
	       ;;                                                                ;; gerade die des = sein !!! HACK BY AMEIER
	       (new-term (term~appl-create otter*equality-object
					   (list new-var new-var)
					   ))
	       (new-clause (cl~create (list (lit~literal-create new-term
								't))
				      :justification (res~reflex-create (gensym)))))

	  (env~remove (keim~name new-var) otter*current-environment)

	  ;; Entfaellt ab Version 3.3
	  ;; setzt Kappa-Slot im Reflexivity-Item-ATOM
	  ;; (setf (data~kappa new-term) (list new-type-var))                ;; (list (data~c-domain (term~type otter*equality-object))))
	  
	  (setq otter*reflexivity-item new-clause)
	  (when insert
	    (setf (res~proof-clauses res-proof) (cons new-clause (res~proof-clauses res-proof)))))
      (setq otter*reflexivity-item nil))))

#| ---------------------------------------------- handle avoid propositional ------------------------------------------------------- |#

#| Wird otter im auto mode gestartet macht er eine fuer uns dumme Sache: falls alle Klauseln propositional sind, wird ein flag
   im AUto mode gestetzt, das es uns unmoeglich macht den OTTER-PROOF zu parsen.
   Um nicht immer auf den combinde-modus zurueckgreifen zu muessen, was uber das NEtz unmoeglich sein duerfte wird hier einfach
   eine neue Unit-Klause erzeugt, im Fall von auto mode wird diese klausel einfach mitausgegeben durch otter=print
   (+additionalnonproppred(additionalnonpropconst)), deren Anwesenheit genau das verhindert.
   Diese Klausel wird NUR IM AUTO MODE beigefuegt. Da die normale Strategie von Otter ist negative Klauseln in die USABLES zu stecken
   und positive in die SOS um dann hyper-resolution machen zu koennen, sollte diese Klausel als negative Klausel normalerweise in die
   USABLES rutschen und die Strategie und Vollstaendigkeit nicht weiter beeinfluassen.

   Letzte Aussage muss noch verifiziert werden !
   |#

(defun otter=non-prop-clause ()
  (let* ((env otter*current-environment)
	 (pred-name (term~generate-new-name 'additionalnonproppred_ env))
	 (cons-name (term~generate-new-name 'additionalnonpropcons_ env))
	 (new-pred (term~constant-create pred-name (post~read-object '(o i) env :existing-type)))
	 (new-cons (term~constant-create cons-name (post~read-object 'i env :existing-type)))
	 (new-clause (cl~create (list (lit~literal-create (term~appl-create new-pred (list new-cons)) nil)))))
    new-clause))

#| ------------------------------------------- handle user input-strings (weights, flags) ------------------------------------------ |#


(defun otter=parse-converted-objects (in-string)
  (declare (edited  "03-SEP-1997")
	   (authors Ameier)
	   (input   "A string.")
	   (effect  "None.")
	   (value   "In the string all substrings, that correspond to names of converted-objects"
		    "are also converted."))
  (let* ((string-list (atptop~divide-string-by-lists in-string (list #\( #\) #\. #\, #\[ #\$ #\_ #\] #\=) (list #\space))))
    (do* ((rest-list string-list (rest rest-list))
	  (back-string ""))
	((null rest-list)
	 back-string)
      (let* ((head-string (first rest-list))
	     (new-string (if (atptop~parse-number head-string)
			     head-string
			   (otter=get-conv-string-to-obj-string head-string))))
	(setq back-string (format nil "~A~A" back-string new-string))))))
					

(defun otter=get-conv-string-to-obj-string (in-string)
  (declare (edited  "27-AUG-1997")
	   (authors Ameier)
	   (input   "A string.")
	   (effect  "None.")
	   (value   "If the string is the name of an object in the otter*convert-list, the corresponding"
		    "converted name string, otherwise the input string itself."))
  (let* ((con-string (second (find in-string otter*convert-list :test #'(lambda (string1 pair)
									  (let* ((obj-name (keim~name (first pair))))
									    (string= (string-downcase string1)
										     (string-downcase (if (stringp obj-name)
													  obj-name
													(string obj-name))))))))))
    (if con-string
	con-string
      in-string)))


(defun otter=check-input-as-file (in-string description)
  (declare (edited  "27-AUG-1997")
	   (authors Ameier)
	   (input   "A string and a description symbol.")
	   (effect  "None.")
	   (value   "If the first five chars are not 'file:' the string, otherwise"
		    "the rest of the string is interpreted as a file-name and the input"
		    "of this file is returned, if it exists, otherwise error."))
  (if (string= in-string "")
      in-string
    (with-open-file (stream in-string
			    :direction :input
			    :if-does-not-exist nil)
		    (if stream
			(let* ((in-lines (with-open-file (stream in-string
								 :direction :input
								 :if-does-not-exist nil)
							 (do* ((eof-flag nil)
							       (return-list nil))
							     (eof-flag
							      return-list)
							   (let* ((string-line (read-line stream nil '+-*/pippuppap)))
							     ;; +-*/ is a value never will be in the file, that's the only sence of +-*/
							     (if (equal string-line '+-*/pippuppap) 
								 (setq eof-flag 't)
							       (setq return-list (append return-list (list string-line)))))))))
			  (omega~message "~% File found for ~A, ~A interpreted as file." description description)
			  (do* ((rest-lines in-lines (rest rest-lines))
				(return-string ""))
			      ((null rest-lines)
			       return-string)
			    (setq return-string (format nil "~A~A~A" return-string #\Newline (first rest-lines)))))
		      (progn
			(omega~message "~% No File found for ~A, ~A interpreted as direct user input." description description)
			in-string)))))

#| --------------------------------------------------- get sos clauses by user ----------------------------------------------------- |#

(defun otter=get-sos-clauses (res-proof otter-lists) 	       
  (declare (edited  "26-AUG-1997")
	   (authors Ameier)
	   (input   "A resolution proof and a list.")
	   (effect  "None.")
	   (value   "A list of initial clauses of the resolution proof, that should be used"
		    "for the otter call as sos-clauses."))
  (let* ((clauses (res~proof-clauses res-proof))
	 (ass-clauses (otter=get-assumption-clauses res-proof))
	 (thm-clauses (otter=get-conclusion-clauses res-proof)))
    (cond ((null otter-lists)
	   ;; keine Otter Einschaetzung
	   (omega~message "~%The problem consists of the following input clauses:")
	   (mapcar #'(lambda (clause)
		       (omega~message "~% ~A : ~A" (keim~get clause 'clause-number) clause))
		   clauses)
	   (omega~message "~%~% The separation in clauses corresponding to conclusion and assumptions of the problem is:")
	   (omega~message "~% The assumption clauses: ~A" (mapcar #'(lambda (clause)
								      (keim~get clause 'clause-number))
								  ass-clauses))
	   (omega~message "~% The theorem clauses: ~A" (mapcar #'(lambda (clause)
								   (keim~get clause 'clause-number))
							       thm-clauses))
	   (if (inter~prompt-for-input-with-default
		(comint~interface comint*current-comint)
		(format nil "~%Do you accept this partition for SOS and usable clauses ?")
		(arg~find-argtype 'boolean)
		nil)
	       (append thm-clauses
		       (if otter*reflexivity-item
			   (if (inter~prompt-for-input-with-default
				(comint~interface comint*current-comint)
				(format nil "~%Should the reflexivity clause ~A : ~A be added to the sos-clauses (otherwise it is added to the usable-clauses)" (keim~get otter*reflexivity-item 'clause-number) otter*reflexivity-item)
				(arg~find-argtype 'boolean)
				t)
			       (list otter*reflexivity-item)
			     nil)
			 nil))
	     (otter=parse-clauses-by-user clauses)))
	  (t
	   (let* ((pre-usable-clauses (first otter-lists))
		  (pre-sos-clauses (second otter-lists)))
	     (omega~message "~%The problem consists of the following input clauses:")
	     (mapcar #'(lambda (clause)
			 (omega~message "~% ~A : ~A" (keim~get clause 'clause-number) clause))
		     clauses)
	     (omega~message "~%~% The separation in clauses corresponding to the Theorem of the problem and the assumptions is:")
	     (omega~message "~% The assumption clauses: ~A" (mapcar #'(lambda (clause)
								   (keim~get clause 'clause-number))
							       ass-clauses))
	     (omega~message "~% The theorem clauses: ~A" (mapcar #'(lambda (clause)
								(keim~get clause 'clause-number))
							    thm-clauses))
	     (omega~message "~%~% The separation of the clauses by otter in auto-mode is:")
	     (omega~message "~% The usable clauses: ~A" (mapcar #'(lambda (clause)
								   (keim~get clause 'clause-number))
							       pre-usable-clauses))
	     (omega~message "~% The sos clauses: ~A" (mapcar #'(lambda (clause)
								(keim~get clause 'clause-number))
							pre-sos-clauses))
	     (when otter*reflexivity-item
	       (omega~message "~%WARNING: Problem contains equality! Maybe the otter partition is incomplete !!")) 
	     (let* ((answer (inter~prompt-for-input-with-default
			     (comint~interface comint*current-comint)
			     (format nil "~%Do you accept the problem-partition (problem) or the otter-partion (otter) or do you want to specify yourself (user)")
			     (arg~find-argtype 'symbol)
			     'otter)))
	       (cond ((eq answer 'problem)
		      (append thm-clauses
			      (if otter*reflexivity-item
				  (if (inter~prompt-for-input-with-default
				       (comint~interface comint*current-comint)
				       (format nil "Should the reflexivity clause ~A : ~A be added to the sos-clauses (otherwise it is added to the usable-clauses)" (keim~get otter*reflexivity-item 'clause-number) otter*reflexivity-item)
				       (arg~find-argtype 'boolean)
				       t)
				      (list otter*reflexivity-item)
				    nil)
				nil)))
		     ((eq answer 'otter)
		      pre-sos-clauses)
		     (t
		      (otter=parse-clauses-by-user clauses)))))))))			     
			 
(defun otter=parse-clauses-by-user (clauses)
  (declare (edited  "26-AUG-1997")
	   (authors Ameier)
	   (input   "A set of clauses")
	   (effect  "None.")
	   (value   "A list of clauses, that are specified by their numbers from the user."))
  (let* ((number-list (inter~prompt-for-input-with-default
		       (comint~interface comint*current-comint)
		       (format nil "~%Please give the list of the numbers of the clauses to be put into the sos-list")
		       (arg~find-argtype 'list-pos-int)
		       nil)))
    (apply 'append (mapcar #'(lambda (number)
			       (let* ((acc-clause (find number clauses :test #'(lambda (num claus)
										 (= num (keim~get claus 'clause-number))))))
				 (if (not acc-clause)
				     (progn
				       (omega~message "~% WARNING: ~A can't be read as the number of a clause." number)
				       nil)
				   (list acc-clause))))
			   number-list))))

;; zum lesen einer list positiver integers
(eval-when (load compile eval)
  (arg~deftype list-pos-int
	       (read-function otter=read-list-of-pos-int)
	       (predicate otter=list-of-pos-int-p)
	       (help "a Lisp symbol"))
  )

(defun otter=list-of-pos-int-p (liste)
  (declare (edited  "27-AUG-1997")
	   (authors Ameier)
	   (input   "A list.")
	   (effect  "None.")
	   (value   "T if the list contains only positiv integers, nil otherwise."))
  (eval (cons 'and (mapcar #'(lambda (element)
			       (and (typep element 'integer)
				    (> element 0)))
			   liste))))

(defmethod otter=read-list-of-pos-int ((obj list) &rest others)
  (declare (ignore others))
  obj)


  
(defun otter=get-assumption-clauses (res-proof)
  (declare (edited  "26-AUG-1997")
	   (authors Ameier)
	   (input   "A resolution proof.")
	   (effect  "None.")
	   (value   "The list of all initial clauses of the resolution proof, that are conected by"
		    "the delta relation with a assumption of the resolution proof."))    
  (let* ((assumptions (res~proof-assumptions res-proof))
	 (delta-pairs (delta~relation-pairs (res~proof-delta-relation res-proof)))
	 (delta-pairs-to-ass (remove-if-not #'(lambda (pair)
						(find (delta~delta-formula pair) assumptions))
					    delta-pairs)))
    (remove-duplicates (mapcar #'delta~delta-clause delta-pairs-to-ass))))

(defun otter=get-conclusion-clauses (res-proof)
  (declare (edited  "26-AUG-1997")
	   (authors Ameier)
	   (input   "A resolution proof.")
	   (effect  "None.")
	   (value   "The list of all initial clauses of the resolution proof, that are conected by"
		    "the delta relation with the conclusion of the resolution proof."))    
  (let* ((conclusion (res~proof-conclusion res-proof))
	 (delta-pairs (delta~relation-pairs (res~proof-delta-relation res-proof)))
	 (delta-pairs-to-conc (remove-if-not #'(lambda (pair)
						(eq (delta~delta-formula pair) conclusion))
					     delta-pairs)))
    (remove-duplicates (mapcar #'delta~delta-clause delta-pairs-to-conc))))


#| -------------------------------------------- Use a complete otter out file ------------------------------------------------------- |#


(defun otter~use-otter-out (open-node ho-pds out-file out-style)
  (declare (edited  "05-NOV-1997")
	   (authors Ameier)
	   (input   "An node, justified by otter, the current pds, a file that contains an otter output"
		    "for the problem specified by the otter justification of the node and the style of the"
		    "output-file (t -> with proof object , nil without proof object).")
	   (effect  "None.")
	   (value   "If the otter proof in the out file can be parsed as a proof to the problem specified"
		    "by the otter justification of the input node, this resolution proof is returned"
		    "(this means especially, that all input clauses of the otter proof must be equal"
		    "till renaming to clauses of the clausenormalized problem), otherwise nil."))
    
  (let* ((otter-problem (otter~generate-otter-problem open-node
						      (just~premises (node~justification open-node))
						      ho-pds))
	 (res-proof (atpprb~problem-part-res-proof otter-problem)))

    ;; Compute the convertion of names
    (otter=compute-convert-list-equal (res~proof-clauses res-proof))
    (otter=request-skolem-functions res-proof)

    ;; setzt global-vars im otter-problem
    (setf (atpprb~problem-global-vars otter-problem)
	  (list otter*convert-list (second (atpprb~problem-global-vars otter-problem)) out-style))
    
    ;; liesst out-file in einen string und setzt atp-out-file des otter-problem
    (setf (atpprb~problem-atp-out-string otter-problem) (atptop~read-file-as-string out-file))
    
    ;; parsen des otter-Beweises
    (otter~complete-otter-problem! otter-problem :parse-back 't)))

;; Konstruktion of the otter*convert-list
;; To read a complete otter.out file it is necessary that the names in the out file and the names of the correspondig objects are EQUAL
;; so the only sense of this convertion function is, to fill the the otter*convert-list with pairs of the form (Object string), where
;; the string is the name of the object

(defun otter=compute-convert-list-equal (clauses)
  (declare (edited  "05-NOV-1997")
	   (authors Ameier)
	   (input   "A list of clauses.")
	   (effect  "A list of all objects and their names (strings)"
		    "is stored in otter*convert-list.")
	   (value   "Undefinded."))
  (setq otter*convert-list nil)
  (mapcar #'otter=convert-object-equal clauses))


(defgeneric otter=convert-object-equal (object)
  (declare (edited  "05-NOV-1997")
	   (authors Ameier)
	   (input   "An object.")
	   (effect  "The otter*convert-list is updated. by converting the parts of this"
		    "object.")
	   (value   "Undifined."))
  (:method ((object cl+clause))
	   (mapcar #'otter=convert-object-equal (cl~literals object)))
  (:method ((object lit+literal))
	   (otter=convert-object-equal (lit~atom object)))
  (:method ((object term+appl))
	   (mapcar #'otter=convert-object-equal
		   (cons (data~appl-function object)
			 (data~appl-arguments object))))
  (:method ((object term+primitive))
	   (otter=convert-name-equal object)))

(defun otter=convert-name-equal (object)
  (declare (edited  "05-NOV-1997")
	   (authors Ameier)
	   (input   "An object, that can be of type term+variable, term+constant or term+number.")
	   (effect  "If a new name-string is produced, a pair of old-name-string"
		    "and new-name-string is added to the otter*convert-list.")
	   (value   "From the name is a otter-compatible name produced."
		    "That means all symbols till alphabetics,numbers and _ are"
		    "deleted from the name and a counter-number is added."
		    "If var is set the resulting string is upcased, otherwise"
		    "it is downcased. If the name was attached before the"
		    "before produced new-string is taken from the otter*convert-list"
		    "and is returned otherwise a new string is produced in the way"
		    "descibed before."))
  (let* ((name (keim~name object))
	 (name-string (if (stringp name)
			  name
			(format nil "~A" name)))
	 ;; Compute whether the element is already in the otter*convert-list
	 (partner-string (second (first (member object otter*convert-list
						:test #'(lambda (thing pair)
							  (keim~equal thing (first pair))))))))
    ;; If element already in the otter*convert-list give back the ob-string already used in the otter*cnvert-list
    (if partner-string
	partner-string
      (let ((checked-new-string (if (term~variable-p object)
				    (string-upcase name-string)
				  (string-downcase name-string)))
	    (env-element (env~lookup-object (intern (string-upcase name-string) (find-package :omega))
					    (res~proof-environment otter*current-problem))))
	(setq otter*convert-list (cons (list (if (and env-element (not (typep object 'term+number)))
						 env-element
					       object)
					     checked-new-string) otter*convert-list))
	checked-new-string))))


;; the only problem with the convertion can be skolem-function because they are produced during the normalization, so it is impossible to
;; name them same as the according symbols in the complete otter.out file. Therefor the following function interactively request the
;; names of the according skolem-ucntion fromn the user.

(defun otter=request-skolem-functions (res-proof)
  (declare (edited  "12-NOV-1997")
	   (authors Ameier)
	   (input   "A resolution proof.")
	   (effect  "All entries for skolem-functions are deleted from the otter*convert-list"
		    "The user is asked about the names of the skolemfunction of the resolution proof."
		    "The otter*convert-list is updated with the pairs of (skolemfunction coresponding-name).")
	   (value   "Undefined."))
  (let* ((skolem-functions (res~proof-skolem-functions res-proof)))
    (setq otter*convert-list (remove-if #'(lambda (pair)
					    (find (first pair) skolem-functions))
					otter*convert-list))
    
    (mapcar #'(lambda (skolem-function)
		(let* ((name-string (inter~prompt-for-input-with-default
				     (comint~interface comint*current-comint)
				     (format nil "~%By the clause-normalization the following skolem-function was produced: ~A ~%What's the name of the according function in the otter output file (please input a string) ?" skolem-function)
				     (arg~find-argtype 'string)
				     nil)))
		  (setq otter*convert-list (cons (list skolem-function name-string)
						 otter*convert-list))))
	    skolem-functions)))
		




#| ---------------------------------------------------- CALL OTTER MAIN ----------------------------------------------------------- |#  


#|

Folgende Funktion ab omega3.1 nicht mehr noetig!

(defun otter=set-current-type-var-subst! ()
  (declare (edited  "27-MAR-1998")
	   (authors Ameier)
	   (input   "None.")
	   (effect  "If the otter*current-environment contains a type-var-subst, the global"
		    "variable otter*current-type-var-subst is set to this value, otherwise"
		    "a new substitution is created, added with key type-var-subst in the"
		    "environment and otter*current-type-var-subst is set to this new substitution.")
	   (value   "Undefined."))
  (let* ((type-var-subst (env~lookup-object 'type-var-subst otter*current-environment)))
    (if type-var-subst
	(setq otter*current-type-var-subst type-var-subst)
      (let ((new-subst (subst~create nil nil)))
	(setq otter*current-type-var-subst new-subst)
	(env~enter 'type-var-subst new-subst otter*current-environment)))))

|#


(defun otter~generate-otter-problem (conclusion-node assumption-nodes ho-pds &key (p2pl nil))

  ;; if p2pl is true, otter is called as propositional prover, that means p2pl~transform is used instead of p2f~transform
  ;; and the produced problem is of type pl-atp

  (declare (edited  "26-MAR-1998")
	   (authors Ameier)
	   (input   "A conclusion-node, a set of assumption-nodes and the pds the nodes are from.")
	   (effect  "The created otter-problem is inserted into the list in the plist entry"
		    "'atp-problems of the conclusion node.")
	   (value   "A atp-problem with type otter, a partial resolution proof and partial settet"
		    "global vars."
		    "Remark: atp-in-string is NOT-SET, and global vars are not set completly !!"))
  
  (setq otter*convert-counter 0)       ;; setzt counter fuer neue Namen auf 0
  (setq otter*temporary-variables nil) ;; setzt temporaere Variablen auf nil
  (setq otter*just-counter 0)          ;; setzt justification counter fuer neue justification auf 0
  
  (let* ((res-proof (atptop~resolution-proof-create-from-nodes conclusion-node assumption-nodes ho-pds)))

    (setq otter*current-problem res-proof)
    (setq otter*current-environment (res~proof-environment res-proof))

    ;; Entfaellt ab Omega-3.3
    ;; (otter=set-current-type-var-subst!)   ;; setzt variable otter*current-type-var-subst
   				       
    ;; translate the initial resolution proof res-proof to f.o. and normalize it
    (if p2pl
	(p2pl~translate res-proof)
      (p2f~translate res-proof))
    
    (omega~message "~% Normalizing ...")
    (hocnf~normalize-res-proof! res-proof)

    ;; beachte das Vorhandensein von '=' -> reflexsivitaets Klausel benoeetigt !
    (otter=handle-equality res-proof)

    ;; Remove clauses that contain abstractions
    ;; After p2f translation it can happen that there are clauses that contain lambda-abstraction
    ;; Such clauses can not be handled by OTTER (or any f.o. ATP and are therefore removed from the
    ;; clauses list
    (atptop~remove-clauses-with-abstractions! res-proof)
    
    ;; Compute the convertion of names
    (otter=compute-convert-list (res~proof-clauses res-proof))
    
	   
    (let* ((otter-problem (atpprb~create-fo-problem (gensym "otter-problem-")
						    (if p2pl
							'pl-atp
						      'otter)
						    nil      ;; otter-in-file kommt erst spaeter dazu: -> otter~add-in-string! 
						    nil
						    res-proof
						    (list otter*convert-list otter*reflexivity-item)
						    (if p2pl
							(list 'p2pl p2pl*domain p2pl*codomain)
						      (list 'p2f p2f*domain p2f*codomain)))))
      
      ;; in die atpprb~problem-translation-settings kommt eine Liste mit entweder:
      ;; 'p2f p2f*domain p2f*codomain, falls p2f~translate
      ;; 'p2pl p2pl*domain p2pl*codomain, falls p2pl~translate  
      
      ;; in the global-var-list ist folgende Ordnung:
      ;; 1.  otter*convert-list
      ;; 2.  otter*reflexivity-item
      ;; 3.  proof-object ;; kommt erst spaeter dazu -> otter~add-in-string!
      
      ;; nicht in der global-var-liste sind folgende Dingens:
      ;; 1.  otter*equality-object           -> reconstruierbar aus res-proof environment 
      ;; 2.  otter*current-problem           -> reconstruierbar aus res-proof environment
      ;; 3.  otter*current-environment       -> reconstruierbar aus res-proof environment
      ;; Folgendes entfaellt ab OMEGA 3.3
      ;; 4.  otter*current-type-var-subst    -> reconstruierbar aus res-proof environment
      ;; 5.  otter*in-string                 -> nicht mehr benoetigt
      ;; 6.  otter*convert-counter           -> nicht mehr benoetigt
      ;; 7.  otter*needed-flags              -> unveraenderlich global
      ;; 8.  otter*auto-standart-flags       -> unveraenderlich global
      ;; 9.  otter*name-symbols              -> unveraenderlich global
      ;; 10. otter*rest-initial-clauses      -> erst beim parsen benoetigt
      ;; 11. otter*local-clause-vars         -> erst beim parsen benoetigt    
      ;; 12. otter*temporary-variables       -> erst beim parsen benoetigt
      ;; 13. otter*just-counter              -> erst beim parsen benoetigt
      ;; 14. otter*number-clause-list        -> erst beim parsen benoetigt
      
      
      ;; unklar:
      ;; 1.  otter*already-set
      
      (keim~put conclusion-node 'atp-problems (cons otter-problem (keim~get conclusion-node 'atp-problems)))

      otter-problem
      )))

(defun otter~complete-otter-problem! (otter-problem &key (parse-back 't))
  (declare (edited  "25-MAR-1998")
	   (authors Ameier)
	   (input   "A otter problem and as keyword parse-back, a flag whether the may found proof"
		    "should be parsed back from the string.")
	   (effect  "The atp-out-string is read and then the resolution proof is parsed"
		    "from this string to complete the partial resolution proof.")
	   (value   "If there was a atp-out-string that represents a resolution proof (if otter has found a"
		    "proof) the complete resolution proof if flag parse-back was T or T if parse-bacl was"
		    "nil. If there is no atp-out-string (otter has failed to find a proof) nil."))
  (if (null (atpprb~problem-atp-out-string otter-problem))
      nil
    (let* ((otter-out-string (atpprb~problem-atp-out-string otter-problem))
	   (res-proof (atpprb~problem-part-res-proof otter-problem))
	   (global-vars (atpprb~problem-global-vars otter-problem))
	   (proof-object (third global-vars))
	   (translation-settings (atpprb~problem-translation-settings otter-problem)))
      
      (setq otter*convert-list (first global-vars))
      (setq otter*reflexivity-item (second global-vars))
      
      (setq otter*current-problem res-proof)
      (setq otter*equality-object (env~lookup-object '= (res~proof-environment res-proof)))
      (setq otter*current-environment (res~proof-environment res-proof))

      ;; Folgendes entfaellt ab OMEGA 3.3
      ;; (otter=set-current-type-var-subst!)   ;; setzt otter*current-type-var-subst

      (setq otter*temporary-variables nil) ;; setzt temporaeri variablen auf nil
      (setq otter*just-counter 0)      ;; setzt justification counter fuer neue justification auf 0

      (if (equal (first translation-settings) 'p2pl)
	  (progn
	    (setq p2pl*domain (second translation-settings))         ;; stellt Uebersetzungsinformation p2pl wieder her
	    (setq p2pl*codomain (third translation-settings)))
	(progn
	  (setq p2f*domain (second translation-settings))            ;; stellt Uebersetzungsinformation pl2p wieder her
	  (setq p2f*codomain (third translation-settings))))
      
      ;; read otter.out file
      (let* ((proof-flag (if (not parse-back)
			     (otter=read-without-parsing res-proof otter-out-string)
			   (progn (omega~message "Parsing Otter Proof ... ~%")
				  (if proof-object
				      (otter=read-with-proof-object res-proof otter-out-string)
				    (otter=read-without-proof-object res-proof otter-out-string))))))
	;; output
	(if proof-flag
	    (omega~message "~% OTTER HAS FOUND A PROOF ~%")
	  (omega~message "~% OTTER HAS FAILED TO FIND A PROOF ~%"))

	;; remove temporary-variables
	(mapcar #'(lambda (var)
		    (env~remove (keim~name var) otter*current-environment))
		otter*temporary-variables)
	
	;; values
	(if proof-flag
	    (if parse-back
		(progn
		  (setf (res~proof-empty-clause res-proof)
			(otter=find-empty-clause (res~proof-step-clauses res-proof)))
		  (when proof-object
		  ;; (otter=correct-paramods! (res~proof-empty-clause res-proof) (node~justification (res~proof-empty-clause res-proof)))
		    (mapcar #'(lambda (clause)
		  		(keim~remprop clause 'pospos-list))
		  	    (res~proof-clauses res-proof)))
		  (setq omega*current-resolution-proof res-proof)
		  (res~add-proof-in-hash-table res-proof)		  
		  (atptop~order-resolution-steps! res-proof)
		  (omega~message "~% OMEGA*CURRENT-RESOLUTION-PROOF IS SET TO THE FOUND RESOLUTION PROOF")
		  res-proof)
	      't)
	  nil))))) 

(defun otter~add-in-string! (otter-problem mode declarations sos-clauses usable-clauses proof-object &key (docu 't))
  (declare (edited  "26-MAR-1998")
	   (authors Ameier)
	   (input   "An otter-problem, the mode to call otter, the declarations of the flag settings"
		    "for otter, the sos-clauses, the usable clauses and whether proof-object is set or"
		    "not. The keyword docu signs, whether the convert-list should be added to the otter.in"
		    "string or not.")
	   (effect  "The otter*in-string is produced and added to the otter-problem. proof-object is added"
		    "as third element to the global-vars of the otter-problem.")
	   (value   "Undefined."))
  
  ;; konstruiert das otter.in file im otter*in-string
  (otter=print mode
	       declarations
	       sos-clauses
	       usable-clauses
	       :docu docu)
  
  ;; setzt atp-in-string im otter-problem
  (setf (atpprb~problem-atp-in-string otter-problem) otter*in-string)
  
  ;; setzt das 3. Argument in den global-vars auf proof-object
  (setf (atpprb~problem-global-vars otter-problem) (append (atpprb~problem-global-vars otter-problem)
							   (list proof-object))))



(defun otter~call-otter (open-node ho-pds dir ressource mode parse-back proof-object user-flag-string user-weight-string
				   &key
				   (p2pl nil)
				   (agent nil))
  
  ;; if p2pl is true, otter is called as propositional prover, that means p2pl~transform is used instead of p2f~transform
  ;; and the produced problem is of type pl-atp
  
  (declare (edited  "21-AUG-1997")
	   (authors Ameier)
	   (input   "An open node and the according pds, that contains this node. A directory in that"
		    "the new files should stand, the time-ressource and six arguments that determine"
		    "the use of otter:"
		    "1. mode (auto,combined,user) to determine how strong the user want to interact"
		    "   with the system."
		    "2. parse-back (t/nil) to determine, whether a possibly found proof is read back"
		    "3. proof-object (t/nil) to determine, whether build_proof_object should be used"
		    "4. user-flags (specified by user), a string for additional setting, from user"
		    "5. user-weights (specified by user), a string for additional settings from user,"
		    "   interpreted as weights."
		    "If by 4. and 5. the first five chars are 'file:', the rest of the string is"
		    "interpreted as the name of an according file.")
	   (effect  "None.")
	   (value   "If Otter has found a proof t or the according resolution proof (if parse-back was t)"
		    "nil otherwise"))
  (let* ((problem-name (keim~name ho-pds))
	 (otter-problem-dir (merge-pathnames (format nil "~A/" (string-downcase problem-name)) dir))
	 (in-file (merge-pathnames "otter.in" otter-problem-dir))
	 (out-file (merge-pathnames "otter.out" otter-problem-dir))
	 (otter-problem (otter~generate-otter-problem open-node
						      (just~premises (node~justification open-node))
						      ;; (remove open-node (pds~node-supports open-node))
						      ho-pds
						      :p2pl p2pl))
	 (user-flags (list (otter=check-input-as-file user-flag-string 'user-flags)))
	 (user-weights (list (otter=parse-converted-objects (otter=check-input-as-file user-weight-string 'user-weights))))
	 (res-proof (atpprb~problem-part-res-proof otter-problem)))

    ;; erzeugt directory falls es nicht existiert
    (unless (probe-file otter-problem-dir)
      (sys~call-system (format nil "mkdir ~A" (string-right-trim '(#\/) (namestring otter-problem-dir)))))
    
    ;; (setq global*otter-problem otter-problem)
    
    (cond ((eq mode 'auto)
	   
	   ;; erzeuge otter.in file in otter*in-string, fuege otter*in-string zum otter-problem hinzu (einschlieslich proof-object)
	   (otter~add-in-string! otter-problem
				 mode
				 (append (list (format nil "%%%% MODE FLAGS %%%%~A" #\Newline)
					       "set(auto).")
					 (if proof-object
					     (list "set(build_proof_object).")
					   nil)
					 (cons (format nil "~A%%%% USER INPUT %%%%~A" #\Newline #\Newline) user-flags)
					 otter*needed-flags)
				 (res~proof-clauses res-proof)
				 nil
				 proof-object)
	   
	   ;; call-otter vot Ort -> schreibt otter.out file in den out-string des otter-problems
	   (otter=call-otter! otter-problem otter-problem-dir ressource :agent agent)

	   ;; parsen des otter-beweises
	   (otter~complete-otter-problem! otter-problem :parse-back parse-back))
	  
	  ((eq mode 'user)
	       
	       ;; Nummerieren der Klauseln 
	       (do* ((rest-clauses (res~proof-clauses res-proof) (rest rest-clauses))
		     (i 1 (+ i 1)))
		   ((null rest-clauses)
		    nil)
		 (let* ((head-clause (first rest-clauses)))
		   (keim~put head-clause 'clause-number i)))

	       (mixin~save-and-set-input)
	       
	       (let* ((sos-clauses (otter=get-sos-clauses res-proof nil))
		      (usable-clauses (atptop~remove-list sos-clauses (res~proof-clauses res-proof))))

		 (mixin~reset-input)
		 
		 ;; Loeschen der Nummerierung der Klauseln 
		 (do* ((rest-clauses (res~proof-clauses res-proof) (rest rest-clauses)))
		     ((null rest-clauses)
		      nil)
		   (let* ((head-clause (first rest-clauses)))
		     (keim~remprop head-clause 'clause-number)))
		 
		 ;; erzeuge otter.in file in otter*in-string, fuege otter*in-string zum otter-problem hinzu(einschlieslich proof-object)
		 (otter~add-in-string! otter-problem
				       mode
				       (append (list (format nil "%%%% MODE FLAGS %%%%~A" #\Newline))
					       (if proof-object
						   (list "set(build_proof_object).")
						 nil)
					       (cons (format nil "~A%%%% USER INPUT %%%%~A" #\Newline #\Newline) user-flags)
					       (cons (format nil "~A%%%% USER WEIGHTS %%%%~A" #\Newline #\Newline) user-weights)
					       otter*needed-flags)
				       usable-clauses
				       sos-clauses
				       proof-object)

		 
		 ;; call-otter vot Ort -> schreibt otter.out file in den out-string des otter-problems
		 (otter=call-otter! otter-problem otter-problem-dir ressource :agent agent)
		 
		 ;; parsed otter-beweis
		 (otter~complete-otter-problem! otter-problem :parse-back parse-back)))
	  
	  (t
	   
	   ;; combined use -> first call otter in auto mode and get back the otter informations
	   
	   (multiple-value-bind
	       (otter-flags otter-usable-cl otter-sos-cl)
	       (otter=get-otter-informations res-proof otter-problem-dir)
	     
	     ;; Nummerieren der Klauseln 
	     (do* ((rest-clauses (res~proof-clauses res-proof) (rest rest-clauses))
		   (i 1 (+ i 1)))
		     ((null rest-clauses)
		      nil)
	       (let* ((head-clause (first rest-clauses)))
		 (keim~put head-clause 'clause-number i)))

	     (mixin~save-and-set-input)
	     
	     (let* ((sos-clauses (otter=get-sos-clauses res-proof (list otter-usable-cl otter-sos-cl)))
		    (usable-clauses (atptop~remove-list sos-clauses (res~proof-clauses res-proof))))
	       
	       (mixin~reset-input)
	       
	       ;; Loeschen der Nummerierung der Klauseln 
	       (do* ((rest-clauses (res~proof-clauses res-proof) (rest rest-clauses)))
		   ((null rest-clauses)
		    nil)
		 (let* ((head-clause (first rest-clauses)))
		   (keim~remprop head-clause 'clause-number)))
	       
	       ;; erzeuge otter.in file in otter*in-string, fuege otter*in-string zum otter-problem hinzu (einschlieslich proof-object)
	       (otter~add-in-string! otter-problem
				     mode
				     (append (list (format nil "%%%% MODE FLAGS %%%%~A" #\Newline))
					     (if proof-object
						 (list "set(build_proof_object).")
					       nil)
					     (cons (format nil "~A%%%% SELECTED BY OTTER IN AUTO MODE %%%%~A" #\Newline #\Newline)
						   otter-flags)
					     (cons (format nil "~A%%%% USER INPUT %%%%~A" #\Newline #\Newline) user-flags)
					     (cons (format nil "~A%%%% USER WEIGHTS %%%%~A" #\Newline #\Newline) user-weights)
					     
					     otter*needed-flags)
				     usable-clauses
				     sos-clauses
				     proof-object)
	       
	       ;; call-otter vot Ort -> schreibt otter.out file in den out-string des otter-problems
	       (otter=call-otter! otter-problem otter-problem-dir ressource :agent agent)
	       
	       ;; parsen des otter-beweis
	       (otter~complete-otter-problem! otter-problem :parse-back parse-back)))))))

(defun otter=call-otter! (otter-problem otter-problem-dir ressource &key (agent nil))
  (declare (edited  "26-MAR-1998")
	   (authors Ameier)
	   (input   "An otter-problem, the directory for input, output and a time-ressource.")
	   (effect  "Writes the in-string of the otter-problem to the file otter.in in the"
		    "directory, calls otter on it, reads the file otter.out from the directory"
		    "and writes it into the out-string of the otter-problem.")
	   (value   "Undefined."))
  
  (let* ((in-file (merge-pathnames "otter.in" otter-problem-dir))
	 (temp-out-file (merge-pathnames "tmp.out" otter-problem-dir))
	 (out-file (merge-pathnames "otter.out" otter-problem-dir)))

    (atptop~print-string-in-file (atpprb~problem-atp-in-string otter-problem) in-file)

    (let* ((call-flag (atptop~call-with-time-ressource (format nil "~A < ~A >! ~A;mv ~A ~A &"
							       (otter~program) in-file temp-out-file
							       temp-out-file out-file)
						       out-file
						       "otter"
						       otter-problem-dir
						       ressource
						       :agent-name agent)))
      ;; der komplizierte Befehl bei atptop~call-with-time-ressource ist notwendig, da otter leider sofort sein out file zu schreiben
      ;; beginnt. Daher wird das eigentliche out-file erst nachdem otter fertig ist geschrieben.
      
      
      (if (null call-flag)
	  (omega~message "~% Otter was not able to find a proof in the given time ressource. ~%")
	
	;; read otter.out file as string ans set atp-out-string of the otter-problem
	(setf (atpprb~problem-atp-out-string otter-problem)
	      (atptop~read-file-as-string out-file))))))

(defun otter~generate-otter-problem-default! (conclusion-node assumption-nodes ho-pds &key (p2pl nil))
  (declare (edited  "26-MAR-1998")
	   (authors Ameier)
	   (input   "A conclusion-node, a set of assumption-nodes and the pds the nodes are from.")
	   (effect  "The created otter-problem is inserted into the list in the plist entry"
		    "'atp-problems of the conclusion node.")
	   (value   "A atp-problem with type otter, a partial resolution proof."
		    "The global-vars are set completely: proof-object -> t"
		    "The atp-in-string is also already set, using the standart auto-mode"
		    "settings without additional information."))
  (let* ((otter-problem (otter~generate-otter-problem conclusion-node assumption-nodes ho-pds :p2pl p2pl)))
    
    (otter~add-in-string! otter-problem
			  'auto
			  (append (list (format nil "%%%% MODE FLAGS %%%%~A" #\Newline)
					"set(auto)."
					"clear(print_proofs).")
				  (list "set(build_proof_object).")
				  (list "clear(print_given).")
				  otter*needed-flags)
			  (res~proof-clauses (atpprb~problem-part-res-proof otter-problem))
			  nil
			  't
			  :docu nil)

    ;; the following line is now already in otter~generate-otter-problem
    ;;(keim~put conclusion-node 'atp-problems (cons otter-problem (keim~get conclusion-node 'atp-problems)))
    
    otter-problem))

#| -------------------------------------------- Stuff for Reading Output free for OTTER --------------------------------------------- |#

(defun otter~read-otter-output (open-node file)
  (declare (edited  "02-JUN-2000")
	   (authors Ameier)
	   (input   "An open node and a file.")
	   (effect  "Mayby changes the otter global variables.")
	   (value   "1. For the open node a new resolution proof is created."
		    "2. The file is tried to read as a otter proof for this resolution proof"
		    "   In particular, a mapping is computed from initial clauses of the otter"
		    "   file and the initial clauses of the new reslution proof."
		    "If it was possible to read the file as a otter proof file, the resolution"
		    "proof is completed (an empty clauses is dreived according to the proof in"
		    "the file) and this complete resolution proof is returned."
		    "If it was not possible to read the file as a otter proof file nil is"
		    "returned."))
  (let* ((otter-problem (otter~generate-otter-problem open-node
						      (remove open-node (pds~node-supports open-node))
						      omega*current-proof-plan))
	 (res-proof (otter~complete-otter-problem-from-file! otter-problem file)))
    
    res-proof))

(defun otter~complete-otter-problem-from-file! (atp-problem file)
  (let* ((out-string (atptop~read-file-as-string file))
	 (res-proof (atpprb~problem-part-res-proof atp-problem))
	 (global-vars (atpprb~problem-global-vars atp-problem)))
    
    ;; was ist mit translation settings? -> braucht man zumindest nicht fuer reine first order probleme

    (setf (atpprb~problem-atp-out-string atp-problem) out-string)
      
    ;; Note: in the atp-problem we have in the global-var-list:
    ;; 1.  otter*convert-list       -> we have to reconstruct by matching the clauses!
    ;; 2.  otter*reflexivity-item   -> we can use!
    
    (setq otter*convert-list (otter=reconstruct-convert-list! res-proof out-string))
    (setq otter*reflexivity-item (second global-vars))
    
    ;; Other necessary settings
    (setq otter*current-problem res-proof)
    (setq otter*equality-object (env~lookup-object '= (res~proof-environment res-proof)))
    (setq otter*current-environment (res~proof-environment res-proof))
    (setq otter*temporary-variables nil)                      ;; setzt temporaeri variablen auf nil
    (setq otter*just-counter 0)                               ;; setzt justification counter fuer neue justification auf 0

    (if (null otter*convert-list)
	(progn
	  (omega~message "~% Could not match the out-file to the problem.")
	  nil)
      (let* ((proof-object (otter=check-proof-object-p out-string))
	     (proof-flag (if proof-object
			     (otter=read-with-proof-object res-proof out-string)
			   (otter=read-without-proof-object res-proof out-string))))
	(if proof-flag
	    (progn
	      (setf (res~proof-empty-clause res-proof)
		    (otter=find-empty-clause (res~proof-step-clauses res-proof)))
	      (when proof-object
	      	;; (otter=correct-paramods! (res~proof-empty-clause res-proof) (node~justification (res~proof-empty-clause res-proof)))
	      	(mapcar #'(lambda (clause)
	      		    (keim~remprop clause 'pospos-list))
	      		(res~proof-clauses res-proof)))
	      (setq omega*current-resolution-proof res-proof)
	      (res~add-proof-in-hash-table res-proof)		  
	      (atptop~order-resolution-steps! res-proof)
	      (omega~message "~% OMEGA*CURRENT-RESOLUTION-PROOF IS SET TO THE FOUND RESOLUTION PROOF ~%")
	      res-proof)
	  nil)))))

(defun otter=check-proof-object-p (out-string)
  (declare (edited  "30-MAY-2000")
	   (authors Ameier)
	   (input   "A string containing the output produced by otter.")
	   (effect  "None.")
	   (value   "If the string contains the line 'Proof object:' (this signs, that there exists a proof object in the"
		    "output) t, otherwise nil."))
  (let* ((line-strings (atptop~divide-string out-string #\Newline)))
    (or (find "Proof object:" line-strings :test #'string=)                    ;; Version 3.0.5
	(find ";; BEGINNING OF PROOF OBJECT" line-strings :test #'string=))))   ;; Version 3.0.6

(defun otter=reconstruct-convert-list! (res-proof out-string)
  (declare (edited  "29-MAY-2000")
	   (authors Ameier)
	   (input   "The current resolution proof and the out-string of a otter call.")
	   (effect  "The content of otter*convert-list and otter*local-clause-vars can be changed."
		    "If the matching results in such a clause matching that some literals have to be permutated,"
		    "the resolution proof is changed. First, in each clause in the input clauses of the resolution proof"
		    "(which are created by clause normalization) we change -according to the found corresponding clause-"
		    "the order of the literals. Second, in the delta-relation we also change the positions of the literals"
		    "in a corresponding manner.")
	   (value   "First the input clauses of the proof in the out-string are read. Thereby, the new"
		    "created constants have as names exactly the names they have in the out-file."
		    "Then these read clauses are matched against the input clauses in the resolution proof."
		    "This results (if successfull) in a mapping, that mapps each constant in the"
		    "read clauses to a constant in the input clauses. From this mapping we compute"
		    "a list of pairs of constants and strings (where the constant is taken from the"
		    "new resolutiomn proof whereas the string is from the out-file."))

  (otter=handle-equality res-proof :insert 't)

  (let* ((res-proof-input-clauses (res~proof-clauses res-proof))
	 (out-file-input-clauses (otter=read-input-clauses out-string)))
    
    (multiple-value-bind
	(success mapping info-triples)
	(atptop~subset-by-equality-except-names-p out-file-input-clauses res-proof-input-clauses)
      
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

(defun otter=read-input-clauses (out-string)
  (declare (edited  "29-MAY-2000")
	   (authors Ameier)
	   (input   "A otter outfile.")
	   (effect  "During the parsing of the input clauses (see value) the otter*convert-list is changed.")
	   (value   "Reads the input clauses, without a convertion setting."
		    "Thereby, for each name that starts with a capital letter a new variable of type i is produced, and"
		    "for each other name a constant of type (i <- ...) is produced."))
  
  (setf otter*convert-list nil)
  
  (let* ((line-strings (atptop~divide-string out-string #\Newline))
	 (lines-of-initial-clauses (do* ((rest-string-lines line-strings (rest rest-string-lines))
					 (break-flag nil)
					 (read-flag nil)
					 (return-list nil))
				       ((or (null rest-string-lines)
					    break-flag)
					return-list)
				     (let* ((string-line (first rest-string-lines)))
				       (cond ((string= string-line "======= end of input processing =======")
					      (setf break-flag 't))
					     ((or (string= string-line "list(sos).")
						  (string= string-line "list(usable)."))
					      (setf read-flag 't))
					     ((string= string-line "end_of_list.")
					      (setf read-flag nil))
					     (read-flag
					      (setf return-list (append return-list (list string-line))))))))
	 
	 ;; Note:
	 ;; If a input clause is parsed, each name that starts with a capital letter is interpreted as variable!
	 ;; -> a new local variable of type i is created (local = relevant only for the clause itself) 
	 ;; Each other letter is interpreted as a constant. Hence, e new constant is created. The type of the constant
	 ;; is (o <- i ...) or (i <- i ...) depending on whether the constant is the predicat of a literal or internal
	 ;; and on the number of the arguments on which it is applied.
	 ;; Since constants are not local for a clause, for each new created constant a new entry is made in the
	 ;; otter*convert-list
	 (input-clauses (apply #'append (mapcar #'(lambda (line)
						    (otter=parse-free-input-clause-line line))
						lines-of-initial-clauses))))
    input-clauses))

(defun otter=parse-free-input-clause-line (input-clause-line)
  (declare (edited  "29-MAY-2000")
	   (authors Ameier)
	   (input   "A string representing a input clause.")
	   (effect  "The variables otter*local-clause-vars and otter*convert-list can be changed"
		    "(otter*local-clause-vars is set to nil at the beginning, then new local variables"
		    " are added. For otter*convert-list see otter=read-input-clauses.")
	   (value   "A list containing the new created clause."))
  
  (setf otter*local-clause-vars nil)

  (multiple-value-bind
      (number-string rest-string)
      (atptop~get-next-word input-clause-line #\space)
    (multiple-value-bind
	(just-string clause-string)
	(atptop~get-next-word rest-string #\space :ignore-char-list '(#\. #\space))
      
      (if (atptop~string-is-prefix-of-string-p "-ADDITIONALNONPROPPRED_" clause-string)
	  nil
	(let* ((literal-list (if (string= "$F." clause-string)
				 nil
			       (do* ((rest-literal-strings (atptop~divide-string clause-string #\|) (rest rest-literal-strings))
				     (literal-list nil))
				   ((null rest-literal-strings)
				    literal-list)
				 (let* ((head-literal (first rest-literal-strings)))
				   
				   (setf literal-list (append literal-list
							      (list (otter=parse-free-literal head-literal)))))))))
	  (list (cl~create literal-list)))))))

(defun otter=parse-free-literal (literal-string)
  (declare (edited  "29-MAY-2000")
	   (authors Ameier)
	   (input   "A string representing a literal.")
	   (effect  "See otter=parse-free-input-clause-line.")
	   (value   "A literal."))
  (if (equal (char literal-string 0) #\-)
      (lit~literal-create (otter=parse-free-term (atptop~cut-first-char literal-string) :predicat 't) nil)
    (let ((lit-atom (otter=parse-free-term literal-string :predicat 't)))
      (if (atptop~equation-p lit-atom)
	  (let* ((literal (lit~literal-create lit-atom (keim~get lit-atom :polarity))))
	    (keim~remprop lit-atom :polarity)
	    literal)
	(lit~literal-create lit-atom 't)))))

(defun otter=parse-free-term (term-string &key (predicat nil))
  (if (> (atptop~number-of-char-in-string #\= term-string) 0)

      ;; If the term is an equation:
      (let* ((args (atptop~divide-string term-string #\=))
	     (first-string (first args)))
	(if (equal #\! (char first-string (- (length first-string) 1))) ;; -> ungleichung
	    (let* ((first-arg (otter=parse-free-term (atptop~cut-last-char first-string)))
		   (second-arg (otter=parse-free-term (second args)))
		   (application (term~appl-create (otter=free-string2object "=") (list first-arg second-arg))))
	      (keim~put application :polarity nil)
	      application)
	  (let* ((first-arg (otter=parse-free-term first-string))
		 (second-arg (otter=parse-free-term (second args)))
		 (application (term~appl-create (otter=free-string2object "=") (list first-arg second-arg))))
	    (keim~put application :polarity t)
	    application)))
    
    ;; If the term isn't an equation:
    (multiple-value-bind
	(functor-string rest-string)
	(atptop~get-next-word term-string #\()
      ;; reads till a "(" is reached 
      ;; if rest-string = "" (empty) -> term isn't an application -> otherwise term is an application
      (if (string= rest-string "")
	  (otter=free-string2object functor-string :predicat predicat :number-of-args 0)
	(let* ((args (mapcar #'(lambda (term-string)
				 (otter=parse-free-term term-string))
			     (otter=parse-term-list (atptop~cut-last-char rest-string))))
	       (functor (otter=free-string2object functor-string :predicat predicat :number-of-args (length args))))
	  (term~appl-create functor args))))))

(defun otter=free-string2object (string &key (predicat nil) (number-of-args 0))
  (declare (edited  "29-MAY-2000")
	   (authors Ameier)
	   (input   "A string, and as keywords predicat (to sign whether the string should be intrpreted as"
		    "predicat or as function) and number-of-args (to sign how many arguments the premitive"
		    "corresponding to the string should have.")
	   (effect  "The otter*convert-list and otter*local-clause-vars can be changed:"
		    "1.) If the string starts with a capital letter and it is not alsready conatined in an entry"
		    "    in the otter*local-clause-vars, a new variable with type i is created and a corresponding"
		    "    entry is made in the otter*local-clause-vars."
		    "2.) Othwewise: if the string is not contained in an entry in the otter*convert-list, a new"
		    "    constant (whose type depends on the keywords predicat and number-of-args) is created"
		    "    and a corresponding entry is added to otter*convert-list.")		    
	   (value   "The object corresponding wrt. otter*convert-list or otter*local-clause-vars to the string."))
  (let ((member-convert-list (first (find string otter*convert-list
					  :test #'(lambda (string pair)
						    (string= string (second pair))))))
	(member-local-clause (first (find string otter*local-clause-vars
					      :test #'(lambda (string pair) (string= string (second pair)))))))

    (cond ((string-equal string "=")
	   (env~lookup-object '= otter*current-environment))

	  (member-convert-list
	   ;; -> string is already in otter*convert-list -> give back the corresponding object
	   
	   member-convert-list)
	  
	  (member-local-clause
	   ;; -> string is already in otter*local-clause-vars -> return it

	   member-local-clause)

	  (;; string neither in otter*convert-list nor otter*local-clause-vars
	   ;; -> create new object and add entry to otter*convert-list or otter*local-clause-vars
	   
	   (if (find (char string 0) otter*capital-letters)
	       ;; -> first letter of string is a capital letter
	       ;; -> produce new variable and add it to otter*local-clause-vars
	       
	       (let* ((new-var (term~generate-term-primitive-with-new-name 'orv- (type~i) 'term+variable otter*current-environment)))
		 
		 (setq otter*temporary-variables (cons new-var otter*temporary-variables))
		 (setq otter*local-clause-vars (cons (list new-var string) otter*local-clause-vars))
		 
		 new-var)

	     ;; first letter of string is not a capital letter
	     ;; -> produce new constant and add it to otter*convert-list
	     
	     (let* ((type (if predicat 
			      (type~predicate-create number-of-args)
			    (type~function-create number-of-args)))
		    (new-constant (term~constant-create string type)))
	       
	       (setq otter*convert-list (cons (list new-constant string) otter*convert-list))
	       
	       new-constant))))))
