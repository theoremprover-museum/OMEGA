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


(mod~defmod EQP 
            :uses (atpprb atptop cl data delta env hocnf just keim lit node omega otter p2f pos res sys term)
            :documentation "Handles the use of EQP."
            :exports (
                      
                      eqp~add-in-string!
                      eqp~call-eqp
                      eqp~complete-eqp-problem!
		      eqp~generate-eqp-problem
                      eqp~generate-eqp-problem-default!
                      eqp~program
		      eqp~read-eqp-output
		      eqp~complete-eqp-problem-from-file!
		      ))

;;; The following functions are internal in other modules and should not be used:
;;; (otter=add-string-to-in-string otter=compute-convert-list otter=find-empty-clause otter=handle-equality otter=number2clause otter=parse-literal otter=print-sos-clauses otter=set-current-type-var-subst!)

(defun eqp~program ()
  (let* ((prog (sys~getenv "EQPHOME")))

    (when (or (null prog) (null (probe-file prog)))
      (error "There is no EQP-executable at ~A, please check your path to the EQP-executable." prog))
    
    prog))

#| -------------------------------------------------------- READ -> EQP.OUT ------------------------------------------------------- |#


(defun eqp=read (res-proof eqp-out-string parse-back)
  (declare (edited  "03-SEP-1998")
	   (authors Ameier)
	   (input   "A partial resolution proof, the eqp-out-string and a boolean to sign, whether the found proof"
		    "(if one is found) should be parsed or not.")
	   (effect  "If a found proof is parsed the resolution proof is changed.")
	   (value   "Undefined."))
  
  (omega~message "~% PARSING EQP OUTPUT ... ~%")
  
  (let* ((line-strings (atptop~divide-string eqp-out-string #\Newline))
	 (proof-flag (eqp=read-direct line-strings)))
    
    (when (and proof-flag parse-back)
      (eqp=read-proof line-strings res-proof)
      )
    
    proof-flag))

(defun eqp=read-direct (strings)
  (declare (edited  "03-SEP-1998")
	   (authors Ameier)
	   (input   "The eqp out-file as a list of strings.")
	   (effect  "None.")
	   (value   "T if a proof is found, that means if the line:"
		    "'---------------- PROOF ----------------' is contained, nil otherwise."))
  (find "---------------- PROOF ----------------" strings :test 'string=)
  )

(defun eqp=read-proof (line-strings res-proof)
  (declare (edited  "03-SEP-1998")
	   (authors Ameier)
	   (input   "A list of strings representing the output of EQP.")
	   (effect  "The resolution proof res-proof is updated by new readed steps.")
	   (value   "Undefined."))
  
  (setq otter*rest-initial-clauses (res~proof-initial-clauses res-proof))
  (setq otter*number-clause-list nil)

  (multiple-value-bind
	(proof-flag file-lines end-step-line)
	(do* ((rest-string-lines (reverse line-strings) (rest rest-string-lines))
	      (return-list nil)
	      (proof-part-flag 'start))
	    ((or (null rest-string-lines) (equal proof-part-flag 'end))
	     (values proof-part-flag (reverse return-list) (second rest-string-lines)))
	  (let* ((string-line (first rest-string-lines)))
	    (if (equal proof-part-flag 'proof)
		(if (string= string-line "---------------- PROOF ----------------")
		    (setq proof-part-flag 'end)
		  (if (not (string= string-line ""))
		      (setq return-list (append return-list (list string-line)))))
	      (if (string= string-line "------------ end of proof -------------")
		  (setq proof-part-flag 'proof)))))

    ;;(format t "~%LINES: ~A" file-lines)
    ;;(format t "~%END-STEP: ~A" end-step-line)
    ;;(error "hihi")

    (eqp=set-factors-of-initials! res-proof)
    
    (if (equal proof-flag 'end) ;; proof is da !!

	(let* ((clauses (mapcar #'(lambda (line)
				    (eqp=parse-line line res-proof))
				file-lines))
	       (last-clause (first (last clauses))))
	  (eqp=make-end-step end-step-line res-proof))
      

      (error "~% In function eqp=read-proof proof not found !"))))

(defun eqp=set-factors-of-initials! (res-proof)
  (declare (edited  "04-SEP-1998")
	   (authors Ameier)
	   (input   "The resolution proof.")
	   (effect  "Computes to every clause in the resolution proof the factored-clause and"
		    "sets the global var eqp*factors-of-initials to them.")
	   (value   "Undefined."))

  ;; diese bloede Sache wird in eqp=use-justifications benoetigt, da leider bei flip justifications die number nicht stimmt,
  ;; wenn flip auf initial clauses angewandt wird -> schmeisse einfach alle geflippten initials dazu !
  (let* ((initial-clauses (res~proof-clauses res-proof))
	 (flip-clauses (mapcar #'eqp=get-flip-clause initial-clauses)))
    (setq eqp*factors-of-initials flip-clauses)))

(defun eqp=make-end-step (end-step-line res-proof)
  (declare (edited  "03-SEP-1998")
	   (authors Ameier)
	   (input   "A string describing the end-step of the proof and the resolution proof.")
	   (effect  "Adds the end-step to the resolution proof.")
	   (value   "Undefined."))
  (let* ((string-list (atptop~divide-string end-step-line #\space)))
    
    (when (null (and (string= (first string-list) "UNIT")
		     (string= (second string-list) "CONFLICT")
		     (string= (third string-list) "from")))
      (error "~% In function eqp=make-end-step: New kind of end-step."))

    (let* ((numstr-1 (fourth string-list))
	   (numstr-2 (sixth string-list))
	   (num1 (atptop~parse-number numstr-1))
	   (num2 (atptop~parse-number numstr-2))
	   (parent1 (if num1
			(otter=number2clause num1)
		      otter*reflexivity-item))
	   (parent2 (if num2
			(otter=number2clause num2)
		      otter*reflexivity-item))
	   (resolvents (res~binary-resolution parent1 parent2))
	   (empty-clause (otter=find-empty-clause resolvents)))
      
      (when (null empty-clause)
	(error "~% SOmething wrong in function eqp=find-end-step, no empty clause found."))
      
      (eqp=add-steps-to-proof! empty-clause res-proof)
      
      empty-clause)))

  
(defun eqp=parse-line (line-string res-proof)
  (declare (edited  "03-SEP-1998")
	   (authors Ameier)
	   (input   "A string, representing a line in the EQP-OUTfile.")
	   (effect  "The resolution proof res-proof is changed by new steps." )
	   (value   "The clause produced by this line."))
  (multiple-value-bind
      (numbers justifications-string clause-string)
      (eqp=divide-line-string line-string)
    (let* ((eqp-clause (eqp=parse-clause clause-string))
	   (new-clause (eqp=use-justifications justifications-string eqp-clause res-proof)))
      
      (setq otter*number-clause-list (append (mapcar #'(lambda (number)
							 (list number new-clause))
						     numbers)
					     otter*number-clause-list))
      
      (eqp=add-steps-to-proof! new-clause res-proof)
      
      (when (null new-clause)
	(omega~message "~%Something wrong in Function eqp=parse-line -> return-clause is nil."))

      new-clause)))

(defun eqp=add-steps-to-proof! (clause res-proof)
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
		  (eqp=add-steps-to-proof! parent-clause res-proof))
	      (res~justification-parents (node~justification clause))))))


(defun eqp=use-justifications (just-string eqp-clause res-proof)
  (declare (edited  "03-SEP-1998")
	   (authors Ameier)
	   (input   "The justs-string, the eqp-clause and the resolution proof.")
	   (effect  "A clause is build from the just-string with our justifications, that is"
		    "equal to the eqp-clause modulo renaming and ordering and flip steps.")
	   (value   "The new-clause."))
  (cond ((string= just-string "")
	 ;; -> initial clause
	 (let* ((clause (find eqp-clause otter*rest-initial-clauses  :test #'(lambda (clause1 clause2)
								       (atptop~clauses-equal-till-renaming-and-ordering-p
									clause1 clause2)))))

	   (setq otter*rest-initial-clauses (remove clause otter*rest-initial-clauses))

	   clause))
	(t	 
	 (do* ((rest-string just-string)
	       (first-just 't)
	       (current-clause-list nil))
	     ((string= rest-string "")
	      (find eqp-clause current-clause-list :test #'(lambda (clause1 clause2)
							     (atptop~clauses-equal-till-renaming-and-ordering-p
							      clause1 clause2))))
	   (multiple-value-bind
	       (next-just rest-rest-string)
	       (atptop~get-next-word rest-string #\()
	     (cond ((string= next-just "flip")
		    ;; -> flipping
		    (if first-just

			;; -> interpretiere number als number of clause !
			;; -> funktioniert leider nicht immer ! -> zweiter Ansatz: Schau in den factorisierten initials
			;; eqp*factors-of-initials nach !!!!!
			;; Leider wird bei flip justifications die number nicht korrekt angegeben,
			;; wenn flip auf initial clauses angewandt wird -> schmeisse einfach alle geflippten initials dazu !
			
			(multiple-value-bind
			    (number-string others-string)
			    (atptop~get-next-word rest-rest-string #\))
			  
			  (setq rest-string (atptop~cut-first-char others-string))
			  (setq first-just nil)
			  
			  (let* ((number (atptop~parse-number number-string))
				 (parent (otter=number2clause number))
				 (flip-clause (if parent
						  (list (eqp=get-flip-clause parent))
						nil)))
			    
			    (setq current-clause-list (append flip-clause
							      eqp*factors-of-initials))))
		      
		      ;; -> nimm vorherige clauses, ignoriere number
		      (multiple-value-bind
			  (fubbes-string others-string)
			  (atptop~get-next-word rest-rest-string #\,)

			(setq rest-string (atptop~cut-first-char others-string))

			(setq current-clause-list (mapcar #'eqp=get-flip-clause current-clause-list))))
		    
		    )
		   ((string= next-just "para")
		    (multiple-value-bind
			(numbers-string others-string)
			(atptop~get-next-word rest-rest-string #\))

		      (setq rest-string (atptop~cut-first-char others-string))

		      (let* ((number-strings (atptop~divide-string numbers-string #\,))
			     (numbers (mapcar #'atptop~parse-number number-strings))
			     (parent1 (otter=number2clause (first numbers)))
			     (parent2 (otter=number2clause (second numbers)))
			     (new-clauses (res~binary-paramodulation parent1 parent2)))

			(setq first-just nil)
			
			(setq current-clause-list new-clauses) ;; -> para immer erste angewandte just -> direckt setzen!
			
			)))
		   ((string= next-just "demod")
		    ;; ->demodulation
		    (let* ((cut-parenthesis (atptop~cut-first-char rest-rest-string)))
		      (multiple-value-bind
			  (numbers-string others-string)
			  (atptop~get-next-word cut-parenthesis #\])
			(let* ((number-strings (atptop~divide-string numbers-string #\,))
			       (numbers (mapcar #'atptop~parse-number number-strings))
			       (demodulators (mapcar #'otter=number2clause numbers))
			       (new-clauses (eqp=handle-demodulators current-clause-list demodulators)))

			  (setq rest-string (atptop~cut-first-char
					     (atptop~cut-first-char others-string)))
			  
			  (setq current-clause-list new-clauses)
			  
			  )))
		    
		    )
		   ((string= next-just "back_demod")
		    ;; weitere demodulation
		    
		    (multiple-value-bind
			(number-string others-string)
			(atptop~get-next-word rest-rest-string #\))
		      
		      (setq rest-string (atptop~cut-first-char others-string))

		      (let* ((number (atptop~parse-number number-string))
			     (parent (otter=number2clause number)))
			
			(setq first-just nil)
			
			(setq current-clause-list (list parent)) ;; -> back-demod immer erste angewandte just -> direckt setzen!
			
			)))
		   
		    
		   (t
		    (omega~message "~% Found new just ~A in function eqp=use-justifications"
				   next-just))))))))


(defun eqp=get-flip-clause (clause)
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
      (error "~% In function eqp=get-flip-clause, clause has more as one literals."))
    
    (cl~create (list flipped-lit)
	       :justification (res~flip-create clause (pos~list-position '(0))))))
		

(defun eqp=handle-demodulators (current-clause-list demodulators)
  (do* ((rest-demods demodulators (rest rest-demods))
	(current-clauses current-clause-list))
      ((null rest-demods)
       current-clauses)
    (let* ((next-demod (first rest-demods))
	   (all-paramods (apply 'append (mapcar #'(lambda (clause)
						    (res~binary-demodulation clause next-demod))
						current-clauses))))
      
      (setq current-clauses all-paramods))))

		   
(defun eqp=divide-line-string (line-string)
  (declare (edited  "03-SEP-1998")
	   (authors Ameier)
	   (input   "A line-string.")
	   (effect  "None.")
	   (value   "Multiple-value:"
		    "First: A list of numbers."
		    "Second: A list of justification strings."
		    "Third: The clause string."))
  (multiple-value-bind
      (number-string rest-stringi)
      (atptop~get-next-word line-string #\space)
    (multiple-value-bind
	(weight-string rest-stringii)
	(atptop~get-next-word rest-stringi #\space)
      (multiple-value-bind
	  (just-string clause-string)
	  (atptop~get-next-word rest-stringii #\space)
	(let* ((numbers (mapcar #'atptop~parse-number (atptop~divide-string number-string #\,))))
	  (values numbers
		  (atptop~cut-first-char
		   (atptop~cut-last-char just-string))
		  (atptop~cut-last-char clause-string)))))))


(defun eqp=parse-clause (clause-string)
  (declare (edited  "03-SEP-1998")
	   (authors Ameier)
	   (input   "A string, representing a clause in the eqp.out file.")
	   (effect  "The otter*local-clause-vars is changed.")
	   (value   "The corresponding clause object."))

  (setq otter*local-clause-vars nil)
  (let* ((literal-list (do* ((rest-literal-strings (atptop~divide-string clause-string #\|
									 :ignore-char-list '(#\. #\space))
						   (rest rest-literal-strings))
			     (literal-list nil))
			   ((null rest-literal-strings) literal-list)
			 (setq literal-list (append literal-list
						    (list (eqp=parse-literal (first rest-literal-strings))))))))
    (cl~create literal-list)))

(defun eqp=parse-literal (literal-string)
  (let* ((literal (if (equal (char literal-string 0) #\-)
		      (lit~literal-create (otter=parse-term (atptop~cut-first-char
							     (atptop~cut-first-char
							      (atptop~cut-last-char literal-string)))) nil)
		    (lit~literal-create (otter=parse-term literal-string) 't))))
    (keim~remprop (lit~atom literal) :polarity)
    
    literal))
#| -------------------------------------------------------- PRINT -> EQP.IN ------------------------------------------------------- |#

(defun eqp=print (clauses command-string)
  (declare (edited  "02-SEP-1998")
	   (authors Ameier)
	   (input   "The list of clauses and the command-string.")
	   (effect  "Writes the eqp.in file as string in otter*in-string.")
	   (value   "The string."))
  
  (setq otter*in-string "")

  (otter=add-string-to-in-string command-string)

  (otter=add-string-to-in-string #\Newline)

  (otter=add-string-to-in-string "end_of_commands.")

  (otter=add-string-to-in-string #\Newline)
  
  (otter=print-sos-clauses clauses)

  otter*in-string)

#| ---------------------------------------------------- CALL OTTER MAIN ----------------------------------------------------------- |#  

(defun eqp~generate-eqp-problem (conclusion-node assumption-nodes ho-pds)
  
  (declare (edited  "26-MAR-1998")
	   (authors Ameier)
	   (input   "A conclusion-node, a set of assumption-nodes and the pds the nodes are from.")
	   (effect  "None.")
	   (value   "A atp-problem with type eqp, a partial resolution proof and partial settet"
		    "global vars."
		    "Remark: atp-in-string is NOT-SET, and global vars are not set completly !!"
		    "        As input clauses only such clauses of the CNF are allowed that contain"
		    "        only equality-literals."
		    ))
  
  (setq otter*convert-counter 0)       ;; setzt counter fuer neue Namen auf 0
  
  (let* ((res-proof (atptop~resolution-proof-create-from-nodes conclusion-node assumption-nodes ho-pds)))

    (setq otter*current-problem res-proof)
    (setq otter*current-environment (res~proof-environment res-proof))
    (setq otter*equality-object (env~lookup-object '= otter*current-environment))

    ;; Entfaellt ab Version 3.3
    ;; (otter=set-current-type-var-subst!)   ;; setzt variable otter*current-type-var-subst
   				       
    ;; translate the initial resolution proof res-proof to f.o. and normalize it
    (p2f~translate res-proof)
    (omega~message "~% Normalizing ...")
    (hocnf~normalize-res-proof! res-proof)

    ;; removes all clauses that are not pure unit-equality-clauses
    (atptop~filter-resolution-proof res-proof) 

    (when (null (res~proof-clauses res-proof))
      (omega~message "~%REMARK: This problem seems not to be convinient for EQP, because it contains NO unit-equation clauses!"))

    ;; Remove clauses that contain abstractions
    ;; After p2f translation it can happen that there are clauses that contain lambda-abstraction
    ;; Such clauses can not be handled by OTTER (or any f.o. ATP and are therefore removed from the
    ;; clauses list
    (atptop~remove-clauses-with-abstractions! res-proof)
        
    ;; Compute the convertion of names
    (otter=compute-convert-list (res~proof-clauses res-proof))
    
    (atpprb~create-fo-problem (gensym "eqp-problem-")
			      'eqp
			      nil      ;; eqp-in-file kommt erst spaeter dazu: -> eqp~add-in-string! 
			      nil
			      res-proof
			      (list otter*convert-list)
			      (list 'p2f p2f*domain p2f*codomain))
    
    ;; in die atpprb~problem-translation-settings kommt eine Liste mit:
    ;; 'p2f p2f*domain p2f*codomain 
    
    ;; in the global-var-list ist folgende Ordnung:
    ;; 1.  otter*convert-list
        
    ;; nicht in der global-var-liste sind andere Dingens, siehe otter !
    ))

(defun eqp~complete-eqp-problem! (eqp-problem &key (parse-back 't))
  (declare (edited  "25-MAR-1998")
	   (authors Ameier)
	   (input   "A eqp problem and as keyword parse-back, a flag whether the may found proof"
		    "should be parsed back from the string.")
	   (effect  "The atp-out-string is read and then the resolution proof is parsed"
		    "from this string to complete the partial resolution proof.")
	   (value   "If there was a atp-out-string that represents a resolution proof (if eqp has found a"
		    "proof) the complete resolution proof if flag parse-back was T or T if parse-bacl was"
		    "nil. If there is no atp-out-string (eqp has failed to find a proof) nil."))
  
  (if (null (atpprb~problem-atp-out-string eqp-problem))
      nil
    (let* ((eqp-out-string (atpprb~problem-atp-out-string eqp-problem))
	   (res-proof (atpprb~problem-part-res-proof eqp-problem))
	   (global-vars (atpprb~problem-global-vars eqp-problem))
	   (translation-settings (atpprb~problem-translation-settings eqp-problem)))

      (setq otter*convert-list (first global-vars))
            
      (setq otter*current-problem res-proof)
      (setq otter*equality-object (env~lookup-object '= (res~proof-environment res-proof)))
      (setq otter*current-environment (res~proof-environment res-proof))

      ;; Entfaellt ab Version 3.3
      ;; (otter=set-current-type-var-subst!)   ;; setzt otter*current-type-var-subst

      (setq otter*temporary-variables nil) ;; setzt temporaeri variablen auf nil
      (setq otter*just-counter 0)      ;; setzt justification counter fuer neue justification auf 0
      
      (setq p2f*domain (second translation-settings))            ;; stellt Uebersetzungsinformation pl2p wieder her
      (setq p2f*codomain (third translation-settings))

      ;; -> beim end-step eventuell reflex-item benoetigt 
      (otter=handle-equality res-proof :insert nil)
      
      ;; read otter.out file
      (let* ((proof-flag (eqp=read res-proof eqp-out-string parse-back)))
	
	;; output
	(if proof-flag
	    (omega~message "~% EQP HAS FOUND A PROOF ~%")
	  (omega~message "~% EQP HAS FAILED TO FIND A PROOF ~%"))

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
		  (setq omega*current-resolution-proof res-proof)
		  (res~add-proof-in-hash-table res-proof)		  
		  (atptop~order-resolution-steps! res-proof)
		  (omega~message "~% OMEGA*CURRENT-RESOLUTION-PROOF IS SET TO THE FOUND RESOLUTION PROOF")
		  res-proof)
	      't)
	  nil))))) 

(defun eqp~add-in-string! (eqp-problem clauses command-string)
  (declare (edited  "26-MAR-1998")
	   (authors Ameier)
	   (input   "An eqp-problem, the clauses and the commadn-string.")
	   (effect  "The otter*in-string is produced and added to the eqp-problem.")
	   (value   "Undefined."))

  (let* ((res-proof (atpprb~problem-part-res-proof eqp-problem))
	 )
    
    ;; konstruiert das eqp.in file im otter*in-string
    (eqp=print clauses
	       command-string)
    
    ;; setzt atp-in-string im eqp-problem
    (setf (atpprb~problem-atp-in-string eqp-problem) otter*in-string)
    
    ))

(defun eqp~call-eqp (open-node ho-pds dir ressource command-string parse-back)
  (declare (edited  "21-AUG-1997")
	   (authors Ameier)
	   (input   "An open node and the according pds, that contains this node. A directory in that"
		    "the new files should stand, the time-ressource, the command-string for settings for eqp"
		    "and whether the given proof should be parsed-back or not.")
	   (effect  "None.")
	   (value   "If EQP has found a proof t or the according resolution proof (if parse-back was t)"
		    "nil otherwise"))

  (let* ((problem-name (keim~name ho-pds))
	 (eqp-problem-dir (merge-pathnames (format nil "~A/" (string-downcase problem-name)) dir))
	 (in-file (merge-pathnames "eqp.in" eqp-problem-dir))
	 (out-file (merge-pathnames "eqp.out" eqp-problem-dir))
	 (eqp-problem (eqp~generate-eqp-problem open-node
						(remove open-node (pds~node-supports open-node))
						ho-pds))
	 (res-proof (atpprb~problem-part-res-proof eqp-problem)))
    
    ;; erzeugt directory falls es nicht existiert
    (unless (probe-file eqp-problem-dir)
      (sys~call-system (format nil "mkdir ~A" (string-right-trim '(#\/) (namestring eqp-problem-dir)))))
    
    ;; erzeuge eqp.in file in otter*in-string, fuege otter*in-string zum otter-problem hinzu (einschlieslich proof-object)
    (eqp~add-in-string! eqp-problem
			(res~proof-clauses res-proof)
			"set(prolog_style_variables).
set(para_pairs).
set(basic_paramod).
clear(print_given).
clear(print_back_demod).
clear(print_kept).
clear(print_new_demod).
set(demod_history).
")
    
    
    ;; call-eqp vor Ort -> schreibt eqp.out file in den out-string des otter-problems
    (eqp=call-eqp! eqp-problem eqp-problem-dir ressource)
    
    ;; parsen des eqp-beweises
    (eqp~complete-eqp-problem! eqp-problem :parse-back parse-back)
    
    ))
;; parsen des eqp-beweises
;;(eqp~complete-eqp-problem! eqp-problem :parse-back parse-back)))

    
(defun eqp=call-eqp! (eqp-problem eqp-problem-dir ressource)
  (declare (edited  "26-MAR-1998")
	   (authors Ameier)
	   (input   "An eqp-problem, the directory for input, output and a time-ressource.")
	   (effect  "Writes the in-string of the eqp-problem to the file eqp.in in the"
		    "directory, calls eqp on it, reads the file eqp.out from the directory"
		    "and writes it into the out-string of the eqp-problem.")
	   (value   "Undefined."))
  
  (let* ((in-file (merge-pathnames "eqp.in" eqp-problem-dir))
	 (temp-out-file (merge-pathnames "tmp.out" eqp-problem-dir))
	 (out-file (merge-pathnames "eqp.out" eqp-problem-dir)))

    (atptop~print-string-in-file (atpprb~problem-atp-in-string eqp-problem) in-file)

    (let* ((call-flag (atptop~call-with-time-ressource (format nil "~A < ~A >! ~A;mv ~A ~A &"
							       (eqp~program) in-file temp-out-file
							       temp-out-file out-file)
						       out-file
						       "EQP"
						       eqp-problem-dir
						       ressource)))      
      
      (if (null call-flag)
	  (omega~message "~% EQP was not able to find a proof in the given time resource. ~%")
	
	;; read otter.out file as string ans set atp-out-string of the otter-problem
	(setf (atpprb~problem-atp-out-string eqp-problem)
	      (atptop~read-file-as-string out-file))))))

(defun eqp~generate-eqp-problem-default! (conclusion-node assumption-nodes ho-pds)
  (declare (edited  "26-MAR-1998")
	   (authors Ameier)
	   (input   "A conclusion-node, a set of assumption-nodes and the pds the nodes are from.")
	   (effect  "The created eqp-problem is inserted into the list in the plist entry"
		    "'atp-problems of the conclusion node.")
	   (value   "A atp-problem with type eqp, a partial resolution proof."
		    "The global-vars are set completely: proof-object -> t"
		    "The atp-in-string is also already set, using the standart auto-mode"
		    "settings without additional information."))
  (let* ((eqp-problem (eqp~generate-eqp-problem conclusion-node assumption-nodes ho-pds)))
    
    (eqp~add-in-string! eqp-problem
			(res~proof-clauses (atpprb~problem-part-res-proof eqp-problem))
			"set(prolog_style_variables).
set(para_pairs).
set(basic_paramod).
clear(print_given).
clear(print_back_demod).
clear(print_kept).
clear(print_new_demod).
set(demod_history).
")
    
    (keim~put conclusion-node 'atp-problems (cons eqp-problem (keim~get conclusion-node 'atp-problems)))
    
    eqp-problem))


#| --------------------------------------------------- Anmerkung -------------------------------------------------------------------- |#

#|

Es ist momentan nicht moeglich die Flags commutative und assoc-comm zu bneutzen, da dann die AC-Eigenschaft direkt in die Unification eingeht. Und wir haben momentan leider keine AC-Unification !

Betrachte dazu folgendes Beispiel:

(problem test-com
	 (in base)
	 (constants (+ (i i i))
		    (zero i))

	 (assumption com
		     (forall (lam (x i)
				  (forall (lam (y i)
					       (= (+ x y)
						  (+ y x)))))))

	 (assumption left-neut
		     (forall (lam (x i)
				  (= (+ zero x)
				     x))))

	 (conclusion right-neut
		     (forall (lam (x i)
				  (= (+ x zero)
				     x)))))



EQP-mit COMMUTATIVITAETS0-KLUASEL:

UNIT CONFLICT from 4 and 1 at   0.00 seconds.

---------------- PROOF ----------------

1 (wt=5) [] -(ob__1(ob_sk_0_x_66071_2,ob_zero_3) = ob_sk_0_x_66071_2).
2 (wt=7) [] ob__1(A,B) = ob__1(B,A).
3 (wt=5) [] ob__1(ob_zero_3,A) = A.
4 (wt=5) [para(3,2),flip(1)] ob__1(A,ob_zero_3) = A.

------------ end of proof -------------


Ohne Klausel:

UNIT CONFLICT from 2 and 1 at   0.00 seconds.

---------------- PROOF ----------------

1 (wt=5) [] -(ob__1(ob_sk_0_x_66071_2,ob_zero_3) = ob_sk_0_x_66071_2).
2 (wt=5) [] ob__1(ob_zero_3,A) = A.

------------ end of proof -------------


|#

#| --------------------------------------------------- READ FREE OUTPUT STUFF FOR EQP ----------------------------------------------- |#

(defun eqp~read-eqp-output (open-node file)
  (declare (edited  "02-JUN-2000")
	   (authors Ameier)
	   (input   "An open node and a file.")
	   (effect  "Mayby changes the eqp global variables.")
	   (value   "1. For the open node a new resolution proof is created."
		    "2. The file is tried to read as a eqp proof for this resolution proof"
		    "   In particular, a mapping is computed from initial clauses of the eqp"
		    "   file and the initial clauses of the new reslution proof."
		    "If it was possible to read the file as a eqp proof file, the resolution"
		    "proof is completed (an empty clauses is dreived according to the proof in"
		    "the file) and this complete resolution proof is returned."
		    "If it was not possible to read the file as a eqp proof file nil is"
		    "returned."))
  (let* ((eqp-problem (eqp~generate-eqp-problem open-node
						(remove open-node (pds~node-supports open-node))
						omega*current-proof-plan))
	 (res-proof (eqp~complete-eqp-problem-from-file! eqp-problem file)))
    
    res-proof))

(defun eqp~complete-eqp-problem-from-file! (atp-problem file)
  (let* ((out-string (atptop~read-file-as-string file))
	 (res-proof (atpprb~problem-part-res-proof atp-problem))
	 (global-vars (atpprb~problem-global-vars atp-problem)))

    ;; was ist mit translation settings? -> braucht man zumindest nicht fuer reine first order probleme

    ;; Note: in the atp-problem we have in the global-var-list:
    ;; 1.  otter*convert-list       -> we have to reconstruct by matching the clauses!
    ;; 2.  otter*reflexivity-item   -> we can use!
    
    (setq otter*convert-list (eqp=reconstruct-convert-list res-proof out-string))
    (setq otter*reflexivity-item (second global-vars))
    
    ;; Other necessary settings
    (setq otter*current-problem res-proof)
    (setq otter*equality-object (env~lookup-object '= (res~proof-environment res-proof)))
    (setq otter*current-environment (res~proof-environment res-proof))
    (setq otter*temporary-variables nil)                      ;; setzt temporaeri variablen auf nil
    (setq otter*just-counter 0)                               ;; setzt justification counter fuer neue justification auf 0
    
    ;; -> beim end-step eventuell reflex-item benoetigt 
    (otter=handle-equality res-proof :insert 't)

    (if (null otter*convert-list)
	(progn
	  (omega~message "~% Could not match the out-file to the problem.")
	  nil)
      ;; -> read the proof from the file
      (let* ((proof-flag (eqp=read res-proof out-string 't)))
	
	;; output
	(if proof-flag
	    (omega~message "~% EQP HAS FOUND A PROOF ~%")
	  (omega~message "~% EQP HAS FAILED TO FIND A PROOF ~%"))
	
	;; remove temporary-variables
	(mapcar #'(lambda (var)
		    (env~remove (keim~name var) otter*current-environment))
		otter*temporary-variables)
	
	;; values
	(if proof-flag
	    (progn
	      (setf (res~proof-empty-clause res-proof)
		    (otter=find-empty-clause (res~proof-step-clauses res-proof)))
	      (setq omega*current-resolution-proof res-proof)
	      (res~add-proof-in-hash-table res-proof)		  
	      (atptop~order-resolution-steps! res-proof)
	      (omega~message "~% OMEGA*CURRENT-RESOLUTION-PROOF IS SET TO THE FOUND RESOLUTION PROOF ~%")
	      res-proof)
	  nil)))))

;; NOTICE: IN CONTRAST TO THE FULL F.O. PROVERS LIKE OTTER,BLIKSEM,...
;; WALDMEISTER AND EQP HANDLE ONLY PURE EQUALITY CLAUSES! CONSISTING ONLY OF ONE EQUATION!
;; HEMCE, FOR WALDMEISTER AND EQP WE DON'T HAVE THE PROBLEM THAT IN THE OUTPUT THE ORDER OF THE
;; LITERALS IS MAYBY CHANGED! SO, WE DO NOT NEED TO REORDER THE LITERALS!
(defun eqp=reconstruct-convert-list (res-proof out-string)
  (declare (edited  "29-MAY-2000")
	   (authors Ameier)
	   (input   "The current resolution proof and the out-string of a eqp call.")
	   (effect  "The content of otter*convert-list and otter*local-clause-vars can be changed.")
	   (value   "First the input clauses of the proof in the out-string are read. Thereby, the new"
		    "created constants have as names exactly the names they have in the out-file."
		    "Then these read clauses are matched against the input clauses in the resolution proof."
		    "This results (if successfull) in a mapping, that mapps each constant in the"
		    "read clauses to a constant in the input clauses. From this mapping we compute"
		    "a list of pairs of constants and strings (where the constant is taken from the"
		    "new resolutiomn proof whereas the string is from the out-file."))

  (let* ((res-proof-input-clauses (res~proof-clauses res-proof))
	 (out-file-input-clauses (eqp=read-input-clauses out-string)))
    
    (multiple-value-bind
	(success mapping clauses-pairs)
	(atptop~subset-by-equality-except-names-p out-file-input-clauses res-proof-input-clauses)

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


(defun eqp=read-input-clauses (out-string)
  (declare (edited  "29-MAY-2000")
	   (authors Ameier)
	   (input   "A eqp outfile.")
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
				       (cond ((string= string-line "Starting to process input.")
					      (setf break-flag 't))
					     ((or (string= string-line "Usable:")
						  (string= string-line "Sos:")
						  (string= string-line "Demodulators:")
						  (string= string-line "Passive:"))
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
						    (eqp=parse-free-input-clause-line line))
						lines-of-initial-clauses))))
    input-clauses))


(defun eqp=parse-free-input-clause-line (input-clause-line)
  (declare (edited  "29-MAY-2000")
	   (authors Ameier)
	   (input   "A string representing a input clause.")
	   (effect  "The variables otter*local-clause-vars and otter*convert-list can be changed"
		    "(otter*local-clause-vars is set to nil at the beginning, then new local variables"
		    " are added. For otter*convert-list see otter=read-input-clauses.")
	   (value   "A list containing the new created clause."))
  
  (setf otter*local-clause-vars nil)

  (multiple-value-bind
      (number-string rest-string1)
      (atptop~get-next-word input-clause-line #\space)
    (multiple-value-bind
	(wt-string rest-string2)
	(atptop~get-next-word rest-string1 #\space)
      (multiple-value-bind
	  (just-string literal-string)
	  (atptop~get-next-word rest-string2 #\space :ignore-char-list '(#\. #\space))
	(list (cl~create (list (eqp=parse-free-literal literal-string))))))))

(defun eqp=parse-free-literal (literal-string)
  (declare (edited  "29-MAY-2000")
	   (authors Ameier)
	   (input   "A string representing a literal.")
	   (effect  "See eqp=parse-free-input-clause-line.")
	   (value   "A literal."))
  (if (equal (char literal-string 0) #\-)
      (lit~literal-create (otter=parse-free-term (atptop~cut-last-char
						  (atptop~cut-first-char
						   (atptop~cut-first-char literal-string)))
						 :predicat 't)
			  nil)
    (lit~literal-create (otter=parse-free-term literal-string)
			't)))
