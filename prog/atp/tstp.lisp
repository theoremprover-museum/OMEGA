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

(mod~defmod tstp
	    :uses (keim post env pos data subst term type res cl lit delta hocnf node just pds pdsj pdsn prob f2p p2f inter comint arg p2pl mixin otter
			)
	    :documentation "Transforming TSTP proofs"
	    :exports (
		      tstp~parse-resolution-proof
		      )
	    )


(defvar tstp*proof-string nil)

(defvar tstp*local-free-clause-vars nil)

(defvar tstp*step-lines nil)

(defvar tstp*ass-name-counter nil)

(defvar tstp*convert-counter nil)

(defvar tstp*convert-list nil)

(defvar tstp*equality-object nil)

(defvar tstp*name-symbols '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l
			    #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x
			    #\y #\z #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j
			    #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v
			    #\w #\x #\y #\z #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7
			    #\8 #\9 #\_)) ;; the allowed symbols in names

#| ----------------------------------------------------------- The main function ------------------------------------------------ |#

;; NOTE: the function tstp~parse-resolution-proof takes a prover output file in TSTP syntax and parses it as resolution proof!
;; The function assumes that the output file is in tstp*proof-string. Moreover, the function gets as sole input the (open) node
;; into whose proof the resolution proof should be translated. 
;; For the successful application of tstp~parse-resolution-proof it is necessary that the constants and functions in the
;; tstp file have the same names than the constants and functions in the PDS nodes!

(defun tstp~parse-resolution-proof (open-node out-style)
  (let* ((otter-problem (otter~generate-otter-problem open-node
						      (just~premises (node~justification open-node))
						      omega*current-proof-plan))
	 (res-proof (atpprb~problem-part-res-proof otter-problem)))
        
    ;; computes the convertion of names table
    (otter=compute-convert-list-equal (res~proof-clauses res-proof))

    (let* ((skolems-mapping? (tstp=parse-initial-clauses-and-complete-convert-list (res~proof-clauses res-proof))))
      (if (null skolems-mapping?)
	  (progn
	    (omega~message "~%Not able to match the internal skolem functions with the functions in TSTP output!")
	    nil)
	(progn 
	  ;; sets the global-vars in the otter-problem
	  (setf (atpprb~problem-global-vars otter-problem)
		(list otter*convert-list
		      (second (atpprb~problem-global-vars otter-problem))
		      out-style))
	  
	  ;; sets the atp-out-file in the otter-problem
	  (setf (atpprb~problem-atp-out-string otter-problem) tstp*proof-string)
	  
	  ;; parses the tstp proof
	  (tstp~complete-otter-problem! otter-problem :parse-back 't))))))

(defun tstp~complete-otter-problem! (otter-problem &key (parse-back 't))
  (if (null (atpprb~problem-atp-out-string otter-problem))
      nil
    (let* ((tstp-out-string (atpprb~problem-atp-out-string otter-problem))
	   (res-proof (atpprb~problem-part-res-proof otter-problem))
	   (global-vars (atpprb~problem-global-vars otter-problem))
	   (proof-object (third global-vars))
	   (translation-settings (atpprb~problem-translation-settings otter-problem)))
      
      (setq otter*convert-list (first global-vars))
      (setq otter*reflexivity-item (second global-vars))
      
      (setq otter*current-problem res-proof)
      (setq otter*equality-object (env~lookup-object '= (res~proof-environment res-proof)))
      (setq otter*current-environment (res~proof-environment res-proof))

      (setq otter*temporary-variables nil) 
      (setq otter*just-counter 0)      

      (if (equal (first translation-settings) 'p2pl)
	  (progn
	    (setq p2pl*domain (second translation-settings))
	    (setq p2pl*codomain (third translation-settings)))
	(progn
	  (setq p2f*domain (second translation-settings))   
	  (setq p2f*codomain (third translation-settings))))
      
      ;; read otter.out file
      (let* ((proof-flag (if (not parse-back)
			     (omega~error "Not implemented yet: read TSTP proof without parsing!")
			   (progn (omega~message "Parsing TSTP Proof ... ~%")
				  (if proof-object
				      (omega~error "Not implemented yet: read TSTP with otter proof object")
				    (tstp=read-without-proof-object res-proof))))))
	;; output
	(if proof-flag
	    (omega~message "~% Proof in TSTP format was delivered ~%")
	  (omega~message "~% No Proof in TSTP format was delivered ~%"))

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

#| ------------------------------------------------ Read Initials and Complete convert ------------------------------------- |#

(defun tstp=parse-initial-clauses-and-complete-convert-list (initial-proof-clauses)
  (setf tstp*local-free-clause-vars nil)
   
  (let* ((step-lines (tstp=get-step-lines tstp*proof-string))
	 (initial-clauses-strings (tstp=get-initial-clauses-strings step-lines))
	 (read-initial-clauses (mapcar #'tstp=parse-clause-free initial-clauses-strings))
	 (read-initial-clauses-with-free-variables (remove-if-not #'(lambda (clause)
								      (intersection (data~free-variables (cl~literals clause)) 
										    tstp*local-free-clause-vars))
								  read-initial-clauses)))

    (setf tstp*step-lines step-lines)
    
    (tstp=match-clauses-and-complete-convert-list read-initial-clauses-with-free-variables initial-proof-clauses)
    ))

(defun tstp=match-clauses-and-complete-convert-list (read-clauses initial-proof-clauses)
  (let* ((matching-subst (tstp=find-matching-clauses read-clauses initial-proof-clauses (subst~create nil nil))))
    
    (if matching-subst
	(let* ((domain (subst~domain matching-subst))
	       (codomain (subst~codomain matching-subst)))
	  (mapcar #'(lambda (dom codom)
		      (setf otter*convert-list (append otter*convert-list (list (list codom (string (keim~name dom)))))))
		  domain codomain)
	  't)
      nil)))      

(defun tstp=find-matching-clauses (read-clauses proof-clauses subst)
  (if (null read-clauses)
      subst
    (let* ((head-read-clause (first read-clauses)))
      
      (do* ((rest-proof-clauses proof-clauses (rest rest-proof-clauses))
	    (final-subst nil))
	  ((or final-subst
	       (null rest-proof-clauses))
	   (if final-subst
	       final-subst
	     nil))
	(let* ((head-proof-clause (first rest-proof-clauses))
	       (clause-subst (tstp=match-clauses-p (subst~apply subst head-read-clause)
						   (subst~apply subst head-proof-clause))))
	  (when clause-subst
	    (let* ((only-free-variables-subst (tstp=only-free-variables-subst clause-subst)))
	      (when (tstp=accept-subst-p only-free-variables-subst)
		(let* ((rest-matching-clauses-list (tstp=find-matching-clauses (rest read-clauses)
									       proof-clauses 
									       (subst~compose-substitution subst only-free-variables-subst))))
		  (when rest-matching-clauses-list
		    (setf final-subst rest-matching-clauses-list)))))))))))

(defun tstp=accept-subst-p (subst)
  (let* ((codomain (subst~codomain subst)))
    (every #'sksym~p codomain)))	 

(defun tstp=only-free-variables-subst (subst)
  (let* ((domain (subst~domain subst))
	 (codomain (subst~codomain subst)))
    (do* ((rest-domain domain (rest rest-domain))
	  (rest-codomain codomain (rest rest-codomain))
	  (back-domain nil)
	  (back-codomain nil))
	((null rest-domain)
	 (subst~create back-domain back-codomain))
      (let* ((head-domain (first rest-domain)))
	(when (find head-domain tstp*local-free-clause-vars)
	  (setf back-domain (append back-domain (list head-domain)))
	  (setf back-codomain (append back-codomain (list (first rest-codomain)))))))))

(defun tstp=match-clauses-p (clause1 clause2)
  (do* ((rest-literals1 (cl~literals clause1))
	(rest-literals2 (cl~literals clause2))
	(current-subst (subst~create nil nil))
	(flag 't))
      ((or (null rest-literals1)
	   (null flag))
       (if flag
	   current-subst
	 nil))
    (let* ((head-lit1 (first rest-literals1))
	   (matching-lit2 (find head-lit1 rest-literals2 :test #'(lambda (lit1 lit2)
								   (tstp=match-literals-p lit1 lit2 current-subst)))))
      (if matching-lit2
	  (let* ((subst (tstp=match-literals-p head-lit1 matching-lit2 current-subst)))
	    (setf rest-literals1 (subst~apply subst (rest rest-literals1)))
	    (setf rest-literals2 (subst~apply subst (remove matching-lit2 rest-literals2)))
	    (setf current-subst (subst~compose-substitution subst current-subst)))
	(setf flag nil)))))

(defun tstp=match-literals-p (lit1 lit2 current-subst)
  (if (null (equal (lit~polarity lit1) (lit~polarity lit2)))
      nil
    (let* ((atom1 (lit~atom lit1))
	   (atom2 (lit~atom lit2)))
      (term~alpha-match atom1 atom2))))
  
	 
      

      
		 
	





(defun tstp=parse-clause-free (init-clause-string)

  (setf otter*local-clause-vars nil)

  (if (string= "(false)" init-clause-string)
      (values (cl~clause-create nil)
	      free-variables)
    (let* ((literal-strings (atptop~divide-string (atptop~cut-first-char (atptop~cut-last-char init-clause-string))
						  #\|
						  :ignore-char-list '(#\space)))
	   (literals (mapcar #'(lambda (literal-string)
				 (let* ((pol (if (atptop~string-is-prefix-of-string-p "~" literal-string)
						 nil
					       't))
					(atom-string (if (atptop~string-is-prefix-of-string-p "~" literal-string)
							 (atptop~cut-first-char literal-string)
						       literal-string))
					(atom  (tstp=parse-term-free atom-string)))
				   (lit~literal-create atom pol)))
			     literal-strings)))
      (cl~create literals))))

(defun tstp=parse-term-free (term-string)
  (multiple-value-bind
      (functor-string rest-string)
      (atptop~get-next-word term-string #\()
    ;; reads till a "(" is reached 
    ;; if rest-string = "" (empty) -> term isn't an application -> otherwise term is an application
    (if (string= rest-string "")
	(tstp=string2object functor-string 0)
      (let* ((args-strings (otter=parse-term-list (atptop~cut-last-char rest-string)))
	     (functor (if (string= functor-string "equal")
			  (env~lookup-object '= (th~env 'base))
			(tstp=string2object functor-string (length args-strings))))
	     (args (mapcar #'(lambda (termstr)
			       (tstp=parse-term-free termstr))
			   args-strings)))
	(term~appl-create functor args)))))

(defun tstp=string2object (string arity)
  (let ((member-convert-list (first (first (member string otter*convert-list
						   :test #'(lambda (string pair) (string= string (second pair))))))))
    (if member-convert-list
	
	;; -> string is in convert-list
	;; -> reconvert the string to an object + return object
	(cond ((typep member-convert-list 'term+number)
	       member-convert-list)
	      ((string-equal (keim~name member-convert-list) "=")
	       (env~lookup-object '= otter*current-environment))
	      (t
	       member-convert-list))
      
      ;; -> string not in convert-list
      ;; -> look in otter*local-clause-vars
      (let ((member-local-clause (first (first (member string otter*local-clause-vars
						       :test #'(lambda (string pair) (string= string (second pair))))))))
	(if member-local-clause

	    ;; -> string corresponds to new local clause var 
	    ;; -> return it
	    member-local-clause
	  
	  ;; -> string does not correpson to new local clause var
	  ;; -> check in tstp*local-free-clause-vars
	  (let* ((symboli (make-symbol string))
		 (free-var (first (remove-if-not #'(lambda (vari)
						     (string-equal (keim~name vari)
								   symboli))
						 tstp*local-free-clause-vars))))
	    (if free-var
		free-var
	      
	      ;; string not in free-vars
	      ;; -> check whether first letter of string is capital letter
	      ;;    -) if this is the case -> create new local clause var
	      ;;       update the local clause vars list and return the new local clause var
	      ;;    -) if not create new free variable and update the free-variable list 
	      (let* ((capital-chars (list "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M"
					  "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"))
		     (first-letter-string (format nil "~A" (char string 0))))
		(if (find first-letter-string capital-chars :test #'string=)
		    (let* ((needed-type (type~i))
			   (new-var (term~generate-term-primitive-with-new-name 'orv- needed-type 'term+variable otter*current-environment)))
		      (setf otter*temporary-variables (cons new-var otter*temporary-variables))
		      (setf otter*local-clause-vars (cons (list new-var string) otter*local-clause-vars))
		      
		      new-var)
		  (let* ((new-var (term~variable-create (make-symbol string) (tstp=make-i-type arity))))
		    (setf tstp*local-free-clause-vars (cons new-var tstp*local-free-clause-vars))
		    new-var))))))))))

(defun tstp=make-i-type (arity)
  (if (= arity 0)
      (type~i)
    (do* ((counter 0 (+ counter 1))
	  (back-list nil (cons (type~i) back-list)))
	((= counter arity)
	 (type~func-create back-list (type~i))))))

(defun tstp=make-o-type (arity)
  (if (= arity 0)
      (type~o)
    (do* ((counter 0 (+ counter 1))
	  (back-list nil (cons (type~i) back-list)))
	((= counter arity)
	 (type~func-create back-list (type~o))))))


(defun tstp=get-initial-clauses-strings (step-lines)
  (apply #'append (mapcar #'(lambda (step-line)
			      (multiple-value-bind
				  (number kind clause just remark)
				  (tstp=break-step-line step-line)
				(if (string= kind "initial")
				    (list clause)
				  nil)))
			  step-lines)))
			      
  

#| ------------------------------------------------------------------ Reading --------------------------------------------- |# 

(defun tstp=read-without-proof-object (res-proof)
  
  (setf otter*number-clause-list nil)
  (setf otter*local-clause-vars nil)
  (setf otter*rest-initial-clauses (if otter*reflexivity-item
				       (cons otter*reflexivity-item (res~proof-initial-clauses res-proof))
				     (res~proof-initial-clauses res-proof)))
  
  (let* ((step-lines  tstp*step-lines)
	 (check (do* ((rest-lines step-lines (rest rest-lines))
		      (check-flag 't))
		    ((or (null check-flag)
			 (null rest-lines))
		     check-flag)
		  (let* ((line (first rest-lines))
			 (clause (tstp=parse-line line res-proof)))
		    (when (null clause)
		      (setq check-flag nil))))))
    (if check
	't
      (progn
	(omega~message "~% WARNING: TSTP PROOF can not be parsed !! Sorry !!")
	nil))))

(defun tstp=get-step-lines (tstp-out-string)
  (let* ((line-strings (atptop~divide-string tstp-out-string #\Newline))
	 (step-lines (do* ((rest-string-lines line-strings (rest rest-string-lines))
			   (flag 'search)
			   (current-string "")
			   (return-list nil))
			 ((null rest-string-lines)
			  return-list)
		       (let* ((string-line (first rest-string-lines)))
			 (if (equal flag 'search)
			     (when (atptop~string-is-prefix-of-string-p "cnf(" string-line)
			       (setf current-string (atptop~cut-x-first-chars string-line 4))
			       (setf flag 'collecting))
			   (if (atptop~string-is-prefix-of-string-p ".)" (reverse string-line))
			       (progn
				 (setf flag 'search)
				 (setf return-list (append return-list (list (format nil "~A ~A" current-string (atptop~cut-x-last-chars string-line 2))))))
			     (setf current-string (format nil "~A ~A" current-string string-line))))))))
    step-lines))
	 

(defun tstp=break-step-line (step-line-string)
  (let* ((string-parts (do* ((i 0 (+ i 1))
			     (number-of-p 0)
			     (current-string "")
			     (back-list nil))
			   ((>= i (length step-line-string))
			    (append back-list (list current-string)))
			 (let* ((current-char (char step-line-string i)))
			   (cond ((and (equal current-char #\,)
				       (= number-of-p 0))
				  (setf back-list (append back-list (list current-string)))
				  (setf current-string ""))
				 ((equal current-char #\space)
				  nil)
				 (t
				  (setf current-string (format nil "~A~A" current-string current-char))
				  
				  (cond ((or (equal current-char #\[)
					     (equal current-char #\())
					 (setf number-of-p (+ number-of-p 1)))
					((or (equal current-char #\])
					     (equal current-char #\)))
					 (setf number-of-p (- number-of-p 1))))))))))
    (values (first string-parts)
	    (second string-parts)
	    (third string-parts)
	    (fourth string-parts)
	    (fifth string-parts))))

(defun tstp=parse-line (line res-proof)
  (multiple-value-bind
      (number kind clause just remark)
      (tstp=break-step-line line)
    (let* ((numbers (list number))
	   (otter-clause (tstp=parse-clause clause))
	   (new-clause (tstp=use-justification otter-clause
					       (cond ((string= kind "initial")
						      "")
						     ((string= kind "derived")
						      just)
						     (t
						      (omega~error "~%Unexpected kind: ~A" kind)))
					       res-proof)))
      (setf otter*number-clause-list (append (mapcar #'(lambda (number)
							 (list number new-clause))
						     numbers)
					     otter*number-clause-list))
      (otter=add-steps-to-proof! new-clause res-proof)
      new-clause)))

(defun tstp=use-justification (otter-clause justification-string res-proof)
  (if (string= justification-string "")
      
      ;; -> initial clause
      (let* ((clause (find otter-clause otter*rest-initial-clauses  :test #'(lambda (clause1 clause2)
									      (atptop~clauses-equal-till-renaming-and-ordering-p
									       clause1 clause2
									       ;; :flip 't
									       )))))
	clause)
    
    (let* ((derived-clauses (tstp=perform-justifications justification-string))
	   (clause (find otter-clause derived-clauses  :test #'(lambda (clause1 clause2)
								 (atptop~clauses-equal-till-renaming-and-ordering-p
								  clause1 clause2
								  ;; :flip 't
								  )))))
      
      (if (null clause)
	  (omega~error "~%Sorry, but could not produce clause ~A from the specified proof!"  otter-clause)
	clause))))
;; WHAT ABOUT FLIPS?????

(defun tstp=perform-justifications (justification-string)
  (let* ((cut-inference (atptop~cut-x-first-chars (atptop~cut-last-char justification-string) 10)))
    (multiple-value-bind
	(step-string rest-string1)
	(atptop~get-next-word cut-inference #\,)
      (multiple-value-bind
	  (brabl rest-string2)
	  (atptop~get-next-word rest-string1 #\,)
	(let* ((parents-clauses-lists (tstp=get-parents-clauses rest-string2)))

	  (cond ((or (string= step-string "copy")
		     (string= step-string "back_demod"))
		 (first parents-clauses-lists))
		((string= step-string "flip")
		 (append (first parents-clauses-lists) (apply #'append (mapcar #'blik=complete-flipping (first parents-clauses-lists)))))
		((string= step-string "binary")
		 (let* ((parents1 (first parents-clauses-lists))
			(parents2 (second parents-clauses-lists)))
		   (apply #'append (mapcar #'(lambda (parent1)
					       (apply #'append (mapcar #'(lambda (parent2)
									   (res~binary-resolution parent1 parent2))
								       parents2)))
					   parents1))))
		((or (string= step-string "factor")
		     (string= step-string "factor_simp"))
		 (let* ((parents (first parents-clauses-lists)))
		   (apply #'append (mapcar #'(lambda (parent)
					       (res~binary-factoring parent))
					   parents))))
		((or (string= step-string "para_from")
		     (string= step-string "para_into")
		     (string= step-string "demod"))
		 (let* ((parents1 (first parents-clauses-lists))
			(parents2 (second parents-clauses-lists)))
		   (apply #'append (mapcar #'(lambda (parent1)
					       (apply #'append (mapcar #'(lambda (parent2)
									   (res~binary-paramodulation parent1 parent2))
								       parents2)))
					   parents1))))
		((or (string= step-string "hyper")
		     (string= step-string "unit_del"))
		 (tstp=recursive-resolution parents-clauses-lists))
		(t
		 (omega~error "~%Sorry, but don't know how to read the justification step ~A" step-string))))))))

(defun tstp=recursive-resolution (parents-clauses-lists)
  (do* ((rest-parents-clauses-lists parents-clauses-lists (rest rest-parents-clauses-lists))
	(current-clauses nil))
      ((null rest-parents-clauses-lists)
       current-clauses)
    (let* ((head-parents-clauses (first rest-parents-clauses-lists)))
      ;;(format T "~%~%CURRENT-CLAUSES: ~A" current-clauses)
      (if (null current-clauses)
	  (setf current-clauses head-parents-clauses)
	(setf current-clauses (apply #'append (mapcar #'(lambda (parent1)
							  (apply #'append (mapcar #'(lambda (parent2)
										      (res~binary-resolution parent1 parent2))
										  head-parents-clauses)))
						      current-clauses)))))))

;; (defun tstp=parents-tupels (parents-clauses-lists)
;;   (do* ((i 0 (incf i))
;; 	(back-clauses-tupels nil))
;;       ((= i (length parents-clauses-lists))
;;        back-clauses-tupels)
;;     (setf back-clauses-tupels (append back-clauses-tupels
;; 				      (tstp=parents-tupels-rec (cons (nth i parents-clauses-lists)
;; 								     (remove (nth i parents-clauses-lists) parents-clauses-lists)))))))

;; (defun tstp=parents-tupels-rec (parents-clauses-lists)
;;   (if (null parents-clauses-lists)
;;       (list nil)
;;     (let* ((head-parents-clauses (first parents-clauses-lists))
;; 	   (back-tupels (tstp=parents-tupels-rec (rest parents-clauses-lists))))
;;       (apply #'append (mapcar #'(lambda (head-parent)
;; 				  (mapcar #'(lambda (head-tupel)
;; 					      (cons head-parent head-tupel))
;; 					  back-tupels))
;; 			      head-parents-clauses)))))



(defun tstp=get-parents-clauses (parent-string)
  (let* ((cutted (atptop~cut-last-char (atptop~cut-first-char parent-string)))
	 (parents-strings (remove-if #'(lambda (stringi)
					 (atptop~string-is-prefix-of-string-p "theory" stringi))
				     (tstp=cut-parents-string cutted))))
    (mapcar #'(lambda (parent-string)
		(let* ((inference? (atptop~string-is-prefix-of-string-p "inference" parent-string)))
		  (if inference?
		       (tstp=perform-justifications parent-string)
		    (list (tstp=number2clause parent-string)))))
	    parents-strings)))

(defun tstp=number2clause (parent-string) 
  (second (assoc parent-string otter*number-clause-list :test #'string=)))


(defun tstp=cut-parents-string (parents-string)
  (let* ((string-parts (do* ((i 0 (+ i 1))
			     (number-of-p 0)
			     (current-string "")
			     (back-list nil))
			   ((>= i (length parents-string))
			    (append back-list (list current-string)))
			 (let* ((current-char (char parents-string i)))
			   (cond ((and (equal current-char #\,)
				       (= number-of-p 0))
				  (setf back-list (append back-list (list current-string)))
				  (setf current-string ""))
				 ((equal current-char #\space)
				  nil)
				 (t
				  (setf current-string (format nil "~A~A" current-string current-char))
				  
				  (cond ((or (equal current-char #\[)
					     (equal current-char #\())
					 (setf number-of-p (+ number-of-p 1)))
					((or (equal current-char #\])
					     (equal current-char #\)))
					 (setf number-of-p (- number-of-p 1))))))))))
    string-parts))
  

    
(defun tstp=parse-clause (clause-string)  
  (setq otter*local-clause-vars nil)
  (let* ((literal-list (if (string= "(false)" clause-string)
			   nil
			 (do* ((rest-literal-strings (atptop~divide-string (atptop~cut-first-char (atptop~cut-last-char clause-string))
									   #\|
									   :ignore-char-list '(#\. #\space))
						     (rest rest-literal-strings))
			       (literal-list nil))
			     ((null rest-literal-strings) literal-list)
			   (setf literal-list (append literal-list
						      (list (tstp=parse-literal (first rest-literal-strings)))))))))
    (cl~create literal-list)))

(defun tstp=parse-literal (literal-string)
  (if (equal (char literal-string 0) #\~)
      (lit~literal-create (tstp=parse-term (atptop~cut-first-char literal-string)) nil)
    (lit~literal-create (tstp=parse-term literal-string) 't)))

(defun tstp=parse-term (term-string)
  (multiple-value-bind
      (functor-string rest-string)
      (atptop~get-next-word term-string #\()
    ;; reads till a "(" is reached 

    (if (string= rest-string "")
	
	;; -> if rest-string = "" (empty)
	;; -> term isn't an application
	(otter=string2object functor-string)
      
      ;; -> term is an application
      (let* ((functor (if (string= functor-string "equal")
			  (env~lookup-object '= (th~env 'base))
			(otter=string2object functor-string)))
	     (args (mapcar #'(lambda (term-string)
			       (tstp=parse-term term-string))
			   (otter=parse-term-list (atptop~cut-last-char rest-string)))))
	(term~appl-create functor args)))))



#| ------------------------------------------------------------- Initial Clauses 2 Problem --------------------------------------- |#

;; (defun tstp=initialclauses2problem ()

;;   (setf otter*convert-list nil)
;;   (setq otter*current-environment (env~create (th~env 'base)))
;;   (setf tstp*ass-name-counter 0)
  
;;   (let* ((step-lines (tstp=get-step-lines tstp*proof-string))
;; 	 (initial-clauses-strings (tstp=get-initial-clauses-strings step-lines))
;; 	 (initial-clauses (mapcar #'tstp=parse-clause-initial initial-clauses-strings))
;; 	 (formulas-of-clauses (mapcar #'tstp~clause2formula initial-clauses))
;; 	 (assumptions (mapcar #'(lambda (formula)
;; 				  (node~create (make-symbol (format nil "ass~A" (incf tstp*ass-name-counter)))
;; 					       formula
;; 					       (just~create (infer~find-method 'hyp) nil)))
;; 			      formulas-of-clauses))
;; 	 (conclusion (node~create 'theorem (env~lookup-object 'false (th~env 'base)) (just~create (infer~find-method 'open) nil)))
;; 	 (new-env (env~create (th~env 'base)))
;; 	 (new-problem (prob~create 'transproblem
;; 				   (th~find-theory 'base)
;; 				   new-env
;; 				   assumptions
;; 				   conclusion)))

;;     (mapcar #'(lambda (conv-pair)
;; 		(let* ((obj (first conv-pair)))
;; 		  (env~enter (keim~name obj) obj new-env)))
;; 	    otter*convert-list)
    
;;     (setf (gethash (keim::th=read-string 'transproblem) (keim::th=problems (th~find-theory 'base)))
;; 	  (keim::th=env-entry 'transproblem
;; 			      new-problem
;; 			      new-env))

;;     (setq glo*prob new-problem)
    
;;     (oc=prove-pre new-problem)))

(defun tstp~clause2formula (clause)
  (let* ((or-obj (env~lookup-object 'or (th~env 'base)))
	 (not-obj (env~lookup-object 'not (th~env 'base)))
	 (forall-obj (env~lookup-object 'forall (th~env 'base)))
	 (literals (cl~literals clause))
	 (pre-back-formula (do* ((rest-literals literals (rest rest-literals))
				 (back-formula nil))
			       ((null rest-literals)
				back-formula)
			     (let* ((head-lit (first rest-literals))
				    (head-formula (if (lit~positive-p head-lit)
						      (lit~atom head-lit)
						    (term~appl-create not-obj (list (lit~atom head-lit))))))
			       (if (null back-formula)
				   (Setf back-formula head-formula)
				 (setf back-formula (term~appl-create or-obj (list back-formula head-formula)))))))
	 (free-variables (term~free-variables pre-back-formula)))
    (do* ((rest-free-variables free-variables (rest rest-free-variables))
	  (back-formula pre-back-formula))
	((null rest-free-variables)
	 back-formula)
      (setf back-formula (term~appl-create forall-obj (list (term~abstr-create (list (first rest-free-variables)) back-formula)))))))
			  
(defun tstp=parse-clause-initial (init-clause-string)

  (setf otter*local-clause-vars nil)
  
  (if (string= "(false)" init-clause-string)
      (values (cl~clause-create nil)
	      free-variables)
    (let* ((literal-strings (atptop~divide-string (atptop~cut-first-char (atptop~cut-last-char init-clause-string))
						  #\|
						  :ignore-char-list '(#\space)))
	   (literals (mapcar #'(lambda (literal-string)
				 (let* ((pol (if (atptop~string-is-prefix-of-string-p "~" literal-string)
						 nil
					       't))
					(atom-string (if (atptop~string-is-prefix-of-string-p "~" literal-string)
							 (atptop~cut-first-char literal-string)
						       literal-string))
					(atom  (tstp=parse-term-initial atom-string :head 't)))
				   (lit~literal-create atom pol)))
			     literal-strings)))
      (cl~create literals))))

(defun tstp=parse-term-initial (term-string &key (head nil))
  (multiple-value-bind
      (functor-string rest-string)
      (atptop~get-next-word term-string #\()
    ;; reads till a "(" is reached 
    ;; if rest-string = "" (empty) -> term isn't an application -> otherwise term is an application
    (if (string= rest-string "")
	(tstp=string2object-initial functor-string 0 :head head)
      (let* ((args-strings (otter=parse-term-list (atptop~cut-last-char rest-string)))
	     (functor (if (string= functor-string "equal")
			  (env~lookup-object '= (th~env 'base))
			(tstp=string2object-initial functor-string (length args-strings) :head head)))
	     (args (mapcar #'(lambda (termstr)
			       (tstp=parse-term-initial termstr))
			   args-strings)))
	(term~appl-create functor args)))))

(defun tstp=string2object-initial (string arity &key (head nil))
  (let ((member-convert-list (first (first (member string otter*convert-list
						   :test #'(lambda (string pair) (string= string (second pair))))))))
    (if member-convert-list
	
	;; -> string is in convert-list
	;; -> reconvert the string to an object + return object
	(cond ((typep member-convert-list 'term+number)
	       member-convert-list)
	      ((string-equal (keim~name member-convert-list) "=")
	       (env~lookup-object '= otter*current-environment))
	      (t
	       member-convert-list))
      
      ;; -> string not in convert-list
      ;; -> look in otter*local-clause-vars
      (let ((member-local-clause (first (first (member string otter*local-clause-vars
						       :test #'(lambda (string pair) (string= string (second pair))))))))
	(if member-local-clause

	    ;; -> string corresponds to new local clause var 
	    ;; -> return it
	    member-local-clause
	  
	  ;; -> string does not correpsond to new local clause var
	  ;; -> check whether first letter of string is capital letter
	  ;;    -) if this is the case -> create new local clause var
	  ;;       update the local clause vars list and return the new local clause var
	  ;;    -) if not create new constant and update the otter*convert-list
	  (let* ((capital-chars (list "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M"
				      "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"))
		 (first-letter-string (format nil "~A" (char string 0))))
	    (if (find first-letter-string capital-chars :test #'string=)
		(let* ((needed-type (type~i))
		       (new-var (term~generate-term-primitive-with-new-name 'orv- needed-type 'term+variable otter*current-environment)))
		  (setf otter*temporary-variables (cons new-var otter*temporary-variables))
		  (setf otter*local-clause-vars (cons (list new-var string) otter*local-clause-vars))
		  
		  new-var)
	      (let* ((new-const (term~constant-create (make-symbol string)
						      (if (null head)
							  (tstp=make-i-type arity)
							(tstp=make-o-type arity)))))
		(setf otter*convert-list (cons (list new-const string) otter*convert-list))
		new-const))))))))



    
(defun tstp~create-tstp-problem (open-node ho-pds)
  
  (let* ((problem-name (keim~name ho-pds))
         (tstp-problem (tstp~generate-tstp-problem open-node
                                                   (remove open-node (pds~node-supports open-node))
                                                   ho-pds))
	 (res-proof (atpprb~problem-part-res-proof tstp-problem)))
    (setf resproof res-proof)
    res-proof
    ;; erzeugt directory falls es nicht existiert
    ))

(defun tstp~generate-tstp-problem (conclusion-node assumption-nodes ho-pds)

  (setq tstp*convert-counter 0)       ;; setzt counter fuer neue Namen auf 0
     
  (let* ((res-proof (atptop~resolution-proof-create-from-nodes conclusion-node assumption-nodes ho-pds)))
    
    ;; translate the initial resolution proof res-proof to FOL and normalize it
    (p2f~translate res-proof)

    ;; Normalisierung 
    (omega~message "~% Normalizing ...")
    (hocnf~normalize-res-proof! res-proof)

    ;; beachte behandlung der Gleichheit ! jzimmer: equality built-in in TSTP
    (tstp=handle-equality res-proof)

    ;; Remove clauses that contain abstractions
    ;; After p2f translation it can happen that there are clauses that contain lambda-abstraction
    ;; Such clauses can not be handled by OTTER (or any first-order. ATP and are therefore removed from the
    ;; clauses list
    (atptop~remove-clauses-with-abstractions! res-proof)
    
    ;; Compute the convertion of names
;;    (tstp=compute-convert-list (res~proof-clauses res-proof))
    
    (atpprb~create-fo-problem (gensym "tstp-problem-")
			      'tstp
			      nil    ;; tstp-in-file kommt erst spaeter dazu: -> tstp~add-in-string!
			      nil
			      res-proof
			      (list tstp*convert-list)
			      (list 'p2f p2f*domain p2f*codomain))))


;; (defun tstp=compute-convert-list (clauses)
;;   (declare (edited  "21-AUG-1997")
;; 	   (authors Jzimmer)
;; 	   (input   "A list of clauses.")
;; 	   (effect  "All names of Constants and variables of the clauses are converted"
;; 		    "into names, that are usable by tstp."
;; 		    "A list of these objects and their corresponding new names (strings)"
;; 		    "is stored in tstp*convert-list.")
;; 	   (value   "Undifined."))
;;   (setq tstp*convert-list nil)
;;   (mapcar #'tstp=convert-object clauses))

;; (defgeneric tstp=convert-object (object)
;;   (declare (edited  "21-AUG-1997")
;; 	   (authors Jzimmer)
;; 	   (input   "An object.")
;; 	   (effect  "The tstp*convert-list is updated. by converting the parts of this"
;; 		    "object.")
;; 	   (value   "Undifined."))
;;   (:method ((object cl+clause))
;; 	   (mapcar #'tstp=convert-object (cl~literals object)))
;;   (:method ((object lit+literal))
;; 	   (tstp=convert-object (lit~atom object)))
;;   (:method ((object term+appl))
;; 	   (mapcar #'tstp=convert-object
;; 		   (cons (data~appl-function object)
;; 			 (data~appl-arguments object))))
;;   (:method ((object term+primitive))
;; 	   (tstp=convert-name object)))

;; (defun tstp=convert-name (object)
;;   (declare (edited  "14-MAY-1996")
;; 	   (authors Jzimmer)
;; 	   (input   "An object, that can be of type term+variable, term+constant or term+number.")
;; 	   (effect  "If a new name-string is produced, a pair of old-name-string"
;; 		    "and new-name-string is added to the tstp*convert-list.")
;; 	   (value   "From the name is a tstp-compatible name produced."
;; 		    "That means all symbols till alphabetics,numbers and _ are"
;; 		    "deleted from the name and a counter-number is added."
;; 		    "If var is set the resulting string is upcased, otherwise"
;; 		    "it is downcased. If the name was attached before the"
;; 		    "before produced new-string is taken from the tstp*convert-list"
;; 		    "and is returned otherwise a new string is produced in the way"
;; 		    "descibed before."))
;;   (if (and (or (keim~equal object tstp*equality-object)
;; 	       (and (term~constant-p object)
;; 		    (null (typep object 'term+number))
;; 		    (string= (string (keim~name object)) "=")))
;; 	   (null (find "=" tstp*convert-list :test #'(lambda (str pair)
;; 						       (string= str (second pair))))))
      
;;       ;;(setq tstp*convert-list (cons (list tstp*equality-object "=") tstp*convert-list))

;;       (setq tstp*convert-list (cons (list (if (term~schema-p tstp*equality-object)
;; 					      (data~schema-range tstp*equality-object)
;; 					    tstp*equality-object)
;; 					  "=")
;; 				    tstp*convert-list))
    
;;     (let* ((name (keim~name object))
;; 	   (name-string (if (stringp name)
;; 			    name
;; 			  (format nil "~A" name)))
;; 	   ;; Compute whether the element is already in the tstp*convert-list
;; 	   ;;(partner-string (second (first (member object tstp*convert-list
;; 	   ;;					  :test #'(lambda (thing pair)
;; 	   ;;						    (let* ((thing2 (first pair)))
;; 	   ;;						      (if (term~schema-p thing2)
;; 	   ;;							  (data~equal-p thing (data~schema-range thing2))
;; 	   ;;							(keim~equal thing thing2)))))))))
;; 	   (partner-string (second (first (member object tstp*convert-list
;; 						  :test #'(lambda (thing pair)
;; 	   						    (let* ((thing2 (first pair)))
;; 	   						      (or (eq thing thing2)
;; 								  (and (data~equal thing thing2)
;; 								       (data~equal (term~type thing) (term~type thing2)))))))))))
      
;;       ;; If element already in the tstp*convert-list give back the ob-string already used in the tstp*convert-list
;;       (if partner-string
;; 	  partner-string
;; 	;; If not a new string is computed and is together with the input object, or, if existing its representation in the
;; 	;; environment, added to the tstp*convert-list (list object string).
;; 	;; If polymorphie is used, it is necessary to use the object from the environment.
;; 	;; For example set with Type (aa -> o), but in the application (set a) with a of type i, set is of type (i -> o)
;; 	;; If we would save in the tstp*convert-list this set, we couldn't create a term (set 1) with 1 of type num.
;; 	(let ((new-string ""))
;; 	  (do ((i 0 (+ i 1)))
;; 	      ((>= i (length name-string)) nil)
;; 	    (when (member (char name-string i) tstp*name-symbols)
;; 	      (setq new-string (format nil "~A~A" new-string (char name-string i)))))
;; 	  (setq new-string (format nil "ob_~A_~A" new-string (incf tstp*convert-counter)))
;; 	  (let ((checked-new-string (if (term~variable-p object)
;; 					(string-upcase new-string)
;; 				      (string-downcase new-string)))
;; 		(env-element (env~lookup-object (intern (string-upcase name-string) (find-package :omega))
;; 						(res~proof-environment tstp*current-problem))))
	    
;; 	    (setq tstp*convert-list (cons (list object
;; 						;;(if (and env-element (not (typep object 'term+number)))
;; 						;;     env-element
;; 						;;   object)
;; 						checked-new-string) tstp*convert-list))
;; 	    checked-new-string)))))) 


(defun tstp=handle-equality (res-proof)
  (declare (edited  "26-AUG-1998")
	   (authors Ameier)
	   (input   "A resolution proof.")
	   (effect  "tstp*equality-object is set to the equality in the environment.")
	   (value   "undefined."))

  (setq tstp*equality-object (env~lookup-object '= (res~proof-environment res-proof))))

