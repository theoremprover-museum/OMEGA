;;; -*- syntax: common-lisp; package: keim; base: 10; mode: keim -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     OMEGA Project                                                        ;;
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

(in-package :omega)



(defvar class*current-alstruct-entries nil)
(defvar class*first-isoclass nil)
(defvar class*unknown-problems nil)
(defvar class*counter 0)
(defvar class*verbose nil)

#| -------------------------------
(mod~defmod CLASS 
            :uses (agenda comint env infer inter just keim node omega pds post prob rcl sod sys term th)
            :documentation "Functions for the classifcation of residueclass sets"
            :exports (class+algebraic-structure
                      class+algebraic-structure
                      class+iso-run
                      class+isoclass
                      class+isomorphie-class-entry
                      class+isomorphie-class-entry-reason
                      
                      class~add-alstruct
                      class~algebraic-structure
                      class~alstruct-operation
                      class~alstruct-set
                      class~ass-new-structure-to-isoclass-exploration
                      class~compare-all-isos
                      class~compare-non-isos
                      class~create-algebraic-structure
                      class~create-iso-run
                      class~display-isoclass-results
                      class~explore-alstructures
                      class~iso-run-backtrack-steps
                      class~iso-run-p
                      class~iso-run-problem
                      class~iso-run-proof
                      class~iso-run-structure1
                      class~iso-run-structure2
                      class~isoclass-create
                      class~isoclass-entries
                      class~isoclass-entry-create
                      class~isoclass-entry-isoclass
                      class~isoclass-entry-p
                      class~isoclass-entry-reason
                      class~isoclass-entry-reason-create
                      class~isoclass-entry-reason-kind
                      class~isoclass-entry-reason-p
                      class~isoclass-entry-reason-pds-list
                      class~isoclass-entry-structure
                      class~isoclass-initial
                      class~isoclass-p
                      class~isoclass-predecesor
                      class~isoclass-successor
                      class~read-alstructs-from-file
                      class~reset-alstruct-entries
                      class~set-verbose!
                      class~show-alstruct-entries
                      
                      class*counter
                      class*current-alstruct-entries
                      class*current-alstructs1
                      class*current-alstructs2
                      class*current-iso-runs
                      class*first-isoclass
                      class*hint
                      class*msolve-check-line
                      class*unknown-problems
                      class*verbose))

;;; The following functions are internal in other modules and should not be used:
;;; (crihelp=convert-operation crihelp=decompose-cartproduct-sets-and-ops crihelp=number-set-to-resclass-set oc=prove oc=prove-pre pds=label-counter sod=system-work zmztac=class-factor)

---------- Struct-ENTRIES ---------------------------------------------- |#

(eval-when (load compile eval)
  (defclass class+algebraic-structure (keim+object)
    ((set :initarg :set
	  :initform nil
	  :accessor class~alstruct-set)
     (operation :initarg :operation
		:initform nil
		:accessor class~alstruct-operation))))

(defun class~algebraic-structure (obj)
  (typep obj 'class+algebraic-structure))

(defun class~create-algebraic-structure (set op)
  (make-instance 'class+algebraic-structure
		 :set set
		 :operation op))

(defmethod print-object ((alstruct class+algebraic-structure) stream)
  (format stream "<ALSTRUCTURE: set ~A, op: ~A>"
	  (class~alstruct-set alstruct)
	  (class~alstruct-operation alstruct)))

#| -------------------------------------------- Al-Structs lesen etc. -------------------------------------- |#

(defmacro class~add-alstruct (set op var)
  `(setf ,var (append ,var (list (class~create-algebraic-structure ,set ,op)))))

(defmacro class~reset-alstruct-entries (var)
  `(setf ,var nil))

(defun class~show-alstruct-entries (var)
  (mapcar #'(lambda (alstruct)
	      (omega~message "~%~A" alstruct))
	  var))

(defmacro class~read-alstructs-from-file (fname var)
  `(sys~handler-case
    (with-open-file (in ,fname :direction :input
			:if-does-not-exist :error)
		    (let ((flist (read in))
			  (env (th~env 'zmz)))
		      (setf ,var nil)
		      (mapcar #'(lambda (struct)
				  (let* ((pre-set (first struct))
					 (pre-op (second struct))
					 (set (post~read-object pre-set env :existing-term))
					 (op (post~read-object pre-op env :existing-term)))
				    (class~add-alstruct set op ,var)))
			      flist)))
    (file-error (c) (inter~print-error (comint~interface comint*current-comint) c))))

;;(com~defcommand add-algebraic-structure
;;		(argnames set operation)
;;		(argtypes term term)
;;		(arghelps "Set?"
;;			  "Operation?")
;;		(frag-cats planning)
;;		(function class=add-alstruct-current-alstructs)
;;		(log-p T)
;;		(help ""))

(defun class=add-alstruct-current-alstructs (set op)
  (class~add-alstruct set op class*current-alstruct-entries))

;;(com~defcommand reset-alstruct-entries
;;		(argnames )
;;		(argtypes )
;;		(arghelps )
;;		(frag-cats planning)
;;		(function class=reset-alstruct-entries-current-alstructs)
;;		(log-p T)
;;		(help ""))

(defun class=reset-alstruct-entries-current-alstructs ()
  (class~reset-alstruct-entries class*current-alstruct-entries))

;;(com~defcommand show-alstruct-entries
;;		(argnames )
;;		(argtypes )
;;		(arghelps )
;;		(frag-cats planning)
;;		(function class=show-alstruct-entries-current-alstructs)
;;		(log-p T)
;;		(help ""))

(defun class=show-alstruct-entries-current-alstructs ()
  (class~show-alstruct-entries class*current-alstruct-entries))

;;(com~defcommand read-alstructures-from-file
;;		(argnames file-name)
;;		(argtypes existing-post-file)
;;		(arghelps "File?")
;;		(frag-cats  omega-basic file-io)
;;		(function class=read-alstructs-from-file-current-alstructs)
;;		(log-p T)
;;		(help ""))

(defun class=read-alstructs-from-file-current-alstructs (fname)
  (class~read-alstructs-from-file fname class*current-alstruct-entries))

#| ----------------------------------------------------- Isomorphie-classes -------------------------------------------- |#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Reasons

(eval-when (load compile eval)
  (defclass class+isomorphie-class-entry-reason (keim+object)
    ((kind :initarg :kind
	   :initform nil
	   :accessor class~isoclass-entry-reason-kind)
     (pds-list :initarg :pds-list
	       :initform nil
	       :accessor class~isoclass-entry-reason-pds-list))))

(defun class~isoclass-entry-reason-p (obj)
  (typep obj 'class+isomorphie-class-entry-reason))

(defun class~isoclass-entry-reason-create (kind pds-list)
  (make-instance 'class+isomorphie-class-entry-reason
		 :kind kind
		 :pds-list pds-list))

(defmethod print-object ((reason class+isomorphie-class-entry-reason) stream)
  (format stream "<REASON: Kind ~A>"
	  (class~isoclass-entry-reason-kind reason)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Entry

(eval-when (load compile eval)
  (defclass class+isomorphie-class-entry (keim+object)
    ((isoclass :initarg :isoclass
	       :initform nil
	       :accessor class~isoclass-entry-isoclass)
     (structure :initarg :structure
		:initform nil
		:accessor class~isoclass-entry-structure)
     (reason :initarg :reason
	     :initform nil
	     :accessor class~isoclass-entry-reason))))

(defun class~isoclass-entry-p (obj)
  (typep obj 'class+isomorphie-class-entry)) 

(defun class~isoclass-entry-create (structure reason &key (isoclass nil))
  (make-instance 'class+isomorphie-class-entry
		 :structure structure
		 :reason reason
		 :isoclass isoclass))

(defmethod print-object ((entry class+isomorphie-class-entry) stream)
  (format stream "<ENTRY: ~A>"
	  (class~isoclass-entry-structure entry)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Isoclass

(eval-when (load compile eval)
  (defclass class+isoclass (keim+object)
    ((entries :initarg :entries
	      :initform nil
	      :accessor class~isoclass-entries)
     (initial :initarg :initial
	      :initform nil
	      :accessor class~isoclass-initial)
     (predecessor :initarg :predecessor
		  :initform nil
		  :accessor class~isoclass-predecesor)
     (successor :initarg :successor
		:initform nil
		:accessor class~isoclass-successor))))


(defun class~isoclass-p (obj)
  (typep obj 'class+isoclass))

(defun class~isoclass-create (initial predecessor)
  (make-instance 'class+isoclass
		 :initial initial
		 :entries (list initial)
		 :predecessor predecessor))

(defmethod print-object ((isoclass class+isoclass) stream)
  (format stream "~%<ISOCLASS with ~A entries>"
	  (length (class~isoclass-entries isoclass))))


#| -------------------------------------------------------- EXPLORATION ------------------------------------------------ |#

;; GEGEBEN:
;;     -) EINE MENGE VON STRUCTUREN (in class*current-alstruct-entries)
;;     -) Die erste Isomorphieclasse (in class*first-isoclass (am anfang nil))
;;
;; Fuer jede neue Struktur S wird folgendes gemacht:
;;        Gehe die Reihe der bestehenden Isomorphisclassen ICLs durch
;;        Fuer jede ICL versuche entweder zu beweisen, dass S isomorph ist zu einer Struktur S' in ICL oder nicht
;;        (dazu picken wir uns einfach die erste Struktur aus ICL raus, und testen was rcl~check-isomorphism meint,
;;         wenn rcl~check-isomorphism meint JA, dann versuchen wir den Isomorphismus zu beweisen, andernfalls
;;         den nicht isomorphismus)
;;        -> wenn ein Isomorphiebeweis gelingt, dann wir S in die entsprechenede ICL eingetragen (mit der reason 'proof
;;           + der PDS)
;;        -> Finden wir keinen Isomorphiebweis fuer alle bestehenden ICLs, dann wird eine neue ICL in die Reihe
;;           der ICLs eingetragen mit S als initial Eintrag. S bekommt dann als Reason 'initial + alle PDSsen die
;;           beweisen, dass S nicht isomorph zu einer der Strukturen vorher.
;;
;;

(defun class~explore-alstructures (test-only &optional (list-of-structures class*current-alstruct-entries))
  (let* ((first-entry-reason (class~isoclass-entry-reason-create 'initial nil))
	 (first-entry (class~isoclass-entry-create (first list-of-structures) first-entry-reason))
	 (first-isoclass (class~isoclass-create first-entry nil)))

    (when class*verbose
      (omega~message "~%Initialising the firts Isoclass with Strcuture: ~A" (first list-of-structures)))
    
    (setf (class~isoclass-entry-isoclass first-entry) first-isoclass)
    (setf class*first-isoclass first-isoclass)
    (setf class*unknown-problems nil)
    
    (mapcar #'(lambda (structure)
		(class=process-structures structure :test-only test-only))
	    (rest list-of-structures))))

(defun class=process-structures (structure &key (test-only nil))
  
  (when class*verbose
    (omega~message "~%Processing structure ~A ..." structure))
  
  (do* ((current-isoclass class*first-isoclass (class~isoclass-successor current-isoclass))
	(not-iso-proofs nil)
	(flag nil))
      ((or flag
	   (null current-isoclass))
       (when (null flag)
	 (when class*verbose
	   (omega~message "~%Creating new ISOCLASS to structure!"))
	 (class=add-new-isoclass! structure not-iso-proofs))
       ;; if flag is true we have already added the structure to the respectively isoclass -> nothing more to do
       )
    (let* ((current-initial (class~isoclass-initial current-isoclass)))
      
      (when class*verbose
	(omega~message "~%     Checking against structure ~A" (class~isoclass-entry-structure current-initial)))
      
      (multiple-value-bind
	  (success proof backtrack-steps)
	  (class=test-isomorphie structure (class~isoclass-entry-structure current-initial) :test-only test-only)
	
	(cond ((string-equal success 'unknown)
	       ;; we were (why ever) neither able to proof the isomorphism theorem nor the non-isomorphism theorem
	       ;; -> store such problems in an additional list:
	       (when class*verbose
		 (omega~message "~%     -> New entry in unknown List!"))
	       (setf class*unknown-problems (append class*unknown-problems (list (list structure current-isoclass)))))
	      ((null success)
	       ;; we could prove the non-isomorphism theorem
	       ;; -> add the proof to the non-isomorphism proofs
	       (when class*verbose
		 (omega~message "~%     -> New entry in non-isomorphism List!"))
	       (setf not-iso-proofs (append not-iso-proofs (list proof))))
	      (t
	       ;; we have found a isoclass such that our structre is isomorphic to one the structures in this class
	       ;; -> add it to this class
	       (when class*verbose
		 (omega~message "~%ADDED TO ISOCLASS!"))
	       (class=add-structure-to-isoclass! structure current-isoclass proof)
	       (setf flag 't)))))))

(defun class=add-structure-to-isoclass! (structure isoclass proof)
  (let* ((reason (class~isoclass-entry-reason-create 'proof (list proof)))
	 (entry (class~isoclass-entry-create structure reason :isoclass isoclass)))
    (setf (class~isoclass-entries isoclass)
	  (append (class~isoclass-entries isoclass) (list entry)))
    entry))

(defun class=add-new-isoclass! (structure not-iso-proofs)
  (let* ((last-isoclass (class=get-last-isoclass class*first-isoclass))
	 (new-reason (class~isoclass-entry-reason-create 'initial not-iso-proofs))
	 (new-entry (class~isoclass-entry-create structure new-reason))
	 (new-isoclass (class~isoclass-create new-entry last-isoclass)))
    
    (setf (class~isoclass-successor last-isoclass) new-isoclass)
    (setf (class~isoclass-entry-isoclass new-entry) new-isoclass)
    new-isoclass))

(defun class=get-last-isoclass (isoclass)
  (if (class~isoclass-successor isoclass)
      (class=get-last-isoclass (class~isoclass-successor isoclass))
    isoclass))



(defun class=test-isomorphie (struct1 struct2 &key (start-strategy nil) (test-only nil))
  (let* ((set1 (class~alstruct-set struct1))
	 (op1 (class~alstruct-operation struct1))
	 (set2 (class~alstruct-set struct2))
	 (op2 (class~alstruct-operation struct2))
	 (table1 (class=create-mult-table set1 op1))
	 (table2 (class=create-mult-table set2 op2))
	 (check-success (rcl~check-isomorphism table1 table2)))

    (when class*verbose
      (omega~message "~%        -> Hint system suggests ~A" check-success)
      (if check-success
	  (omega~message "~%        -> Primary Check: Isomorphic")
	(omega~message "~%        -> Primary Check: Not Isomorphic")))
    
    (if test-only

	(values check-success nil 'test)
      
      (if check-success
	  (multiple-value-bind
	      (test-success proof backtrack-steps)
	      (class=prove-isomorphism set1 op1 set2 op2 :pol 't :start-strategy start-strategy)
	    (if test-success
		(progn
		  (when class*verbose
		    (omega~message "~%             Succeed!"))
		  (values 't proof backtrack-steps))
	      ;; mayby the hint was wrong -> control test try to prove the non-isomorphism
	      (progn
		(when class*verbose
		  (omega~message "~%             Failed!")
		  (omega~message "~%        -> Secondary Check: Not Isomorphic"))
		(multiple-value-bind
		    (test-success2 proof2 backtrack-steps2)
		    (class=prove-isomorphism set1 op1 set2 op2 :pol nil :start-strategy start-strategy)
		  (if test-success2
		      (progn
			(when class*verbose
			  (omega~message "~%             Succeed!"))
			(omega~message "~%WARNING: WRONG HINT FOR THE STRUCTURES: ~A AND ~A" struct1 struct2)
			(values nil proof2 backtrack-steps2))
		    (progn
		      (when class*verbose
			(omega~message "~%             Failed AGAIN!")
			(omega~message "~%             RESULT: UNKNOWN"))
		      (values 'unknown nil)))))))
	(multiple-value-bind
	    (test-success proof backtrack-steps)
	    (class=prove-isomorphism set1 op1 set2 op2 :pol nil :start-strategy start-strategy)
	  (if test-success
	      (progn
		(when class*verbose
		  (omega~message "~%             Succeed!"))
		(values nil proof backtrack-steps))
	    ;; mayby the hint was wrong -> control test try to prove the isomorphism
	    (progn
	      (when class*verbose
		(omega~message "~%             Failed!")
		(omega~message "~%        -> Secondary Check: Isomorphic"))
	      (multiple-value-bind
		  (test-success2 proof2 backtrack-steps2)
		  (class=prove-isomorphism set1 op1 set2 op2 :pol 't :start-strategy start-strategy)
		(if test-success2
		    (progn
		      (when class*verbose
			(omega~message "~%             Succeed!"))
		      (format t "~%WARNING: WRONG HINT FOR THE STRUCTURES: ~A AND ~A" struct1 struct2)
		      (values 't proof2 backtrack-steps2))
		  (progn
		    (when class*verbose
		      (omega~message "~%             Failed AGAIN!")
		      (omega~message "~%             RESULT: UNKNOWN"))
		    (values 'unknown nil)))))))))))

(defun class=prove-isomorphism (set1 op1 set2 op2 &key (pol 't) (start-strategy nil) (ressource 20000))
  (let* ((env (th~env 'zmz))
	 (conclusion-formula (if pol
				 (term~appl-create (env~lookup-object 'isomorphic env)
						   (list set1 op1 set2 op2))
			       (term~appl-create (env~lookup-object 'not env)
						 (list (term~appl-create (env~lookup-object 'isomorphic env)
									 (list set1 op1 set2 op2))))))
	 (conclusion (node~create 'conc
				  conclusion-formula
				  (just~create (infer~find-method 'open) nil)))
	 (prob-env (env~create (th~env 'zmz)))
	 (prob-name (make-symbol (if pol
				     (format nil "ISOTEST-~A" (incf class*counter))
				   (format nil "NON-ISOTEST-~A" (incf class*counter)))))				   
	 (problem (prob~create prob-name (th~find-theory 'zmz) prob-env nil conclusion)))

    (oc=prove-pre problem)

    ;; Calling Multi
    (sod=system-work sod*current-strategies sod*current-strategic-control-rules nil
		     ressource
		     :start-strategy start-strategy)
    
    (let* ((remaining-goal-tasks (remove-if-not #'agenda~goal-or-goal-schema-task-p
						(agenda~all-tasks (pds~agenda omega*current-proof-plan)))))
      (if (null remaining-goal-tasks)
	  (values 't omega*current-proof-plan sod*backtrack-counter)
	(values nil omega*current-proof-plan sod*backtrack-counter)))))

(defun class=create-mult-table (set op)
  (multiple-value-bind
      (sets ops)
      (crihelp=decompose-cartproduct-sets-and-ops set op)
    (let* ((class-factors (mapcar #'zmztac=class-factor sets))
	   (class-factor-numbers (mapcar #'keim~name class-factors))
	   (number-lists (mapcar #'crihelp=number-set-to-resclass-set sets))
	   (operations-on-nums (mapcar #'(lambda (op class-factor)
					   (crihelp=convert-operation op class-factor nil))
				       ops class-factors))
	   (table (if (= (length sets) 1)
		      (rcl~multiplication-table (first number-lists)
						(first class-factor-numbers)
						:operation (first operations-on-nums))
		    (rcl~product-multiplication-table number-lists
						      :modulo class-factor-numbers
						      :operation operations-on-nums))))
      table)))
	   
#| ------------------------------------------------- COMMANDS + DISPLAY ------------------------------------------------ |#

;;(com~defcommand set-isoclass-explore-verbose
;;		(argnames flag)
;;		(argtypes boolean)
;;		(arghelps "Verbose?")
;;		(defaults ('t))
;;		(frag-cats planning)
;;		(function class~set-verbose!)
;;		(log-p T)
;;		(help ""))

(defun class~set-verbose! (flag)
  (setf class*verbose flag))



;;(com~defcommand start-isoclass-explore
;;		(argnames test-only)
;;		(argtypes boolean)
;;		(arghelps "Only Testing?")
;;		(defaults ('t))
;;		(frag-cats planning)
;;		(function class~explore-alstructures)
;;		(log-p T)
;;		(help ""))
;;
;;(com~defcommand display-isoclass-explore-results
;;		(argnames )
;;		(argtypes )
;;		(arghelps )
;;		(defaults )
;;		(frag-cats planning)
;;		(function class~display-isoclass-results)
;;		(log-p T)
;;		(help ""))


(defun class~display-isoclass-results ()
  (omega~message "~%~%FOUND THE FOLLOWING ISOMORPHY CLASSES:")
  (do* ((current-isoclass class*first-isoclass (class~isoclass-successor current-isoclass))
	(counter 1 (incf counter)))
      ((null current-isoclass)
       nil)
    (let* ((initial (class~isoclass-initial current-isoclass))
	   (entries (class~isoclass-entries current-isoclass)))
      (omega~message "~%ISOCLASS ~A:" counter)
      (omega~message "  Initial Structure: ~A" initial)
      (omega~message "  Further Strcutures:")
      (mapcar #'(lambda (structure)
		  (omega~message "    ~A" structure))
	      (remove initial entries))))
  (omega~message "~%~%FAILED TO INSERT THE FOLLOWING STRUCTURES:")
  (mapcar #'(lambda (structure)
	      (omega~message "    ~A" structure))
	  (mapcar #'first class*unknown-problems)))


;;(com~defcommand write-isoclass-exploration-into-file
;;		(argnames outfile supersede?)
;;		(argtypes pathname boolean)
;;		(arghelps "Name of file?"
;;			  "Supersede?")
;;		(frag-cats omega-basic file-io)
;;		(function class=write-expo-in-file)
;;		(log-p T)
;;		(help ""))

(defun class=write-expo-in-file (path supersede?)
  (sys~handler-case
   (with-open-file (out path :direction :output 
			:if-exists (if supersede? :supersede :append)
			:if-does-not-exist :create)
		   (class=output-expo-into-stream out) 
		   (omega~message "Wrote file ~A." (truename out)))
   (file-error (c) (omega~error c)))
  )

(defun class=output-expo-into-stream (stream)
  (format stream "~%~%FOUND THE FOLLOWING ISOMORPHY CLASSES:")
  (do* ((current-isoclass class*first-isoclass (class~isoclass-successor current-isoclass))
	(counter 1 (incf counter)))
      ((null current-isoclass)
       nil)
    (let* ((initial (class~isoclass-initial current-isoclass))
	   (initial-structure (class~isoclass-entry-structure initial))
	   (entries (class~isoclass-entries current-isoclass))
	   (entry-counter 0))
      (format stream "~%~%ISOCLASS ~A:" counter)
      (format stream "~%-----------")
      (format stream "~%  Initial Structure: Set: ~A, Op: ~A"
	      (class~alstruct-set initial-structure)
	      (class~alstruct-operation initial-structure))
      (format stream "~%  Further Strcutures:")
      (mapcar #'(lambda (structure)
		  (let* ((alstruct (class~isoclass-entry-structure structure)))
		    (cond ((< entry-counter 9)
			   (format stream "~%  ~A) Set: ~A,Op:~A"
				   (incf entry-counter)
				   (class~alstruct-set alstruct)
				   (class~alstruct-operation alstruct)))
			  ((< entry-counter 99)
			   (format stream "~% ~A) Set: ~A,Op:~A"
				   (incf entry-counter)
				   (class~alstruct-set alstruct)
				   (class~alstruct-operation alstruct)))
			  (t
			   (format stream "~%~A) Set: ~A,Op:~A"
				   (incf entry-counter)
				   (class~alstruct-set alstruct)
				   (class~alstruct-operation alstruct))))))
	      (remove initial entries))))
  (format stream "~%~%FAILED TO INSERT THE FOLLOWING STRUCTURES:")
  (mapcar #'(lambda (structure)
	      (let* ((alstruct (class~isoclass-entry-structure structure)))
		(format stream "~%    Set: ~A, Op:~A"
			(class~alstruct-set alstruct)
			(class~alstruct-operation alstruct))))
	  (mapcar #'first class*unknown-problems)))

;;(com~defcommand add-new-structure-to-isoclass-exploration
;;		(argnames set op)
;;		(argtypes term term)
;;		(arghelps "Set?" "Operation?")
;;		(frag-cats planning)
;;		(function class~ass-new-structure-to-isoclass-exploration)
;;		(log-p T)
;;		(help ""))

(defun class~ass-new-structure-to-isoclass-exploration (set op)
  (let* ((new-alstruct (class~create-algebraic-structure set op)))
    (class=process-structures new-alstruct)))


#| ------------------------------------------------------ Problem RUn ------------------------------------------------- |#

(eval-when (load compile eval)
  (defclass class+iso-run (keim+object)
    ((structure1 :initarg :structure1
		 :initform nil
		 :accessor class~iso-run-structure1)
     (structure2 :initarg :structure2
		 :initform nil
		 :accessor class~iso-run-structure2)
     (problem :initarg :problem
	      :initform nil
	      :accessor class~iso-run-problem)
     (proof :initarg :proof
	    :initform nil
	    :accessor class~iso-run-proof)
     (backtrack-steps :initarg :backtrack-steps
		      :initform nil
		      :accessor class~iso-run-backtrack-steps))))
  
(defun class~iso-run-p (obj)
  (typep obj 'class+iso-run))

(defun class~create-iso-run (structure1 structure2 problem proof backtrack-steps)
  (make-instance 'class+iso-run
		 :structure1 structure1
		 :structure2 structure2
		 :problem problem
		 :proof proof
		 :backtrack-steps backtrack-steps))

(defmethod print-object ((iso-run class+iso-run) stream)
  (format stream "<ISO-RUN: ~A>" (node~formula (prob~conclusion (class~iso-run-problem iso-run)))))

#| --------------------------------------------------- Performing ISO-RUN --------------------------------------------- |#

(defvar class*current-iso-runs nil)
(defvar class*current-alstructs1 nil)
(defvar class*current-alstructs2 nil)
(defvar class*msolve-check-line nil)
(defvar class*hint nil)


(defun class~compare-all-isos (path &key (start-strategy 'tryanderror))
  ;; each problem in class*current-alstructs1 is compared whit each structire in class*current-alstructs2
  
  (setf class*current-iso-runs nil)
  
  (mapcar #'(lambda (alstruct1)
	      (mapcar #'(lambda (alstruct2)
			  (class=iso-proof-run alstruct1 alstruct2 path :start-strategy start-strategy))
		      class*current-alstructs2))
	  class*current-alstructs1))

(defun class=iso-proof-run (alstruct1 alstruct2 path &key (start-strategy 'tryanderror))
  (multiple-value-bind
      (success proof backtrack-steps)
      (class=test-isomorphie alstruct1 alstruct2 :start-strategy start-strategy)
    
    (let* ((iso-run (if (string-equal success 'unknown)
			nil
		      (class~create-iso-run alstruct1 alstruct2 (prob~proof-problem proof) proof backtrack-steps))))
      (when iso-run 
	(setf class*current-iso-runs (append class*current-iso-runs (list iso-run))))
      
      (class=write-iso-run-into-file iso-run alstruct1 alstruct2 path))))

(defun class=write-iso-run-into-file (iso-run alstruct1 alstruct2 path)
  (sys~handler-case
   (with-open-file (out path :direction :output 
			:if-exists :append
			:if-does-not-exist :create)
		   (if (null iso-run)
		       (format out "~%~%Could prove nothing for the structures ~A and ~A" alstruct1 alstruct2)
		     (progn
		       (format out "~%~%Proved for the structures ~A and ~A the following:" alstruct1 alstruct2)
		       (format out "~%Problem ~A" (node~formula (prob~conclusion (class~iso-run-problem iso-run))))
		       (format out "~%Proof of length ~A with ~A created nodes and ~A Backtracksteps."
			       (length (prob~proof-steps (class~iso-run-proof iso-run)))
			       (keim::pds=label-counter (class~iso-run-proof iso-run))
			       (class~iso-run-backtrack-steps iso-run))))
		   (omega~message "Wrote iso-run-result into file ~A." (truename out)))
   (file-error (c) (omega~error c)))
  )

#| ------------------------------------------------------ COMMANDOS ---------------------------------------------------- |#

;;(com~defcommand change-to-pds
;;		(argnames number-of-iso-run)
;;		(argtypes integer)
;;		(arghelps "Number of iso-run?")
;;		(frag-cats planning)
;;		(function class=change-to-pds)
;;		(log-p T)
;;		(help ""))

(defun class=change-to-pds (number)
  (let* ((iso-run (nth (- number 1) class*current-iso-runs)))
    (if (null iso-run)
	(omega~message "~%SORRY, no iso-rumn number ~A available" number)
      (let* ((pds (class~iso-run-proof iso-run)))
	(oc=prove pds)))))


;;(com~defcommand show-alstructs1
;;		(argnames )
;;		(argtypes )
;;		(arghelps )
;;		(frag-cats planning)
;;		(function class=show-alstructs1)
;;		(log-p T)
;;		(help ""))

;;(com~defcommand show-alstructs2
;;		(argnames )
;;		(argtypes )
;;		(arghelps )
;;		(frag-cats planning)
;;		(function class=show-alstructs2)
;;		(log-p T)
;;		(help ""))

(defun class=show-alstructs1 ()
  (class~show-alstruct-entries class*current-alstructs1))

(defun class=show-alstructs2 ()
  (class~show-alstruct-entries class*current-alstructs2))


;;(com~defcommand reset-alstructs1
;;		(argnames )
;;		(argtypes )
;;		(arghelps )
;;		(frag-cats planning)
;;		(function class=reset-alstructs1)
;;		(log-p T)
;;		(help ""))

;;(com~defcommand reset-alstructs2
;;		(argnames )
;;		(argtypes )
;;		(arghelps )
;;		(frag-cats planning)
;;		(function class=reset-alstructs2)
;;		(log-p T)
;;		(help ""))

(defun class=reset-alstructs1 ()
  (class~reset-alstruct-entries class*current-alstructs1))

(defun class=reset-alstructs2 ()
  (class~reset-alstruct-entries class*current-alstructs2))


;;(com~defcommand add-algebraic-struct-to-1
;;		(argnames set operation)
;;		(argtypes term term)
;;		(arghelps "Set?"
;;			  "Operation?")
;;		(frag-cats planning)
;;		(function class=add-alstruct-to-1)
;;		(log-p T)
;;		(help ""))

;;(com~defcommand add-algebraic-struct-to-2
;;		(argnames set operation)
;;		(argtypes term term)
;;		(arghelps "Set?"
;;			  "Operation?")
;;		(frag-cats planning)
;;		(function class=add-alstruct-to-2)
;;		(log-p T)
;;		(help ""))

(defun class=add-alstruct-to-1 (set op)
  (class~add-alstruct set op class*current-alstructs1))

(defun class=add-alstruct-to-2 (set op)
  (class~add-alstruct set op class*current-alstructs2))


;;(com~defcommand read-alstructures1-from-file
;;		(argnames file-name)
;;		(argtypes existing-post-file)
;;		(arghelps "File?")
;;		(frag-cats  omega-basic file-io)
;;		(function class=read-alstructs1-from-file)
;;		(log-p T)
;;		(help ""))

;;(com~defcommand read-alstructures2-from-file
;;		(argnames file-name)
;;		(argtypes existing-post-file)
;;		(arghelps "File?")
;;		(frag-cats  omega-basic file-io)
;;		(function class=read-alstructs2-from-file)
;;		(log-p T)
;;		(help ""))

(defun class=read-alstructs1-from-file (fname)
  (class~read-alstructs-from-file fname class*current-alstructs1))

(defun class=read-alstructs2-from-file (fname)
  (class~read-alstructs-from-file fname class*current-alstructs2))



;;(com~defcommand show-iso-runs
;;		(argnames )
;;		(argtypes )
;;		(arghelps )
;;		(frag-cats planning)
;;		(function class=show-iso-runs)
;;		(log-p T)
;;		(help ""))

(defun class=show-iso-runs ()
  (mapcar #'(lambda (iso-run)
	      (omega~message "~%While proving ~A got proof of length ~A with ~A backtracks"
			     (node~formula (prob~conclusion (class~iso-run-problem iso-run)))
			     (length (prob~proof-steps (class~iso-run-proof iso-run)))
			     (class~iso-run-backtrack-steps iso-run)))
	  class*current-iso-runs))
					   

;;(com~defcommand run-iso-runs
;;		(argnames outfile)
;;		(argtypes pathname)
;;		(arghelps "OutFile?")
;;		(frag-cats planning)
;;		(function class~compare-all-isos)
;;		(log-p T)
;;		(help ""))

#| ------------------------------------------------------ HINTS RAUS ------------------------------------------------ |#


#|(defun class~test-all-alstructs-on-non-isomorphism ()
  (mapcar #'(lambda (alstruct1)
	      (mapcar #'(lambda (alstruct2)
			  (class=test-non-isomorphie alstruct1 alstruct2))
		      class*current-alstructs2))
	  class*current-alstructs1))

(defun class=test-non-isomorphie (struct1 struct2)
  (let* ((set1 (class~alstruct-set struct1))
	 (op1 (class~alstruct-operation struct1))
	 (set2 (class~alstruct-set struct2))
	 (op2 (class~alstruct-operation struct2))
	 (table1 (class=create-mult-table set1 op1))
	 (table2 (class=create-mult-table set2 op2)))
    (multiple-value-bind
	(success pair)
	(rcl~check-non-isomorphism table1 table2)
      
      (omega~message "~%~%Non Isomorphism-Test on structures ~A and ~A gives ~A and the pairs ~A"
		     struct1 struct2 success pair))))

(com~defcommand run-non-iso-tests
		(argnames )
		(argtypes )
		(arghelps )
		(frag-cats planning)
		(function class~test-all-alstructs-on-non-isomorphism)
		(log-p T)
		(help ""))
|#

;;(com~defcommand run-non-iso-runs
;;		(argnames outfile start-strategy ressource)
;;		(argtypes pathname symbol integer)
;;		(arghelps "OutFile?"
;;			  "StartStrategy?"
;;			  "STRAT-RESSOURCE?")
;;		(frag-cats planning)
;;		(function class=non-iso-runs)
;;		(log-p T)
;;		(help ""))

(defun class=non-iso-runs (path start-strategy ressource)
  (class~compare-non-isos path
			  :start-strategy start-strategy
			  :ressource ressource))

(defun class~compare-non-isos (path &key (start-strategy 'tryanderror) (ressource 20000))
  ;; each problem in class*current-alstructs1 is compared whit each structire in class*current-alstructs2
  ;; in a non-iso-test!
  
  (setf class*current-iso-runs nil)
  
  (mapcar #'(lambda (alstruct1)
	      (mapcar #'(lambda (alstruct2)
			  (class=non-iso-proof-run alstruct1 alstruct2 path
						   :start-strategy start-strategy
						   :ressource ressource))
		      class*current-alstructs2))
	  class*current-alstructs1))

(defun class=non-iso-proof-run (alstruct1 alstruct2 path &key (start-strategy 'tryanderror) (ressource 20000))
  (multiple-value-bind
      (success proof backtrack-steps)
      (class=prove-non-isomorphie alstruct1 alstruct2
				  :start-strategy start-strategy
				  :ressource ressource)
    
    (let* ((iso-run (class~create-iso-run alstruct1 alstruct2 (prob~proof-problem proof) proof backtrack-steps)))
      
      (setf class*current-iso-runs (append class*current-iso-runs (list iso-run)))
      
      (class=write-non-iso-run-into-file iso-run alstruct1 alstruct2 success path))))

(defun class=prove-non-isomorphie (struct1 struct2 &key (start-strategy nil) (ressource 20000))
  (let* ((set1 (class~alstruct-set struct1))
	 (op1 (class~alstruct-operation struct1))
	 (set2 (class~alstruct-set struct2))
	 (op2 (class~alstruct-operation struct2)))
    
    (multiple-value-bind
	(success proof backtrack-steps)
	(class=prove-isomorphism set1 op1 set2 op2 :pol nil
				 :start-strategy start-strategy
				 :ressource ressource)
      
      (values success proof backtrack-steps)
      )))


(defun class=write-non-iso-run-into-file (iso-run alstruct1 alstruct2 success path)
  (sys~handler-case
   (with-open-file (out path :direction :output 
			:if-exists :append
			:if-does-not-exist :create)
		   (if (null success)
		       (progn
			 (format out "~%~%Failed to prove for the structures ~A and ~A the following:" alstruct1 alstruct2)
			 (format out "~%Problem ~A" (node~formula (prob~conclusion (class~iso-run-problem iso-run))))
			 (format out "~%Open PDS of length ~A (~A open nodes) with ~A created nodes and ~A Backtracksteps."
				 (length (prob~proof-steps (class~iso-run-proof iso-run)))
				 (length (remove-if-not #'agenda~goal-or-goal-schema-task-p
							(agenda~all-tasks (pds~agenda (class~iso-run-proof iso-run)))))
				 (keim::pds=label-counter (class~iso-run-proof iso-run))
				 (class~iso-run-backtrack-steps iso-run))
			 (format out "~%Maple gave hint: ~A, where Maples Solution was: ~A"
				 (second class*hint)
				 (third class*hint)))
		     (progn
		       (format out "~%~%Proved for the structures ~A and ~A the following:" alstruct1 alstruct2)
		       (format out "~%Problem ~A" (node~formula (prob~conclusion (class~iso-run-problem iso-run))))
		       (format out "~%Proof of length ~A with ~A created nodes and ~A Backtracksteps."
			       (length (prob~proof-steps (class~iso-run-proof iso-run)))
			       (keim::pds=label-counter (class~iso-run-proof iso-run))
			       (class~iso-run-backtrack-steps iso-run))
		       (format out "~%Maple gave hint: ~A, where Maples Solution was: ~A"
				 (second class*hint)
				 (third class*hint))
		       (format out "~%Maple proved finitely the equation: ~%~A ~%with Solution ~A under mod-value ~A"
			       (first class*msolve-check-line)
			       (second class*msolve-check-line)
			       (third class*msolve-check-line))))
		   (omega~message "Wrote iso-run-result into file ~A." (truename out)))
   (file-error (c) (omega~error c)))
  )

;;(com~defcommand create-and-write-non-iso-problems-in-file
;;		(argnames outfile)
;;		(argtypes pathname)
;;		(arghelps "File to write?")
;;		(frag-cats planning)
;;		(function class=create-and-write-non-iso-problems-in-file)
;;		(log-p T)
;;		(help ""))

(defun class=create-and-write-non-iso-problems-in-file (path)
  (let* ((all-non-iso-problems (apply #'append (mapcar #'(lambda (alstruct1)
							   (mapcar #'(lambda (alstruct2)
								       (class=create-non-iso-problem alstruct1 alstruct2))
								   class*current-alstructs2))
						       class*current-alstructs1))))
    (class=write-problems-in-file path all-non-iso-problems)))

(defun class=create-non-iso-problem (struct1 struct2)
  (let* ((set1 (class~alstruct-set struct1))
	 (op1 (class~alstruct-operation struct1))
	 (set2 (class~alstruct-set struct2))
	 (op2 (class~alstruct-operation struct2))
	 (env (th~env 'zmz))
	 (conclusion-formula (term~appl-create (env~lookup-object 'not env)
					       (list (term~appl-create (env~lookup-object 'isomorphic env)
								       (list set1 op1 set2 op2)))))
	 (conclusion (node~create 'conc
				  conclusion-formula
				  (just~create (infer~find-method 'open) nil)))
	 (prob-env (env~create (th~env 'zmz)))
	 (prob-name (make-symbol (format nil "NON-ISOTEST-PROBLEM-~A" (incf class*counter)))))
    (prob~create prob-name (th~find-theory 'zmz) prob-env nil conclusion)))

(defun class=write-problems-in-file (path problems)
  (sys~handler-case
   (with-open-file (out path :direction :output 
			:if-exists :supersede
			:if-does-not-exist :create)
		   (format out "~%(~%")
		   (mapcar #'(lambda (problem)
			       (post~print problem out)
			       (format out "~%"))
			   problems)
		   (format out ")~%")
		   (omega~message "Wrote iso-run-result into file ~A." (truename out)))
   (file-error (c) (omega~error c)))
  )

		   
  

#| -------------------------------------------------------- GET TABLE ---------------------------------------------- |#

;;(com~defcommand show-table
;;		(argnames set op)
;;		(argtypes term term)
;;		(arghelps "Set?" "Op?")
;;		(frag-cats planning)
;;		(function class=show-table)
;;		(log-p T)
;;		(help ""))

(defun class=show-table (set op)
  (let* ((table (class=create-mult-table set op)))
    (omega~message "~%~A" table)))




