;;; -*- Syntax: common-lisp; package: OMEGA; base: 10; mode: keim -*-
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; cosie.lisp; This file is part of the OMEGA system
;;
;; major updates: 24.2.1999,
;; 
;;
;; Author: Juergen Zimmer
;; email: jzimmer@ags.uni-sb.de 
;;
;; For information about this program, write to:                          
;;   OMEGA Project                                                        
;;   AG Siekmann/FB Informatik                                            
;;   Universitaet des Saarlandes                                          
;;   Bau 36, 4. Stock                                                     
;;   D-66041 Saarbruecken                                                 
;;   Germany    
;;
;; For information about the newest version of Omega, see 
;;   http://www.ags.uni-sb.de/~omega/
;;
;; This program is free software; it can be used under the terms of the GNU General
;; Public License as published by the Free Software Foundation; either version 2 of
;; the License, or any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; merchantibility or fitness for a particular purpose. 
;; See the GNU General Public License (http://www.fsf.org/copyleft/gpl.html)
;; for more details.
;;
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
(in-package :omega)

(mod~defmod COSIE 
            :uses (cosint data env keim logic meta mod node omega pds pdsn post serv subst term type)
            :documentation "The data-structures and algorithms for the constraint solver CoSIE"
            :exports (
                      
                      CoSIE~announce
                      CoSIE~ask
                      CoSIE~call
                      CoSIE~constraint-p
                      CoSIE~eigenVars
                      CoSIE~init
                      CoSIE~leave
                      CoSIE~meta-var
                      CoSIE~reflect
                      CoSIE~reset
                      CoSIE~show
                      CoSIE~solve
                      CoSIE~backtrack
                      CoSIE~tell
                      CoSIE~tellMeta
                      CoSIE~trace
		      CoSIE~searchTrace
                      
                      cosie*signal-errors
                      cosie*signal-interface-errors
                      cosie*announced
                      cosie*show
                      cosie*trace
		      cosie*searchtrace))


(defvar cosie*announced nil "T, iff the lisp process is connected to the
CoSIE service in mathweb.")
(defvar cosie*show nil "T, iff the constraint printer of CoSIE is active.")
(defvar cosie*daVinci nil "T, iff the daVinci browser of CoSIE is active.")
(defvar cosie*trace nil "T, iff the trace mode of CoSIE is turned on.")

(defvar cosie*searchTrace nil "The trace of the search procedure")

(defun CoSIE~announce ()
  (if (CoSIE=restart)
      (progn (setf cosie*announced t)
	     (omega~message "The CoSIE-service is available!")
	     (cosint~new-cs omega::'CoSIE
			    #'CoSIE~call
			    )
	     )
    (progn (setf cosie*announced nil)
	   (omega~warn "The CoSIE-service is not available."))
    ))


(defun CoSIE~call (command &rest args)
  (if cosie*announced
      (let ((function
	     (case command
	       ('INITIALIZE         'CoSIE~init)
	       ('RESET              'CoSIE~reset)
	       ('TESTCONSTRAINT     'CoSIE~test)
	       ('TELLCONSTRAINT     'CoSIE~tell)
	       ('TESTASSUMPTION     'CoSIE~test-ass)
	       ('TELLASSUMPTION     'CoSIE~tell-ass)
	       ('ASKCONSTRAINT      'CoSIE~ask)
	       ('TELLMETACONSTRAINT 'CoSIE~tellMeta)
	       ('GETMETAVAR         'CoSIE~meta-var)
	       ('GETEIGENVARS       'CoSIE~eigenVars)
	       ('GETDETVARS         'CoSIE~getDetVars)
	       ('REFLECT            'CoSIE~reflect)
	       ('SOLVE              'CoSIE~solve)
               ('BACKTRACK          'CoSIE~backtrack)
	       ('SHOW               'CoSIE~show)
	       ('DAVINCI            'CoSIE~daVinci)
	       ('TRACE              'CoSIE~trace)
	       ('SEARCHTRACE        'CoSIE~searchTrace)
	       (T 	  	    'unknown))))
	(if (eq function 'unknown)
	    (progn (omega~trace "The constraint solver CoSIE does not provide the functionality ~S" command)
		   (values nil nil))
	  (apply function args)))
    (progn (omega~warn "You must connect to the CoSIE service first.")
	   (values nil nil))))
	
(defun CoSIE~init ()
  (let ((env (pds~environment omega*current-proof-plan))
	(meta-cs (meta~variable-create 'CoSIE-Meta-CS (type~o)))
	)
    (env~enter 'CoSIE-Meta-CS meta-cs env)
    meta-cs))


(defun CoSIE~reset ()
  (let ((problem-name (if omega*current-proof-plan
			  (keim~name omega*current-proof-plan)
			'UNKNOWN))
	)
    (setf cosie*searchTrace nil)
    (CoSIE=call (format nil "reset('~A')" problem-name))))


;; NEW VERSION ADDED 21.2.00 by AMeier
(defun CoSIE~test (node)
  (let* ((relevant-formula (cosie=get-relevant-formula node))
	 )
    
    (if (CoSIE~constraint-p relevant-formula)
	(let* ((form-string (CoSIE=parse relevant-formula))
	       (constraint-hyps (remove-if-not #'(lambda (hyp)
						   (CoSIE~constraint-p (if (pdsn~schematic-p hyp)
									   (pdsn~current-formula hyp)
									 (node~formula hyp))))
					       (pdsn~hyps node)))
	       (hyps-string (if constraint-hyps
				(format nil "[~A]" (CoSIE=parse constraint-hyps))
			      "nil"))
	       (name-string (format nil "'~A'" (keim~name node)))
	       ;;(adf (omega~message  "~% messge: ~S" (format nil "testConstraint(~A ~A ~A)"
	       ;;						  name-string
	       ;;						  hyps-string
	       ;;						  form-string)))
	       (result (CoSIE=call (format nil "testConstraint(~A ~A ~A)"
					   name-string
					   hyps-string
					   form-string)))
	       )
	  (and result
	       (not (string-equal result "NIL"))))
      nil)))


;; NEW VERSION ADDED 21.2.00 by AMeier
(defun CoSIE~tell (node)
  (let* ((relevant-formula (cosie=get-relevant-formula node))
	 (form-string (CoSIE=parse relevant-formula))
	 (constraint-hyps (remove-if-not #'(lambda (hyp)
					     (CoSIE~constraint-p (if (pdsn~schematic-p hyp)
								     (pdsn~current-formula hyp)
								   (node~formula hyp))))
					 (pdsn~hyps node)))
	 ;;(asfd (format t "~& hyps: ~S" constraint-hyps))
	 (hyps-string (if constraint-hyps
			  (format nil "[~A]" (CoSIE=parse constraint-hyps))
			"nil"))
	 (name-string (format nil "'~A'" (keim~name node)))
; 	 (adf (omega~message  "~% messge: ~S" (format nil "tellConstraint(~A ~A ~A)"
; 	 						name-string
; 	 						hyps-string
					; 	 						form-string)))
	 (result (CoSIE=call (format nil "tellConstraint(~A ~A ~A)"
				     name-string
				     hyps-string
				     form-string)))
	 )
    (when (and result
	       (not (string-equal result "NIL")))
      (let ((term-printer-lists
       	     (CoSIE=call (format nil "getConstraints()")))
	    )
	;;(format t "~& printer-lists: ~S" term-printer-lists)
	;;(socket~write (format nil "updateConstraints(~A)"
	;;		      term-printer-lists))
	T))))


;; NEW VERSION added 21.2.00 by Ameier
(defun CoSIE~test-ass (node)
  (let* ((relevant-formula (cosie=get-relevant-formula node))
	 )
    
    (if (CoSIE~constraint-p relevant-formula)
	(let* ((form-string (CoSIE=parse relevant-formula))
	       (constraint-hyps (remove-if-not #'(lambda (hyp)
						   (CoSIE~constraint-p (if (pdsn~schematic-p hyp)
									   (pdsn~current-formula hyp)
									 (node~formula hyp))))
					       (pdsn~hyps node)))
	       (hyps-string (if constraint-hyps
				(format nil "[~A]" (CoSIE=parse constraint-hyps))
			      "nil"))
	       (name-string (format nil "'~A'" (keim~name node)))
	       ;;(adf (omega~message  "~% messge: ~S" (format nil "testConstraint(~A ~A ~A)"
	       ;;						  name-string
	       ;;						  hyps-string
	       ;;						  form-string)))
	       (result (CoSIE=call (format nil "testAssumption(~A ~A ~A)"
					   name-string
					   hyps-string
					   form-string)))
	       )
	  (and result
	       (not (string-equal result "NIL")))))))


;; NEW VERSION added 21.2.00 by Ameier
(defun CoSIE~tell-ass (node)
  (let* ((relevant-formula (cosie=get-relevant-formula node))
	 (form-string (CoSIE=parse relevant-formula))
	 (constraint-hyps (remove-if-not #'(lambda (hyp)
					     (CoSIE~constraint-p (if (pdsn~schematic-p hyp)
								     (pdsn~current-formula hyp)
								   (node~formula hyp))))
					 (pdsn~hyps node)))
	 (hyps-string (if constraint-hyps
			  (format nil "[~A]" (CoSIE=parse constraint-hyps))
			"nil"))
	 (name-string (format nil "'~A'" (keim~name node)))
	 (result (CoSIE=call (format nil "tellAssumption(~A ~A ~A)"
				     name-string
				     hyps-string
				     form-string)))
	 )
    (when (and result
	       (not (string-equal result "NIL")))
      (let ((term-printer-lists
	     (CoSIE=call (format nil "getConstraints()")))
	    )
 	;;(format t "~& printer-lists: ~S" term-printer-lists)
	;;(socket~write (format nil "updateConstraints(~A)"
	;;		      term-printer-lists))
	T))))


;; NEW VERSION ADDED 21.2.00 by Ameier
(defun CoSIE~ask (node)
  (let* ((relevant-formula (cosie=get-relevant-formula node))
	 )
    (if (CoSIE~constraint-p relevant-formula)
	(let* ((form-string (CoSIE=parse relevant-formula))
	       (constraint-hyps (remove-if-not #'(lambda (hyp)
						   (CoSIE~constraint-p (if (pdsn~schematic-p hyp)
									   (pdsn~current-formula hyp)
									 (node~formula hyp))))
					       (pdsn~hyps node)))
	       (hyps-string (if constraint-hyps
				(format nil "[~A]" (CoSIE=parse constraint-hyps))
			      "nil"))
	       (name-string (format nil "'~A'" (keim~name node)))
	       ;;	 (adf (omega~message  "~% messge: ~S" (format nil "askConstraint(~A ~A ~A)"
	       ;;						name-string
	       ;;						hyps-string
	       ;;						form-string)))
	       (result (CoSIE=call (format nil "askConstraint(~A ~A ~A)"
					   name-string
					   hyps-string
					   form-string)))
	       )
	  (and result
	       (not (string-equal result "NIL"))))
      nil)))

(defun CoSIE~tellMeta (constraint &rest args)
  (let* ((oz-args (CoSIE=parse args))
	 ;;(adsf (format t "~% message: ~S" (format nil "tellMetaConstraint('~A'(~A))"
	 ;;					  constraint
	 ;;					  oz-args)))
	 (result (CoSIE=call (format nil "tellMetaConstraint('~A'(~A))"
				     constraint
				     oz-args)))
	 )
    t))
;;(and result
	;; (not (string-equal result "NIL")))))

(defun CoSIE~eigenVars ()
  (let* ((result (CoSIE=call "eigenVars()"))
	 )
    (when (and result
	       (not (member result (list "" "NIL" "TRUE" "T") :test #'string-equal)))
      (let* ((env (pds~environment omega*current-proof-plan))
	     (symbol-list (read-from-string (format nil "~A" (read-from-string result))))
	     (const-list (mapcar #'(lambda (sym)
				     (post~read-object sym env :existing-term))
				 symbol-list))
	     )
	const-list))))

(defun CoSIE~getDetVars ()
  (let ((result (CoSIE=call "getDetVars()"))
	)
    (when (and result
	       (not (string-equal result "NIL")))
      (let* ((env (pds~environment omega*current-proof-plan))
	     (alist (read-from-string result))
	     (post-alist
	      (mapcar #'(lambda (pair)
			  (let* ((var (post~read-object (car pair) env :existing-term))
				 (term (post~read-object (cdr pair) env :existing-term))
				 )
			    (cons var term)))
		      alist))
	     )
	post-alist))))


(defun CoSIE~constraint-p (formula)
  (and (term~p formula)
       (and (and (term~appl-p formula)
		 (term~primitive-p (data~appl-function formula)))
	    (let ((func (intern (keim~name (data~appl-function formula))))
		  (args (data~appl-arguments formula)))
	      (and (member func '(LESS LEQ = GEQ GREATER IN)) ;; IN for sort constraints
		   (= (length args) 2))))))

(defun CoSIE=var-in (formula)
  (let ((subterms (remove-duplicates (data~all-substructs formula))))
    (some #'term~variable-p subterms)))

;; Basisconstraint: B ::= X<=u | X>=l
;; Nicht-Basisconstraints:
;;                 C  ::= C1 & C2 | t1 < t2 | t1 <= t2 | t1 > t2 | t1 >= t2 | X=t
;;                 t  ::= t1 op t2 | X | a | |t'|        ;; 'a' is a symbolic/numeric constant
;;                 t' ::= a
;;                 
;; alternatively:  t' ::= t'1 op t'2 | a                 ;; ground terms in absolute value
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun CoSIE~show ()
  (if cosie*show
      (let ((result (CoSIE=call "unshowConstraints()")))
	(setf cosie*show nil)
	(omega~trace "The CoSIE constraint printer is closed."))
    (let ((result (CoSIE=call (format nil "showConstraints()"))))
      (setf cosie*show T)
      (omega~trace "The CoSIE constraint printer is running."))))

(defun CoSIE~daVinci ()
  (if cosie*daVinci
      (let ((result (CoSIE=call "unshowContextTree()")))
	(setf cosie*daVinci nil)
	(omega~trace "DaVinci is closed."))
    (let ((result (CoSIE=call (format nil "showContextTree()"))))
      (setf cosie*daVinci T)
      (omega~trace "DaVinci is running."))))


(defun CoSIE~trace ()
  (if cosie*trace
      (let* ((message (format nil "stopTrace()"))
	     (result (CoSIE=call message))
	     )
	(setf cosie*trace nil)
	result)
    (let* ((message (format nil "startTrace('/tmp/~A.oz')"
			    (keim~name omega*current-proof-plan))
		    )
	   (result (CoSIE=call message))
	   )
      (setf cosie*trace t)
      result)))

(defun CoSIE~reflect ()
  (let* ((env (pds~environment omega*current-proof-plan))
	 (meta (env~lookup-object 'CoSIE-Meta-CS env))
	 (res1 (CoSIE=call (format nil "stopTrace()")))
	 (formula-string (CoSIE=call (format nil "reflect()")))
	 ;;(asfd (format t "~& reflect: ~S" formula-string))
	 (formula-symbol
	  (read-from-string (format nil "~A" (read-from-string formula-string))))
	 ;;(CoSIE=instantiate-hyps (read-from-string formula-string) env))))
	 (formula (post~read-object formula-symbol
				    env :existing-term))
	 )
    (cons meta formula)))

(defun CoSIE~searchTrace()
  (if cosie*searchTrace
      cosie*searchTrace
    (let* ((message (format nil "getSearchTrace()"))
	   (result (CoSIE=call message))
	   )
      (if result
	  (let* ((reslist (read-from-string result))
		 (env (pds~environment omega*current-proof-plan))
		 (instlist (mapcar #'(lambda (entry)
				       (list (first entry)
					     (mapcar #'(lambda (formula-symbol)
							 (post~read-object formula-symbol
									   env :existing-term))
						     (second entry))
					     (mapcar #'(lambda (formula-symbol)
							 (post~read-object formula-symbol
									   env :existing-term))
						     (third entry))
					     (mapcar #'(lambda (formula-symbol)
							 (post~read-object formula-symbol
									   env :existing-term))
						     (fourth entry))
					     ))
				   reslist))
		 )
	    (setf cosie*searchTrace instlist)
	    instlist)
	(progn (setf cosie*searchTrace nil)
	       nil)))))

(defun CoSIE~solve ()
  (let* ((env (pds~environment omega*current-proof-plan))
	 (result (CoSIE=call (format nil "findSolution()")))
	 )
    ;(omega~message "~& result: ~S" result)
    (if (and result
	     (not (string-equal result "NIL")))
	(let* ((formula-symbol (read-from-string result))
	       (formula (post~read-object formula-symbol
					  env :existing-term))
	       (subst (subst~create nil nil))
	       )
	  (CoSIE=conjunct2subst formula subst)
	  (omega~message "~%CoSIE found the instantiations:")
	  (omega~message "~%  ~S" subst)
	  subst)
      (subst~create nil nil))))

(defun CoSIE~backtrack (Node)
  (let* ((env (pds~environment omega*current-proof-plan))
         (name-string (format nil "'~A'" (keim~name Node)))
         (constraint-hyps (remove-if-not #'(lambda (hyp)
                                             (CoSIE~constraint-p (if (pdsn~schematic-p hyp)
                                                                     (pdsn~current-formula hyp)
                                                                   (node~formula hyp))))
                                         (pdsn~hyps node)))
	 (hyps-string (if constraint-hyps
			  (format nil "[~A]" (CoSIE=parse constraint-hyps))
			"nil"))
         (result (CoSIE=call (format nil "withdrawConstraint(~A ~A)" name-string hyps-string)))
	 )
    (omega~message "~& result: ~S" result)
    (and result
         (not (string-equal result "NIL")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some usefull functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun CoSIE=get-relevant-formula (node)
  (if (pdsn~schematic-p node)
      (pdsn~current-formula node)
    (node~formula node)))

(defun CoSIE=conjunct2subst (formula subst)
  (cond ((null formula) nil)
	((logic~conjunction-p formula)
	 (CoSIE=conjunct2subst (first (data~appl-arguments formula)) subst)
	 (CoSIE=conjunct2subst (second (data~appl-arguments formula)) subst))
	((logic~equality-p formula)
	 (subst~insert-component! (first (data~appl-arguments formula))
				  (second (data~appl-arguments formula))
				  subst))
	(T nil)))
	    

;; New Version added 21.2.00 by AMeier
(defgeneric CoSIE=instantiate-hyps (symbol env)
  (:method ((tlist list) env)
	   (mapcar #'(lambda (el)
		       (CoSIE=instantiate-hyps el env)
		       )
		   tlist)
	   )
  (:method ((sym symbol) env)
	   (let ((val (env~lookup-object sym env)))
	     (if val
		 val
	       (let ((node (pds~label2node sym)))
		 (omega~message "~% node: ~A" node)
		 (if node
		     (read-from-string (post~print (if (pdsn~schematic-p node)
						       (pdsn~current-formula node)
						     (node~formula node))
						   nil))
		   (progn (omega~message "Don't know how to read ~A" sym)
			  sym)))))
	   )
  (:method ((num fixnum) env)
	   num)
  )


;; NEW VERSION ADDED 21.2.00 by Ameier
(defgeneric CoSIE=parse (term)
  (declare (edited  "16-SEP-1998")
	   (authors Jzimmer)
	   (input   )
	   (effect  )
	   (value   ))
  (:method ((tlist list))
	   (if tlist
	       (format nil "~A ~A"
		       (CoSIE=parse (first tlist))
		       (CoSIE=parse (rest tlist)))
	     ""))
  (:method ((node pdsn+node))
	   (let* ((relevant-formula (cosie=get-relevant-formula node))
		  )
	     (if (CoSIE~constraint-p relevant-formula)
		 (format nil "'~A'#~A" (keim~name node) (CoSIE=parse relevant-formula))
	       ""  ;;(format nil "'~A'" (keim~name node)))
	       )))
  (:method ((num term+number))
	   (CoSIE=parse (keim~name num)))
  
  (:method ((num integer))
	   (if (< num 0)
	       (format nil "~A~A" "~" (abs num))
	     (format nil "~A" num)))
  
  (:method ((float single-float))
	   (if (< float 0)
	       (format nil "~A~A" "~" (abs float))
	     (format nil "~A" float)))
  
  (:method ((term term+constant))
					;(omega~message  "~% term: ~S" term)
	   (if (type~primitive-p (term~type term))
	       (format nil "'~A'#const"
		       (string-downcase
			(format nil "~A" (keim~name term))))
	     (let ((name (keim~name term)))
					;(omega~message "~% term: ~S" term)
	       (cond ((eq name 'LESS)
		      "'<'")
		     ((eq name 'LEQ)
		      "'<='")
		     ((eq name '=)
		      "'='")
		     ((eq name 'GREATER)
		      "'>'")
		     ((eq name 'GEQ)
		      "'>='")
		     ((eq name 'PLUS)
		      "'+'")
		     ((eq name 'MINUS)
		      "'-'")
		     ((eq name 'TIMES)
		      "'*'")
		     ((eq name 'DIV)
		      "'/'")
		     ((eq name 'ABSVAL)
		      "'||'")
		     ((eq name 'MIN)
		      "'MIN'")
		     ((eq name 'SQRT)
		      "'SQRT'")
		     ((eq name 'POWER)
		      "'^'") 
		     (T (format nil "'~A'" (keim~name term)))
		      ))))
  (:method ((term term+variable))
	   (if (type~primitive-p (term~type term))
	       (format nil "'~A'#var" (keim~name term))
	     (format nil "'~A'" (keim~name term))))
  
  (:method ((term term+appl))
	   (let* ((func (CoSIE=parse (data~appl-function term)))
		  (args (CoSIE=parse (data~appl-arguments term)))
		  )
	     (format nil "~A(~A)" func args)))
  (:method ((term term+abstr))
	   (let* ((bound (data~abstr-bound-var term))
		  (scope (data~abstr-scope term))
		  (output (format nil "'LAM'(~A ~A)"
				  (CoSIE=parse bound)
				  (CoSIE=parse scope))))
	     output)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  leave   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun CoSIE~leave ()
  (CoSIE=leave)
  (setf cosie*announced nil)
  (omega~message "Disconnecting from service CoSIE!"))

(defun CoSIE=enter ()
  (declare (edited  "16-SEP-1998")
	   (authors Jzimmer)
	   (input   )
	   (effect  )
	   (value   ))
  (cond ((serv~enter "COSIE" :signal-errors nil) T) 
        (Cosie*signal-interface-errors
         (CoSIE=signal-interface-error "CoSIE=enter"))
        (T nil)))

(defun CoSIE=leave ()
  (declare (edited  "16-SEP-1998")
	   (authors Jzimmer)
           (input   "None.")
           (effect  "Leave the COSIE service.")
           (value   "T iff successfull."))
  (cond ((serv~leave "COSIE" :signal-errors nil) T) 
        (Cosie*signal-interface-errors
         (CoSIE=signal-interface-error "CoSIE=leave"))
        (T nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  restart  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun CoSIE=restart ()
  (declare (edited  "16-SEP-1998")
	   (authors Jzimmer)
           (input   "None.")
           (effect  "Restart the COSIE service.")
           (value   "T iff successfull."))
  (and (serv~available "COSIE")
       (cond ((serv~restart "COSIE" :signal-errors nil) T) 
	     (Cosie*signal-interface-errors
	     (CoSIE=signal-interface-error "CoSIE=restart"))
	     (T nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   call   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun CoSIE=call (method &optional (function-name "CoSIE=call"))
  (declare (edited  "16-SEP-1998")
	   (authors Jzimmer)
           (input   "A string containing the Oz-method to be executed.")
           (effect  "Call the COSIE-service to execute the method.")
           (value   "The result if everything went allright, otherwise nil."))
  
  (cond ((serv~apply "COSIE" (concatenate 'string "exec(" method ")")
                     :signal-errors nil)
         (serv~error-message))
        ((and Cosie*signal-interface-errors (serv~interface-error?))
         (CoSIE=signal-interface-error function-name))
        ((and Cosie*signal-errors (serv~service-error?))
         (CoSIE=signal-error function-name method))
        (T nil)))


(defvar Cosie*signal-interface-errors T)  ; corresponds to serv~signal-errors
(defvar Cosie*signal-errors nil)         

(defun CoSIE=error? ()
  (declare (edited  "02-SEP-1998")
           (authors afranke)
           (input   "None.")
           (effect  "None.")
           (value   "T iff the last service-operation returned an error. "))
  (serv~service-error?))

(defun CoSIE=error-number ()
  (declare (edited  "02-SEP-1998")
           (authors afranke)
           (input   "None.")
           (effect  "None.")
           (value   "The original COSIE-service-error-number. "))
  (serv~service-error-number))

(defun CoSIE=error-message ()
  (declare (edited  "02-SEP-1998")
           (authors afranke)
           (input   "None.")
           (effect  "None.")
           (value   "The error message. "))
  (serv~error-message))

(defun CoSIE=signal-errors-on ()
  (declare (edited  "02-SEP-1998")
           (authors afranke)
           (input   "None.")
           (effect  "Cosie*signal-errors is set to T.")
           (value   "T."))
  (setq Cosie*signal-errors T)
  T)

(defun CoSIE=signal-errors-off ()
  (declare (edited  "02-SEP-1998")
           (authors afranke)
           (input   "None.")
           (effect  "Cosie*signal-errors is set to nil.")
           (value   "T."))
  (setq Cosie*signal-errors nil)
  T)

(defun CoSIE=signal-interface-errors-on ()
  (declare (edited  "02-SEP-1998")
           (authors afranke)
           (input   "None.")
           (effect  "Cosie*signal-interface-errors is set to T.")
           (value   "T."))
  (setq Cosie*signal-interface-errors T)
  T)

(defun CoSIE=signal-interface-errors-off ()
  (declare (edited  "02-SEP-1998")
           (authors afranke)
           (input   "None.")
           (effect  "Cosie*signal-interface-errors is set to nil.")
           (value   "T."))
  (setq Cosie*signal-interface-errors nil)
  T)

(defun CoSIE=signal-interface-error (function-name)
  (declare (edited  "02-SEP-1998")
           (authors afranke)
           (input   "None.")
           (effect  "Signals an error.")
           (value   "Nil, if continued."))
  (cerror "let ~A return nil."
          "An interface error occured while executing function ~A.~%~A: ~A"
          function-name (serv~error-number) (serv~error-message)))

(defun CoSIE=signal-error (function-name method)
  (declare (edited  "02-SEP-1998")
           (authors afranke)
           (input   "None.")
           (effect  "Signals an error.")
           (value   "Nil, if continued."))
  (cerror "let ~A return nil."
          "In Function ~A: The COSIE-service could not apply method '~A' successfully.~%~A: ~A"
          function-name method
          (CoSIE=error-number) (CoSIE=error-message)))


#|
old constraint-p
			(let* ((a (first args))
			       (b (second args))
			       (absval (env~lookup-object 'absval (pds~environment omega*current-proof-plan)))
			       (aposs (data~substruct-positions absval a))
			       (bposs (data~substruct-positions absval a)))
			  (and
			   (every #'(lambda (pos)
				      (let ((arg (first
						  (data~appl-arguments
						   (data~struct-at-position a (pos~butlast pos))))))
					(term~constant-p arg)))
					;;(not (CoSIE=var-in arg))))
				  aposs)
			   (every #'(lambda (pos)
				      (let ((arg (first
						  (data~appl-arguments
						   (data~struct-at-position a (pos~butlast pos))))))
					(term~constant-p arg)))
				  ;;(not (CoSIE=var-in arg))))
				  bposs)))))))))
|#


