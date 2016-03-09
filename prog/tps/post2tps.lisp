;;; -*- syntax: common-lisp; package: OMEGA; base: 10; mode: LISP -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1997 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     OMEGA Project                                                        ;;
;;     AG Siekmann/FB Informatik                                            ;;
;;     Universitaet des Saarlandes                                          ;;
;;     Bau 36, 2. Stock                                                     ;;
;;     D-66041 Saarbruecken                                                 ;;
;;     Germany                                                              ;;
;;   electronic mail: keim@ags.uni-sb.de                                     ;;
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


(let ((p2t*typed-symbols)
      (p2t*unused-types)  
      (p2t*type-translation)
      (p2t*symbol-translation)
      (p2t*plan-counter 0))
  
  (defgeneric p2t~proof-plan (plan &optional (defs nil))
    (declare (edited  "17-JUL-1998 16:50")
	     (authors SORGE)
	     (input   "A proof-plan or a list of proof-llnes.")
	     (effect  "None.")
	     (value   "An ATPPRB+TPS-PROBLEM containing the TPS version of the proof."))
    (:method (plan &optional defs)
	     (declare (ignore defs))
	     (omega~error "~A is not of the expected type." plan))
    (:method ((plan pds+proof-plan) &optional (defs nil))
	     (multiple-value-bind (d1 d2 d3 day month year)
		 (decode-universal-time (get-universal-time))
	       (declare (ignore d1 d2 d3))
	       (p2t=initialize)
	       (let* ((date (list year month day))
		      (root (prob~proof-root plan))
		      (open-lines (pds~open-nodes plan))
		      (supports (mapcar #'(lambda (line)
					    (pds~node-supports line plan))
					open-lines))
		      (line-list (p2t=enumerate-lines (prob~proof-steps plan)
						      root supports open-lines))
		      (assertion (p2t~formula (node~formula root)))
		      )
		 (p2t=atpprb-object-create
		  (list 'defsavedproof
			(read-from-string (symbol-name (keim~name plan)))
			date
			(list 'assertion assertion)
			(list 'next-plan-no (1+ (length line-list)))
			(list 'plans (p2t=plans-entry open-lines supports line-list))
			(cons 'lines
			      (mapcar #'(lambda (node)
					  (p2t=node node line-list))
				      line-list))
			0
			(p2t=defs-nodes-translate (prob~proof-steps plan) plan defs)
			(list 'comment "OMEGA proof (report problems to the OMEGA group)")
			(list 'locked (mapcar #'car line-list))))
		 )))
    (:method ((plan list) &optional (defs nil))
	     (declare (ignore defs))
	     (when (every #'node~p plan)
	       (p2t=initialize)
	       (mapcar #'(lambda (line)
			   (p2t~formula (node~formula line)))
		       plan)))
    )
  

  (defun p2t~subproblem (node &optional (pds omega*current-proof-plan) (defs nil) (supports (reverse (pds~node-supports node pds))))
    (declare (edited  "20-JUL-1998 19:57")
	     (authors SORGE)
	     (input   "An open node.")
	     (effect  "Creates an instance of ATPPRB+TPS-PROBLEM.")
	     (value   "An ATPPRB+TPS-PROBLEM containing the TPS version of the subproblem."))
    (when (pdsn~open-node-p node)
      (multiple-value-bind (d1 d2 d3 day month year)
	  (decode-universal-time (get-universal-time))
	(declare (ignore d1 d2 d3))
	(p2t=initialize)
	(let* ((date (list year month day))
	       ;;	       (supports (reverse (pds~node-supports node pds)))
	       (line-list (p2t=enumerate-lines (append supports (list node)) node supports node))
	       (assertion (p2t~formula (node~formula node))))
	  (p2t=atpprb-object-create
	   (list 'defsavedproof
		 (read-from-string (format nil "OMEGA-SUBPROBLEM-~A" (keim~name node)))
		 date
		 (list 'assertion assertion)
		 (list 'next-plan-no (1+ (length line-list)))
		 (list 'plans (p2t=plans-entry (list node) (list supports) line-list))
		 (cons 'lines
		       (append
			(mapcar #'(lambda (node)
				    (p2t=node node line-list :set-just :hyp))
				(butlast line-list))
			(list (p2t=node (car (last line-list)) line-list))))
		 0
		 (p2t=defs-nodes-translate (cons node supports) pds defs)
		 (list 'comment "OMEGA proof (report problems to the OMEGA group)")
		 (list 'locked (mapcar #'car line-list)))
	  )))))
  
  (defun p2t=atpprb-object-create (output)
    (declare (edited  "21-JUL-1998 15:49")
	     (authors SORGEChris)
	     (input   "An sexpression representing a TPS proof object and a sexpression"
		      "representing the problem file send to TPS.")
	     (effect  "Creates an instance of an ATPPRB+TPS-PROBLEM.")
	     (value   "The newly created instance."))
    (let ((id (gensym "tps-problem-"))
	  (type 'tps))
      (atpprb~create-tps-problem id type nil nil output nil nil
				 p2t*typed-symbols p2t*unused-types
				 nil p2t*type-translation nil)))
  
  
  (defun p2t=initialize ()  ;MP: added THAT
    (let ((post-symbols (list "EXISTS" "FORALL" "AND" "OR" "NOT" "IMPLIES" "EQUIV" "=" "TRUE"  "FALSE" "THAT"))
	  (tps-symbols  (list "EXISTS" "FORALL" "AND" "OR" "~"  "IMPLIES" "EQUIV" "=" "TRUTH" "FALSEHOOD" "THAT")))
      (setf p2t*symbol-translation (pairlis post-symbols tps-symbols)))
    (setf p2t*plan-counter 0)
    (setf p2t*typed-symbols (list "EXISTS" "FORALL" "AND" "OR" "NOT"
				  "IMPLIES" "EQUIV" "LAM" "=" "THAT"))
    (setf p2t*unused-types  (list 'a 'b 'c 'd 'e 'f 'g 'h 'j 'k 'l 'm 'n 'p 'q 'r 't 'u 'v 'w 'x 'y 'z))
    (setf p2t*type-translation (acons 'NUM 's (acons 'KEIM::I 'i (acons 'KEIM::O 'o nil)))))
  
  
  (defun p2t=node (assoc-node node-list &key (set-just nil))
    (declare (edited  "20-JUL-1998 18:38")
	     (authors SORGE)
	     (input   "An OMEGA node related to a TPS label and an assoc list"
		      "of OMEGA nodes related to corresponding TPS labels.")
	     (effect  "None.")
	     (value   "A TPS representation of the node."))
    (let* ((omega-node (cdr assoc-node))
	   (label (car assoc-node))
	   (justification (node~justification omega-node))
	   (just (cond ((and set-just (stringp set-just)) set-just)
		       (set-just (symbol-name set-just))
		       ((pdsn~open-node-p omega-node)
			(read-from-string (format nil "PLAN~A" (incf p2t*plan-counter))))
		       (t (let ((meth-name (keim~name (just~method justification))))
			    (if (stringp meth-name) meth-name
			      (symbol-name meth-name))))))
	   (prems (unless set-just (just~premises justification))))
      (flet ((get-label (node)
			(car (rassoc node node-list))))
	(list label
	      (mapcar #'get-label (pdsn~hyps omega-node))
	      (p2t~formula (node~formula omega-node))
	      just
	      nil
	      (mapcar #'get-label prems)
	      (format nil "((OMEGA-LABEL ~A) (OMEGA-JUSTIFICATION ~A))"
		      (keim~name omega-node)
		      (cond ((pdsn~open-node-p omega-node) 'OPEN)
			    (set-just (keim~name (just~method justification)))
			    (t just)))))))
  
  
  (defun p2t~formula (formula)
    (declare (edited  "20-JUL-1998 16:57")
	     (authors SORGE)
	     (input   "A POST formula.")
	     (effect  "None.")
	     (value   "A string containing the TPS representation of the formula."))
    (with-output-to-string (str)
			   (p2t=formula formula str t)
			   str))
  
  (data~defgeneric p2t=formula ((object) outst &optional sch)
		   (declare (edited  "26-SEP-1997")
			    (authors gebhard)
			    (input  "The problem which is to translate to tps syntax."
				    "A stream to which the output is moved, and a decision parameter")
			    (effect "Writes the result to outst." )
			    (value  "A ass.list with the type-relation."))		 
		   (:method ((object term+appl) outst &optional sch)
			    (declare (ignore sch))
			    (format outst "[")
			    (let ((sw (p2t=formula (data~appl-function object) outst t)))
			      (mapcar #'(lambda (x) (p2t=formula x outst sw))
				      (data~appl-arguments object)))
			    (format outst "] ")
			    t)
		   (:method ((object term+abstr) outst &optional tp) ;; soll lambda geschrieben werden 
			    (when tp (format outst "[LAMBDA ")) 
			    (p2t=formula (data~abstr-c-binder object) outst t) ;; Achtung binder
			    ;; koennen mehrere
			    ;; Vars enthalten
			    ;; ... Aendern !!!
			    (p2t=formula (data~abstr-c-range object) outst t)
			    (when tp (format outst "] "))
			    t)
		   (:method ((object term+variable) outst &optional tp)
			    (declare (ignore tp))
			    (format outst "~A" (keim~name object))
			    (format outst "(") (p2t=formula (data~annotation object) outst t) (format outst ")")
			    t)
		   (:method ((object term+constant) outst &optional tp)
			    (declare (ignore tp))
			    (let ((object-name (keim~name object)))
			      (if (assoc object-name p2t*symbol-translation :test 'string=)
				  (format outst "~A" (cdr (assoc object-name p2t*symbol-translation :test 'string=)))
				(format outst "~A" object-name))
			      (when (string= "LAM" object-name)
				(format outst "Vulgaer:scheisse."))
			      (if (find object-name p2t*typed-symbols :test 'string=)
				  (format outst " ")
				(progn
				  (format outst "(") (p2t=formula (data~annotation object) outst t)(format outst ")")
				  ;(setf p2t*typed-symbols (append p2t*typed-symbols (list object-name)))
				  ;MP: error when a formula contain a polymorphic constant with different type instantiations
				  ))
			      (not (or (string= "FORALL" object-name)
				       (string= "EXISTS" object-name)
				       (string= "THAT" object-name)))))
		   (:method ((object term+schema) outst &optional tp)
			    (p2t=formula (data~schema-range object) outst tp))
		   (:method ((object list) outst &optional tp) ;; tp: toplevel oder nicht
			    (if tp
				(progn 
				  (cond ((= (length object) 1) (p2t=formula (car object) outst t)) ;;keine supports
					((= (length object) 2)   ;;nur eine supportline, keine konjunktion
					 (progn
					   (format outst "[implies ")
					   (p2t=formula (car object) outst t)        (format outst "~%")
					   (p2t=formula (car (last object)) outst t) (format outst "]")))
					(t
					 (progn
					   (format outst "[implies [")
					   (p2t=formula (butlast object) outst nil)  (format outst "~%")
					   (p2t=formula (car (last object)) outst t) (format outst "]"))))
				  p2t*type-translation)
			      (let ((kudda (cdr object)))
				(progn
				  (p2t=formula (car object) outst t)
				  (if kudda (progn (format outst " and ~%") (p2t=formula kudda outst nil))
				    (format outst "] "))))))
		   (:method ((object type+schema) outst &optional tp)
			    (p2t=formula (data~schema-range object) outst tp))
		   (:method ((object type+complex) outst &optional tp)
			    (unless tp (format outst "("))
			    (p2t=formula (data~n-range object) outst)
			    (mapcar #'(lambda (x) (p2t=formula x outst)) (reverse (data~n-domain object)))
			    (unless tp (format outst ")")))
		   (:method ((object type+constant) outst &optional tp)
			    (declare (ignore tp))
			    (cond ((assoc (keim~name object) p2t*type-translation)
				   (format outst "~A" (cdr (assoc (keim~name object) p2t*type-translation))))
				  (p2t*unused-types
				   (let ((act-typ (pop p2t*unused-types)))
				     (format outst "~A" (string-downcase act-typ))
				     (setf p2t*type-translation (acons (keim~name object) act-typ p2t*type-translation))))
				  (t (error "no type-symbols left for tps"))))
		   (:method ((object type+variable) outst &optional tp)     ;;; ganz brutaler Versuch (identisch mit constant); DEF
			    (declare (ignore tp))
			    (cond ((assoc (keim~name object) p2t*type-translation)
				   (format outst "~A" (cdr (assoc (keim~name object) p2t*type-translation))))
				  (p2t*unused-types
				   (let ((act-typ (pop p2t*unused-types)))
				     (format outst "~A" (String-Downcase act-typ))
				     (setf p2t*type-translation (acons (keim~name object) act-typ p2t*type-translation))))
				  (t (error "no type-symbols left for tps"))))
		   (:method ((object t) outst &optional tp)
			    (declare (ignore tp outst))
			    (error "none prepared method-case in p2t=formula")))
  
  ) ;;; end of global let

(defun p2t=enumerate-lines (line-list root supports open-lines);;; noch etwas elaborieren!!!
  (declare (edited  "20-JUL-1998 18:16")
	   (authors SORGE)
	   (input   "A list of lines, a root node, a list of support lines of the list of open-lines")
	   (effect  "None.")
	   (value   "A assoc-list with the lines enumerated in TPS style."))
  (let ((list line-list)
	(already-used))
    (reverse
     (do* ((lines list (cdr lines))
	   (n 1 (+ n 10))
	   (line (car lines) (car lines))
	   (new-lines (list (cons n line)) (acons n line new-lines)))
	 ((null (cdr lines)) new-lines)))))


(defun p2t=plans-entry (open-lines supports line-list)
  (declare (edited  "20-JUL-1998 18:28")
	   (authors SORGE)
	   (input   "A list of opne lines, a list of lists of corresponding support lines,"
		    "and a assoc list relating OMEGA nodes to TPS line numbers.")
	   (effect  "None.")
	   (value   "An entry for the PLANS slot in the TPS file."))
  (mapcar #'(lambda (line supports)
	      (cons (car (rassoc line line-list :test #'keim~equal))
		    (mapcar #'(lambda (support)
				(car (rassoc support line-list :test #'keim~equal)))
			    supports)))
	  open-lines
	  supports))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stuff for translation of definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun p2t=defs-nodes-translate (node-list &optional (pds omega*current-proof-plan) (prohibited-defs nil))
  (declare (edited  "20-JULI-1998")
	   (authors Chris)
	   (input   "A list of OMEGA Lines")
	   (effect  "None")
	   (value   "A string in TPS Syntax describing the definitions recursively"
		    "embedded in the input lines."))
  (let* ((embedded-defs (mapcan #'(lambda (node)
				    (p2t=contained-defs (node~formula node) pds))
				node-list))
	 (prohibited-defs-new
	  (append 
	   (mapcar #'(lambda (x)
		       (cond ((th~definition-p x) x)
			     ((or (stringp x) (symbolp x))
			      (let ((found-def (th~find-assumption x (prob~theory pds))))
				(if found-def found-def
				  (omega~error "Definition for ~A not found." x))))
			     (t (omega~error "~A is not a valid argument." x))))
		   prohibited-defs)
	   (list  ;;; these definitions shouldn't be redefined as TPS proof seacrh heavily
		  ;;; depends on them
	    (th~find-assumption 'equiv (prob~theory pds))
	    (th~find-assumption '= (prob~theory pds)))))
	 (new-embedded-defs
	  (remove-if
	   #'(lambda (def) (find def prohibited-defs-new
				 :test #'(lambda (def1 def2)
					   (string-equal (keim~name def1) (keim~name def2)))))
	   (reverse (remove-duplicates embedded-defs :test #'keim~equal)))))
    (mapcar #'p2t=definition new-embedded-defs)))


(defun p2t=contained-defs (formula &optional (pds omega*current-proof-plan))  
  (declare (edited  "20-JULI-1998")
	   (authors Chris)
	   (input   "A formula.")
	   (effect  "None")
	   (value   "All definitions recursively embedded in LINE."))
  (let* ((defs (th~definitions-recursed (prob~theory pds)))
	 (poslist (data~positions formula #'(lambda (x) (term~constant-p x))))
	 (constants (mapcar #'(lambda (pos) (data~struct-at-position formula pos))
			    poslist)))
    (mapcan #'(lambda (const)
		(let ((def (find-if #'(lambda (def) (keim~equal (keim~name const)
								(keim~name def)))
				    defs)))
		  (when def
		    (adjoin def (p2t=contained-defs (th~ass-node def) pds)
			    :test #'keim~equal))))
	    constants)))

(defun p2t=definition (def)  
  (declare (edited  "20-JULI-1998")
	   (authors Chris Sorge)
	   (input   "A definition.")
	   (effect  "None")
	   (value   "Translates DEF into TPS Syntax."))
  (let* ((name (keim~name def))
	 (formula (data~copy (th~ass-node def) :downto '(type+type)))
	 (type (data~annotation formula))
	 (typelist (when (data~schema-p formula)
		     (data~schema-domain formula)))
;;;	 (new-formula (data~alpha-copy formula nil))
	 (mhelp (help~help-string def))
	 ;;         (prefix nil)
	 ;;         (infix nil)
	 )
    (list 'def-abbrev name
	  (list 'type (p2t~formula type))
	  (list 'typelist (mapcar #'p2t~formula typelist))
	  (list 'printnotype t)
	  (list 'face name)
	  (list 'fo-single-symbol t)
	  (list 'defn (p2t~formula formula))
	  (list 'mhelp mhelp)
	  )))
