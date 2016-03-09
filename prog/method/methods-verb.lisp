;;; -*- Syntax: Common-lisp; package: OMEGA; base: 10; mode: keim -*-
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; methods-verb.lisp; This file is part of the OMEGA system
;;
;; major updates: 5.10.1999
;; 
;;
;; Authors: Carsten Ullrich
;; email: cullrich@ags.uni-sb.de 
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

(in-package "OMEGA")

; this file contains the functions needed for the verbalization of methods


(mod~defmod MVERB 
            :uses (data env infer just keim logic meth node omega parse pds pdsj pdsn prob socket term)
            :documentation "Verbalization of methods."
            :exports (
                      
                      mverb~back
                      mverb~forward
                      mverb~get-construction-style-verbalization
                      mverb~get-detailed-verbalization
                      mverb~get-local-verbalization
                      mverb~get-remark
                      mverb~get-textbook-style-verbalization
                      mverb~parse
                      mverb~parse-complete-term
                      mverb~parse-constraint-store
                      mverb~parse-term
                      mverb~verbalize
                      
                      mverb*counter
                      mverb*history
                      mverb*subplan-methods
                      mverb*variants-for-action
                      mverb*variants-for-cause
                      mverb*variants-for-complexestimate
                      mverb*variants-for-conclusion
                      mverb*variants-for-estimations
                      mverb*variants-for-motivation
                      mverb*variants-for-motivation2
                      mverb*variants-for-reasoning
                      mverb*variants-for-restrictions))



(defvar mverb*counter (list 0) "Needed for the recursive numbering of the steps")

(defvar mverb*subplan-methods '(tellcs-m askcs-m solve*<-m solve*<-<=-m solve*<=-<-m
  solve*>-m solve*-leq-m complexestimate<-m ComplexEstimate>-m FactorialEstimate-m
  EnvEstimate<-m))

(defun mverb~parse (string env just &optional indicator)
  (cond
   ((eql indicator 'term)
    (let ((term (mverb=parse-until "</TERM>" string nil)))
      (if term
	  (let ((str (mverb=get-object-for-string (first term)
						  #'(lambda (x)
						      (format nil "(~{~A~^ ~})" x))
						  #'(lambda (x)
						      (let ((str (parse~term x)))
							(subseq str
								1
								(- (length str) 1))))
						  env just)))
	    (concatenate 'string
			 "<TERM>"
			 str
			 "</TERM>"
			 (mverb~parse (second term) env just))))))
   ((eql indicator 'get-local-verbalization)
    (let ((node (mverb=parse-until "\"" string nil)))
      (if node
	  (concatenate 'string
		       "<A HREF=\"ltp:get-local-verbalization"
		       (mverb=get-object-for-string (first node) #'identity
						    #'(lambda (x)
							(format nil " ~a" (keim~name x)))
						    env just)
		       "\""
		       (mverb~parse (second node) env just)))))
   ((eql indicator 'get-detailed-verbalization)
    (let ((node (mverb=parse-until "\"" string nil)))
      (if node
	  (concatenate 'string
		       "<A HREF=\"ltp:get-detailed-verbalization"
		       (mverb=get-object-for-string (first node) #'identity
						    #'(lambda (x)
							(format nil " ~a" (keim~name x)))
						    env just)
		       "\""
		       (mverb~parse (second node) env just)))))
   
   ((eql indicator 'lisp)
    (let ((command (mverb=parse-until "</LISP>" string nil)))
      (if command
	  (let ((to-do (mverb=get-object-for-string (first command) #'identity #'identity
						    env just)))
	    (concatenate 'string
			 (apply (first to-do) (rest to-do))
			 (mverb~parse (second command) env just))))))

   ((not indicator)
     (when (> (length string) 0)
	 (let* ((s (char string 0));; get the first character
		(rest-string (subseq string 1)));; get the rest of the string
	   (if (char= s #\<) ;; if the first character is a '<' then a tag may follow:
	       (let ((result (mverb=parse-is-prefix-p "TERM>" rest-string)))
		 ;; is the tag "TERM", then parse for terms
		 (if result (mverb~parse result env just 'term)
		   (let ((result (mverb=parse-is-prefix-p "A HREF=\"ltp:get-local-verbalization" rest-string)))
		     ;; else is the tag a reference for get-local-verbalization, parse the following node
		     (if result (mverb~parse result env just 'get-local-verbalization)
		       (let ((result (mverb=parse-is-prefix-p "LISP>" rest-string)))
			 (if result (mverb~parse result env just 'lisp)
			   (let ((result (mverb=parse-is-prefix-p "A HREF=\"ltp:get-detailed-verbalization" rest-string)))
			     (if result (mverb~parse result env just 'get-detailed-verbalization)
			   (let ((result (mverb=parse-is-prefix-p "VERB-HYPS>" rest-string)))
			     (if result (mverb~parse result env just 'verb-hyps)
;	       (let ((result (parse-until ">" rest-string nil)))
;		 (if (and (first result) (member (first result) '("TERM") :test #'string-equal))
			       ;; else (< was not an indicator), continue to parse normaly
			       (concatenate 'string "<" (mverb~parse rest-string env just)
					    )))))))))))
	     (concatenate 'string (coerce (list s) 'string) (mverb~parse rest-string env just))
	     ))))
   (t (format t "unknown indicator ~A" indicator))
   ))


(defun mverb=parse-until (object string up-string)
;; returns nil or and a list:the string up to the object and the rest string 
  (unless (= 0 (length string)) ;; the string is empty, so we did not find the object
    (let ((prefix? (mverb=parse-is-prefix-p object string)))
      ;; we get the prefix
      (if prefix?
	  (list up-string prefix?) ;prefix? containts the rest-string
	(let* ((r (char string 0)) ;; get the first character
	       (rest-string (subseq string 1))) ;; get the rest of the string
	  (mverb=parse-until object rest-string (concatenate 'string up-string (coerce (list r) 'string))))))))

(defun mverb=parse-is-prefix-p (string1 string2)
;; return nil if string1 is not a prefix of string2, string2 without the prefix otherwise
  (if (= 0 (length string1)) string2
    (and (> (length string2) 0)
	 (string-equal (char string1 0) (char string2 0))
	 (mverb=parse-is-prefix-p (subseq string1 1) (subseq string2 1)))))



;;----------------------------------
;; replacing objects in the remarks
;;----------------------------------

(defun mverb=get-object-for-string (string fct1 fct2 pds-env just)
  (mverb=get-object-for-string-h (read-from-string string)
				 fct1 fct2 pds-env just))

(defun mverb=get-object-for-string-h (symbols fct1 fct2 pds-env just)
  (if (listp symbols)
      (apply fct1 (list (mapcar #'(lambda (sub-symbol)
				    (mverb=get-object-for-string-h sub-symbol fct1 fct2 pds-env just))
				symbols)))
    (apply fct2 (list (mverb=replace-symbol symbols pds-env just)))))

(defun mverb=replace-symbol (symbol pds-env just)
  (or (let ((infer (just~method just)))
	(cond ((infer~method-p infer)
	       (let ((method (mverb=get-inference-application infer (pdsj~outline-pattern just)))
		     (mapping (pdsj~subst just)))
		 (meth~mapp-get-component (env~lookup-object symbol (meth~environment method)) mapping :both)))
	      ((infer~bbox-p infer)
	       (let ((state-des (first (pdsj~parameters just))))
		 (cond ((eql symbol 'state-des) state-des)
		       (t nil))))
	      (t nil)))
      (env~lookup-object symbol pds-env)
      symbol))

;;------------------------------
;; verbalising routines
;;------------------------------

(defun mverb=strat-just-of-node (node)
  (let* ((start-steps (remove-if-not #'(lambda (step) (keim~equal (infer~find-method 'start-strategy-ks-application)
								  (just~method (scon~strategy-step-just step))))
				     (scon~all-steps)))
	 (step-of-node (find-if #'(lambda (step)
				    (keim~equal node (agenda~task-node (roc~start-task (first (pdsj~parameters (scon~strategy-step-just step)))))))
				start-steps)))
    (when step-of-node
      (scon~strategy-step-just step-of-node))))

(defun mverb=parse-node (node pds class type)
  (declare (ignore type))
  (let* ((infer (just~method (node~justification node)))
	 (just (cond ((infer~method-p infer)
		      (node~justification node))
		     ((infer~bbox-p infer)
		      (mverb=strat-just-of-node node))
		     (t nil)))
	 (remark (cond ((infer~method-p infer)
			(meth~remark (mverb=get-inference-application infer (pdsj~outline-pattern just))))
		       ((infer~bbox-p infer)
			(strat~strategy-ks-remark (roc~strategy-ks (first (pdsj~parameters just)))))
		       (t ""))))
    (if (infer~dummy-p infer)
	(concatenate 'string
		     "By hypothesis "
		     (mverb~parse-complete-term (node~formula node))
		     ); not "." because of solve*<(>)
      (mverb~parse
       (mverb~get-remark class remark)
       (pds~environment pds)
       just))))



(defun mverb~verbalize (node-h pds class type &optional out)
  (declare (edited  "12-JUL-1999")
	   (authors Cullrich)
	   (input   "A node, a pds, the class (local, detailed, global-cons,"
		    "global-text), the type (sub (if it is a node from a subplan),"
		    "sub-with-hyps (if the hypothesis should be verbalized),"
		    "next (if it just follows another mode)), and out (no-browse"
		    "doesn't browse)."
	   (effect  "Sends the lml-string to the lml-browser or to standard output"
		    "(if no-browse is set)")
	   (value   "not important")))
  ;; we get the important nodes (the ones which have remarks) and create a verbalization
  (let ((node (if (pdsn~p node-h) node-h (pds~label2node node-h))))
    (case out
      (debug (format t "browseLML(~s)"
		   (mverb=verbalize
		    node
		    (mverb=get-important-premises (list node)
						  nil
						  class type)
		    pds class type)
		   ))
      (no-browse (mverb=verbalize node ;; we enter this case only when we make a global verbalization
				  (mverb=get-important-premises (list node)
								nil
								class type)
				  pds class type)
		 )
      
      (t ;;if we make a global verbalization, we add the goal to prove and verbalize the rest
       (if (or (eq class 'global-cons) (eq class 'global-text))
	   (let ((nodes (mverb=get-important-premises (list node)
						      nil
						      class type)))
	     (when (and nodes (not (eq nodes 'open)))
	       (let ((text1 (format nil "selectByLabel(~s)" (format nil "~a" (keim~name (first nodes)))))
		     (text2 (format nil "browseLML(~s)"
				    (concatenate 'string "We want to prove "
						 (mverb~parse-complete-term (node~formula node))
						 ".<BR><BR>"
						 (mverb=verbalize node
								  nodes
								  pds class type)
						 "<BR>Q.E.D."))))
		 ;; output
		 (socket~write text1)
		 (socket~write text2)
		 ;; save the texts in the history
		 (push (list text1 text2) (first mverb*history)))))
       ;; otherwise, we do not make a global verbalization, so we just verbalize the node
	 (let ((nodes (mverb=get-important-premises (list node)
						    nil
						    class type)))
	   ;(when (and nodes (not (eq nodes 'open)))
	   (let ((text1 (format nil "selectByLabel(~s)" (format nil "~a" (keim~name (or (first nodes) node)))))
		 (text2 (format nil "browseLML(~s)" ;;if there is only one node then cut
						    ;;the leading 1.
				(if (> (length nodes) 1)
				    (mverb=verbalize node
						     nodes
						     pds class type)
				  (subseq
				   (mverb=verbalize node
						    nodes
						    pds class type)
				   6)
				  ))))
	     ;; output
	     (socket~write text1)
	     (socket~write text2)
	     ;; save the texts in the history
	     (push (list text1 text2) (first mverb*history))
	     )
	     
	   ))))))


(defun mverb=verbalize (node important-nodes pds class type)
  (let (;(mverb*counter (or mverb*counter '(0)))
	(text ""))
    (if (consp important-nodes)
	(if (> (length important-nodes) 1)
	    ;(concatenate 'string ;"<BLOCKQUOTE><BR>"
	    (dolist (premise important-nodes text)
	      (setf text
		    (concatenate 'string
				 text
				 ;"<BR>"
				 (if (or(eq type 'sub-with-hyps)(eq type 'sub))
				     ""
				   (concatenate 'string
						"<BR>"
						(item))) ;;wird fuer solve* gebraucht, da unwraphyp
				                ;;in dem fall kein item haben soll
				 ;(let ((mverb*counter (mverb=increase-depth mverb*counter)))
				 (mverb=parse-node premise pds class type)
				 ;"<BR>"
				 ))
	      ;)
	      )
	  ;; else there is only one important-node, so use a different verbalization
	  (concatenate 'string
		       ;"<BR>"
		       (if (or(eq type 'sub-with-hyps)(eq type 'sub))
			   ""
			 (concatenate 'string
				      "<BR>"
				      (item))) ;; s.o.
		       (mverb=parse-node (first important-nodes) pds class type)
		       ;"<BR>"
		       ))
      ;; there is no important important-nodes:
      (if (eq important-nodes 'open)
	  (concatenate 'string
		       "      " ;;need in order to be able to cut the leading numbers
		       (mverb~parse-complete-term (node~formula node))
		       " is still open!")
	(if (eq class 'local)
	    (concatenate 'string
			 "      " ;;need in order to be able to cut the leading numbers
			 (mverb~parse-complete-term (node~formula node))
			 " is obvious."))))))
  
(defun mverb=get-important-premises (nodelist result class type)
  ;; nodeslist is a list of nodes. As we try to find the important nodes, we check if the
  ;; nodes has a remark. If so, it is an important node, else we check his premises.
  (if nodelist
      (let* ((first-node (first nodelist))
	     (just (node~justification first-node))
	     (infer (just~method just))
	     (method (meth~find-method  (mverb=get-inference-application
		       (just~method just)
		       (pdsj~outline-pattern just))))
	     (method-remark (and method (meth~remark method))))
	(if (infer~open-p infer)
	    'open
	  (if  ;;we want to verbalize if the node is a hypothesis or has a remark:
	      (or (and (eq type 'sub-with-hyps)
		       (infer~dummy-p infer))
		  (and method-remark (mverb~get-remark class method-remark)
		       (not (string= "" (mverb~get-remark class method-remark))))
		  (and (infer~bbox-p infer))
		   )
	      (mverb=get-important-premises (rest nodelist) (cons first-node result) class
					    type)
	    (mverb=get-important-premises (append (rest nodelist) (reverse (pdsn~just-premises first-node)))
					  result class type)
	    
	    )))
    (reverse result)))
      

   

(defun mverb~parse-term (term)
  (let ((str (parse~term term)))
    (subseq str
	    1
	    (- (length str) 1))))

(defun mverb~parse-complete-term (term)
  (let ((str (parse~term term)))
    (concatenate 'string
		 "<TERM>"
		 (subseq str
			 1
			 (- (length str) 1))
		 "</TERM>"
		 )))



(defun mverb~get-remark (class remark)
  ;; remarks can still have the follwing three forms:
  ;; 1. (""): no remark
  ;; 2. ("x" ...) just the lokal remark (maybe with more detailed verbs)
  ;; 3. (("x" ..) ("y" ...)) the local and global remarks
  (if (and (consp remark) (consp (first remark))) ;;case 3
      (case class
	(local (first (first remark)))
	(detailed (second (first remark)))
	(global-text (first (second remark)))
	(global-cons (or (second (second remark)) (first (second remark))))
;	(global-cons-with-hyps (or (second (second remark)) (first (second remark))))
;	(global-text-with-hyps (first (second remark)))
	(t ""))
    (case class ;; else (case 1 and 2)
      (local (if (consp remark) (first remark) remark))
      (t ""))))

;;----------------------------------------
;; global verbalization
;;----------------------------------------

(defun verbalize-cons-subproof (node)
  ;; for the methods
;  (let ((mverb*counter (mverb=increase-depth mverb*counter)))
;    (mverb~verbalize node omega*current-proof-plan 'global-cons 'sub 'no-browse))
  (let ((method (keim~name (pdsn~just-method node)))
	)
    (if (member method mverb*subplan-methods)
	(concatenate 'string
		     (mverb~verbalize node omega*current-proof-plan 'global-cons 'sub
				      'no-browse))
      (let ((mverb*counter (mverb=increase-depth mverb*counter)))
	(concatenate 'string
		     "<BR>"
		     (item)
		     (mverb~verbalize node omega*current-proof-plan 'global-cons 'sub
				      'no-browse))
	    ))))

(defun verbalize-cons-subproof-with-hyps (node)
  ;; needed for solve*<(>): Normally we do not want to verbalize unimportant nodes and hypothesises.
  ;; But sometimes (solve*<(or >)) we need to verbalize a hypothesis. Otherwise it would not be clear
  ;; why the node is justified.
;  (let ((mverb*counter (mverb=increase-depth mverb*counter)))
  (mverb~verbalize node omega*current-proof-plan 'global-cons 'sub-with-hyps 'no-browse))

(defun verbalize-text-subproof-with-hyps (node)
  ;; needed for solve*<(>): Normally we do not want to verbalize unimportant nodes and hypothesises.
  ;; But sometimes (solve*<(or >)) we need to verbalize a hypothesis. Otherwise it would not be clear
  ;; why the node is justified.
  ;(let ((mverb*counter (mverb=increase-depth mverb*counter)))
    (mverb~verbalize node omega*current-proof-plan 'global-text 'sub-with-hyps 'no-browse))

(defun verbalize-text-subproof (node)
  ;; for the methods
  (let ((method (keim~name (pdsn~just-method node)))
	)
    (if (member method mverb*subplan-methods)
	(concatenate 'string
		     (mverb~verbalize node omega*current-proof-plan 'global-text 'sub
				      'no-browse))
      (let ((mverb*counter (mverb=increase-depth mverb*counter)))
	(concatenate 'string
		     "<BR>"
		     (item)
		     (mverb~verbalize node omega*current-proof-plan 'global-text 'sub
				      'no-browse))
	    ))))

(defun verbalize-text-next (node)
;  (let* ((i (item))
;         (mverb*counter (mverb=increase-depth mverb*counter))
;         ;; why do we increase the depth??
;         (text (mverb~verbalize node omega*current-proof-plan 'global 'no-browse)))
;    (unless (or (not text) (string= "" text))
;      (concatenate 'string
;                   i
;                   text))))
  (mverb~verbalize node omega*current-proof-plan 'global-text 'next 'no-browse))
  
(defun verbalize-cons-next (node)
  (mverb~verbalize node omega*current-proof-plan 'global-cons 'next 'no-browse))
#|
(defun verbalize-text-subproof-prems (premises)
  (concatenate 'string
	       "<A HREF=\"ltp:get-next-lml "
	       (format nil "~A" (keim~name(first premises)))
	       "\"> 1. </A>"
	       (mverb~parse-complete-term (node~formula(first premises)))
	       "<BR><BLOCKQUOTE>"
	       (verbalize-text-subproof (keim~name(first premises)))
	       "</BLOCKQUOTE><BR>"
	       "and <A HREF=\"ltp:get-next-lml "
	       (format nil "~A" (keim~name(second premises)))
	       "\"> 2. </A>"
	       (mverb~parse-complete-term (node~formula(second premises)))
	       "<BR><BLOCKQUOTE>"
	       (verbalize-text-subproof (keim~name(second premises)))
	       "</BLOCKQUOTE><BR>"
	       ))
|#
;;----------------------------------------
;; functions for the commands
;;----------------------------------------
(defun mverb~get-detailed-verbalization (node)
  (let ((mverb*counter '(0)))
    (mverb~verbalize node omega*current-proof-plan 'detailed 'not-important)))

(defun mverb~get-local-verbalization (node)
  (let ((mverb*counter '(0)))
    (mverb~verbalize node omega*current-proof-plan 'local 'not-important)))

(defun mverb~get-textbook-style-verbalization ()
  (let ((mverb*counter '(0)));(if mverb*counter (mverb=increase-depth mverb*counter) '(0))))
    (mverb~verbalize (prob~proof-root omega*current-proof-plan)
		     omega*current-proof-plan 'global-text 'not-important)))
  
(defun mverb~get-construction-style-verbalization ()
  (let ((mverb*counter '(0)));(if mverb*counter (mverb=increase-depth mverb*counter) '(0))))
    (mverb~verbalize (prob~proof-root omega*current-proof-plan)
		     omega*current-proof-plan 'global-cons 'not-important)))

(defun mverb~parse-constraint-store (node-label)
  (socket~write
   (concatenate 'string
		"browseLML(\"We assume that<BR><TERM>"
		(mverb~parse-term (node~formula (pds~label2node node-label)))
		"</TERM><BR>The verbalization of the constraint-store will be nicer
soon!\")")))

;;----------------------------------------
;; functions for the back/forward buttons
;;----------------------------------------

(defvar mverb*history '(nil nil))
;; the history is list: first the predecessors (backward), then the successors (forward).
;; the items consists of two texts: first the blinking instruction, then the actual text.

(defun mverb~back ()
  (let ((predecessor (second (first mverb*history))))
    ;; if we do have only one text, we cannot go back!
    (when predecessor
      (let ((ac-text (pop (first mverb*history))))
	; first let the predecessor blink
	(socket~write (first predecessor))
	;; then browse the text
	(socket~write (second predecessor))
	;; then save the old actual text
	(push ac-text (second mverb*history))
	))))

(defun mverb~forward ()
  (let ((successor (pop (second mverb*history))))
    (when successor
      (progn ; first let the successor blink
	(socket~write (first successor))
	;; then browse the text
	(socket~write (second successor))
	;; then save it
	(push successor (first mverb*history))
	))))
       
		    

;;----------------------------------------
;; functions that should belong to keim
;;----------------------------------------
(defun mverb=get-inference-application (infer outln-pat)
  (when (or (infer~method-p infer) (infer~supermethod-p infer))
    (pds~inference-application
     infer
     (mapcar #'(lambda (pattern)
		 (if (stringp pattern) pattern
		 "NONEXISTENT"))
	     outln-pat)
     )))



;;=========================================
;; verbalizations for methods
;;=========================================

;; ------------------BASE----------------------

;; itemizing
(defun item ()
  ;; increase the pointer by one (no depth added!)
;  (let ((text (format nil "~{~A~^.~}. " mverb*counter)))
  (format nil "~{~A~^.~}. "
	  (setf mverb*counter 
		(append (butlast mverb*counter)
			(list (1+ (first (last mverb*counter))))))))
    ;text))

(defun increase-depth()
  (progn
    (setf mverb*counter (append mverb*counter (list 0)))
    "")
  )

(defun decrease-depth ()
  (progn
    (setf mverb*counter (butlast mverb*counter))
    "")
  )

(defun mverb=increase-depth (counter)
  ;; counter is of the form (a b c...). We add a level -> (a b c... 0)
  (append counter (list 0)))

;; linguistic variants
(defvar mverb*variants-for-cause '("As " "Because of " "Since "))

(defun get-cause ()
  (nth (random (length mverb*variants-for-cause))
       mverb*variants-for-cause))
;---------------
(defvar mverb*variants-for-conclusion '(" we know " " it follows " " it holds "
					" we infer " " we conclude "))   ;" we can show ")) ;

(defun get-conclusion ()
  (nth (random (length mverb*variants-for-conclusion))
       mverb*variants-for-conclusion))
;---------------
(defvar mverb*variants-for-motivation '("As we need to " "As we want to " "In order to "
					"As we have to "))
(defun get-motivation ()
  (nth (random (length mverb*variants-for-motivation))
               mverb*variants-for-motivation))
;---------------
(defvar mverb*variants-for-motivation2 '("we need to " "we want to " "we have to "
"it suffices to "))

(defun get-motivation2 ()
  (nth (random (length mverb*variants-for-motivation2))
               mverb*variants-for-motivation2))
;---------------

(defvar mverb*variants-for-complexestimate '("We estimate this magnitude with the
following three steps:" "The following three steps estimate this magnitude:" "This
magnitude can be established by the following three steps:" "This magnitude can be approximated with the following
three steps:"))

(defun get-complex-estimate-variants()
  (nth (random (length mverb*variants-for-complexestimate))
       mverb*variants-for-complexestimate))
;---------------
(defvar mverb*variants-for-estimations '(" estimate " " establish " " determine "))

(defun get-estimate-variants()
  (nth (random (length mverb*variants-for-estimations))
       mverb*variants-for-estimations))
;---------------
(defvar mverb*variants-for-reasoning '("Now " "Hence " "Therefore " "Consequently "))

(defun get-reasoning-variants ()
  (nth (random (length mverb*variants-for-reasoning))
       mverb*variants-for-reasoning))
;---------------
(defvar mverb*variants-for-restrictions '("From the collected restrictions follows" "From
the restrictions we can infer" "The collected restrictions imply"))

(defun get-restriction-variants()
  (nth (random (length mverb*variants-for-restrictions))
       mverb*variants-for-restrictions))
;---------------
(defvar mverb*variants-for-action '(" prove " " show "))

(defun get-action-variants()
  (nth (random (length mverb*variants-for-action))
       mverb*variants-for-action))
;----------------
;for complexestimate
(defun header()
  ;; we have different verbalizations depending on the level the method is applied. If the
  ;; method was applied on top level, we verbalize an introduction, otherwise if the
  ;; method was applied in the subplan of another method, we give a shorter
  ;; verbalization. The current level is give by the length of the item counter.
  (if (> (length mverb*counter) 1)
      "Since "
    (concatenate 'string
		 (get-motivation)
		 (get-estimate-variants)
		 "the magnitude of <BR>")))


;; ------------------LIMIT---------------------
;;--- solve*-S ;; obsolete???
;(defun get-first-verb-for-solve (prec conc)
;  (concatenate 'string
;               "<A HREF=\"ltp:get-next-lml "
;               (format nil "~A" (keim~name (second prec)))
;               "\"> 1. </A>"
;               (mverb~parse-complete-term (node~formula (second prec)))
;               ))

;  (omega~message "~S ~S" prems conc))

;(defun get-second-verb-for-solve (prec conc)
;  (let* ((n1 (node~formula (second prec)))
;         (n2 (node~formula conc))
;         (e1 (second (data~appl-arguments n1)))
;         (e2 (second (data~appl-arguments n2)))
;         (e-rel-e2 (data~appl-create
;                    (if (eq (keim~name (data~appl-function n1))
;                            'greater)
;                        (env~lookup-object 'geq (pds~environment
;                                                 omega*current-proof-plan))
;                      (env~lookup-object 'leq (pds~environment omega*current-proof-plan)))
;
;                                    (list e1 e2)))
;         )
;    (concatenate 'string
;                 "<A HREF=\"ltp:get-local-verbalization-for-constraint-state "
;                 (format nil "~A" (keim~name (first prec)))
;                 "\"> 2. </A>"
;                 (mverb~parse-complete-term e-rel-e2)
;                 )))

  
;;;---------------verbalizations for unwrap-hyp
(defun verbalize-hyp (node)
;  (mverb=verbalize-formula (node~formula (mverb=find-hyp node))))
  (mverb~parse-complete-term (node~formula (mverb=find-hyp node))))
  
(defun mverb=find-hyp (nodes)
  (when nodes
    (if (infer~dummy-p (pdsn~just-method (first nodes)))
	(first nodes)
      (mverb=find-hyp (append (rest nodes)
			      (pdsn~just-premises (first nodes)))))))

(defgeneric mverb=verbalize-formula (node)
  (:method ((term cons))
	   (format nil "~{~A~^ and ~}" (mapcar #'mverb~parse-complete-term term)))
  
  (:method ((term term+constant))
	   (mverb~parse-complete-term term))

  (:method ((term term+variable))
	   (mverb~parse-complete-term term))

  (:method ((term term+appl))
	   (case (keim~name (data~appl-function term))
	     (and (format nil " ~A and ~A"
			  (mverb=verbalize-formula (first (data~appl-arguments term)))
			  (mverb=verbalize-formula (second (data~appl-arguments term)))))
	     (or (format nil " ~A or ~A"
			  (mverb=verbalize-formula (first (data~appl-arguments term)))
			  (mverb=verbalize-formula (second (data~appl-arguments term)))))
	     (implies (format nil " if ~A then ~A"
			  (mverb=verbalize-formula (first (data~appl-arguments term)))
			  (mverb=verbalize-formula (second (data~appl-arguments term)))))
	     (forall (let* ((arg (first (data~appl-arguments term)))
			    (range (data~abstr-range arg))
			    (domain (data~abstr-domain arg)))
		       (format nil " for all ~A it holds that ~A"
			       (mverb=verbalize-formula domain)
			       (mverb=verbalize-formula range))))
	     (exists (let* ((arg (first (data~appl-arguments term)))
			    (range (data~abstr-range arg))
			    (domain (data~abstr-domain arg)))
		       (format nil " there exists a ~A such that ~A"
			       (mverb=verbalize-formula domain)
			       (mverb=verbalize-formula range))))
;	     ((less leq greater geq)
;	      
	     (t (mverb~parse-complete-term term))))
	     
  (:method ((term term+abstr))
	   (mverb~parse-complete-term term)))

(defun verbalize-prems (premises)
  (concatenate 'string
	       "<A HREF=\"ltp:get-local-verbalization "
	       (format nil "~A" (keim~name(first premises)))
	       "\"> 1. </A>"
	       (mverb~parse-complete-term (node~formula(first premises)))
	       "and <A HREF=\"ltp:get-local-verbalization "
	       (format nil "~A" (keim~name(second premises)))
	       "\"> 2. </A>"
	       (mverb~parse-complete-term (node~formula(second premises)))
	       ))

;; one alternative for unwrap-hyp:
;; all introduced subgoals are completly verbalized
#|
(defun verbalize-prems2 (goals premises)
  ;; we search for important premises (if there are none, we do not verbalize)
  (let ((ac-premises (mverb=get-important-premises premises nil 'global-cons)))
    (if ac-premises
	(let ((mverb*counter (butlast mverb*counter)))
	  (concatenate 'string
		       ",<BR>we can show "
		       (mverb~parse-complete-term (node~formula (first goals)))
		       " by the following steps:<BR><BLOCKQUOTE>"
		       (mverb=verbalize (first goals) ac-premises omega*current-proof-plan 'global-cons)
		       "</BLOCKQUOTE>"))
      " daswffww ewrwrs")))
|#

;; another alternative for unwrap-hyp
;; only the restrictions (method solve-m-b) are verbalized
(defun collect-restrictions (premises)
  (let ((restrictions (mverb=get-restriction-nodes premises nil)))
    (if restrictions
	(format nil ",<BR>if <B>we restrict ~{~A~^, ~} and ~a</B>,<BR>then"
		(mapcar #'(lambda (term)
			    (mverb~parse-complete-term (node~formula term)))
			(butlast restrictions))
		(mverb~parse-complete-term (node~formula (first (last restrictions)))))
      "")))

(defun mverb=get-restriction-nodes (nodelist result)
  ;; almost identical to mverb=get-important-premises
  ;; nodeslist is a list of nodes. As we try to find the important nodes, we check if the
  ;; nodes has a remark. If so, it is an important node, else we check his premises.
  (if nodelist
      (let* ((first-node (first nodelist))
	     (just (node~justification first-node))
	     (infer (mverb=get-inference-application
		       (just~method just)
		       (pdsj~outline-pattern just)))
	     (method (meth~find-method infer)))
	(if (or (infer~open-p infer) (not (eq (keim~name method) 'tellcs-m-b)))
	    (mverb=get-restriction-nodes (append (rest nodelist) (pdsn~just-premises first-node))
					 result)
	  (mverb=get-restriction-nodes (rest nodelist) (cons first-node result))
	  ))
  (reverse result)))


(defun verbalize-prec (prec)
  (concatenate 'string
;	       "<A HREF=\"lpt:get-next-lml "
;	       (format nil "~A" (keim~name(first prec)))
;	       "\"> hypothesis </A> <BR><TERM>"
	       "hypothesis<BR>"
	       (mverb~parse-complete-term (node~formula(first prec)))
	       ))

(defun verbalize-concls (concls)
  (mverb~parse-complete-term (node~formula(first concls))))
  

;; solve*<(>)-m
(defun verbalize=conjunct (term)
  (if (logic~truth-constant-p term) ""
    (concatenate 'string
		 " and "
		 (mverb~parse-complete-term term))
    ))

;;; solve:

;(defun get-term-to-restrict (a b)
;  (if (data~appl-p b) (mverb~parse-complete-term a) (mverb~parse-complete-term b)))
;
;(defun get-verbalization-for-predicate (rel a b)
;  (if (data~appl-p b) ;; dann ist a die einzuschraenkende Variable
;      (case (keim~name rel)
;        (less (format nil "smaller than"))
;        (leq (format nil "equal or smaller than"))
;        (= (format nil "equal to"))
;        (greater (format nil "greater than"))
;        (geq (format nil "equal or greater than")))
;    (case (keim~name rel) ;; sonst ist b die einzuschraenkende Variable
;        (less (format nil "greater than"))
;        (leq (format nil " equal or greater than"))
;        (= (format nil "equal to"))
;        (greater (format nil "smaller than"))
;        (geq (format nil "equal or smaller than")))
;    ))
;
;(defun get-other-term (a b)
;  (if (data~appl-p b) (mverb~parse-complete-term b) (mverb~parse-complete-term a)))

;; internal analogy

(defun verbalize-node (label)
  (mverb~parse-complete-term (node~formula label)))

(defun verbalize-term-list (terms)
  (cond ((null terms) "")
	((= (length terms) 1) (mverb~parse-complete-term (first terms)))
	((= (length terms) 2) (concatenate 'string (mverb~parse-complete-term (first terms)) "<BR>and " (mverb~parse-complete-term (second terms))))
	(t (concatenate 'string (mverb~parse-complete-term (first terms)) ",<BR>" (verbalize-term-list (rest terms))))))

(defun verbalize-node-list (nodes count)
  (when nodes
    (concatenate 'string
		 (format nil "<A HREF=\"ltp:get-local-verbalization ~A\"> ~A. </A> " (first nodes) count)
		 (mverb~parse-complete-term (node~formula (first nodes))) "<BR>"
		 (verbalize-node-list (rest nodes) (+ count 1)))))

(defun verbalize-outlines (state-des)
  (verbalize-node-list (roc~pplanner-outline-lines state-des) 1))

(defun verbalize-text-outlines-subproofs (state-des)
  (apply 'concatenate (cons 'string (mapcar #'(lambda (node) (concatenate 'string
									  "<BR>"
									  (item)
									  "We have to show "
									  (mverb~parse-complete-term (node~formula node))
									  "<BLOCKQUOTE>Proof: "
									  (verbalize-text-subproof-with-hyps node)
									  "</BLOCKQUOTE><BR>"))
					    (roc~pplanner-outline-lines state-des)))))

(defun verbalize-cons-outlines-subproofs (state-des)
  (apply 'concatenate (cons 'string (mapcar #'(lambda (node) (concatenate 'string
									  "<BR>"
									  (item)
									  "We have to show "
									  (mverb~parse-complete-term (node~formula node))
									  "<BLOCKQUOTE>Proof: "
									  (verbalize-cons-subproof-with-hyps node)
									  "</BLOCKQUOTE><BR>"))
					    (roc~pplanner-outline-lines state-des)))))

(defun verbalize-number-of-outlines (state-des)
  (format nil "~A" (length (roc~pplanner-outline-lines state-des))))

(defun verbalize-start-task (state-des)
  (mverb~parse-complete-term (node~formula (agenda~task-node (roc~start-task state-des)))))

(defun verbalize-first-parameter (state-des)
  (format nil "~A" (first (roc~parameters state-des))))

(defun verbalize-unwrapped (state-des)
  (mverb~parse-complete-term (data~struct-at-position (node~formula (first (roc~parameters state-des)))
						      (second (roc~parameters state-des)))))

