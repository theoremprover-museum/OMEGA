;;; -*- syntax: common-lisp; package: OMEGA; base: 10; mode: LISP -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     OMEGA Project                                                        ;;
;;     AG Siekmann/FB Informatik                                            ;;
;;     Universitaet des Saarlandes                                          ;;
;;     Bau 36, 4. Stock                                                     ;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file contains parts of the MIPPA interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :omega)

;; The data structure to collect instantiations for proof lines and
;; parameters to apply a method is the class mippa+method-matching.
;; This data structure is succesively filled up by agents and is kept
;; on their blackboard.

(setf testmatch nil)
(setf mippa*agent-hashtable (make-hash-table)) 

(defclass mippa+method-matching ()
  ((method        :initarg     :method
		  :initform    nil
		  :accessor    mippa~matching-method)
   (mapping       :initarg     :mapping
		  :initform    (make-instance 'meth+mapping)
		  :accessor    mippa~matching-mapping)
;  ((outline      :initarg     :outline
;		  :initform    nil
;		  :reader      mippa~get-outline
;		  :writer      mippa~set-outline))
;  ((new-outline   :initarg     :new-outline
;       	  :initform    nil
;		  :reader      mippa~get-new-outline
;		  :writer      mippa~set-new-outline))
   (parameters    :initarg     :parameters
		  :initform    nil
		  :accessor    mippa~matching-parameters))
;  ((variables     :initarg     :variables
;		  :initform    nil
;		  :reader      mippa~get-parameters
;		  :writer      mippa~set-parameters))
  (:documentation "mippa+method-matching is a data structure to collect
                   instantiations of proof lines and parameters found by
                   some agent so far"))

;; Some helpers for the automatic examination of methods, e.g. to
;; get the meta variables that have to be matched to fill up the
;; mipp+method-matching:

(defun mippa~method-conclusions (method)
  (declare (edited  "25-MAR-2004")
	   (authors Ftheiss)
	   (input   "The name of a method")
	   (effect  "none")
	   (value   "The list of method node names of the method's
                     existent conclusions"))
  (map 'list #'keim~name
       (map 'list #'keim~name
	    (remove-if-not
	     #'(lambda (x) (and (typep x 'meth+node) (not (typep x 'meth+meta-node))))
	     (meth~exist-conclusions (meth~find-method method))))))

(defun mippa~method-premises (method)
  (declare (edited  "25-MAR-2004")
	   (authors Ftheiss)
	   (input   "The name of a method")
	   (effect  "none")
	   (value   "The list of method node names of the method's
                     existent premises"))
  (map 'list #'keim~name
       (map 'list #'keim~name
	    (remove-if-not
	     #'(lambda (x) (and (typep x 'meth+node)) (not (typep x 'meth+meta-node)))
	     (meth~exist-premises (meth~find-method method))))))

(defun mippa=make-symbol-list (name n)
  (if (= n 0) nil
    (cons (intern (make-symbol (concatenate 'string
					    (symbol-name name)
					    "-MATCH-"
					    (format nil "~A" n))))
	  (mippa=make-symbol-list name (- n 1)))))

(defun mippa=get-crs (method)
  (declare (edited  "25-MAR-2004")
	   (authors Ftheiss)
	   (input   "The name of a method")
	   (effect  "none")
	   (value   "A list of control rules related to the method."))
    (let (useful)
    (maphash #'(lambda (x cr)
		 (declare (ignore x))
		 (some #'(lambda (to-do)
			   (some #'(lambda (single)
				     (when (and (consp single)
						(string-equal method (first single))
						(not (null (third single))))
				       (push (keim~name cr) useful)))
				 (when  (listp (second to-do)) (second to-do))))
		       (cri~to-do-part cr)))
	     cri*control-rules-hashtable)
    useful))

(defun mippa=cri-call (meth cr open)
 ; (mapcar #'third
  	  (remove-if #'(lambda (answer) (or (atom answer)
					    (not (eq (car answer) meth))))
		     (cri~call (list meth)
			       :kind 'methods
			       :task (plan~find-task-for-goal-in-agenda open
									(pds~agenda omega*current-proof-plan))
			       :task-node open 
			       :task-formula (node~formula open)
			       :agenda (pds~agenda omega*current-proof-plan)
			       :crules (list cr)
			       :pds omega*current-proof-plan)));)


(defun mippa=produce-parameters (meth open)
  (let ((crs (mippa=get-crs meth)))
    (reverse
     (remove-duplicates
      (mapcan #'(lambda (cr)
		  (mippa=cri-call meth cr open))
	      crs)))))



;; The function used by agents to apply the method matching to a
;; candidate node:

(defun mippa~match-node (method pds-node meth-node-name &optional (mapping (make-instance 'meth+mapping)))
  (declare (edited  "28-MAR-2004")
	   (authors Ftheiss)
	   (input   "The name of a method, a pds node, the name of a method node and
                     optionally a method mapping")
	   (effect  "none")
	   (value   "The new method mapping if the pds node and the method node can
                     be matched, nil otherwise"))
  (let* ((meth (meth~find-method method))
	 (method-node (find-if #'(lambda (y) (string-equal (keim~name (keim~name y)) meth-node-name))
			      (remove-if-not
			       #'(lambda (x) (and (typep x 'meth+node)) (not (typep x 'meth+meta-node)))
			       (append (meth~exist-premises meth) (meth~exist-conclusions meth))))))
    (if (null method-node) nil
      (meth~match-p method-node pds-node mapping :one2one))))

(defun mippa~agent-match-node (pds-node meth-node-name matching)
  (declare (edited  "28-MAR-2004")
	   (authors Ftheiss)
	   (input   "a pds node, the name of a method node and a mippa-matching ")
	   (effect  "none")
	   (value   "The new mippa-matching if the pds node and the method node can
                     be matched, nil otherwise"))
  (let ((result (first (mippa~match-node (mippa~matching-method matching)
				  pds-node
				  meth-node-name
				  (meth~mapping-copy (mippa~matching-mapping matching))))))
    (pprint (format nil "new matching: ~a" result))
    (setf testmatch (cons (make-instance 'mippa+method-matching
		           :method (mippa~matching-method matching)
		           :mapping result) testmatch))
    (if (null result) nil
      (list (make-instance 'mippa+method-matching
		           :method (mippa~matching-method matching)
		           :mapping result)))))

(defun mippa~initial-matching (method)
  (declare (edited  "28-MAR-2004")
	   (authors Ftheiss)
	   (input   "the name of a method")
	   (effect  "none")
	   (value   "a new mippa-matching with the method and an empty matching "))
  (make-instance 'mippa+method-matching
		 :method method))


(defun mippa~cri-parameters (matching)
  (declare (edited  "14-JUN-2004")
	   (authors Ftheiss)
	   (input   "a mippa-matching ")
	   (effect  "none")
	   (value   "The mippa-matching enhanced by parameters computed by control
                     crules, if possible"))
  (let* ((method (mippa~matching-method matching))
	 (prems (mippa~method-premises method))
	 (goal (first (mippa~method-conclusions method)))
	 (goal-node (if goal (mippa=get-node goal matching) nil))
	 (cri-result (if goal-node (mippa=produce-parameters method goal-node) nil))
	 (params (some #'(lambda (result)
		  (if (every #'(lambda (a b)
				 (keim~equal a (mippa=get-node b matching)))
			     (second result)
			     prems)
		      (third result) nil))
	      cri-result)))
    (map nil #'(lambda (x y) (pprint (format nil "par: ~a, val: ~a" x y)))
	 (meth~parameters (meth~find-method method))
	 params)
    (let ((result (if params (make-instance 'mippa+method-matching
		           :method (mippa~matching-method matching)
		           :mapping (mippa~matching-mapping matching)
			   :parameters params)
		    matching)))
      (mippa~send-request "mippa.suggestMethod" (list 1 (om~print result nil)))
      result)))


(defun mippa=get-node (node matching)
  (let ((mapping (meth~mapp-mapp (mippa~matching-mapping matching))))
  (some #'(lambda (var term)
            (if (string-equal (keim~name var) node) term nil))
        (mapp~domain mapping)
        (mapp~codomain mapping))))
    




;; Generating the agent definition for a method:


(defun mippa~generate-agents (method)
  (eval `(agent~defmatrix mippa-suggest
			  ,(cons 'agents (mippa~generate-agent-definitions method)))))

(defun mippa~show-agents (method)
  (pprint `(agent~defmatrix mippa-suggest
			  ,(cons 'agents (mippa~generate-agent-definitions method)))))

(defun mippa~generate-agent-definitions (method)
  (declare (edited  "28-MAR-2004")
	   (authors Ftheiss)
	   (input   "The name of a method")
	   (effect  "Agents to identify suitable nodes to apply the method are generated ")
	   (value   "none"))
  (let* ((conclusions (mippa~method-conclusions method))
	 (premises (mippa~method-premises method))
         (node-list (append conclusions premises))
	 (matchings (reverse
		     (cons 'parameter-match
			   (mippa=make-symbol-list method (list-length node-list)))))
	 (pred-types (append (make-list (list-length conclusions)
					:initial-element 'c-predicate)
			     (make-list (list-length premises)
					:initial-element 's-predicate)))
	 (start-matchings (butlast matchings))
	 (goal-matchings (cdr matchings))
	 (qt (first ''bla)))
    (append
     (list `(function (for ,(car start-matchings))
		     (uses)
		     (definition (mippa~initial-matching ,(list qt method))))
	   `(function (for command-match)
		      (uses parameter-match)
		      (definition (mippa~cri-parameters (:param parameter-match)))))
	  (map 'list #'(lambda (a b c d)
			 `(,a (for ,b ,d)
			      (uses ,c)
			      (multiple)
			      (definition (mippa~agent-match-node
					   ,(list :node b)
					   ,(list :param (list qt b))
					   ,(list :param c)))))
	       pred-types
	       node-list
	       start-matchings
	       goal-matchings))))


	 



;; testing:

(defun all-methods ()
  (let ((methods nil))
    (maphash #'(lambda (x y) (setf methods (cons x methods))) meth*method-hashtable)
    methods))

(defun all-theory-methods (theory)
  (remove-if-not
   #'(lambda (x)
       (string-equal
	theory
	(keim~name (meth~theory (meth~find-method (intern (make-symbol x)))))))
   (all-methods)))

(defun n-args-methods (n)
  (remove-if
   #'(lambda (x)
       (let ((meth-symbol (meth~find-method (intern (make-symbol x)))))
	 (<
	  (list-length (append (mippa~method-conclusions meth-symbol)
			       (mippa~method-premises meth-symbol)))
	  n)))
   (all-methods)))

(defun all-agents ()
  (dolist (x (all-methods))
    (mippa~generate-agents (intern (make-symbol x)))))

(defun all-theory-agents (theory)
  (dolist (x (all-theory-methods theory))
    (mippa~generate-agents (intern (make-symbol x)))))

 




;; print-object methods:

(defmethod print-object ((obj mippa+method-matching) stream)
  (setf testmapp (mippa~matching-mapping obj))
  (format stream "mippa+method-matching:~%method: ")
  (print-object (mippa~matching-method obj) stream)
  (format stream "~%mapping: ")
  (print-object (mippa~matching-mapping obj) stream)
  (format stream "~%parameters: ")
  (print-object (mippa~matching-parameters obj) stream)
  (format stream "~%"))
  





;; testarea omdoc:


; Crap! to be properly implemented!

(defun mippa~sugg2rpc (sugg)
  (let ((mapping (mippa~matching-mapping sugg)))
  (rpc~compose-methodcall 'newsuggestion
   (list
    (mippa~matching-method sugg)
    (om~string (first (subst~codomain (meth~mapp-subst mapping))))
    (list (om~string (mapp~domain (meth~mapp-mapp mapping)))
	  (om~string (mapp~codomain (meth~mapp-mapp mapping))))))))


(defun mippa~send (sugg host port)
  (socket~define :new)
  (socket~connect host port :new)
  (http~send-request :new (mippa~sugg2rpc sugg))
  (http~read-page :new)
  (socket~close :new)
  (socket~delete :new))






;; testcrap:

(defun cr-inspect ()
  (let (useful)
(maphash #'(lambda (x cr)
		 ;(lea=trace "~A" x)
		 (declare (ignore x))
		 (some #'(lambda (to-do)
			   ;(lea=trace "~A" to-do)
			   (some #'(lambda (single)
				     ;(lea=trace "~A" single)
				     (when (and (consp single)
						(not (null (third single))))
				       (push (list (keim~name cr) single) useful)))
				 (when  (listp (second to-do)) (second to-do))))
		       (cri~to-do-part cr)))
	     cri*control-rules-hashtable)
    useful))
