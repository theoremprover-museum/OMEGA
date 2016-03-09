;;; -*- syntax: common-lisp; package: OMEGA; base: 10; mode: Keim -*-
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
;;; The Wild Tactics Module
;;; =======================
;;;
;;; Wild Tactics are of an unpredictable nature, i.e. their outline
;;; can vary. Therefore they cannot be captured with the
;;; regular (declarative thus static) tactic mechanism.
;;;
;;; Their general form is:
;;;  (I dont know yet.... will fill in later......
;;;  So far the idea is that outline computations are done on (not
;;;  necessarily complete) sets of premises and conclusions.
;;;  Return value are then the completed sets plus a set of
;;;  newly introduced hypotheses.
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :omega)


(mod~defmod WTAC 
            :uses (infer keim node omega pds pdsc pdsj pdsn tac)
            :documentation "The functionality for Wild Tactics, i.e. tactics without a fixed outline."
            :exports (
                      wtac~apply
                      wtac~pass-on
                      wtac~preprocess-outline

		      wtac*verbose
                      ))

;;   AGENDA
;; - conclusions und premises berechnen
;; - sind gegebene Conclusions offen?
;; - conventions for outline-functions:
;;   lists of conclusions, premises and parameters
;; - call function with formulas
;;     functions need to return nil or
;;     list of formulas for new conclusions and new premises (with new hyps)
;;     in the following form:
;;     (c1 c2 ... cm)  (p1 (p2 (h1 1) h2) (p3 (h1 1) h3)) 
;;     or simply true in case nothing new is computed.
;;     hypotheses have to be distinguished if the same hypthesis occurs several times
;;     for different premises. Since they are only formulae they need to be explicitly
;;     marked with something that tests T on equal.
;;   --> success? conclusions premises (with hyps)
;;   
;; - make nodes and insert them
;; - rewrite outline pattern
;;    --> either conventionally or with lists????
;;        TESTEN!
;;
;; - conventions for expansion functions: 
;;   lists of conclusions, premises and parameters
;;   calls to  tacl~init are of the form:
;;   (tacl~init (append conclusions premises))

(eval-when (load compile eval)
  (defvar wtac*verbose nil "If nil, warnings will be suppressed, otherwise they will be printed.")
)

(defgeneric wtac~pass-on (nodes key)
  (declare (edited  "01-FEB-1999 22:35")
	   (authors SORGE)
	   (input   "A list of nodes and a symbol.")
	   (value   "Extracts parts of the nodes according to the given key."))
  (:method ((nodes list) (key (eql :formula)))
	   (mapcar #'node~formula nodes))
  (:method ((node pdsn+node) (key (eql :formula)))
	   (node~formula node))
  (:method ((nodes list) (key (eql :node)))
	   nodes)
  (:method ((node pdsn+node) (key (eql :node)))
	   node)
  (:method ((nodes list) (key (eql :just)))
	   (mapcar #'node~justification nodes))
  (:method ((node pdsn+node) (key (eql :just)))
	   (node~justification node))
  (:method ((nodes list) (key (eql :hyps)))
	   (mapcar #'pdsn~hyps nodes))
  (:method ((node pdsn+node) (key (eql :hyps)))
	   (pdsn~hyps node))
  )

(defgeneric wtac=open-conclusions (conc)
  (declare (edited  "01-FEB-1999 21:58")
	   (authors SORGE)
	   (input   "A list of conclusion nodes or a single node.")
	   (value   "True if all given nodes are open."))
  (:method ((conc list))
	   (every #'pdsn~open-node-p conc))
  (:method ((conc pdsn+node))
	   (pdsn~open-node-p conc)))

(defun wtac~preprocess-outline (outline pattern)
  (declare (edited  "01-FEB-1999 18:44")
	   (authors SORGE)
	   (input   "An outline and the corresponding outline-pattern.")
	   (effect  "None.")
	   (value   "Two (possibly empty) lists of nodes, where the first"
		    "contains the conclusion nodes and the second the premises."))
  (let ((concs (car outline))
	(prems (cadr outline))
	(conc-pat (car pattern))
	(prem-pat (cadr pattern)))
    (values 
     (if (or (infer~existent-pattern-p conc-pat)
	     (infer~closed-pattern-p conc-pat))
	 (list concs)
       concs)
     (if (or (infer~existent-pattern-p prem-pat)
	     (infer~closed-pattern-p prem-pat))
	 (list prems)
       prems))))

(defun wtac=no-cycles (concs prems)
  (declare (edited  "01-FEB-1999 22:02")
	   (authors SORGE)
	   (input   "A list of conclusions and a list of premises.")
	   (value   "True if none of premises is derived from the given conclusions."))
  (let ((all-premises (pdsn~justifying-nodes prems)))
    (notany #'(lambda (conc) (find conc all-premises)) concs)))

(defun wtac=hyps-subsetp (concs prems)
  (declare (edited  "01-FEB-1999 22:09")
	   (authors SORGE)
	   (input   "A list of conclusions and a list of premises.")
	   (value   "True if the hypotheses of the premises are a subset of the"
		    "hypotheses of the conclusions."))
  (let ((prem-hyps (tac~set-manage-list #'union (mapcar #'pdsn~hyps prems))))
    (every #'(lambda (conc) (subsetp prem-hyps (pdsn~hyps conc))) concs)))


(defun wtac~apply (tactic application conclusions premises parameters &optional (wtac*verbose wtac*verbose) 
			  &key ((:purpose purpose) :apply) ((:passkey passkey) :formula))
  (declare (edited  "01-FEB-1999 20:12")
	   (authors SORGE)
	   (input   "A tactic, a funcallable function, lists of conclusions and premises,"
		    "a list of parameter, a purpose key with possible values:"
		    ":apply, :test, and :expand, and a passkey specifying how premises and"
		    "conclusions are passed to the actual application. This is influenced by"
		    "the function WTAC~~PASS-ON function.")
	   (effect  "Applys the wild tactic TACTIC according to the given APPLICATION."
		    "Computes and inserts new nodes for purposes :apply and :expand/")
	   (value   "Returns according to the key PURPOSE:"
		    "- :apply, four values: T, and lists of conclusion, premises"
		    "and hypotheses nodes."
		    "- :expand, four values: T, and lists of conclusion, premises"
		    "and hypotheses nodes."
		    "- :test, T."
		    "If computation failed, returns nil."))
  (let ((apply (string-equal purpose :apply))
	(expand (string-equal purpose :expand))
	(test (string-equal purpose :test))
	(tac-name (keim~name tactic)))
    (unless (or apply test expand)
      (return-from wtac~apply (omega~error ";;;WTAC~~APPLY: invalid purpose ~A." purpose)))
    (unless (or test (wtac=open-conclusions conclusions))
      (return-from wtac~apply
	(omega~warn ";;;WTAC~~APPLY: tactic ~A cannot be applied to already justified nodes." tac-name)))
    (unless (wtac=no-cycles conclusions premises)
      (return-from wtac~apply
	(omega~warn ";;; Tactic ~A cannot justify a node by its own derivative.~%" tac-name)))
    (unless (wtac=hyps-subsetp conclusions premises)
      (return-from wtac~apply
	(omega~warn ";;; Tactic ~A cannot be applied to the given conclusions.~%" tac-name)))
    (multiple-value-bind (res-concs res-prems)
	(funcall application
		 (wtac~pass-on conclusions passkey)
		 (wtac~pass-on premises passkey)
		 parameters)
      (when (some #'term~free-type-variables (append (when (listp res-concs) res-concs)
						     (when (listp res-prems) (wtac=res-prems2term-list res-prems))))
	(return-from wtac~apply
	  (omega~warn ";;; Some schematic terms could not be grounded while applying Tactic ~A!~%" tac-name)))
      (cond ((and (or res-concs res-prems) test) t)
	    ((and (null res-concs) (null res-prems))
	     (omega~warn ";;; Tactic ~A could not be applied to the given conclusions.~%" tac-name))
	    (t      ;;; everything's all right: fill in the nodes....
	     (let* ((new-stuff (listp res-concs))
		    (new-res-prems (when new-stuff (wtac=make-new-hyps res-prems)))
		    (hyp-nodes (when new-res-prems
				 (remove-duplicates
				  (apply #'append
					 (mapcar #'(lambda (x) (when (listp x) (cdr x))) new-res-prems)))))
		    (conc-hyps (tac~set-manage-list #'union (mapcar #'pdsn~hyps conclusions)))
		    (prem-hyps (tac~set-manage-list #'union (mapcar #'pdsn~hyps premises)))
		    (new-prems (when new-stuff (wtac=make-new-prems new-res-prems conc-hyps (when apply t))))
		    (prem-nodes (append premises new-prems))
		    (new-concs (when new-stuff (wtac=make-new-concs res-concs prem-nodes prem-hyps parameters tactic)))
		    (conc-nodes (append
				 (wtac=update-old-concs conclusions prem-nodes parameters tactic)
				 new-concs)))
	       (when wtac*verbose (omega~trace "Conclusions: ~A  Premises: ~A  Hypotheses:  ~A" conc-nodes prem-nodes hyp-nodes))
	       (unless expand (tac~change-pds-structure conc-nodes prem-nodes hyp-nodes))
	       (values t conc-nodes prem-nodes hyp-nodes)
	       ))))))

(defun wtac=res-prems2term-list (res-prems)
  (declare (edited  "02-MAR-2000")
	   (authors Sorge)
	   (input   "A list of resulting premises, i.e. of formulas possibly together with marked formulas.")
	   (effect  "None.")
	   (value   "A list containing only terms."))
  (let ((fprem (car res-prems)))
    (cond ((null res-prems) res-prems)
	  ((term~p fprem)
	   (cons fprem (wtac=res-prems2term-list (cdr res-prems))))
	  ((listp fprem)
	   (append
	    (cons (car fprem)
		  (mapcar #'(lambda (x) (if (listp x) (car x) x)) (cdr fprem)))
	    (wtac=res-prems2term-list (cdr res-prems)))))))

(defun wtac=make-new-concs (conc-list prem-nodes hyps parameters tactic)
  (declare (edited  "02-FEB-1999 00:41")
	   (authors SORGE)
	   (input   "A list of conclusion formulas, a list of premises"
		    "a list of hypotheses, a list of parameters and a tactic.")
	   (effect  "Creates new conclusion nodes and inserts them into the PDS.")
	   (value   "The list of newly created nodes."))
  (mapcar #'(lambda (formula)
	      (let ((new-node
		     (pdsn~create (pds~new-node-name) hyps formula 
				  (pdsj~closed-just-create tactic prem-nodes parameters))))
		(pds~only-insert-node! new-node)
		new-node))
	  conc-list))

(defun wtac=update-old-concs (conc-nodes prem-nodes parameters tactic)
  (declare (edited  "02-FEB-1999 00:41")
	   (authors SORGE)
	   (input   "A list of conclusion nodes, a list of premises"
		    "a list of parameters and a tactic.")
	   (effect  "Inserts new justifications and reasons into the conclusion nodes.")
	   (value   "The list of newly created nodes."))
  (mapcar #'(lambda (old-concl)
	      (let* ((old-just (node~justification old-concl))
		     (old-reasons (pdsj~reasons old-just))
		     (old-sponsors (pdsj~sponsors old-just))
		     (old-unsponsors (pdsj~unsponsors old-just))
		     (new-control (pdsc~create old-reasons old-sponsors old-unsponsors)))
		(setf (node~justification old-concl)
		      (pdsj~replace-justification! (node~justification old-concl)
						   (pdsj~closed-just-create tactic prem-nodes parameters)))
		(setf (pdsj~control (node~justification old-concl)) new-control)
		old-concl))
	  conc-nodes))

(defun wtac=make-new-hyps (prem-list)
  (declare (edited  "01-FEB-1999 23:57")
	   (authors SORGE)
	   (input   "A list of premise formulas possibly together with hypotheses.")
	   (effect  "Creates new hypotheses nodes and inserts them into the PDS.")
	   (value   "The list of newly created nodes."))
  (let (double-hyps)
    (labels ((make-hyp (formula)
		     (let ((new-node
			    (pdsn~create (pds~new-node-name) nil formula
					 (pdsj~closed-just-create (infer~find-method "HYP")
							     nil nil "grounded"))))
		       (setf (pdsn~hyps new-node) (list new-node))
		       (pds~only-insert-node! new-node)
		       new-node))
	     (create-hyp (hyp)
			 (if (listp hyp)
			     (let* ((formula (first hyp))
				    (marker (second hyp))
				    (exist? (assoc marker double-hyps :test #'equal)))
			       (if exist?
				   (cdr exist?)
				 (let ((new-node (make-hyp formula)))
				   (push (cons marker new-node) double-hyps)
				   new-node)))
			   (make-hyp hyp))))
      (mapcar #'(lambda (prem)
		  (if (listp prem)
		      (cons (car prem) (mapcar #'create-hyp (cdr prem)))
		    prem))
	      prem-list))))
    
(defun wtac=make-new-prems (prem-list extra-hyps &optional (apply t))
  (declare (edited  "01-FEB-1999 23:57")
	   (authors SORGE)
	   (input   "A list of premise formulas possibly together with hypotheses,"
		    "a list of additional hypotheses, and a flag.")
	   (effect  "Creates new premise nodes and inserts them into the PDS.")
	   (value   "The list of newly created nodes."))
  (mapcar #'(lambda (prem)
	      (let* ((formula (if (listp prem) (car prem) prem))
		     (hyps (append (when (listp prem) (cdr prem)) extra-hyps))
		     (new-node (pdsn~create  (pds~new-node-name) hyps formula (pdsj~open-just-create))))
		  (if apply
		      (pds~insert-node! new-node)
		    (pds~only-insert-node! new-node))
		  new-node))
	    prem-list))
  
(defmethod infer~compute-outline ((tactic infer+wild-tactic) (outline list) (parameters list))
    (let* ((outline-pattern (infer~compute-outline-pattern outline))
	   (application (if (infer~outline-function tactic)
			    (infer~outline-function tactic)
			  (infer~outline-pattern2application tactic outline-pattern))))
      (if application
	  (multiple-value-bind (conclusions premises)
	      (wtac~preprocess-outline outline outline-pattern)
	    (multiple-value-bind (success new-conclusions new-premises hypotheses)
		(wtac~apply tactic application conclusions premises parameters wtac*verbose
			    :passkey (infer~passkey tactic))
	      (if (and (not success)
		       (not (omega~warn ";;; Something went wrong while applying the outline-function to ~A" outline)))
		  (values outline nil)
		(let* ((new-pattern (append
				     (make-list (length conclusions) :initial-element "EXISTENT")
				     (make-list (- (length new-conclusions)
						   (length conclusions))
						:initial-element "NONEXISTENT")
				     (make-list (length premises) :initial-element "EXISTENT")
				     (make-list (- (length new-premises)
						   (length premises))
						:initial-element "NONEXISTENT"))))
		  (tac~rewrite-outline-pattern new-pattern new-conclusions)
		  (tac~change-reason-structure new-conclusions new-premises hypotheses)
		  (values (append new-conclusions new-premises hypotheses) t)))))
	(omega~warn ";;; Cannot compute outline for pattern ~A" outline-pattern)
	)))

(defmethod infer~apply-expansion-function ((wtac infer+wild-tactic) (outline list) (parameters list))
  (let* ((premises (just~premises (node~justification (car outline))))
	 (conclusions (subseq outline 0 (- (length outline) (length premises)))))
    (apply (symbol-function (infer~expansion-function wtac)) (list conclusions premises parameters))))
