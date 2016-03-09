;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: LISP -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                         ;;
;;   Copyright (C) 1993 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     KEIM Project                                                         ;;
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

#| ------------------------------------------- LPLANNER ALS REFINEMENT ALGORITHM ----------------------------------------------------- |#

;; Die Invokation Funktion:

(defun lplan~lplanner-invokation-function (strategy-ks task parameters)
  (declare (edited  "19-MAR-1999")
	   (authors Ameier)
	   (input   "The strategy-ks (using LPlanner as refinement algorithm) and the task.")
	   (effect  "LPlanner is invoked (what ever this may affects ...")
	   (value   "Message returned from the invokation of the strategy.")) 

  (let* ((strat-parameter-hashtable (strat~strategy-ks-hash-table strategy-ks))
	 ;; neue LPLANNER ROC-State-Description 
	 (new-Lplanner-roc-state-description (roc~fresh-lplanner-state-description strategy-ks task nil parameters))
	 ;; Neue Start-strategy-ks Justification
	 (new-start-strategy-ks-application-just (strat~start-strategy-ks-application-create-just
						  (list new-lplanner-roc-state-description)))
	 ;; Neuer Strategy-step aus dieser Justification
	 (start-step (scon~create-scon-step nil nil new-start-strategy-ks-application-just)))
    
    ;; Roc bekommt start-step als Eintrag
    (setf (roc~start-step new-lplanner-roc-state-description) 
	  start-step)

    ;; ROC wird in ROCS-list eingetragen
    (store~add-element! (black~get-blackboard-object-content 'rocs-list sod*solution-blackboard)
			new-lplanner-roc-state-description)
    
    ;; Strategy-step wird in Strategy-steps eingefuegt
    (sod~introduce-new-strategy-ks-step! start-step)
        
    ;; -> HIER MUSS JETZT EURE FUNKTION REIN UM LPLANNER AUFZURUFEN!
    ;; z.B. (lplan=calling-lplanner new-lplanner-roc-state-description)
    (lplan=calling-lplanner new-lplanner-roc-state-description)
    
    ))

(defun lplan=calling-lplanner (state-des)
  (let* ((task (roc~start-task state-des))
	 (task-node (agenda~task-node task)))
  (sys~handler-case
   (with-open-file (out "/tmp/subproblem.post" :direction :output :if-exists :supersede :if-does-not-exist :create)
		   (lplan=write-subproblem task-node out omega*current-proof-plan)
		   (omega~message "Wrote file ~A." (truename out)))
   (file-error (c) (omega~error c)))
   ;;(serv~enter "LPLAN")
   ;;(serv~apply "LPLAN" "send(\"read-problem /tmp/subproblem.post\")")
   ;;(serv~apply "LPLAN" "send(\"plan\")")
   ;;(if (probe-file "/tmp/result.post") (delete-file "/tmp/result.post"))
   ;;(serv~apply "LPLAN" "send(\"write-result /tmp/result\")")
  (loop until (probe-file "/tmp/result.post") do nil)
  (setf pplan*roc-state-description (roc~fresh-pplanner-state-description (strat~find-strategy-ks 'dummy) task nil nil))
  (omega~message "Result available.")
  ;; (serv~leave "LPLAN")
  (delete-file "/tmp/subproblem.post")
  (let ((result nil))
    (sys~handler-case
     (with-open-file (in "/tmp/result.post" :direction :input :if-does-not-exist :error)
		     (let ((list (read in)))
		       (when (> (length list) 1)
			 (lplan=read-proof task list omega*current-proof-plan)
			 (setq result t))))
     (file-error (c) (inter~print-error (comint~interface comint*current-comint) c)))
    ;;(delete-file "/tmp/result.post")
    (if result
	(progn
	  ;;(setf (pds~agenda omega*current-proof-plan) (agenda~replace-task task nil nil nil (pds~agenda omega*current-proof-plan)))
	  (exmes~create-termination-message state-des nil))
      (exmes~create-failure-message state-des nil)))))
  
;;
;; Es sollte beim Terminieren eine der folgenden EXECUTION-MESSAGES zurueckgegeben werden:
;;
;; (exmes~create-termination-message <STATE-DES>
;;				     nil))        
;; -> Falls LPlanner hat das Ziel geschlossen
;;
;; (exmes~create-failure-message <STATE-DES>
;;                               report)))
;; -> Falls LPlanner schlaegt fehl auf ZIel
;;
;;
;; Dabei ist <STATE-DES> die State-description die in der lplan~lplanner-invokation-function erzeugt wird!
;; report kann im prinzip alles MOegliche sein, sollte aber moeglichsy eine Liste die ausdrueckty was schiefgegenagen
;; ist, so was wie '(no-method-applicable)
;;

(defun lplan=write-subproblem (task-node stream pds)
  (let* ((assumptions (pds~node-supports task-node pds))
	 (problem (prob~proof-problem pds))
	 (env (prob~environment problem))
	 (pds-env (pds~environment pds))
	 (type-vars  (env~class-keys env 'type+variable nil))
	 (type-constants (env~class-keys env 'type+constant nil))
	 (constants (env~class-keys env 'term+constant nil))
	 (variables (env~class-keys env 'term+variable nil))
	 (pds-type-vars  (env~class-keys pds-env 'type+variable nil))
	 (pds-type-constants (env~class-keys pds-env 'type+constant nil))
	 (pds-constants (env~class-keys pds-env 'term+constant nil))
	 (pds-variables (env~class-keys pds-env 'term+variable nil)))
    (format stream "(problem ~A-subproblem-~A ~%(in ~A) ~%" (keim~name problem) (keim~name task-node)
	    (keim~name (prob~theory problem)))
    (mapc #'(lambda (x) (env~post-print x (env~lookup-object x pds-env) stream))
	  (append type-vars pds-type-vars type-constants pds-type-constants
		  constants pds-constants variables pds-variables))
    (dolist (assumption assumptions)
      (format stream "(assumption ~A " (keim~name assumption))
      (post~print (pds~node-formula assumption pds) stream)
      (format stream ")~%"))
    (format stream "(conclusion ~A " (keim~name task-node))
    (post~print (pds~node-formula task-node pds) stream)
    (format stream ")~%")
    (format stream ")")))

(defun lplan=look-up-func (label-node-map)
  #'(lambda (label) (cdr (assoc label label-node-map))))

(defun lplan=create-node (label formula pds)
  (let ((exist-node (pds~label2node label pds)))
    (if exist-node
	(progn 
	  (setf (node~formula exist-node) formula)
	  exist-node)
      (let ((node (make-instance 'pdsn+node :name label :formula formula)))
	(setf (gethash (symbol-name (keim~name node)) (pds~label-node-hashtable pds)) node)
	node))))

(defun lplan=read-formula (post-formula pds)
  (if (and (listp post-formula) (string-equal (first post-formula) :all-types))
      (post~read-object post-formula (pds~environment pds) :existing-term-closed)
    (post~read-object post-formula (pds~environment pds) :existing-term)))

(defun lplan=read-hyps (post-hyps look-up-func)
  (mapcar look-up-func post-hyps))

(defun lplan=read-param (post-param look-up-func pds)
  (pds~post-read-obj post-param (pds~environment pds) (prob~theory pds) look-up-func nil nil))

(defun lplan=read-subst (post-subst inference outline-pat adapted-pat outline nodeformula look-up-func pds)
  ;; Creates a substitution from its PDS-POST representation:
  (when post-subst
    (let ((label-pat (find-if #'(lambda (pat)
				  (not (or (infer~nonexistent-pattern-p pat)
					   (infer~existent-pattern-p pat)
					   (infer~closed-pattern-p pat))))
			      adapted-pat)))
      (if label-pat
	  ;; A method with many conclusions
	  (let* ((conc (funcall look-up-func label-pat))
		 (conc-just (node~justification conc)))
	    (when conc-just
	      ;; CONC is processed
	      (let* ((the-just (pdsj~get-twin-just conc-just inference outline-pat))
		     (the-pat (pdsj~conclusion-outline the-just))
		     (new-adapted-pat (substitute the-pat label-pat adapted-pat))
		     (the-subst (lplan=read-subst post-subst inference outline-pat new-adapted-pat outline nodeformula look-up-func pds)))
		(when the-subst
		  ;; It is possible to compute the subst from the
		  ;; new adapted outline pattern:
		  (setf (pdsj~subst the-just) the-subst)
		  the-subst)
		;; It is not possible to compute the subst, so return NIL.
		))
	    ;; CONC is not yet processed, it is therefore not yet possible
	    ;; to compute the subst, so return NIL: subst will be computed
	    ;; after processing CONC.
	    )
	;; Either a method with only one conclusion, 
	;; or it is now possible to compute the subst
	;; For this the bound variables are temporarly
	;; added to the proof environment.
	(let* ((copy-of-pat (mapcar #'(lambda (x) x) adapted-pat))
	       ;;MP: pds~inference-application is desctructive!!
	       (infer-appl (pds~inference-application inference copy-of-pat))
	       (terms (cons nodeformula (mapcar #'(lambda (node) (node~formula (look-up-func node))) outline)))
	       (added-vars (pds=add-vars-temporary-to-env
			    (mapcan #'term~bound-variables terms) pds))
	       (mapp (pds~post-read-meth-mapping subst pds theory infer-appl look-up-func)))
	  (pds=remove-names-from-env added-vars pds)
	  mapp)))))

(defun lplan=read-just-level (just look-up-func pds)
  (let* ((status (fourth just))
	 (post-subst (fifth just))
	 (meth-str (first just))
	 (outline-pat (sixth just))
	 (post-outline (third just))
	 (meth (infer~find-method meth-str))
	 (the-meth (if meth meth
		     (error "The inference ~A has not been loaded." meth-str))))
    (pdsj~create the-meth (mapcar look-up-func post-outline) nil nil status
		 (mapcar #'(lambda (post-param) (lplan=read-param post-param look-up-func pds)) (second just))
		 nil ;;(lplan=read-subst (post-subst the-meth outline-pat outline-pat post-outline nodeformula look-up-func pds))
		 outline-pat nil)))
  
(defun lplan=read-just (post-just look-up-func pds)
  (let* ((pos (first post-just))
	 (post-justs (rest post-just))
	 (just-list
	  (mapcar #'(lambda (just) (lplan=read-just-level just look-up-func pds))
		  post-justs)))
    (pdsj~nth-chain-justs! pos just-list)))

(defun lplan=read-node-base (post-node existing-nodes label-node-map pds)
  (let* ((label (first post-node))
	 (formula (lplan=read-formula (third post-node) pds)))
    ;; (omega~message "label: ~A, existing-nodes: ~A" label (mapcar 'keim~name existing-nodes))
    (if (find (symbol-name label) (mapcar 'symbol-name (mapcar 'keim~name existing-nodes)) :test 'equal)
	;; node exists already
	(progn
	  ;; (omega~message "found!")
	  (acons label (pds~label2node label) label-node-map))
      ;; node has to be created
      (let ((node (lplan=create-node (pds~new-node-name pds) formula pds)))
	(acons label node label-node-map)))))

(defun lplan=read-node-rest (post-node look-up-func pds)
  (let* ((label (first post-node))
	 (node (funcall look-up-func label))
	 (hyps (lplan=read-hyps (second post-node) look-up-func))
	 (just (lplan=read-just (fourth post-node) look-up-func pds)))
    (setf (pdsn~hyps node) hyps)
    (setf (node~justification node) just)
    (when (not (find node (prob~proof-steps pds)))
      (setf (prob~proof-steps pds) (append (prob~proof-steps pds) (list node))))))


(defun lplan=read-declarations (decl-list pds) 
  (post~read-object-list decl-list (pds~environment pds)))

(defun lplan=read-subproblem (task-node post-proof pds)
  (let ((existing-nodes (cons task-node (pds~node-supports task-node)))
	(decl-list (rest (fourth post-proof)))
	(node-list (fifth post-proof)))
    (lplan=read-declarations decl-list pds)
    (setf (prob~proof-steps pds) (set-difference (prob~proof-steps pds) existing-nodes))
    (let ((label-node-map nil))
      (dolist (post-node (rest node-list))
	(setf label-node-map (lplan=read-node-base post-node existing-nodes label-node-map pds)))
      (dolist (post-node (rest node-list))
	(lplan=read-node-rest post-node (lplan=look-up-func label-node-map) pds)))))

(defun lplan=read-ia-step (post-step label-node-map pds)
  (let* ((method (meth~find-method (second post-step)))
	 (look-up-func (lplan=look-up-func label-node-map))
	 (goal (funcall look-up-func (third post-step)))
	 (existent-prems (mapcar look-up-func (fourth post-step)))
	 (closed-prems (mapcar look-up-func (fifth post-step)))
	 (premises (append existent-prems closed-prems))
	 (parameters (mapcar #'(lambda (post-param) (lplan=read-param post-param look-up-func pds)) (sixth post-step)))
	 (post-subgoals (eighth post-step))
	 (post-add-concs (ninth post-step))
	 (post-new-hyps (tenth post-step)))
    (omega~message "---------------------------------------------------------------------------------------------")
    (omega~message "|")
    (omega~message "| Processing inference application step...")
    (omega~message "| Applying ~A" method)
    (omega~message "| with focus goal ~A, parameters ~A" goal parameters)
    (omega~message "| and premises ~A..." premises)
    (setf pplan*methods (list method))
    (let* ((matching-result (pplan~produce-mmatching method goal premises parameters))
	   (task (first matching-result))
	   (mmatching (second matching-result)))
      ;; (omega~message "| Found matching: ~A" mmatching)
      (if mmatching
	  (multiple-value-bind (new-agenda subgoals new-supps)
	      (pplan=apply-plan-step! task mmatching pds (pds~agenda pds))
	    (setf (pds~agenda pds) new-agenda)
	    (omega~message "| New subgoals: ~A, new supports: ~A" subgoals new-supps)
	    (omega~message "| ...step done.")
	    (omega~message "|")
	    (omega~message "---------------------------------------------------------------------------------------------")
	    (pairlis (append post-subgoals post-add-concs post-new-hyps) (append subgoals new-supps) label-node-map))
	(progn
	  (omega~message "| No matching found for task ~A." task)
	  nil)))))
     
(defun lplan=read-steps (post-steps label-node-map pds)
  (when post-steps
    (let* ((post-step (first post-steps))
	   (kind (first post-step))
	   (new-label-node-map 
	    (cond ((eq kind 'inference-application) (lplan=read-ia-step post-step label-node-map pds))
		  (t (omega~error "Unknown kind of step: ~A" kind) nil))))
      (if new-label-node-map
	  (lplan=read-steps (rest post-steps) new-label-node-map pds)
	(progn
	  (omega~error "The following step could not be executed:")
	  (omega~error "~A" post-step))))))

(defun lplan=read-proof (task post-proof pds)
  (let* ((post-conclusion (second (first post-proof)))
	 (post-assumptions (second (second post-proof)))
	 (post-steps (rest (third post-proof)))
	 (task-node (agenda~task-node task))
	 (assumptions (pds~node-supports task-node))
	 (label-node-map (pairlis (cons post-conclusion post-assumptions) (cons task-node assumptions))))
    (lplan=read-steps post-steps label-node-map pds)))

(com~defcommand start-lplanner
		(argnames )
		(argtypes )
		(arghelps )
		(frag-cats planning)
		(function lplan~start)
		(defaults )
		(log-p T)
		(help ""))

(defun lplan~start ()
  (serv~enter "LPLAN"))

(com~defcommand stop-lplanner
		(argnames )
		(argtypes )
		(arghelps )
		(frag-cats planning)
		(function lplan~stop)
		(defaults )
		(log-p T)
		(help ""))

(defun lplan~stop ()
  (serv~leave "LPLAN"))


;;
;; DEFAULT STRATEGY:
;; (diese Strategy muss in ein eigenes File!)
;; 

;;(defun always-true (task)
;;  't)
;;
;;(strat~define-strategy-ks
;; (name DEFAULT-LPLANNER)
;; (refinement-algorithm LPlanner)
;; (condition always-true)
;; (print "Strategy-KS DEFAULT-LPLANNER: Offer to tackle task ~A with LPLanner"))
