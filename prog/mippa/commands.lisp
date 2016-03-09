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
;; This file contains Commandos for the MIPPA interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :omega)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  MIPPA commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The command to apply a mippa+method-matching found by agents:

(com~defcommand mippa-suggest
		(argnames command-match)
		(argtypes anything)
		(arghelps "a mippa+method-matching")
                (function pprint)
                (frag-cats omega-basic file-io)
                (defaults)
                (log-p t)
                (help "Output a mippa suggestion"))



;; Invocation/Initialization:

(com~defcommand mippa-initialize-ppexercise      
  (argnames exercise strategies)
  (argtypes anything anything)
  (arghelps "The exercise in POST syntax" "The strategies to be applied")
  (frag-cats omega-basic file-io)
  (function oc=mippa-initialize-ppexercise)
  (log-p T)
  (help "Initializes OMEGA with an exercise and the given strategies"))

(defun oc=mippa-initialize-ppexercise (exercise strategies)
  (sys~handler-case
   (with-input-from-string (in exercise)
                   (let ((plist (read in)))
		     (sys~handler-case
		      (let ((problem (prob~find-problem (cadr plist)))
			    (newobj (post~read-object plist (env~create) nil)))
			(when problem
			  (omega~message "Redefining problem ~A~%" (keim~name problem))
			  (when (prob~proofs problem)
			    (dolist (x (prob~proofs problem))
			      (pds~remove-proof-plan x))))
			(oc=prove-pre (ot~read-proof-plan newobj))
			)
		      (error (c)
			     (omega~error c) 
			     (sys~signal
			      (sys~make-condition 'inter+error
						  :format-string "~A is not a problem!"
						  :args (list pname)))))))
   (error (c) (inter~print-error (comint~interface comint*current-comint) c)))
  (let* ((remainings (remove-if-not #'strat~find-strategy-ks strategies)))
    (setf sod*current-strategies remainings)))






;; Request of Proof Information:

(com~defcommand mippa-reflect-proof
  (argnames id)
  (argtypes anything)
  (arghelps "Message ID")
  (function oc=mippa-reflect-proof)
  (frag-cats omega)
  (help "Returns the current PDS as OMDOC."))

(defun oc=mippa-reflect-proof (id)
  (om~print omega*current-proof-plan nil))


(com~defcommand mippa-reflect-tasks
  (argnames id)
  (argtypes anything)
  (arghelps "Message ID")
  (function oc=mippa-reflect-tasks)
  (frag-cats omega)
  (help "Returns all current tasks."))

(defun oc=mippa-reflect-tasks (id)
  (om~print omega*current-proof-plan nil))





;; Handling of Suggestions:

(com~defcommand mippa-suggestions-method         
  (argnames id method-name)
  (argtypes anything anything)
  (arghelps "Message ID"
	    "Name of the method which mippa agents are to be generated for")
  (frag-cats omega-basic theory file-io)
  (function oc=suggestions-method)
  (log-p T)
  (help "Mippa agents are generated for a given method."))

(defun oc=suggestions-method (id name)
  (setf (gethash id mippa*agent-hashtable) (mippa~generate-agents name))
  t) ;return value for xmlrpc


(com~defcommand mippa-deactivate-agents
  (argnames id)
  (argtypes anything)
  (arghelps "Message ID")
  (function oc=mippa-deactivate-agents)
  (frag-cats omega)
  (help "Deactivates agent-based default computations."))

(defun oc=mippa-deactivate-agents (id)
  (oc=deac-agents))


(com~defcommand mippa-deactivate-method-agents         
  (argnames id meth-id)
  (argtypes anything anything)
  (arghelps "Message ID"
	    "ID of the call to the suggested method")
  (frag-cats omega-basic theory file-io)
  (function oc=deactivate-method-agents)
  (log-p T)
  (help "Mippa agents are removed for a given agent group."))

(defun oc=deactivate-method-agents (id meth-id)
  (let ((agentgroup (gethash meth-id mippa*agent-hashtable)))
    (dolist (x agentgroup)
      (agent~remove-agent x)))
  (remhash id mippa*agent-hashtable)
  t) ;return value for xmlrpc










;testarea, to be removed:


(com~defcommand suggestmethod         
  (argnames id method-name)
  (argtypes anything anything)
  (arghelps "Message ID"
	    "Name of the method which mippa agents are to be generated for")
  (frag-cats omega-basic theory file-io)
  (function oc=suggestmethod)
  (log-p T)
  (help "Mippa agents are generated for a given method."))

(defun oc=suggestmethod (id name)
  (omega~message (format nil "~s" rpc*caller))
  (mippa~send-request "mippa.suggestMethod" (list 1 "<blubber/>"))
  (when rpc*caller
    (socket~define :new)
    (socket~connect (first rpc*caller) (second rpc*caller) :new)
    (http~send-request :new (rpc~compose-methodcall "mippa.suggestMethod"
						    (list 1 (om~print omega*current-proof-plan nil)))
		       :uri (third rpc*caller))
    (socket~close :new)
    (socket~delete :new))
  t) ;return value for xmlrpc

