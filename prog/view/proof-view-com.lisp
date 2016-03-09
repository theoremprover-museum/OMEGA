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

(in-package :omega)
;;; redefine commands

(defun oc=prove (proof-plan)
  (when (mixin~activep)
    (let* ((name (keim~name proof-plan))
	   (method (format nil "setProofName(~a)" (parse~atom name))))
      (socket~write method :inout)))
  (when (and view*on (pds~proof-plan-p omega*current-proof-plan))
    (view~hide-proof omega*current-proof-plan))
; (when (and (not (mixin~activep))
;	     view*on (pds~proof-plan-p omega*current-proof-plan))
;    (view~hide-proof omega*current-proof-plan))
  (omega~message "Changing to proof plan ~A" (keim~name proof-plan))
  (let ((oldtheory (when omega*current-proof-plan (prob~proof-theory omega*current-proof-plan))))
    (setq omega*current-proof-plan proof-plan
	  omega*current-theory (prob~proof-theory proof-plan)
	  logic*current-theory omega*current-theory
	  keim::pds*current-proof-plan proof-plan)
    (alift~initialize omega*current-proof-plan) ;; KEIM-3 comment VS
    (setf foci*proof-context (pds~proof-context proof-plan))
    (when (csm~active-p)
      (oc=agents-restart :theo-default (unless (eq omega*current-theory oldtheory) omega*current-theory))))
  (when view*on 
;    (unless (mixin~activep) (view~unhide-proof omega*current-proof-plan))
    (view~unhide-proof omega*current-proof-plan)
    (view~clean-proof view*view)
    (view~display-proof omega*current-proof-plan))
  omega*current-proof-plan) 

(pp~defstyle view-pretty 
	     :parent pds-pretty
	     :pprint-methods
	     ((view+pds-node
	       (lambda (s l)
		 (let ((*standard-output* s))
		   (view=pprint-node l))))
	      (view+pds-schematic-node
	       (lambda (s l)
		 (let ((*standard-output* s))
		   (view=pprint-node l))))))



#{\section{Proof View}#}

#{\subsection{General}#}

;;; Omega-Command to switch proof-view on and off

(com~defcommand proof-view-mode         ;;;  works VS
  (function view=proof-view-mode)
  (argnames on)
  (argtypes boolean)
  (arghelps "T to display all lines in buffer *proof*, else nil")
  (frag-cats proof-view)
  (defaults (T))
  (help "If argument is T, then display all changes to the current proof or its lines in Buffer *proof*."))


(defun view=proof-view-mode (bool)
  (setq view*on bool
	keim::rule*verbose nil))

(defun oc=cleanup-proof ()
  (let ((view*on nil))
    (if (pds~proof-plan-p omega*current-proof-plan)
	(pds~cleanup omega*current-proof-plan)
	(arg~signal-wrong-type 'proof-plan omega*current-proof-plan)))
  (when (and view*on (view~display omega*current-proof-plan))
    (view~display-proof omega*current-proof-plan)))


(defun view=pprint-node (line)
  (if keim::pds*print-entire-node 
      (progn
	(when (pdsn~open-node-p line)
	  (format t "               ...~%~%"))
	(pp~pprint-table 
	 ">>> " (list 4 :l)
	 (symbol-name (keim~name line))
	 (list view*label-width :l "" " ")
	 (mapcar #'symbol-name
		 (mapcar #'keim~name (if (pdsn~th-assumption-p line)
					 (pdsn~hyps line)
				       (remove-if #'pdsn~th-assumption-p (pdsn~hyps line)))))
	 (list view*hyps-width :l nil nil  
	       #'(lambda (s o)
		   (let ((*print-escape* nil)
			 (*print-miser-width* nil))
		     (format s "~%~:/pprint-fill/" o)
		     )))
	 " ! " '(3 :c)
	 (node~formula line) 
	 (list view*formula-width :l)
	 (if view*short-justs
	     (let* ((just (node~justification line))
		    (prems (just~premises just)))
	       (if prems
		   (format nil "~A: ~A" (keim~name (just~method just)) (mapcar #'keim~name (just~premises just)))
		 (symbol-name (keim~name (just~method just)))))
	   (node~justification line))
	 (list view*just-width :r " ")))
    (write (keim~name line))))


(com~defcommand linelength      ;;; works VS
  (argnames linelength)
  (argtypes posinteger)
  (arghelps "Length of the line")
  (frag-cats direct-display proof-view)
  (function view=linelength)
  (help "Set the width of the line (for printing purposes)"))


(defun view=linelength (posint)
  (setq *print-right-margin* posint))


#{\subsection{Displaying}#}
;;; Hiding

(com~defcommand hide-line        ;;; works   VS
  (argnames line)
  (argtypes ndline)
  (arghelps "A line")
  (function view~hide-line)
  (frag-cats proof-view)
  (defaults ((com~unspecified)))
  (help "Hide a line."))

(com~defcommand hide-line*        ;;; works   VS
  (argnames line-list)
  (argtypes ndline-list)
  (arghelps "A list of lines")
  (function view~hide-line*)
  (frag-cats proof-view)
  (defaults ((com~unspecified)))
  (help "Hide a series of lines."))

(com~defcommand unhide-line        ;;; works   VS
  (argnames line)
  (argtypes ndline)
  (arghelps "A line")
  (function view~unhide-line)
  (frag-cats proof-view)
  (defaults ((oc~default-current-planline)))
  (help "Unhide a line."))

(com~defcommand unhide-line*        ;;; works   VS
  (argnames line-list)
  (argtypes ndline-list)
  (arghelps "A list of lines")
  (function view~unhide-line*)
  (frag-cats proof-view)
  (help "Unhide a series of lines."))

(com~defcommand hide-proof        ;;; works   VS
   (argnames)
   (argtypes)
   (arghelps)
   (function view~hide-proof)
   (frag-cats proof-view)
   (defaults)
   (help "Hide the whole proof."))

(com~defcommand unhide-subproof
  (argnames line)
  (argtypes ndline)
  (arghelps "The root line of the subproof")
  (function view~unhide-subproof)
  (frag-cats proof-view)
  (defaults ((com~unspecified)))
  (help "Unhide a line and (recursively) all lines needed to proof this line (up to the plnned lines)."))


(com~defcommand unhide-conclusions        ;;; works   VS
  (argnames line)
  (argtypes ndline)
  (arghelps "The line in a proof")
  (function view~unhide-conclusions)
  (frag-cats proof-view)
  (defaults ((oc~default-current-planline)))
  (help "Unhide a line and all lines deduced (recursively) from this line."))

(com~defcommand show-conclusions
  (argnames line)
  (argtypes ndline)
  (arghelps "The line in a proof")
  (function view~show-conclusions)
  (frag-cats proof-view)
  (defaults ((oc~default-current-planline)))
  (help "Show the names of all lines deduced (immediatly) from this line."))

(com~defcommand hide-parameters
  (function view~hide-parameters)
  (frag-cats proof-view)
  (help "Display justifications without parameters."))

(com~defcommand unhide-parameters
  (function view~unhide-parameters)
  (frag-cats proof-view)
  (help "Display justifications with parameters."))



