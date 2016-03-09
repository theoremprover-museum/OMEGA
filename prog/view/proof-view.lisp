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

(in-package "OMEGA")

(mod~defmod view
	    :uses (com env inter keim mod pdsn pds node ot post pp sys socket mixin)
	    :documentation "Functions for emacs interface."
	    :exports (
		      view+view
		      view~display
		      view~set-display!
		      ;;;view+nd-line -> view+pds-node and view+pds-schematic-node
		      view+pds-node
		      view+pds-schematic-node
		      ;;;view+nd-proof -> view+pds
		      view+pds
		      view~update-proof
		      view~update-step
		      view~clean-proof
		      view~hide-line
		      view~hide-line*
		      view~unhide-line
		      view~unhide-line*
		      view~hide-proof
		      view~unhide-proof
		      view~unhide-subproof
		      view~unhide-conclusions
		      view~show-conclusions
		      view~set-hiding-state
		      view~hiding-state
		      )
	    )

(defvar view*on nil
  "Variable that determines whether some sort of additional output device (GUI or Emacs) is present.")

(defvar view*emacs-on nil
  "Variable that determines whether proof viewing in emacs is on.")

(defclass view+view ()
  ((display :initform nil :accessor view=display)))

(defgeneric view~display (obj)
  (:method ((obj t))
    nil)
  (:method ((obj view+view))
    (view=display obj))
  )

(defgeneric view~set-display! (obj newval)
  (:method ((obj view+view) newval)
    (setf (view=display obj) newval)))

(defclass view+pds-node (view+view pdsn+node)
  ()
  )

;;; Same thing for pdsn+schematic-node
(defclass view+pds-schematic-node (view+view pdsn+schematic-node)
  ()
  )


(defclass view+pds (view+view pds+proof-plan)
  ())

(defvar view*omega-emacs-type :emacs-19
  "A variable that stores which version of Emacs we are using. Value should
be either :emacs-19 or :emacs-18.")

(defmethod shared-initialize :after ((obj pdsn+node) slot-names &rest rest)
  (declare (ignore slot-names rest))
  (when (eq (class-of obj) (find-class 'pdsn+node))
    (change-class obj 'view+pds-node)))

;;; Same thing for pdsn+schematic-node
(defmethod shared-initialize :after ((obj pdsn+schematic-node) slot-names &rest rest)
  (declare (ignore slot-names rest))
  (when (eq (class-of obj) (find-class 'pdsn+schematic-node))
    (change-class obj 'view+pds-schematic-node)))


;(defmethod shared-initialize :after ((obj node+node) slot-names &rest rest)
;  (declare (ignore slot-names rest))
;  (when (eq (class-of obj) (find-class 'node+node))
;    (change-class obj 'view+pds-node)))

(defmethod shared-initialize :after ((obj pds+proof-plan) slot-names &rest rest)
  (declare (ignore slot-names rest))
  (when (eq (class-of obj) (find-class 'pds+proof-plan))
    (change-class obj 'view+pds)))

(defmethod shared-initialize :after ((obj view+pds) slot-names &rest initargs)
  (declare (ignore slot-names initargs))
  (let ((key-list (mapcar #'(lambda (hash)
			      (list obj hash))
			  natac*hash-list)))
    (mapc #'(lambda (key)
              (setf (gethash key natac*hashtable) (make-hash-table)))
          key-list)))

(defvar view*view nil)

(defvar view*hiding nil)

(defun view=step-text (step &optional hiding)
  (let* ((str (with-output-to-string (*standard-output*)
	       (if hiding
		   (if (pdsn~open-node-p step)
		       (format t "               ...~%~%>>> ~A ~%" (keim~name step))
		     (format t ">>> ~A ~%" (keim~name step)))
		   (let ((style 'view-pretty))
		     (unless (pp~find-style style)
		       (setq style 'pds-simple))
		     (pp~pprint step style)))))
	 (lines
	  (with-input-from-string (in str)
	  (let ((lines nil)
		(line nil))
	    (loop
	     (setq line (read-line in nil :eof))
	     (cond ((eq line :eof)
		    (return (nreverse lines)))
		   ((every #'(lambda (x) (char= x #\space)) line))
		   (t (push line lines)
		      (push (string #\newline) lines))))))))
    ;; added the extra newline below because the bridge stuff is 
    ;; swallowing the first character
    ;; actually it is only getting swallowed in emacs-18, not emacs-19.  
    ;; So we hope that the emacs handler has properly set the variable
    ;; view*omega-emacs-type Dan
    (apply #'concatenate 'string 
	   (if (eq view*omega-emacs-type :emacs-18)
		   (string #\newline)
		   "")
	   lines)))


;;;Updating the View when proof changes

(defmethod keim::prob=proof-steps-medium :around ((proof view+pds) steps)
  (let ((old-steps (copy-list (prob~proof-steps proof))))
     (call-next-method)
     (when (and view*on (view~display proof))
       (view=update-proof view*view old-steps steps ))))

(defmethod keim::node=justification-medium :after ((line view+pds-node) just) 
  (declare (ignore just))
  (when (and view*on (view~display line))
    (view~update-step nil line)))

;;; Same thing for pdsn+schematic-node
(defmethod keim::node=justification-medium :after ((line view+pds-schematic-node) just) 
  (declare (ignore just))
  (when (and view*on (view~display line))
    (view~update-step nil line)))

(defsetf keim::pdsn=hyps-medium :after ((line view+pds-node) hyps)
  (declare (ignore hyps))
  (when (and view*on (view~display line))
    (view~update-step nil line)))

;;; Same thing for pdsn+schematic-node
(defsetf keim::pdsn=hyps-medium :after ((line view+pds-schematic-node) hyps)
  (declare (ignore hyps))
  (when (and view*on (view~display line))
    (view~update-step nil line)))

(defun view=sort-steps (steps old-steps)
  (when steps
    (let ((fstep (car old-steps)))
      (cond ((null fstep) steps)
	    ((find fstep steps)
	     (cons fstep
		   (view=sort-steps (remove fstep steps) (cdr old-steps))))
	    (t (view=sort-steps steps (cdr old-steps)))))))

(defun view~update-proof (proof new-steps)
  (when (and view*on (view~display proof))
    (let ((view view*view)    
	  (old-steps (prob~proof-steps proof)))
	 (view=update-proof view old-steps new-steps))))

(defun view=update-proof (view old-steps new-steps)
  (when new-steps
    (let* ((old-step (first old-steps))
	   (new-step (first new-steps))
	   (old-in-new (member old-step new-steps))
	   (new-in-old (member new-step old-steps)))
      (cond ((eq old-step new-step)
	     (view=update-proof view (rest old-steps) (rest new-steps)))
	    ((null new-in-old)
	     (when (pdsn~p new-step)
	       (view=insert-new-step view old-step new-step))
	     (view=update-proof view old-steps (rest new-steps)))
	    ((null old-in-new)
	     (view=delete-step view old-step)
	     (view=update-proof view (rest old-steps) new-steps))
	    (t (view=update-proof view (rest old-steps)
				  (remove new-step new-steps)))))))
;;            (t
;;             (error 
;;              "KEIM (proof-view): Reihenfolge der Schritte vertauscht."))))))

(defun view=insert-new-step (view old-step new-step)
  (declare (ignore view))
  (view~set-display! new-step t)
  (when (mixin~activep)
      (let* ((type (cond
		    ((pds~problem-assumption-p new-step) "assumption")
		    ((pdsn~th-assumption-p new-step) "assertion")
		    ((pdsn~hypothesis-p new-step) "hypothesis")
		    (t (pdsn~just-status new-step))))
	     (action (let ((above (pdsj~above (node~justification new-step)))
			   (below (pdsj~below (node~justification new-step))))
		       (cond
			((equal type "unexpanded") (if above "both" "expand"))
			((equal type "expanded") (if below
						     (if above "both" "expand")
						   (if above "contract" "none")))
			((equal type "grounded") (if above "contract" "none"))
			(t "none"))))
	     (label (parse~string (keim~name new-step)))
	     (hyps (parse~list-of-strings (mapcar #'(lambda (x)
						      (keim~name x))
						  (pdsn~hyps new-step))))
	     (term (parse~term (node~formula new-step))) ; is a string
	     (method (parse~string (keim~name (pdsn~just-method new-step))))
	     (premises (parse~list-of-strings (mapcar #'keim~name
						      (pdsn~just-premises new-step))))
	     (supports (parse~list-of-strings (mapcar #'keim~name
						      (pds~node-supports new-step))))
	     (root-p (if (equal new-step (prob~proof-root omega*current-proof-plan))
			 "true"
		       "false"))
	     (hyp-info (parse~list-of-strings (mapcar #'keim~name
						      (view=compute-hyp-reference new-step))))
	     (node (format nil "~a ~a ~a ~a ~a ~a ~a ~a ~a" type action label hyps term method
			   (if (pdsn~open-node-p new-step) supports premises) root-p hyp-info))
	     )
	(socket~write (format nil "insertNode(~a)" node) :inout)))
  (when view*emacs-on
    (let* ((text (view=step-text new-step view*hiding)))
      (format t " insert ~A ~A" 
	      (if old-step (keim~name old-step) "xxx") text))))

(defun view=delete-step (view old-step)
  (declare (ignore view))
  (view~set-display! old-step nil)
  (when (mixin~activep)
    (socket~write (format nil "deleteNode(~a)"
			  (parse~string (keim~name old-step))) :inout))
  (when view*emacs-on
    (format t " delete ~A" (keim~name old-step))))

(let (loui-update)
  
  (defun view~update-step (view step &optional (hiding view*hiding))
    (declare (ignore view))
    (when (mixin~activep)
	(let* ((type (cond
		      ((pds~problem-assumption-p step) "assumption")
		      ((pdsn~th-assumption-p step) "assertion")
		      ((pdsn~hypothesis-p step) "hypothesis")
		      (t (pdsn~just-status step))))
	       (action (let ((above (pdsj~above (node~justification step)))
			     (below (pdsj~below (node~justification step))))
			 (cond
			  ((equal type "unexpanded") (if above "both" "expand"))
			  ((equal type "expanded") (if below
						       (if above "both" "expand")
						     (if above "contract" "none")))
			  ((equal type "grounded") (if above "contract" "none"))
			  (t "none"))))
	       (label (parse~string (keim~name step)))
	       (term (parse~term (node~formula step)))
	       (method (parse~string (keim~name (pdsn~just-method step))))
	       (premises (parse~list-of-strings (mapcar #'keim~name
							(pdsn~just-premises step))))
	       (supports (parse~list-of-strings (mapcar #'keim~name
							(remove step (pds~node-supports step)))))
	       (old-info (parse~list-of-strings (mapcar #'keim~name (view=compute-inserted-hyps step))))
	       (node (format nil "~a ~a ~a ~a ~a ~a ~a" type action label term method
			     (if (pdsn~open-node-p step) supports premises) old-info)))
	  (unless (string= loui-update node)
	    (socket~write (format nil "updateNode(~a)" node) :inout)
	    (setf loui-update node)
	    )
	  ))
    (when view*emacs-on
      (let* ((text (view=step-text step hiding)))
	(format t " update ~A ~A" (keim~name step) text))))
  
  )

(defun view~clean-proof (view)
  (when (mixin~activep)
    (socket~write "clearProof" :inout))
  (when view*emacs-on
    (format t " clean ~A ~A" view "")))

;;; Hiding Lines

(defun view~hide-line (line)
  (view~set-display! line nil)
  (when view*emacs-on
    (let* ((text (view=step-text line t)))
      (format t " update ~A ~A" (keim~name line) text))))

(defun view~hide-line* (line-list)
  (when view*emacs-on
    (mapc #'view~hide-line line-list)
    )
  )

(defun view~unhide-line (line)
  (view~set-display! line t)
  (when view*emacs-on
    (let* ((text (view=step-text line nil)))
      (format t " update ~A ~A" (keim~name line) text))))

(defun view~unhide-line* (line-list)
  (when view*emacs-on
    (mapc #'view~unhide-line line-list)
    )
  )

(defun view~display-line (line)
  (view~set-display! line nil))

(defun view~undisplay-line (line)
  (view~set-display! line t))

(defun view~hide-proof (&optional (proof omega*current-proof-plan))
  (view~set-display! proof nil)
  (mapc #'view~hide-line (prob~proof-steps proof)))

(defun view~unhide-proof (&optional (proof omega*current-proof-plan))
  (view~set-display! proof t)
  (mapc #'view~unhide-line (prob~proof-steps proof)))

(defun view~unhide-subproof (line)
  (unless (view~display line)
    (view~unhide-line line)
    (mapc #'view~unhide-subproof (pdsn~just-premises line))
    (mapc #'view~unhide-line (pds~node-supports line))))

;;; not sure what this really is supposed to do
(defun view~unhide-conclusions (initial-line)
  (let ((lines (member initial-line (prob~proof-steps omega*current-proof-plan))))
    (keim~put initial-line :view*hide t)
    (view~unhide-line initial-line)
  (dolist (line lines)
    (when (some #'view~display
		(pdsn~just-premises line))
      (view~unhide-line line)))
  (dolist (line lines)
    (keim~put line :unhide nil))))

;;; nor this
(defun view~show-conclusions (initial-line)
  (format t "Conclusions of line ~A: " (keim~name initial-line))
  (let ((lines (member initial-line (prob~proof-steps omega*current-proof-plan))))
  (dolist (line lines)
    (when (member initial-line (pdsn~just-premises line))
      (format t "~A " (keim~name line))))))

(defun view~set-hiding-state (flag)
  (setf view*hiding flag))

(defun view~hiding-state ()
  view*hiding)

(defvar view*label-width   10 "default width for line labels in output")
(defvar view*hyps-width 10 "default width for hypotheses in output")
(defvar view*formula-width 40 "default width for formulas in output")
(defvar view*just-width 15 "default width for justifications in output")
(defvar view*short-justs nil "Indicates whether justifications are written verbosely.")

(defun view~display-proof (proof)
  (let ((keim::pds*print-entire-node t)
	(lines (prob~proof-steps proof))
	(*print-right-margin* (- (or *print-right-margin* 79) 5)))
    (multiple-value-setq (view*label-width view*hyps-width 
	                  view*formula-width view*just-width)
	(pds~figure-margins lines))
    (dolist (line lines)
      (view=insert-new-step view*view nil line))))
 
(defun view~hide-parameters ()
  (setf view*short-justs t))

(defun view~unhide-parameters ()
  (setf view*short-justs nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions to compute reference information for introduced hyps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun view=compute-hyp-reference (hyp)
  (when (or (pdsn~hypothesis-p hyp)
	    (eq (just~method (node~justification hyp)) (infer~find-method 'tps*hyp)))
    (mapcan  
     #'(lambda (node) (when (some #'(lambda (x) (keim~equal x hyp))
				  (view=compute-inserted-hyps node))
			(list node)))
     (prob~proof-steps omega::omega*current-proof-plan))))

(defun view=compute-inserted-hyps (node)
  (set-difference (mapcan #'(lambda (x) (copy-list (pdsn~hyps x)))
			  (just~premises (node~justification node)))
		  (pdsn~hyps node)))

