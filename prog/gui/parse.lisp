;;; -*- Mode: KEIM; Base: 10; Syntax: Common-lisp; Package: OMEGA -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1997 by AG Siekmann, Fachbereich Informatik,             ;;
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

(mod~defmod parse :uses ()
	    :documentation "The parse functions for LOUI."
	    :exports  (parse~proof
		       parse~term
		       parse~command
		       parse~arguments
		       parse~list
		       parse~atom
		       parse~list-of-atoms
		       parse~string
		       parse~list-of-strings

;		       parse~insert-node
;		       parse~delete-node
;		       parse~update-node
		       
		       parse~listtest


		       parse~list
		       parse~name
		       parse~symbol)
	    )


(defvar parse*refnum 1)

(defun parse~proof (proof)
  (declare (edited  "22-JUN-1998")
	   (authors Hess)
	   (input   )
	   (effect  )
	   (value   ))
  (let ((root (prob~proof-root proof))
	(node*hash (make-hash-table)))
    (setq parse*refnum 1)
    (format nil "~a" (parse=proof root node*hash))))

(defun parse=proof (node node*hash)
  (declare (edited  "23-JUN-1998")
	   (authors Hess)
	   (input   )
	   (effect  )
	   (value   ))
  (let* ((name (keim~name node))
	 (meth (keim~name (pdsn~just-method node)))
	 (parameters (pdsn~just-parameters node)) ; list of parameters

	 (para (if parameters (format nil "\"~{~a~^ ~}\""
				      (mapcar #'(lambda (term)
						  (cond ((term~p term)
							 (format nil "$~a$" (parse=term term)))
							((listp term)
							 (format nil "(~{~a~^ ~})"
								 (mapcar #'(lambda (x)
									     (if (term~p x)
										 (format nil "$~a$" (parse=term x))
									       (format nil "~a" x)))
									 term)))
							(t (format nil "~a" term))))
					      parameters))
		 "nil"))

	 (remark "Kleiner Test, hehe") ;(meth~format-remark-string node))

	 (stat (pdsn~just-status node))

	 (just (pdsn~just-premises node))
	 (closed-by-application (format nil "~a" (if (pdsj~own-reason
                                                      (node~justification node))
                                                     "true"
                                                   "false")))
	 (supp (pds~node-supports node))

	 (assum (pds~problem-assumption-p node))
	 (asser (pdsn~th-assumption-p node))
	 (hyp   (pdsn~hypothesis-p node))

	 (hyprefs (mapcar #'keim~name (parse=compute-hyp-reference node)))
	 (hypreflist (if hyprefs (format nil "[~{'~a'~}]" hyprefs) "nil"))
	 
	 (curref parse*refnum)
	 (new (cond
	       (assum
		(format nil "p('~a'~a)" name hypreflist))
	       
	       (asser
		(format nil "t('~a'~a)" name hypreflist))
	       
	       (hyp
		(format nil "h('~a'~a)" name hypreflist))
	       
	       ((equal stat "open")
		(let* ((kids (parse=proof-kids supp node*hash))
		       (kidlist (if kids (format nil "[~{~a~}]" kids) "nil")))
		  (format nil "o('~a'~a ~a ~a)" name hypreflist closed-by-application kidlist)))
					; space between lists (maybe 2 nil)!!!
	       
	       ((equal stat "untested")
		(let* ((kids (parse=proof-kids just node*hash))
		       (kidlist (if kids (format nil "[~{~a~}]" kids) "nil")))
		  (format nil "a('~a''~a'~a \"~a\" ~a ~a ~a)" name meth para remark hypreflist closed-by-application kidlist)))
	       
	       ((equal stat "unexpanded")
		(let* ((kids (parse=proof-kids just node*hash))
		       (kidlist (if kids (format nil "[~{~a~}]" kids) "nil")))
		  (format nil "u('~a''~a'~a \"~a\" ~a ~a ~a)" name meth para remark hypreflist closed-by-application kidlist)))

	       ((equal stat "expanded")
		(let* ((kids (parse=proof-kids just node*hash))
		       (kidlist (if kids (format nil "[~{~a~}]" kids) "nil")))
		  (format nil "e('~a''~a'~a \"~a\" ~a ~a ~a)" name meth para remark hypreflist closed-by-application kidlist)))
	       
	       ((equal stat "grounded")
		(let* ((kids (parse=proof-kids just node*hash))
		       (kidlist (if kids (format nil "[~{~a~}]" kids) "nil")))
		  (format nil "g('~a''~a'~a \"~a\" ~a ~a ~a)" name meth para remark hypreflist closed-by-application kidlist)))
	       (t
		(let* ((kids (parse=proof-kids just node*hash))
		       (kidlist (if kids (format nil "[~{~a~}]" kids) "nil")))
		  (format nil "c('~a''~a'~a \"~a\" ~a ~a ~a)" name meth para remark hypreflist closed-by-application kidlist))))))
    (progn (setf (gethash name node*hash) curref)
	   new)))

(defun parse=proof-kids (kids node*hash)
  (parse=collect-corefs
   (mapcar (lambda (x)
	     (let ((already (gethash (keim~name x) node*hash)))
	       (if already already
		 (progn
		   (setq parse*refnum (+ 1 parse*refnum))
		   (parse=proof x node*hash)))))
	   kids)))

(defun parse=collect-corefs (input &optional (all nil) (output nil))
  (cond ((eq input nil)
	 (if all
	     (cons
	      (format nil "r([~{~a~^ ~}])" (reverse all))
	      (reverse output))
	   (reverse output)))
	(t (if (numberp (car input))
	       (parse=collect-corefs (cdr input) (cons (car input) all) output)
	     (parse=collect-corefs
	      (cdr input) all (cons (car input) output))))))

(defun parse=compute-hyp-reference (node)
  (if (or (pdsn~hypothesis-p node)
	  (eq (just~method (node~justification node)) (infer~find-method 'tps*hyp)))
      (parse=compute-associated-node node)
    (parse=compute-inserted-hyps node)))

(defun parse=compute-inserted-hyps (node)
  (set-difference (mapcan #'(lambda (x) (copy-list (pdsn~hyps x)))
			  (just~premises (node~justification node)))
		  (pdsn~hyps node)))

(defun parse=compute-associated-node (hyp)
  (when (pdsn~hypothesis-p hyp)
    (mapcan  
     #'(lambda (node) (when (some #'(lambda (x) (keim~equal x hyp))
				  (parse=compute-inserted-hyps node))
			(list node)))
     (prob~proof-steps omega::omega*current-proof-plan))))

(defun parse~term (term)
  (declare (edited  "23-JUN-1998")
	   (authors Hess)
	   (input   )
	   (effect  )
	   (value   ))
  (format nil "\"~A\"" (parse=term term)))

(defgeneric parse=term (term)
  (declare (edited  "26-JAN-1998")
	   (authors Hess)
	   (input   )
	   (effect  )
	   (value   ))
  (:method ((term fixnum))
	   (let ((output (format nil "[~A NUM]" term)))
	     output))
  (:method ((term term+constant))
	   (let ((output (format nil "[~A ~A]" (keim~name term)
				 (post~print (term~type term) nil))))
	     output))
  (:method ((term term+variable))
	   (let ((output (format nil "[~A ~A]" (keim~name term)
				 (post~print (term~type term) nil))))
	     output))
  (:method ((term term+appl))
	   (let* ((subterms (mapcar #'(lambda (x) (parse=term x))
				    (data~substructs term)))
		  (output (format nil "(~{~A~^ ~})" subterms)))
	     output))
  (:method ((term term+abstr))
	   (let* ((bound (data~abstr-bound-var term))
		  (scope (data~abstr-scope term))
		  (output (format nil "(LAM ~A ~A)" (parse=term bound)
				  (parse=term scope))))
	     output))
  (:method ((term term+schema))
	   (parse=term (data~schema-range term)))
  )

;; special terms
(defmethod parse=term ((term term+set))
  (let ((output (format nil "([SET ~A] ~{~A~^ ~})" (term~type term) (mapcar #'parse=term (term~normalform term)))))
    output))
(defmethod parse=term ((term term+list))
  (let ((output (format nil "([[LIST ~A] ~{~A~^ ~}])" (term~type term) (mapcar #'parse=term (term~normalform term)))))
	output))
(defmethod parse=term ((term term+cyc))
  (let ((output (format nil "([CYC ~A] ~{~A~^ ~})" (term~type term) (mapcar #'parse=term (term~normalform term)))))
    output))

(defmethod parse=term ((term term+tuple))
  (let ((output (format nil "([TUPLE ~A] ~{~A~^ ~})" (term~type term) (mapcar #'parse=term (term~normalform term)))))
    output))

(defmethod parse=term ((term term+vector))
  (let ((output (format nil "([VEC ~A] ~{~A~^ ~})" (term~type term) (mapcar #'parse=term (term~normalform term)))))
    output))

(defmethod parse=term ((term term+matrix))
  (let ((output (format nil "([MAT ~A] [~{[~{~^ ~A~}]~^ ~}])" (term~type term) (mapcar #'(lambda (row)
											  (mapcar #'parse=term row))
									    (term~normalform term)))))
    output))


(defun parse~command (name argtypes arghelps defaults agents?)
  (format nil "command(name:'~a' args:~a help:~a default:~a agents:~a)"
	  name
	  (if argtypes (format nil "[~{'~a'~}]" argtypes)
	    "nil")
	  (if arghelps (format nil "[~{\"~a\"~}]" arghelps)
	    "nil")
	  (if defaults
	      (if agents?
		  (format nil "[~{~a~}]"
			  (mapcar #'(lambda (x) (format nil "[~{'~a'~}]" x))
				  defaults))
		(format nil "[~{'~a'~}]" defaults))
	    "nil")
	  (if agents? (format nil "true")
	    (format nil "false"))))

(defun parse~arguments (args)
  (mapcar #'(lambda (x)
	      (cond ((equal x '(nil)) nil)
		    ((equal x "") "\"\"")
		    ((pdsn~p x) (keim~name x))
		    ((term~p x) (post~print x nil))
		    (t x)))
	  args))

(defun parse~list (list)
  (declare (edited  "5-AUG-1998")
	   (authors Hess)
	   (input   )
	   (effect  )
	   (value   ))
  (if list (format nil "[~{~a~}]" list) "nil"))

(defun parse~atom (symbol)
  (declare (edited  "22-JUN-1998")
	   (authors Hess)
	   (input   )
	   (effect  )
	   (value   ))
  (format nil "'~a'" symbol))

(defun parse~list-of-atoms (list)
  (declare (edited  "22-JUN-1998")
	   (authors Hess)
	   (input   )
	   (effect  )
	   (value   ))
  (if list (format nil "[~{'~a'~}]" list) "nil"))

(defun parse~string (symbol)
  (declare (edited  "22-JUN-1998")
	   (authors Hess)
	   (input   )
	   (effect  )
	   (value   ))
  (format nil "\"~a\"" symbol))

(defun parse~list-of-strings (list)
  (declare (edited  "22-JUN-1998")
	   (authors Hess)
	   (input   )
	   (effect  )
	   (value   ))
  (if list (format nil "[~{\"~a\"~}]" list) "nil"))

;;;;;;;;;;;;;;;;;;;;;;

(defun parse~insert-node (node)
  (let* ((name (keim~name node))
	 (just (first (pdsn~all-justs node)))
	 (reason (first (pdsj~reasons just)))
	 (parent (if (null reason) 'none
		   (pdsc~an-node reason)))

	 (meth (keim~name (pdsn~just-method node)))
	 
	 (stat (pdsn~just-status node))
	 
	 (just (mapcar (lambda (x)
			 (format nil "'~a'" (keim~name x)))
		       (pdsn~just-premises node)))

	 (supp (mapcar (lambda (x) ; gestrichelt
			 (format nil "'~a'" (keim~name x)))
		       (pds~node-supports node)))
	 
	 (assum (pds~problem-assumption-p node))
	 (asser (pdsn~th-assumption-p node))
	 (hyp   (pdsn~hypothesis-p node))

	 (new (cond
	       (assum
		(if parent (format nil "i(p('~a')'~a')" name parent)
		  (format nil "i(p('~a'))" name)))
	       (asser
		(if parent (format nil "i(t('~a')'~a')" name parent)
		  (format nil "i(t('~a'))" name)))
	       (hyp
		(if parent (format nil "i(h('~a')'~a')" name parent)
		  (format nil "i(h('~a'))" name)))

	       ((equal stat "open")
		(if supp (format nil "i(o'~a''~a'[~{~a~}])" name parent supp)
		  (format nil "i(o'~a''~a')" name parent)))

	       ((equal stat "untested")
		(if parent (format nil "i(a('~a''~a')'~a')" name meth parent)
		  (format nil "i(a('~a''~a'))" name meth)))
	       
	       ((equal stat "unexpanded")
		(if parent (format nil "i(u('~a''~a')'~a')" name meth parent)
		  (format nil "i(u('~a''~a'))" name meth)))

	       ((equal stat "expanded")
		(if parent (format nil "i(e('~a''~a')'~a')" name meth parent)
		  (format nil "i(e('~a''~a'))" name meth)))

	       ((equal stat "grounded")
		(if parent (format nil "i(g('~a''~a')'~a')" name meth parent)
		  (format nil "i(g('~a''~a'))" name meth)))

	       (t
		(if parent (format nil "i(c('~a''~a')'~a')" name meth parent)
		  (format nil "i(c('~a''~a'))" name meth))))))
    (socket~proof new)))

(defun parse~delete-node (node)
  (let* ((name (keim~name node))
	 (command (format nil "d('~a')" name)))
    (socket~proof command)))

(defun parse~update-node (node)
  (let* ((name (keim~name node))
	 (meth (keim~name (pdsn~just-method node)))
	 
	 (stat (pdsn~just-status node))

	 (just (pdsn~just-premises node))
	 (supp (pds~node-supports node))

	 (assum (pds~problem-assumption-p node))
	 (asser (pdsn~th-assumption-p node))
	 (hyp   (pdsn~hypothesis-p node))

	 (new (cond
	       (assum
		(format nil "u('~a'p)" name))
	       (asser
		(format nil "u('~a't)" name))
	       (hyp
		(format nil "u('~a'h)" name))
	       ((equal stat "open")
		(format nil "u('~a'o)" name))
	       ((equal stat "untested")
		(format nil "u('~a'a'~a')" name meth))
	       ((equal stat "unexpanded")
		(format nil "u('~a'u'~a')" name meth))
	       ((equal stat "expanded")
		(format nil "u('~a'e'~a')" name meth))
	       ((equal stat "grounded")
		(format nil "u('~a'g'~a')" name meth))
	       (t
		(format nil "u('~a'c'~a')" name meth)))))
    (socket~proof new)))

;;;;;;;;;;;;;;;;;;;;;;

(defun parse~listtest (list)
  (let* ((objects (mapcar #'(lambda (x) (keim~name x)) list))
	 (string (format nil "[~{'~a'~}]" objects)))
    (socket~proof string)
    t))

;;;;;;;;;;;;;;;;;;;;;;

;(defun parse~methods (hash)
;  (let ((erg nil)
;	(default omega::plcom*actual-method-key-list))
;    (maphash (lambda (x y) (declare (ignore y))
;	       (setq erg (cons x erg))) hash)
;    (if default
;	(socket~write (format nil "method([~{'~a'~}] [~{'~a'~}])" erg default))
;      (socket~write (format nil "method([~{'~a'~}] nil)" erg)))
;    t))

;(defun parse~env (&optional (proof omega::omega*current-proof-plan))
;  (let* ((objects (mapcar #'car (keim::env=locals (pds~environment proof))))
;	 (string (format nil "[~{'~a'~}]" objects)))
;    (socket~write string)
;    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(defun parse~list-of-lists (list)
;  (let ((hlist (when list (mapcar #'parse~list list))))
;    (socket~write (parse~list hlist))
;    t))

;(defun parse~list-of-strings (list)
;  (let ((string (if list (format nil "[~{\"~a\"~}]" list) "nil")))
;    (socket~write string)
;    t))

;(defun parse~string (s)
;  (let ((string (format nil "\"~a\"" s)))
;    (socket~write string)
;    t))

(defun parse~name (name)
  (let ((string (format nil "'~a'" name)))
    (socket~write string)
    t))

(defun parse~symbol (symbol)
  (let ((string (format nil "~a" symbol)))
    (socket~write string)
    t))

;(defun parse~command (command)
;  (let ((string (parse=command command)))
;    (socket~write string)
;    t))

;(defun parse~commands (commands)
;  (let ((string (if commands (format nil "[~{~a~}]"
;				     (mapcar #'parse=command commands))
;		  "nil")))
;    (socket~write string)
;    t))

;(defun parse=command (command)
;  (let ((name (keim~name command))
;	(args (com~argtypes command))
;	(help (com~arghelps command)))
;    (format nil "command(name:'~a' args:~a help:~a)"
;	    name
;	    (if args (format nil "[~{'~a'~}]" args)
;	      "nil")
;	    (if help (format nil "[~{'~a'~}]" help)
;	      "nil"))))

;(defun parse~defaults (defaults)
;  (let ((string (format nil "[~{~a~}]"
;			(mapcar #'(lambda (x) (format nil "[~{'~a'~}]" x))
;				defaults))))
;    (socket~write string)
;    t))

(defun parse~agenda (ordered-tasks)
  (let* ((task-strings (mapcar
			#'(lambda (x)
			    (if x 
				(format nil "[~{~a~}]"
					(mapcar
					 #'(lambda (task)
					     (format nil "'~a'(label:'~a' formula:\"~a\" blocked:~a)"
						     (if (agenda~pseudo-goal-p task) "Pseudo"
						       (if (agenda~goal-schema-p task) "SGoal" "Goal"))
						     (keim~name (agenda~task-node task))
						     (parse=term (agenda~task-formula task))
						     (if (agenda~task-blocked-p task) "true" "false")))
					 x))
			      "nil"))
			ordered-tasks))
	 (string (if task-strings (format nil "[~{~a~}]" task-strings) "nil")))
    (socket~write string)
    t))

(defun parse~plan-methods (methods)
  (let* ((objects (mapcar #'(lambda (x) (keim~name x)) methods))
	 (string (format nil "[~{\"~a\"~}]" objects)))
    (socket~write string)))

(defun parse~task-list (tasks)
  (let* ((objects (mapcar #'(lambda (x) (print-object x nil)) tasks))
	 (string (format nil "[~{\"~a\"~}]" objects)))
    (socket~write string)))

(defun parse~mmatching-list (matchings)
  (let* ((objects (mapcar #'(lambda (m)
			      (let* ((goal (plan~mmatch-goal m))
				     (prems1 (plan~mmatch-clsed-prems m))
				     (prems2 (plan~mmatch-exist-prems m))
				     (node-names (mapcar #'keim~name
							 (if goal
							     (cons goal (append prems1 prems2))
							   (append prems1 prems2)))))
				(concatenate 'string
					     (string (keim~name (plan~matched-method m)))
					     (format nil " on: ~{~A ~}~A"
						     (butlast node-names) (first (last node-names))))))
			  matchings))
	 (string (format nil "[~{\"~a\"~}]" objects)))
    (socket~write string)))
