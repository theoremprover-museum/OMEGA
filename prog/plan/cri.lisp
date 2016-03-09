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

(mod~defmod CRI 
            :uses (keim omega pds)
            :documentation "The module of the control rule interpreter"
            :exports (cri+control-rule
                      cri+kind
                      
                      cri~apply-rule
                      cri~call
                      cri~def-control-rule
                      cri~find-control-rule
                      cri~if-part
                      cri~kind
;                      cri~parameters
                      cri~read-object
                      cri~remove-all-used-control-rules
		      cri~clear-hashtables
		      
                      cri~reset-parameters
                      cri~set-parameters
                      cri~set-used-control-rules
                      cri~set-used-control-rules!
                      cri~side-effect
                      cri~to-do-part
                      cri~used-control-rules-of-kind
                      
                      cri*control-rules-hashtable
                      cri*current-agenda
                      cri*current-pds
                      cri*current-task
                      cri*current-tasks
                      cri*new-hashtable
                      cri*use-rating
                      cri*used-control-rules-hashtable
                      cri*verbose))


;(eval-when (load compile eval)

;-----------------------------------------------
;---- classes and global-variables -------------
  
(defclass cri+kind (keim+name keim+object)
  ()) 
  
(defclass cri+control-rule (keim+name keim+object)
  ((kind       :initarg :kind
	       :accessor cri~kind
	       :documentation "This is/are the kind(s) of a control-rule, a list or a symbol.")
   (if-part    :initarg :if-part
	       :accessor cri~if-part
	       :documentation "This is the if-part of a control-rule: a conjuction of meta-predicates")
   (to-do-part :initarg :to-do-part
	       :accessor cri~to-do-part
	       :documentation "This is the conjuction of what the control-rule should do
when fulfiled")
   (side-effect-part :initarg :side-effect
		     :accessor cri~side-effect
		     :documentation "This is the side-effect of a c-rule")
;   (parameters :initarg :parameters
;               :accessor cri~parameters
;               :documentation "These are the parameters of a c-rule")
   ;(:documentation "the datastructure for a control-rule"))
   )
  )

;(defclass cri+variable (keim+name keim+object)
;  ())
;
;(defvar cri*variables (make-hash-table :test #'equal)
;  "The hashtable for the variables")
;
;(defun cri=create-var (name)
;  (let* ((namesymb (intern name (find-package :omega)))
;         (var (make-instance 'cri+variable
;                             :name namesymb)))
;    (setf (gethash name cri*variables) var)
;    var
;    ))

(defvar cri*used-control-rules-hashtable (make-hash-table :test #'equal)
  "The hashtable containing the control-rules that are actually used, indexed by the kinds")

;(defvar cri*kinds-hashtable (make-hash-table :test #'equal)
;  "The hashtable of the different kinds of control-rules.")

(defvar cri*current-original-alternative-list nil
  "The list of the initial alternative list given to cri-call")

(defvar cri*control-rules-hashtable (make-hash-table :test #'equal)
  "The hashtable for the control-rules.")

(defvar cri*use-rating t "If true, use the rating of the methods")

(defvar cri*verbose nil "Verbose: true/nil")

(defvar cri*current-pds nil)

(defvar cri*current-agenda nil)

(defvar cri*current-tasks nil)

(defvar cri*current-task nil)

(defvar cri*current-task-node nil)

(defvar cri*current-task-formula nil)
 
(defvar cri*current-method nil)

(defvar cri*current-supports nil)

(defvar cri*applied-rules nil)

(defvar cri*mmatchings nil)

;---------------------------------
; matching the control-rules
;---------------------------------
(defun cri~call (alternative-list &key ((:kind kind))
				  ((:pds pds) omega*current-proof-plan)
				  ((:tasks tasks))
				  ((:agenda agenda) (pds~agenda pds))
				  ((:task task))
				  ((:task-node task-node))
				  ((:task-formula task-formula))
				  ((:method method))
				  ((:crules crules))
				  ((:mmatchings mmatchings))
				  )
  (declare (edited  "07-FEB-1997")
	   (authors Cullrich)
	   (input   "An alternative list, a keyword kind, and optional a keyword pds")
	   (effect  "side-effects specified in the control-rules")
	   (value   "the changed alternative-list according to the contrul-rules of the"
		    "specified kind. The controle-rules use the pds given with the"
		    "'pds' keyword"))
;  (format t "~% task: ~S" task)
 ; (format t "~% tasks: ~S" tasks)

  (let* ((cri*current-original-alternative-list (copy-list alternative-list))
	 (cri*used-control-rules-hashtable
	  (if crules (cri~set-used-control-rules crules)
	    cri*used-control-rules-hashtable))
	 (cri*current-tasks (copy-list tasks))
	 (cri*current-agenda agenda)
	 (cri*current-pds pds)
;	 (cri*current-task (if task task
;                             (first tasks)))
	 (cri*current-task task)
	 (cri*current-task-node task-node)
	 (cri*current-task-formula task-formula)
	 (cri*current-method method)
	 (cri*applied-rules nil)
	 (cri*mmatchings mmatchings)
	 (cri*current-supports
	  (when cri*current-task
	    (pds~node-supports (agenda~task-node cri*current-task))))
	 (rules (cri~used-control-rules-of-kind kind))
	 )
;    (format t "~% curren-supp :~S" cri*current-supports)
    (cri=mark-alternative-list alternative-list)
    (when cri*verbose (progn (omega~message "~% Rules: ~S" rules)
			     (omega~message "~% Incoming alternative list: ~S" alternative-list)))
    (let ((result 
	   (if rules
	       (cri=call rules alternative-list kind)
	     alternative-list)))
      (when cri*verbose (omega~message "~% Resulting alternative list: ~S" result))
      (values result
	      cri*applied-rules))
    )
  )


(defun cri=call (rules alternative-list kind)
  (declare (edited  "07-FEB-1997")
	   (input   "A list of rules, an alternative-list and a kind.")
           (effect  "side-effects specified in the control-rules.")
           (value   "the changed alternative-list."))
  (if alternative-list
      (if (not rules)
	  alternative-list ;; no more rules? finished, return alternative-list
	(let* ((rule (cri~find-control-rule (first rules)))
	       (match (cri=rule-is-matching rule)))
	  (when cri*verbose (omega~message "~%Rule ~S matched with ~S" rule match))
	  (if match ;; if the rule does match
	      (progn
		(push rule cri*applied-rules)
		(if (cri=is-return-control-rule-p rule)
                                        ; and is a rule with return action, then apply it and return
		    (cri~apply-rule rule alternative-list match kind) 
                                        ; else apply it and continue with the other control-rules
		  (cri=call
		   (rest rules)
		   (cri~apply-rule rule alternative-list match kind)
		   kind)
		  ))
	    ;; else if the rule does not match continue in the rule-list
	    (cri=call (rest rules) alternative-list kind))))
    ;; else: there is no alternative list. Then check for the next insert-rule
    (multiple-value-bind (next-insert-rule rest-rules)
	(cri=following-rule-is-insert rules)
      (when next-insert-rule
	(let ((match (cri=rule-is-matching next-insert-rule)))
	  (when cri*verbose (omega~message "~%Rule ~S matched with ~S" next-insert-rule match))
	  (if match ;; if the rule does match
	      (progn
		(push next-insert-rule cri*applied-rules)
		(cri=call
		 rest-rules
		 (cri~apply-rule next-insert-rule alternative-list match kind)
		 kind))
	    (cri=call rest-rules alternative-list kind)
	    ))
	))))

(defun cri=following-rule-is-insert (rules)
  (when rules
    (let ((rule (cri~find-control-rule (first rules))))
      (when (member 'insert (mapcar #'first (cri~to-do-part rule)))
;      (when (eq (first (cri~to-do-part rule)) 'insert)
	(values rule (rest rules))))))
  
(defun cri=is-return-control-rule-p (rule)
  (member 'return (mapcar #'first (cri~to-do-part rule))))

(defun cri=rule-is-matching (rule)
  (declare (edited  "07-FEB-1997")
	   (input   "a rule")
           (effect  "none")
           (value   "nil, true or a list of (maybe more than one) possible"
		    "mappings"))
  (cri=eval-if-part
   (cri~if-part rule)
   (list (mapp~create nil nil))
   )
  )


(defun cri=eval-if-part (if-part mappings)
  (declare (edited  "07-FEB-1997")
           (input   "a conjunction of meta-predicates and a list of pos. mappings")
           (effect  "normaly none (depends on the meta-predicates)")
           (value   "nil, t or a list of possible mappings"))
  (let ((action (first if-part))) ;; get the first action of the if-part
    (case action
      ('and         ;; is it an 'and', then evaluate the two meta-predicates,
       (let ((new-mappings (cri=eval-if-part (second if-part) mappings)))
	 (when new-mappings ;; if the first meta-prd evaluates to some mappings
	   (cri=eval-if-part               ;; then evaluate the second in context of the second
	    (third if-part)
	    new-mappings))))
      ('or ;; evaluate both mappings and if at least one evaluates to something then
	   ;; return them
       (let ((new-mappings1 (cri=eval-if-part (second if-part) mappings))
	     (new-mappings2 (cri=eval-if-part (third if-part) mappings)))
	 (when (or new-mappings1 new-mappings2)
	   (append new-mappings1 new-mappings2))))
      ('not
       (unless (cri=eval-if-part (second if-part) mappings)
	 mappings))
      (t
       ;(cri=inst-remove-nil
	(cri=eval-meta-prd if-part mappings)
      ;; evaluate the single meta-predicate and remove empty mappings
       ))))

;(defun cri=inst-remove-nil (mappings)
;  ;; removes nils from mappings ??is this still necessary? don't think so. will remove it
;  (if (consp mappings)
;      (remove-if 'not mappings)
;    mappings))

	   
(defun cri=eval-meta-prd (meta-prd mappings)
  (declare (edited  "07-FEB-1997")
           (input   "a meta-predicate (a special lisp function) and a list of possible mappings")
           (effect  "normaly none (depends on the meta-predicates)")
           (value   "nil, t or a list of possible mappings"))
   (mapcan #'(lambda (x) (cri=eval-and-instantiate meta-prd x))  mappings))
  ;; evaluates and instantiates the meta-predicate in all possible mappings

(defun cri=eval-and-instantiate (meta-prd mapping)
  (declare (edited  "07-FEB-1997")
           (input   "a meta-predicate and the mapping for the meta-prd")
           (effect  "normaly none (depends on the meta-predicates)")
           (value   "nil, t or a list of possible all mappings resulting of the metaprd."))
  (let ((resulted-maps
	 (cri=translate-lists-to-mappings
	  (apply (first meta-prd) (cri=instantiate (rest meta-prd) mapping)))))
    ; resulted-map should either be nil, t or a list of mappings
    (if resulted-maps (cri=merge-maps resulted-maps mapping))
    ;; if it is a list of mapping append them to the original inst.
    ))


(defun cri=translate-lists-to-mappings (lists)
  ;; sometimes we have to translate the lists of lists returned by the metapredicate into mappings
  (if (consp lists) ;;otherwise the metaprd returned T or nil
      (if (mapp~p (first lists)) lists
	(mapcar #'(lambda (liste)
		    (cri=translate-list-to-mapping liste
						   (mapp~create nil nil)))
		lists))
    lists))

(defun cri=translate-list-to-mapping (liste result)
  (if liste
      (cri=translate-list-to-mapping
       (rest liste)
       (mapp~insert-component
	(first (first liste))
	(if (listp (cdr (first liste))) ;;liste can have these forms: (a . b) or (a b) or
					;;(a b c d e) (last one is not OK!!! but is
					;;accepted due to historic reasons
	    (if (= 1 (length (cdr (first liste))))
		(second (first liste))
	      (cdr (first liste)))
	  (cdr (first liste)))
	result))
    result))
  
(defun cri=merge-maps (new-mappings mapping)
  (declare (edited  "07-FEB-1997")
           (input   "a list of mappings and a single mapping (e.g. (I1 I2) I4 )")
           (effect  "none")
           (value   "a list of inst. with the mapping appended to all"))
  (if (consp new-mappings) ;; if it is not a list, it is t
      (mapcar #'(lambda (x) (cri=combine-mapping x mapping)) new-mappings)
    (list mapping)))

(defun cri=combine-mapping (m1 m2)
  (declare (edited  "08-AUG-2000")
	   (authors Cullrich)
	   (input   "Two mappings m1 and m2")
	   (effect  "none")
	   (value   "Inserts m1 in m2"))
  (cri=insert-components
   (mapp~domain m1)
   (mapp~codomain m1)
   m2))

(defun cri=insert-components (d1s c1s m)
  (if (and d1s c1s)
      (cri=insert-components (rest d1s)
			     (rest c1s)
			     (mapp~insert-component (first d1s)
						    (first c1s)
						    m))
      m))

;---------------------------------------
; applying the controle-rules
;---------------------------------------

(defun cri~apply-rule (rule alternative-list mappings kind)
  (declare (edited  "10-FEB-1997")
	   (authors Cullrich)
	   (input   "a rule, an alternative-list and a list of mappings")
	   (effect  "depends on the side-effects")
	   (value   "the alternative-list changed according to the rule"))
;  (when alternative-list ;; apply the rule only if there are still alternatives
;    (if (not mappings) alternative-list
      ;; if there are no more possible mappings then return the alternative-list
;	 (command (first first-action))
;	 (argument (second first-action)))
    (cri=apply-action (first (cri~to-do-part rule))
		      (rest (cri~to-do-part rule))
		      mappings kind alternative-list))

(defun cri=apply-action (action rest-actions mappings kind alternative-list)
  (declare (edited  "23-AUG-2000")
	   (authors Cullrich)
	   (input   "an action, more actions, a mapping, a kind and some alternatives")
	   (effect  "")
	   (value   "applies all actions on the alternative list"))
  (let* ((command (first action))
	 (argument (second action))
	 (result
	  (case command
	    ( 'select (cri=apply-select argument alternative-list mappings))
	    ( 'reject (cri=apply-reject argument alternative-list mappings))
	    ( 'prefer (cri=apply-prefer argument alternative-list mappings))
	    ( 'defer (cri=apply-defer argument alternative-list mappings))
	    ( 'insert (cri=apply-insert argument alternative-list mappings))
	    ( 'insert-end (cri=apply-insert-end argument alternative-list mappings))
	    ( 'order-before (cri=apply-order-before argument alternative-list mappings))
	    ( 'return alternative-list)
;	   ( 'choose (cri=apply-choose argument alternative-list mappings))
	    )))
    (if rest-actions
	(cri=apply-action (first rest-actions)
			  (rest rest-actions)
			  mappings kind result)
      result)))

(defun cri=apply-order-before (to-order-before alternative-list mappings)
  (let* ((result alternative-list))
    (dolist (x to-order-before)
      (if (null (listp x))
	  (omega~warn "~%For order-before crule you need pairs (item1 item2).")
	(let* ((item1 (first x))
	       (item2 (second x))
	       (instantiations-of-item1 (cri=create-all-instantiations item1 mappings))
	       (instantiations-of-item2 (cri=create-all-instantiations item2 mappings)))

	  ;; go through the alternatives-list
	  ;; as soon as you find the first alternative ALT which matchs with the instantiations-of-item2
	  ;; make all alternatives which match with the instantiations-of-item1 and are behind this ALT
	  ;; before this ALT
	  ;; if no ALT is found nothing happens
	  
	  (do* ((rest-result result (rest rest-result))
		(first-item2-flag nil)
		(back nil))
	      ((or (null rest-result)
		   first-item2-flag)
	       (when first-item2-flag
		 (setf result back)))
	    (let* ((head (first rest-result)))
	      (if (find head instantiations-of-item2 :test #'cri=included)
		  (let* ((rest-rest (rest rest-result))
			 (all-alternatives-to-item1 (remove-if-not #'(lambda (alt)
								       (find alt instantiations-of-item1 :test #'cri=included))
								   rest-rest))
			 (other-alternatives (remove-if #'(lambda (alt)
							    (find alt instantiations-of-item1 :test #'cri=included))
							rest-rest)))
		    (setf first-item2-flag 't)
		    (setf back (append back all-alternatives-to-item1 (list head) other-alternatives)))
		(progn
		  (setf back (append back (list head))))))))))
    result))

(defun cri=included (item1 item2)
  (cond ((and (listp item1)
	      (null (listp item2)))
	 ;; item one more specific than item2 
	 nil)
	((and (null (listp item1))
	      (listp item2))
	 (eql item1 (first item2)))
	(t
	 (eql item1 item2))))	       
	       
	
(defun cri=apply-select (to-select alternative-list mappings)
  (let ((result ()))
    (dolist (x to-select)
      (let* ((instantiations (cri=create-all-instantiations x mappings)))
	(dolist (inst instantiations)
	  (when T
	    ;; (or (and (consp inst) (find (first inst) alternative-list))
	    ;;     (and (not (consp inst)) (find inst alternative-list)))
	    (setf result
		  (cons (if (not (consp inst))
			    (cri=set-marker inst)
			  inst)
			result))
	    ))))
    (if result
	;; if there is a result return it
	(reverse (remove-duplicates result :test #'keim~equal))
      ;; if there is no result return the original list
      alternative-list)
    ))

;; In previous versions of cri=apply-select after the when there was a check:
;;                 (or (and (consp inst) (find (first inst) alternative-list))
;;                     (and (not (consp inst)) (find inst alternative-list)))
;; This check was needed for instance, to ensure, that only such things were added that were already in the
;; alternative list and nothing else!
;; However, it turned out that this check becomes quite stupid when manipulating supports and parameters!
;; (How to add all possible parameter instantiations and all possible supports combinations into the list)?
;; Hence, AMEIER remved this check simply. 



;(defun cri=find-method (x)
;  (if (meth~p x) x (meth~find-method x)))

;;(defun cri=create-all-instantiations (element mappings)
;;  (let ((method (cri=find-method (first element))))
;;    (mapcar #'(lambda (inst)
;;		(cri=set-marker
;;		 (cons method
;;		       (cri=instantiate (rest element) inst))))
;;	    mappings)))

(defun cri=create-all-instantiations (element mappings)
  (mapcar #'(lambda (inst)
	      (cri=set-marker (cri=instantiate element inst)))
	  mappings))

;;(defun cri=apply-prefer (to-prefer alternative-list mappings)
;;  (progn 
;;    (dolist (x (reverse to-prefer))
;;      (let ((element (if (consp x)
;;			 ;; in this case we have something like (t1 restargs), so we choose
;;			 ;; on the first argument
;;			 (cri=find-method (first x))
;;		       (cri=find-method x) ;;as methods are given as
;;		       ;;symbols we have to check whether the symbol represents a method
;;		       )))
;;	(if (find element alternative-list)
;;	    (progn 
;;	      (unless (consp x) (delete element
;;					alternative-list))
;;	      ;; if an item to prefer is an item combined with something, thou shall
;;	      ;; not delete it from the alternative-list
;;	      (setf alternative-list
;;		    (if (consp x)
;;			(append (cri=create-all-instantiations x mappings)
;;				alternative-list)
;;		      (cons (cri=set-marker element) alternative-list)))
;;	      ))))
;;   (remove-duplicates alternative-list
;;		       :test #'keim~equal)))

(defun cri=apply-prefer (to-prefer alternative-list mappings)
  (dolist (x (reverse to-prefer))
    (let* ((instantiations (cri=create-all-instantiations x mappings)))
      
      (dolist (y (reverse instantiations))
	(let* ((element (if (consp y)
			    (first y)
			  y)))
	  
	  (when (find element alternative-list)
	    (unless (consp y)
	      (delete element alternative-list))
	    ;; if an item to prefer is an item combined with something, thou shall
	    ;; not delete it from the alternative-list
	    (setf alternative-list
		  (if (consp y)
		      (cons y alternative-list)
		    (cons (cri=set-marker y) alternative-list))))))))
  (remove-duplicates alternative-list
		     :test #'keim~equal))

(defun cri=apply-defer (to-defer alternative-list mappings)
  (let* ((reverse-alternatives (reverse alternative-list))
	 (new-alternatives (cri=apply-prefer to-defer reverse-alternatives mappings)))
    (reverse new-alternatives)))

(defun cri=apply-reject (to-reject alternative-list mappings)
  (unless (eql to-reject 'all)
    (dolist (x to-reject)
      (let* ((instantiations (mapcar #'(lambda (mapping)
					 (cri=instantiate x mapping))
				     mappings)))
	(setf alternative-list
	      (remove-if #'(lambda (alternative)
			     (find alternative instantiations :test #'keim~equal))
			 alternative-list))))
;              (cond ((consp x)
;                     (remove-if #'(lambda (alternative)
;                                    (find alternative instantiations))
;                                alternative-list))
;                    (t
;                     (remove-if #'(lambda (alternative)
;                                    (find alternative instantiations))
;                                alternative-list))))))
    alternative-list))

(defun cri=apply-insert (to-insert alternative-list mappings)
  (let ((result alternative-list))
    (dolist (x (reverse to-insert))
      (setf result
;            (if (consp x)
;                (append (cri=create-all-instantiations x mappings)
;                        result)
	    (append (cri=create-all-instantiations x mappings) result)))
    (remove-duplicates result
		       :test #'keim~equal)))

(defun cri=apply-insert-end (to-insert alternative-list mappings)
  (let ((result alternative-list))
    (dolist (x (reverse to-insert))
      (setf result
;            (if (consp x)
;                (append (cri=create-all-instantiations x mappings)
;                        result)
	    (append result (cri=create-all-instantiations x mappings))))
    (remove-duplicates result
		       :test #'keim~equal)))

;(defun cri=apply-choose (to-choose alternative-list mappings)
;  ;; choose it like a single select that stops further processing by cri:
;  ;; if a choose action is applied, then no other rules are evaluated
;  (let ((result ()))
;    (dolist (x to-choose)
;      (let* ((instantiations (cri=create-all-instantiations x mappings)))
;        (dolist (inst instantiations)
;          (if (or (and (consp x) (find (first inst) alternative-list))
;                  (and (not (consp x)) (find inst alternative-list)))
;              (setf result
;                    (cons (if (not (consp inst))
;                              (cri=set-marker inst)
;                            inst)
;                          result))
;          ))))
;    (if result
;        (reverse (remove-duplicates result :test #'keim~equal))
;      alternative-list) ;; if there is no result return the
;    ;; original list
;    ))

#|
;(defun cri~apply-rule (rule alternative-list mappings kind)
;  (declare (edited  "10-FEB-1997")
;           (authors Cullrich)
;           (input   "a rule, an alternative-list and a list of mappings")
;           (effect  "depends on the side-effects")
;           (value   "the alternative-list changed according to the rule"))
;  (if (eq kind 'methods)
;      (cri=apply-rule-for-method (cri~to-do-part rule) alternative-list mappings)
;    (when alternative-list ;; apply the rule only if there are still alternatives
;      (if (not mappings) alternative-list ;; if there are no more possible
;        ;; mappings then return the
;        ;; alternative-list
;        (cri~apply-rule ;; else apply the side-effect and the commands of the first mapping 
;         rule           ;; and then of the rest of the instations
;         (let* ((to-do-part (cri~to-do-part rule))
;                (side-effect (cri~side-effect rule))
;                (mapping (first mappings)))
;           (cri=apply-side-effect side-effect mapping)
;           (cri=apply-to-do-part to-do-part
;                                 alternative-list
;                                 mapping
;                                 kind))
;         (rest mappings)
;         kind)))))

;;; added for methods: special treatment
;;; will be integrated in the future

(defun cri=apply-rule-for-method (then-part alternative-list mappings)
  (let ((command (first then-part)) (argument (second then-part)))
    (case command
      ( 'select (cri=apply-select-for-methods-h argument alternative-list mappings))
      ( 'reject (cri=apply-reject-for-methods-h argument alternative-list mappings))
      ( 'prefer (cri=apply-prefer-for-methods-h argument alternative-list mappings))
      ( 'choose (cri=apply-choose-for-methods-h argument alternative-list mappings))
      )))

(defun cri=apply-to-do-part (then-part alternative-list mapping kind)
    (declare (edited  "07-FEB-1997")
           (input   "an then-part, an alternative-list and an mapping")
           (effect  "none")
           (value   "the changed alternative-list"))
  (when alternative-list ;; if there are no more alternatives -> finished
    (case kind
      ('schematic-tasks (cri=evaluate-control-rules-for-schematic-tasks then-part alternative-list mapping))
      ('methods (cri=evaluate-control-rules-for-methods then-part alternative-list mapping))
      ('sequents (cri=evaluate-control-rules-for-sequents then-part alternative-list
							  mapping))
      ('supports (cri=evaluate-control-rules-for-supports then-part alternative-list
							  mapping))
      ('ref (cri=evaluate-control-rules-for-boards then-part alternative-list
							  mapping))
      ('strategic (cri=evaluate-strategic-control-rules then-part alternative-list mapping))
      ('strategy-interruption (cri=evaluate-strategy-interruption-control-rules then-part alternative-list mapping))
      ('mmatchings (cri=evaluate-control-rules-for-mmatchings then-part alternative-list mapping))
      ('adaption (cri=evaluate-control-rules-for-adaptions then-part alternative-list mapping))
      (t (cri=evaluate-control-rules-for-schematic-tasks then-part alternative-list mapping))
      )))
;; Some additions to handle strategic, strategy-interruption, and mmatchings CRules, AMEIER

(defun cri=apply-side-effect (side-effect mapping)
    (declare (edited  "07-FEB-1997")
 	   (input   "a side-effect and an mapping")
           (effect  "depends on side-effect")
           (value   "not specified"))
  (if side-effect (apply (first side-effect) (cri=instantiate (rest side-effect) mapping)))
  )

;;------------------ evaluation of control-rules for analogy
;;
;; added by cullrich 18.5.00
(defun cri=evaluate-control-rules-for-adaptions (then-part alternative-list mapping)
  (let* ((command (first then-part))
	 (argument (second then-part)))

    (case command
      ('select
       (cri=instantiate argument mapping))
      (t
       (omega~error "Only select implemented for adaptions.")))))

;;------------------ evaluation of control-rules for mmatchings
;;
;; ADDED BY AMEIER 15.2.00


(defun cri=evaluate-control-rules-for-mmatchings (then-part alternative-list mapping)
  (let* ((command (first then-part))
	 (argument (second then-part)))

    (case command
      ('reject (cri=apply-reject-for-mmatchings (cri=instantiate argument mapping)
						alternative-list))
      (t
       (error "Only reject implemented until now for mmatchings.")))))


(defun cri=apply-reject-for-mmatchings (reject-list alternative-list)
  (declare (edited  "21-JUN-1999")
	   (authors Ameier)
	   (input   "a list of elements to reject and an alternative list")
           (effect  "none")
           (value   "the alternative-list without the elements in reject-list"))
  (if (eql reject-list 'all)
      ()
    (remove-if #'(lambda (alternative)
		   (find alternative reject-list :test #'eq))
	       alternative-list)))

;;------------------ evaluation of control-rules for strategy interruption
;;
;; ADDED BY AMEIER, 15.2.00

(defun cri=evaluate-strategy-interruption-control-rules (then-part alternative-list mapping)
  (let* ((command (first then-part))
	 (argument (second then-part)))

    (case command
      ('demands (cri=apply-create-interruption-message (cri=instantiate argument mapping)
						       alternative-list))
      (t
       (error "Only Demands implemented until now for strategy-interruption control-rules.")))))


(defun cri=apply-create-interruption-message (demands-list alternative-list)
  demands-list)
  ;;(let* ((demands (mapcar #'(lambda (uninterpreted-demand)
  ;;			      (let* ((strategy-ks-entry (first uninterpreted-demand))
  ;;				     (strategy-ks (if strategy-ks-entry
  ;;						      (strat~find-strategy-ks strategy-ks-entry)
  ;;						    nil))
  ;;				     (task-entry (second uninterpreted-demand))
  ;;				     (parameters (third uninterpreted-demand))
  ;;				     (new-demand 
  ;;				      (cond ((and strategy-ks-entry task-entry)
  ;;					     (demand~create-strategy-task-demand strategy-ks task-entry parameters))
  ;;					    ((and (null strategy-ks-entry) task-entry)
  ;;					     (demand~create-task-demand task-entry parameters))
  ;;					    (t
  ;;					     (omega~error "Demand not correctly defined, cri=apply-create-interruption-message-h.")))))
  ;;				
  ;;				new-demand))
  ;;			  demands-list))
  ;;	 (new-interupted-exmes (exmes~create-interruption-message pplan*roc-state-description demands))
  ;;	 (result new-interupted-exmes))
  ;;    
  ;;  (if (null demands)
  ;;	;; da ging wohl was schief!
  ;;	(omega~error "No DEMANDS DEMANDED!")
  ;;    result)))


;;------------------ evaluation of control-rules for Strategies
;;
;; ADDED BY AMEIER, 15.02.00

(defun cri=evaluate-strategic-control-rules (then-part alternative-list mapping)
  (let* ((command (first then-part))
	 (argument (second then-part)))
    
    (case command
      ('choose (cri=apply-select-for-job-offers (cri=instantiate argument mapping)
						alternative-list))
      ('reject (cri=apply-reject-for-job-offers (cri=instantiate argument mapping)
						alternative-list))
      ('prefer (cri=apply-prefer-for-job-offers (cri=instantiate argument mapping)
						alternative-list))
      (t
       (error "Only Choose and reject implemented until now for strategic-control-rules.")))))

(defun cri=apply-select-for-job-offers (select-list alternative-list)
  
  (if select-list
      select-list
    alternative-list) ;; if there is no result return the original list
  )

(defun cri=apply-reject-for-job-offers (reject-list alternative-list)
  (declare (edited  "21-JUN-1999")
	   (authors Ameier)
	   (input   "a list of elements to reject and an alternative list")
           (effect  "none")
           (value   "the alternative-list without the elements in reject-list"))
  (if (eql reject-list 'all)
      ()
    (remove-if #'(lambda (alternative)
		   (find alternative reject-list :test #'eq))
	       alternative-list)))

(defun cri=apply-prefer-for-job-offers (prefer-list alternative-list)
  (declare (edited  "26-MAR-2000")
	   (authors Ameier)
	   (input   "A list of elements to prefer and an alternative list.")
	   (effect  "None.")
	   (value   "The alternative list with the elements in the first positions."))
  (do* ((new-prefer-list nil)
	(new-other-list nil)
	(rest-alternative-list alternative-list (rest rest-alternative-list)))
      ((null rest-alternative-list)
       (append new-prefer-list new-other-list))
    (let* ((head-alt (first rest-alternative-list)))
      (if (find head-alt prefer-list)
	  (setf new-prefer-list (append new-prefer-list (list head-alt)))
	(setf new-other-list (append new-other-list (list head-alt)))))))

;;  (progn 
;;    (dolist (x (reverse prefer-list))
;;      (if (find x alternative-list)
;;	  ;; if x is to be prefered
;;	  (progn 
;;	    (delete x alternative-list)
;;	      ;; remove it
;;	    (push x
;;		  alternative-list)
;;	    ;; and put it in front of the alternative list
;;	    )))
;;   alternative-list))


;;------------------ evaluation of control-rules for methods
(defun cri=apply-select-for-methods-h (select-list alternative-list mappings)
  (let ((result ()))
    (dolist (x select-list)
      (let ((element (if (consp x)
			 (meth~find-method (first x))
		       (meth~find-method x))))
	(if (find element alternative-list)
	    (setf result
		  (if (consp x)
		      (append (cri=create-all-instantiations x mappings)
			      result)
		    (cons (cri=set-marker element) result)))
	  )))
    (if result (reverse result) alternative-list) ;; if there is no result return the
    ;; original list
    ))

(defun cri=apply-choose-for-methods-h (select-list alternative-list mappings)
  (let ((the-select-list (if (stringp select-list)
			     (apply #'append (mapcar #'(lambda (inst)
							 (cri=instantiate select-list inst))
						     mappings))
			   select-list))
	(result))
    (cond ((and (symbolp the-select-list) (string-equal the-select-list :nothing))
	   NIL)
	  (T
	   (dolist (x the-select-list)
	     (let ((element (if (consp x)
				(cri=find-method (first x))
			      (cri=find-method x))))
	       (if (find-if #'(lambda (x) (cond ((meth~p x) (eq x element))
						((symbolp x) (string-equal x (keim~name element)))))
			    alternative-list)
		   (setf result
			 (if (consp x)
			     (append (cri=create-all-instantiations x mappings)
				     result)
			   (cons (cri=set-marker element) result)))
		 )))
	   (reverse result)))
    ))

  
(defun cri=apply-prefer-for-methods-h (prefer-list alternative-list mappings)
  (progn 
    (dolist (x (reverse prefer-list))
      (let ((element (if (consp x) (meth~find-method (first x)) (meth~find-method x))))
	(if (find element alternative-list)
	    (progn 
	      (unless (consp x) (delete element
					alternative-list))
	      ;; if a method to prefer is a method combined with ass and goals, its shall
	      ;; not be deleted from the alternative-list
	      (setf alternative-list
		    (if (consp x)
			(append (cri=create-all-instantiations x mappings)
				alternative-list)
		      (cons (cri=set-marker element) alternative-list)))
	      ))))
    alternative-list))

;(defun cri=apply-reject-for-methods-h (in-reject-list alternative-list mappings)
;  (if (eql in-reject-list 'all)
;      ()
;    (let* ((reject-list (apply #'append (mapcar #'(lambda (mapping)
;                                                    (cri=instantiate in-reject-list mapping))
;                                                mappings)))
;           (result (remove-if-not #'meth~p
;                                  (mapcar #'(lambda (elt) (if (symbolp elt) (meth~find-method elt) elt))
;                                          alternative-list)))
;           (to-reject (remove-if-not #'meth~p
;                                     (mapcar #'(lambda (elt) (if (symbolp elt) (meth~find-method elt) elt))
;                                             reject-list))))      
;      (dolist (m to-reject)
;        (setq result (remove m result)))
;      result)
;    ))

(defun cri=apply-reject-for-methods-h (in-reject-list alternative-list mappings)
  (if (eql in-reject-list 'all)
      ()
    (let* ((reject-list (apply #'append (mapcar #'(lambda (mapping)
						    (cri=instantiate in-reject-list mapping))
						mappings)))
	   (result (remove-if-not #'meth~p
				  (mapcar #'(lambda (elt) (if (symbolp elt) (meth~find-method elt) elt))
					  alternative-list)))
	   (to-reject (remove-if-not #'meth~p
				     (mapcar #'(lambda (elt) (if (symbolp elt) (meth~find-method elt) elt))
					     reject-list))))      
      (dolist (m to-reject)
	(setq result (remove m result)))
      result)
    ))
  
(defun cri=evaluate-control-rules-for-methods (then-part alternative-list mapping)
  (let ((command (first then-part)) (argument (second then-part)))
    (case command
      ( 'select (cri=apply-select-for-methods (cri=instantiate argument mapping)
					      alternative-list))
      ;; select: first instantiate the argument (a list), then select the arguments from
      ;; the alternative-list
      ( 'reject (cri=apply-reject-for-methods (cri=instantiate argument mapping)
					      alternative-list))
      ;; the same
      ( 'prefer (cri=apply-prefer-for-methods (cri=instantiate argument mapping)
					      alternative-list))
      ( 'choose (cri=apply-choose-for-methods (cri=instantiate argument mapping)
					      alternative-list))
      )))


(defun cri=apply-choose-for-methods (choose-list alternative-list)
  (declare (edited  "05-MAY-1999")
	   (authors Lassaad)
	   (input   "A list of method (possibly with additional control) to choose,"
		    "and a list of available methods.")
	   (effect  "None.")
	   (value   "The elements of CHOOSE-LIST which are (whose method is) contained"
		    "in ALTERNATIVE-LIST."))
  (cond ((and (symbolp choose-list) (string-equal choose-list :nothing))
	 NIL)
	(T
	 (let ((result ()))
	   (dolist (x choose-list)
	     (let ((element (if (consp x) (meth~find-method (first x)) (meth~find-method x))))
	       (if (find-if #'(lambda (x) (cond ((meth~p x) (eq x element))
						((symbolp x) (string-equal x (keim~name element)))))
			    alternative-list)
		   (push (if (consp x) (cons element (rest x))
			   element)
			 result))))
	   (if result (reverse result) alternative-list) ;; if there is no result return the
	   ;; original list
	   ))
	))

(defun cri=apply-select-for-methods (select-list alternative-list)
    (declare (edited  "07-FEB-1997")
           (input   "a list of elements to select and the alternative list")
           (effect  "none")
           (value   "the (ordered) intersection of the two lists"))
    (let ((result ()))
      (dolist (x select-list)
	(let ((element (if (consp x) (meth~find-method (first x)) (meth~find-method x))))
	  (if (find element alternative-list)
	      (push (if (consp x) (cons element (rest x))
		      element)
		    result))))
      (if result (reverse result) alternative-list) ;; if there is no result return the
      ;; original list
      ))


(defun cri=apply-reject-for-methods (reject-list alternative-list)
  (declare (edited  "07-FEB-1997")
           (input   "a list of elements to reject and an alternative list")
           (effect  "none")
           (value   "the alternative-list without the elements in reject-list"))
  (if (eql reject-list 'all) ()
    (let ((result (remove-if-not #'meth~p
				 (mapcar #'(lambda (elt) (if (symbolp elt) (meth~find-method elt) elt))
					 alternative-list)))
	  (to-reject (remove-if-not #'meth~p
				    (mapcar #'(lambda (elt) (if (symbolp elt) (meth~find-method elt) elt))
					    reject-list))))
      (dolist (m to-reject)
	(setq result (remove m result)))
      result)
    ))


(defun cri=apply-prefer-for-methods (prefer-list alternative-list)
    (declare (edited  "07-FEB-1997")
           (input   "a list of elements to prefer and an alternative list")
           (effect  "none")
           (value   "the alternative list with the elements in the first positions"))
  (progn 
    (dolist (x (reverse prefer-list))
      (let ((element (if (consp x) (meth~find-method (first x)) (meth~find-method x))))
	(if (find element alternative-list)
	    (progn 
	      (unless (consp x) (delete element
				      alternative-list))
	      ;; if a method to prefer is a method combined with ass and goals, its shall
	      ;; not be deleted from the alternative-list
	      (push (if (consp x) (cons element (rest x)) element)
		    alternative-list)))))
    alternative-list))

;---------------- evaluation of control-rules for sequents

(defun cri=evaluate-control-rules-for-sequents (then-part alternative-list mapping)
  (let ((command (first then-part)) (argument (second then-part)))
    (case command
      ( 'select (cri=apply-select-for-sequents (cri=instantiate argument mapping)
						    alternative-list))
      ( 'reject (cri=apply-reject-for-sequents (cri=instantiate argument mapping)
						    alternative-list))
      )))

(defun cri=apply-select-for-sequents (select-list alternative-list)
  (list ;; select-list and alternative-list are list of two lists,
        ;; first the goals and then the assumptions
   ;; treat the goal list:
   (let ((result ()) (first-list (first alternative-list)))
     (dolist (x (first select-list))
       (if (find x first-list)
	   (push x result)))
     (if result (reverse result) first-list))
   ;; now the assumption list:
   (let ((result ()) (second-list (second alternative-list)))
     (dolist (x (second select-list))
       (if (find x second-list)
	   (push x result)))
     (if result (reverse result) second-list))))

(defun cri=apply-reject-for-sequents (reject-list alternative-list)
  (list
   (let ((result ()) (first-list (first alternative-list))
	 (goal-list (first reject-list)))
     (dolist (x first-list)
       (if (not (find x goal-list))
	   (push x result)))
     (reverse result))
   (let ((result ()) (second-list (second alternative-list))
	 (ass-list (second reject-list)))
     (dolist (x second-list)
       (if (not (find x ass-list))
	   (push x result)))
     (reverse result))))

;--------------- evaluation of control-rules for tasks

(defun cri=evaluate-control-rules-for-schematic-tasks (then-part alternative-list mapping)
  (let ((command (first then-part)) (argument (second then-part)))
    (case command
      ( 'select (cri=apply-select-for-schematic-tasks (cri=instantiate argument mapping)
						      alternative-list))
      ;; select: first instantiate the argument (a list), then select the arguments from
      ;; the alternative-list
      ( 'reject (cri=apply-reject-for-schematic-tasks (cri=instantiate argument mapping)
						      alternative-list))
      ;; the same
      ( 'prefer (cri=apply-prefer-for-schematic-tasks (cri=instantiate argument mapping)
						      alternative-list))
      ( 'choose (cri=apply-choose-for-schematic-tasks (cri=instantiate argument mapping)
						      alternative-list))
      )))

(defun cri=apply-reject-for-schematic-tasks (reject-list alternative-list)
  ;;(declare (ignore alternative-list argument))
  ;;(omega~warn "Reject for schematic tasks is not yet implemented!"))
  (if (eql reject-list 'all)
      ()
    (remove-if #'(lambda (alternative)
		   (find alternative reject-list :test #'eq))
	       alternative-list)))


(defun cri=apply-prefer-for-schematic-tasks (prefer-list alternative-list)
  (declare (edited  "26-MAR-2000")
	   (authors Ameier)
	   (input   "A list of elements to prefer and an alternative list.")
	   (effect  "None.")
	   (value   "The alternative list with the elements in the first positions."))
  (do* ((current-alternative-list alternative-list)
	(rest-prefer-list prefer-list (rest rest-prefer-list)))
      ((null rest-prefer-list)
       current-alternative-list)
    (let* ((head-alt (first rest-prefer-list)))
      (when (find head-alt current-alternative-list)
	(setf current-alternative-list (cons head-alt (remove head-alt current-alternative-list)))))))


(defun cri=apply-select-for-schematic-tasks (select-list alternative-list)
  (remove-if-not #'(lambda (arg)
		     (find arg alternative-list :test #'eq))
		 select-list))

(defun cri=apply-choose-for-schematic-tasks (argument alternative-list)
  (declare (ignore alternative-list))
  (if (listp (first argument)) argument (list argument)))

;-----------evaluation of control-rules for supports

(defun cri=evaluate-control-rules-for-supports (then-part alternative-list mapping)
  (let ((command (first then-part)) (argument (second then-part)))
    (case command
      ( 'select (cri=apply-select-for-supports (cri=instantiate argument mapping)
					      alternative-list))
      ;; select: first instantiate the argument (a list), then select the arguments from
      ;; the alternative-list
      ( 'reject (cri=apply-reject-for-supports (cri=instantiate argument mapping)
					      alternative-list))
      ;; the same
      ( 'prefer (cri=apply-prefer-for-supports (cri=instantiate argument mapping)
					      alternative-list))
      ( 'choose (cri=apply-choose-for-supports (cri=instantiate argument mapping)
					       alternative-list))
      )))

(defun cri=apply-choose-for-supports (chosen-list alternative-list)
  (declare (edited  "14-APR-1999")
	   (authors Lassaad)
	   (input   "a list of parameters-supports lists, and a list of alternative suports.")
	   (effect  "none")
	   (value   "the elements of CHOSEN-LIST whose supports belong to ALTERNATIVE-LIST"))
  ;; CHOSEN-SUPPS ::= '(' {PARAM-SUPPS} ')' | 'nothing'
  ;; PARAM-SUPPS ::= '(' [PARAMS] {NODE} ')'
  ;; PARAMS ::= '(' {PARAM} ')'
  (cond ((consp chosen-list)
	 (mapcar #'(lambda (param-supps)
		     (cond ((consp (first param-supps))
			    (cons (first param-supps) (remove-if-not #'(lambda (n) (find n alternative-list))
								     (rest param-supps))))
			   (T
			    (remove-if-not #'(lambda (n) (find n alternative-list))
					   param-supps))))
		 chosen-list))
	((and (symbolp chosen-list) (string-equal chosen-list :nothing))
	 NIL)
	(T
	 (omega~error ";;;cri=apply-choose-for-supports must be extended for ~A" chosen-list))
	))


(defun cri=apply-select-for-supports (select-list alternative-list)
    (declare (edited  "07-FEB-1997")
           (input   "a list of elements to select and the alternative list")
           (effect  "none")
           (value   "the (ordered) intersection of the two lists"))
    (let ((result ()))
      (dolist (element select-list)
	(if (find element alternative-list)
	    (push element result)))
      (if result (reverse result) alternative-list) ;; if there is no result return the
      ;; original list
      ))


(defun cri=apply-reject-for-supports (reject-list alternative-list)
    (declare (edited  "07-FEB-1997")
           (input   "a list of elements to reject and an alternative list")
           (effect  "none")
           (value   "the alternative-list without the elements in reject-list"))
    (let ((result ()))
      (dolist (x alternative-list)
	;; if x is not to be rejected, keep it!
	(if (not (find x reject-list))
	    (push x result)))
      (reverse result)
      ))

(defun cri=apply-prefer-for-supports (prefer-list alternative-list)
    (declare (edited  "07-FEB-1997")
           (input   "a list of elements to prefer and an alternative list")
           (effect  "none")
           (value   "the alternative list with the elements in the first positions"))
  (progn 
    (dolist (x (reverse prefer-list))
      (if (find x alternative-list)
	  ;; if x is to be prefered
	  (progn 
	      (delete x alternative-list)
	      ;; remove it
	      (push x
		    alternative-list)
	      ;; and put it in front of the alternative list
	      )))
    alternative-list))

;;;---------------evaluation of control rules for boards

(defun cri=evaluate-control-rules-for-boards (then-part alternative-list mapping)
  (let ((command (first then-part)) (argument (second then-part)))
    (case command
      ( 'action (cri=apply-action-for-boards (cri=instantiate argument mapping)
					     alternative-list))
      )))

(defun cri=apply-action-for-boards (action alternative-list)
  action)

|#

;--------------------------
; instantiating control-rules

(defun cri=instantiate (argument mapping)
  ;; needed for the meta-predicates and the arguments in the to-do-part
  ;; replaces the arguments by their mapping
  ;; or leaves them if they have none
  (if (listp argument) ;; if it is an arguments list, check all elements:
   (mapcar #'(lambda (x) (cri=instantiate x mapping)) argument)
   (cri=inst-single argument mapping) ;; else check the one argument
  ))

(defun cri=inst-single (argument mapping)
  (let ((inst (mapp~get-component argument mapping)))
    (cond ((equal inst 'crinil)
	   nil)
	  ((null inst)
	   argument)
	  (t
	   inst))))

;; Ameier
;; Introduced the concept of 'crinil
;; Problem: Whenever a variables was bound to nil in the mapping, this function did return the variable itself instead of nil!
;; New: Bind variable to 'crinil, then nil is returned by this function!

;---------------------------------------
; supporting functions (finding, infos, deleting...)
;---------------------------------------


(defun cri~remove-all-used-control-rules ()
  (clrhash cri*used-control-rules-hashtable)
  )



(defun cri~used-control-rules-of-kind (kind)
  (declare (edited  "07-FEB-1997")
	   (authors Cullrich)
	   (input   "the kind (a symbol)")
	   (effect  "none")
	   (value   "all used rules of that kind (in order)"))
  (gethash kind cri*used-control-rules-hashtable))

(defgeneric cri~find-control-rule (name)
  (declare (edited  "26-FEB-1997")
	   (authors Cullrich)
	   (input "A NAME of a control-rule (symbol or string)."  )
	   (effect "None." )
	   (value "The control-rule.")
	   )
  (:method ((name symbol))
	   (gethash name cri*control-rules-hashtable))
  (:method ((name string))
	   (gethash (intern name (find-package :omega)) cri*control-rules-hashtable))
  )
	   
; printing
;is to-do

;-------------------------------------------
; Defining control-rules
;-------------------------------------------

(defmacro cri~def-control-rule (name kind &rest attribs)
  (declare (edited  "26-FEB-1997")
	   (authors Lassaad Cullrich)
	   (input   "A control-rule specification: name, and the rest.")
	   (effect  "Defines a control-rule using this specification.")
	   (value   "Unspecified."))
  `(if (cri~find-control-rule ',name)
       (let ((new-c-rule (cri~read-object (cons ',name (cons ',kind ',attribs))
					  :control-rule)))
	 (omega~warn " Control-rule: Redefining control-rule ~A~%" ',name)
	 (setf (gethash ',name cri*control-rules-hashtable) new-c-rule)
	 )
     (let ((new-c-rule (cri~read-object (cons ',name (cons ',kind ',attribs))
					:control-rule)))
       (when (member 'crules th*output) (omega~message " Control-rule: Defining control-rule ~A~%" ',name))
       (setf (gethash ',name cri*control-rules-hashtable) new-c-rule)
       )
     )
  )

(defmethod cri~read-object (object (indicator (eql :control-rule)))
  (let* ((kind (second (second object)))
	 (if-part (second (third object)))
	 (to-do-part (rest (fourth object))) ;;was 'second'
	 (side-effect? (third (fourth object))) ;; needed as there may be no side-effect
	 (side-effect (when side-effect? (second side-effect?)))
	 (name (first object))
	 ;(parameters nil)
	 )
    (let ((c-rule (cri=create-control-rule kind if-part to-do-part
					   ;parameters
					   side-effect)))
      (keim~set-name! c-rule name)
      c-rule)))

(defun cri=create-control-rule (kind if-part to-do-part side-effect)
  (make-instance 'cri+control-rule
		 :kind kind
		 :if-part if-part
		 :to-do-part to-do-part
		 ;:parameters parameters
		 :side-effect side-effect)
  )

;(defun cri=parse (object)
;  (declare (edited  "08-AUG-2000")
;           (authors Cullrich)
;           (input   "An 'if' or 'to-do'-part of a control rule")
;           (effect  "")
;           (value   "replaces the strings by cri-variables"))
;  (if (listp object)
;      (mapcar #'cri=parse object)
;    (if (stringp object)
;        (cri=get-cri-variable object)
;      object)))
;
;(defun cri=get-cri-variable (object)
;  (or (gethash object cri*variables)
;      (cri=create-var object)))

;----------- parameters (are now disabled)
;(defun cri~set-parameters (rule params)
;  (let ((c-rule (cri~find-control-rule rule)))
;    (if c-rule (setf (cri~parameters c-rule) params)
;      (omega~warn "Control-Rule ~S not found!" rule))))
;
;(defun cri~reset-parameters (rule)
;  (let ((c-rule (cri~find-control-rule rule)))
;    (if c-rule (setf (cri~parameters c-rule) nil)
;      (omega~warn "Control-Rule ~S not found!" rule))))

;---------------------------------------------------------
;-- defining the control-rules that are actually used
;---------------------------------------------------------

(defun cri~set-used-control-rules! (c-rules-list)
  ;; c-rules-list is a list of symbols
  (progn
					;(format t  "~%Using control rules: ~S" c-rules-list)
    (clrhash cri*used-control-rules-hashtable)
    (dolist (c-rule c-rules-list)
      (let* ((kind (cri~kind (cri~find-control-rule c-rule)))
	     (used-crules (gethash kind cri*used-control-rules-hashtable))
	     )
					;(format t "~% ~S ~S" kind kind-in-used)
	(if used-crules
					; this kind does already exist
	    (setf (gethash kind cri*used-control-rules-hashtable)
		  (append used-crules (list c-rule)))
					; else create it
	  (setf (gethash kind cri*used-control-rules-hashtable)
		(list c-rule)))))))

(defun cri~set-used-control-rules (c-rules-list)
  ;; this is the non-destructive counterpart of the cri~set-used-control-rules!
  ;; it returns a hashtable which has to be assigned to cri*used-control-rules-hashtable
  ;; e.g. (let ((cri*used-control-rules-hashtable (cri~set-used-control-rules (list)))) ...)
  
  (let ((cri*new-hashtable (make-hash-table :test #'equal)))
    (dolist (c-rule c-rules-list ())
      (let* ((kind (cri~kind (cri~find-control-rule c-rule)))
	     (used-crules (gethash kind cri*new-hashtable))
	    )
	(if used-crules
					; this kind does already exist
	    (setf (gethash kind cri*new-hashtable)
		  (append used-crules (list c-rule)))
					; else create it
	  (setf (gethash kind cri*new-hashtable)
		(list c-rule)))))
    cri*new-hashtable
    ))

(defun cri~clear-hashtables ()
  (progn (clrhash cri*control-rules-hashtable)
	 (cri~set-used-control-rules! nil)))

(defgeneric cri~add-control-rules! (c-rules)
  (:method ((c-rule symbol))
	   (cri~add-control-rules! (cri~find-control-rule c-rule)))
  (:method ((c-rule cri+control-rule))
	   (let* ((kind (cri~kind c-rule))
		  (used-crules (gethash kind cri*used-control-rules-hashtable))
		  )
	     (unless (member (keim~name c-rule)
			     used-crules) ; :key #'keim~name)
	       (if used-crules
					; this kind does already exist
		   (setf (gethash kind cri*used-control-rules-hashtable)
			 (append used-crules (list (keim~name c-rule))))
					; else create it
		 (setf (gethash kind cri*used-control-rules-hashtable)
		       (list (keim~name c-rule)))))))
  (:method ((c-rules list))
	   (mapcar #'cri~add-control-rules! c-rules)))

;; marking the alternative list

(defun cri=mark-alternative-list (alternative-list)
  ;; marks the alternative list with nil (needed to be able to distinguish between
  ;; elements that were selected or prefered and those that weren't)
  (mapcar #'(lambda (element)
	      (setf (get :cri-marker element) nil))
	  alternative-list
	  ))


(defun cri~is-marked-p (element)
  (declare (edited  "09-SEP-1999")
	   (authors Cullrich)
	   (input   "anything")
	   (effect  "none")
	   (value   "returns true if the object was marked by CRI"))
  (get :cri-marker element))

(defun cri=set-marker (element)
  (progn
    (setf (get :cri-marker element) t)
    element))
