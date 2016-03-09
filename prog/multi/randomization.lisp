 ;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: LISP -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
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

;; THIS MODULES CONTAINS ALL FACILITIES FOR RANDOMIZATION:
;; THE DEFINITIONS FOR RANDOMIZATION RULES AND THEIR APPLICATIONS
;; FACILITIES FOR RANDOMIZATION OF LISTS


(mod~defmod RAND 
            :uses (keim omega)
            :documentation "Randomization rules and their application"
            :exports (
                      rand+rule
                      
                      rand~call-randomizer
                      rand~create-rule
                      rand~def-randomize-rule
                      rand~find-rand-rule
                      rand~flip-coin
                      rand~make-list-copies
                      rand~randomize
                      rand~rule-action
                      rand~rule-kind
                      rand~rule-p
                      rand~rule-set-weight-function
                      rand~rule-test-function
                      rand~weighted-randomize
                      
                      rand*current-rand-rules
                      rand*rand-rules-hash-table
		      rand*current-task))


#| -------------------------------------------------------------------------------------------------------- |#
;;                                                                                                          ;;
;;                                       RANDIMIZE RULES + INTERPRETER                                      ;;
;;                                                                                                          ;;
#| -------------------------------------------------------------------------------------------------------- |#

(defvar rand*rand-rules-hash-table (make-hash-table :test #'equal))
;; A hash-table to store all defined randomization rules in. The rules are accessible by their names.

(defvar rand*current-rand-rules nil)
;; A glabal variable to store the currently relevant randomization rules in (i.e., the randomization rules of a certain
;; strategy). 

(defvar rand*current-task nil)
;; A global variable to store the current task in wrt. which randomization should be done.
;; This global variable can then be used in the specifications of the randomization-rules.

#| ------------------------------------------------ RANDOMIZE RULES --------------------------------------- |#

(eval-when (load compile eval)
  (defclass rand+rule (keim+name keim+object)
    ((kind :initarg :kind
	   :initform nil
	   :accessor rand~rule-kind)
     (test-function :initarg :test-function
		    :initform nil
		    :accessor rand~rule-test-function)
     (set-weight-function :initarg :set-weight-function
			  :initform nil
			  :accessor rand~rule-set-weight-function)
     (action :initarg :action
	     :initform nil
	     :accessor rand~rule-action))))
;; so far two actions are provided:
;; -> randomize-only: i.e. only the things selected by the test function are randomized, all other are ignored
;;    => that only makes sense, if the things selected by the test function directly stand side by side.
;; -> randomize-except: i.e. all things but the selected by the test function are randomized
;;
;;

(defun rand~rule-p (obj)
  (typep obj 'rand+rule))

(defun rand~create-rule (name kind test-function set-weight-function action)
  (let* ((new-rand-rule (make-instance 'rand+rule
				       :name name
				       :kind kind
				       :test-function test-function
				       :set-weight-function set-weight-function
				       :action action)))
    (setf (gethash (symbol-name name) rand*rand-rules-hash-table)
	  new-rand-rule)
    new-rand-rule))    

(defmethod print-object ((rand-rule rand+rule) stream)
  (format stream "<RAND-RULE: ~A for ~A with Test-Function ~A, Set-Weight-Function ~A, and ACTION ~A>"
	  (keim~name rand-rule)
	  (rand~rule-kind rand-rule)
	  (rand~rule-test-function rand-rule)
	  (rand~rule-set-weight-function rand-rule)
	  (rand~rule-action rand-rule)))

(defun rand~find-rand-rule (name)
  (gethash (etypecase name
	     (symbol (symbol-name name))
	     (string name))
	   rand*rand-rules-hash-table))

#| ---------------------------------------------- READ RAND-RULE ---------------------------------------------- |#

(defmacro rand~def-randomize-rule (name &rest attribs)
  (let* ((kind nil)
	 (test-function nil)
	 (set-weight-function nil)
	 (action nil))
    (do* ((rest-attribs attribs (rest rest-attribs)))
	((null rest-attribs))
      (let* ((head-attrib (first rest-attribs)))
	(if (or (null (listp head-attrib))
		(null (= (length head-attrib) 2)))
	    (omega~error "~%When defining random-rule ~A: ~A has to be a attribute pair!" name head-attrib)
	  (let* ((descriptor (first head-attrib))
		 (argument (second head-attrib)))
	    (cond ((string-equal descriptor 'kind)
		   (setq kind argument))
		  ((string-equal descriptor 'test-function)
		   (setq test-function argument))
		  ((string-equal descriptor 'set-weight-function)
		   (setq set-weight-function argument))
		  ((string-equal descriptor 'action)
		   (setq action argument))
		  (t
		   (omega~error "~%When defining random-rule ~A: ~A is not an allowed descriptor! Allowed are kind, test-function, and action!" name descriptor)))))))
    
    (when (null kind)
      (omega~error "~%When defining random-rule ~A: no kind is given!" name))
    (when (null test-function)
      (omega~error "~%When defining random-rule ~A: no test-function is given!" name))
    (when (null set-weight-function)
      (omega~error "~%When defining random-rule ~A: no set-weight-function is given!" name))
    (when (null action)
      (omega~error "~%When defining random-rule ~A: no action is given!" name))
    
    (rand~create-rule name kind test-function set-weight-function action)))

#| ----------------------------------------------- Interpret Randomize Rules ------------------------------- |#

(defun rand~call-randomizer (list-to-randomize &key (kind 'methods))
  (let* ((set-of-randomize-rules (remove-if-not #'(lambda (rule)
						    (if (and (rand~rule-p rule)
							     (string-equal (rand~rule-kind rule) kind))
							't
						      nil))
						rand*current-rand-rules)))
    (do* ((current-list list-to-randomize)
	  (rest-rand-rules set-of-randomize-rules (rest rest-rand-rules)))
	((null rest-rand-rules)
	 current-list)
      (let* ((head-rand-rule (first rest-rand-rules)))
	(setf current-list (rand=apply-rand-rule current-list head-rand-rule))))))

(defun rand=apply-rand-rule (listi rule)
  (let* ((test-function (rand~rule-test-function rule))
	 (set-weight-function (rand~rule-set-weight-function rule))
	 (action (rand~rule-action rule)))
    (cond ((string-equal action 'randomize-only)
	   (rand=randomize-with-rule listi test-function set-weight-function :pol nil))
	  ((string-equal action 'randomize-except)
	   (rand=randomize-with-rule listi test-function set-weight-function :pol 't))
	  (t
	   (omega~error "~%Unawaited Situation In Function rand=apply-rand-rule")))))

(defun rand=randomize-with-rule (listi test-function set-weight-function &key (pol nil))
  ;; if pol is nil only sublists of elements for which the test-function gives 't are randomized
  ;; if pol is t only sublists of element for which the test-function gives nil are randomized
  
  (do* ((rest-elements listi (rest rest-elements))
	(current-list-to-randomize nil)
	(back-list nil))
      ((null rest-elements)
       (append back-list (rand~weighted-randomize current-list-to-randomize)))
    (let* ((head-element (first rest-elements))
	   (testp (funcall test-function head-element)))
      (if (or (and (null testp)
		   (null pol))
	      (and testp
		   pol))
	  (progn
	    (setf back-list (append back-list (rand~weighted-randomize current-list-to-randomize) (list head-element)))
	    (setf current-list-to-randomize nil))
	(setf current-list-to-randomize (append current-list-to-randomize
						(list (funcall set-weight-function head-element))))))))


#| ------------------------------------------- Randomize LISTS ------------------------------------- |#

(defun rand~randomize (listi)
  (declare (edited  "06-MAR-2000")
	   (authors Ameier)
	   (input   "A list.")
	   (effect  "None.")
	   (value   "The order of the elements of the list are reordered."))
  (if (null listi)
      nil
    (let* ((laenge (length listi))
	   (random-position (rand=myrandom laenge)) ;; man beachte: das geht von 0,..,laenge-1
	   )
      
      (multiple-value-bind
	  (element rest-listi)
	  (rand=get-element-at-position listi random-position)
	
	(cons element (rand~randomize rest-listi))))))

(defun rand=get-element-at-position (listi number)
  (let* ((laengi (length listi)))
    (do* ((rest-listi listi (rest rest-listi))
	  (counter 0 (incf counter))
	  (element nil)
	  (back-list nil))
	((= counter laengi)
	 (values element
		 back-list))
      (let* ((head (first rest-listi)))
	(if (= counter number)
	    (setf element head)
	  (setf back-list (append back-list (list head))))))))

(defun rand~weighted-randomize (listi)
  (declare (edited  "06-MAR-2000")
	   (authors Ameier)
	   (input   "A list.")
	   (effect  "None.")
	   (value   "The order of the elements of the list are reordered by a weigthed randomization process."
		    "This works in the following way:"
		    "each element of the list which is itself a list and contains a pair (random-weight number)"
		    "is copied (only the list itself not keim-objects and so on) and entered additionally into"
		    "the list as the number indicates (weight 1 means no copy is added, 2 means one copy is added ...)."
		    "Each element that carries no weight flag is treated with weight 1. After this duplication"
		    "the normal random function is called. Afterwards are then all copies removed and only"
		    "the topmost of each element remains in the remaning list."
		    "All weight entries are removed afterwards."
		    "At each occurrunce of 'random-break' in the list, the list is splitted into two sublists"
		    "which are randomized individually. The results of the indicidual randomizations are the"
		    "afterwards again appended."
		    "For instance: for (a b c random-break e f g), the sublists (a b c) and (e f g) are"
		    "randomized individually. The result will then consists of the reandomized a-b-c"
		    "then the randomzied e-f-g."
		    "All weight entries as well as all occurrences of 'random-break' are removed afterwards."
		    ))
  (let* ((sublists (rand=break-list listi))
	 (randomized-sublists (mapcar #'rand=weighted-randomization sublists)))
    (apply #'append randomized-sublists)))

(defun rand=break-list (listi)
  (do* ((rest-elements listi (rest rest-elements))
	(current-list nil)
	(back-lists nil))
      ((null rest-elements)
       (append back-lists
	       (list current-list)))
    (let* ((head-element (first rest-elements)))
      (if (and (null (listp head-element))
	       (symbolp head-element)
	       (string-equal head-element 'random-break))
	  (progn 
	    (setq back-lists (append back-lists (list current-list)))
	    (setq current-list nil))
	(setq current-list (append current-list (list head-element)))))))

  
(defun rand=weighted-randomization (listi)  
  (let* ((copies (rand=make-copies listi))
	 (complete-listi (append listi copies))
	 (randomized-complete-listi (rand~randomize complete-listi))
	 (randomized-listi-without-duplicates (reverse (remove-duplicates (reverse randomized-complete-listi)
									  :test #'equal)))
	 (randomized-listi-without-weights (rand=remove-weight-entries randomized-listi-without-duplicates)))
    randomized-listi-without-weights))

(defun rand=remove-weight-entries (listi)
  (mapcar #'(lambda (item)
	      (if (and (listp item)
		       (listp (second item))
		       (symbolp (first (second item)))
		       (string-equal (first (second item)) 'random-weight))
		  (first item)
		item))
	  listi))

(defun rand=make-copies (listi)
  (apply #'append (mapcar #'(lambda (item)
			      (if (listp item)
				  (let* ((weight (second (first (remove-if-not #'(lambda (it)
										   (and (listp it)
											(= (length it) 2)
											(symbolp (first it))
											(string-equal (first it) 'random-weight)))
									       item)))))
				    (if (numberp weight)
					(let* ((copy-number (- weight 1)))
					  (rand~make-list-copies item copy-number))
				      nil))
				nil))
			  listi)))

(defun rand~make-list-copies (list copy-number)
  (if (> copy-number 0)
      (cons (copy-list list) (rand~make-list-copies list (- copy-number 1)))
    nil))


(defun rand~flip-coin (percent)
  (let* ((result (rand=myrandom 100)))
    (if (< result percent)
	't
      nil)))

(defun rand=myrandom (laenge)
  ;;(multiple-value-bind
  ;;    (pups spass number)
  ;;    (excl::run-shell-command "date" :wait nil :output :stream)
  ;;  (mod number laenge))
  
  ;; randomization-state is now handelt by strategies!
  ;;(setf *random-state* (make-random-state T))
  (random laenge))
