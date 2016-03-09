;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     PROVERB Project                                                      ;;
;;     AG Siekmann/FB Informatik                                            ;;
;;     Universitaet des Saarlandes                                          ;;
;;     Bau 36, 4. Stock                                                     ;;
;;     D-66041 Saarbruecken                                                 ;;
;;     Germany                                                              ;;
;;   electronic mail: proverb@cs.uni-sb.de                                  ;;
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


(mod~defmod ALB 
            :uses (env keim misc mod pds pdsn sys)
            :documentation "Basic functions for the assertion level."
            :exports (alb+hash-error
                      alb+proof-hash-tables
                      alb+term-error

                      alb~get-premise-trees
                      alb~line-value
                      alb~make-proof-hash-tables
                      alb~max-value
                      alb~premise-tree-hash-table
                      alb~proof-hash-tables
                      alb~remove-tree!
                      alb~rule-environment
                      alb~rule-tree-count
                      alb~rule-tree-hash-table
                      alb~show-line-values
                      alb~store-tree
                      alb~valuate-proof-line))


;################################################
;##                                            ##
;##                  Errors                    ##
;##                                            ##
;################################################

(sys~define-condition
 alb+hash-error (sys+error)
 ((content))
 (lambda (condition stream)
   (format stream "~S has no hash tables."
	   (alb+hash-error-content condition))))

(sys~define-condition
 alb+term-error (sys+error)
 ((term) (type))
 (lambda (condition stream)
   (format stream "~A is not a ~(~S~)."
	   (alb+term-error-term condition)
	   (alb+term-error-type condition))))



;#########################################
;##                                     ##
;##             Hash tables             ##
;##                                     ##
;#########################################

(defvar alb*proof-hash-tables (misc~make-hash-table 'alb*proof-hash-tables)
  "A hash table that holds instances of {\\vb alb+proof-hash-tables} handling all hash
tables necessary for a proof. The keys are the proof names.")


(defclass alb+proof-hash-tables (keim+name)
  ((rule-tree :reader alb~rule-tree-hash-table
	      :initform (misc~make-hash-table 'rule-tree-hash-table)
	      :documentation
	      "This is the {rule tree hash table} slot of {the proof}.
	       A hash table, indexed by line label of the hypothesis line that built the
               tree. The keys are the labels of hypothesis lines in a proof, and the data
               are the names of the rule nodes according to these lines.
               A call of altr~show with such a rule node shows the rule tree built of
               the formula in the according hypothesis line.")
   (premise-tree :reader alb~premise-tree-hash-table
		 :initform (misc~make-hash-table 'premise-tree-hash-table)
		 :documentation
		 "This is the {premise tree hash table} slot of {the proof}.
		  A hash table, indexed by line label of a proof line that is the
                  conclusion of that tree. The keys are the labels of proof lines and the
                  data are the names of the roots of the according premise trees.")
   (line-values :reader alb=line-values-hash-table
		:initform (misc~make-hash-table 'line-values-hash-table)
		:documentation
	        "This is the {line value hash table} slot of {the proof}.
		 A hash table that holds the values of proof lines.
                 The keys are the labels of proof lines, and the data are integers.
                 The value is the number of proof nodes in the largest subtree of the proof
                 tree with the key line as root.")
   (real-just :reader alb=real-just-hash-table
	      :initform (misc~make-hash-table 'real-just-hash-table)
	      :documentation
	      "This is the {real justifications hash table} slot of {the proof}.
	       A hash table, indexed by a line, that holds the real justification, that
               means skipping Same-lines.")
   (environment :reader alb=proof-rule-environment
		:initform (env~copy (pds~environment pds*current-proof-plan))
		:documentation "This is the {environment} slot of {the proof rules}."))
  (:documentation "A class to handle the proof specific hash tables."))


(defun alb~proof-hash-tables (proof)
  (declare (edited  "27-JAN-1994 11:29")
	   (authors AFIEDLER)
	   (input   "A proof.")
	   (value   "An object, holding the hash tables specific to PROOF."))
  (let ((ok (misc~gethash (keim~name proof) alb*proof-hash-tables)))
    (if ok
	ok
      (error (sys~make-condition 'alb+hash-error :content proof)))))

(defun alb~make-proof-hash-tables (proof)
  (declare (edited  "27-JAN-1994 11:50")
	   (authors AFIEDLER)
	   (input   "A proof.")
	   (effect  "An instance of ALB+PROOF-HASH-TABLES is created and stored in"
		    "ALB*PROOF-HASH-TABLES.")
	   (value   "The instance."))
  (let ((tables (make-instance 'alb+proof-hash-tables :name (keim~name proof))))
    (setf (misc~gethash (keim~name proof) alb*proof-hash-tables)
	  tables)
    tables))


;;; rule-tree-hash-table

(defun alb~rule-tree-count ()
  (declare (edited  "25-SEP-1997")
	   (authors Afiedler)
	   (input   "None.")
	   (value   "The number of rule trees for the current proof."))
  (hash-table-count
   (misc~hash-table
    (alb~rule-tree-hash-table (alb~proof-hash-tables pds*current-proof-plan)))))


;;; premise-tree-hash-table

(defun alb~get-premise-trees (name)
  (declare (edited  "01-FEB-1994 10:47")
	   (authors AFIEDLER)
	   (input   "The reference to trees (i.e. a label of a line).")
	   (value   "A list of names of nodes referencing current proof's premise"
		    "trees, if they exist, otherwise an error is signalled."))
  (misc~gethash name (alb~premise-tree-hash-table
		      (alb~proof-hash-tables pds*current-proof-plan))))



(defgeneric alb~store-tree (line node)
  (declare (edited  "01-FEB-1994 10:49")
	   (authors AFIEDLER)
	   (input   "A proof line and a rule node.")
	   (effect  "Stores the name of NODE the in the current proof's tree hash table"
		    "with the key LINE."
		    "If LINE is a hypothesis line, NODE should be the root node of the"
		    "rule tree, that was created of LINE."
		    "If LINE is not a hypothesis line, NODE should be the root node of the"
		    "premise tree that could justify LINE.")
	   (value   "Undefined."))
  (:method ((line pdsn+node) node )
	   (alb~store-tree (keim~name line) node))
  )

(defgeneric alb~remove-tree! (line)
  (declare (edited  "03-FEB-1994 13:59")
	   (authors AFIEDLER)
	   (input   "A proof line.")
	   (effect  "All trees of the current proof according to LINE are removed.")
	   (value   "Undefined."))
  (:method ((line pdsn+node))
	   (alb~remove-tree! (keim~name line)))
  (:method ((line symbol))
	   (let ((hash-tables (alb~proof-hash-tables pds*current-proof-plan)))
	     (misc~remhash line (alb~premise-tree-hash-table hash-tables))
	     (misc~remhash line (alb~rule-tree-hash-table hash-tables)))))


;;; line-values-hash-table

(defun alb~line-value (line)
  (declare (edited  "02-JUL-1997" "27-JAN-1994 11:47")
	   (authors Afiedler AFIEDLER)
	   (input   "A line of the current proof.")
	   (value   "The value of LINE."))
  (cond ((misc~gethash (list* (keim~name line)
			      (mapcar #'keim~name (pdsn~just-premises line)))
		       (alb=line-values-hash-table
			(alb~proof-hash-tables pds*current-proof-plan))))
	(t (alb~valuate-proof-line line))))

(defsetf alb~line-value (line) (value)
  (declare (edited  "02-JUL-1997" "16-FEB-1995")
	   (authors Afiedler Afiedler)
	   (input   "A line of the current proof and an integer.")
	   (effect  "Attaches LINE with VALUE.")
	   (value   "VALUE"))
  (let ((new-line (gensym))
	(new-value (gensym)))
    `(let ((,new-line ,line)
	   (,new-value ,value))
       (setf (misc~gethash (list* (keim~name ,new-line)
				  (mapcar #'keim~name (pdsn~just-premises ,new-line)))
			   (alb=line-values-hash-table
			    (alb~proof-hash-tables pds*current-proof-plan)))
	     ,new-value))))

(defun alb~valuate-proof-line (line)
  (declare (edited  "02-JUL-1997" "30-SEP-1993 10:27")
	   (authors Afiedler AFIEDLER)
	   (input   "A proof line.")
	   (effect  "Calculates for each proof line in the subproof with root LINE its"
		    "value."
                    "The value of a proof line is the sum of the proof lines that preceed"
		    "the proof line directly, plus 1.")
	   (value   "The value of LINE." ))
  (setf (alb~line-value line)
	(cond ((pdsn~hypothesis-node-p line) '1)
	      (t (apply #'+ (cons '1 (mapcar #'alb~line-value
					     (pdsn~just-premises line))))))))



(defun alb~max-value (lines)
  (declare (edited  "22-MAR-1993 12:28")
	   (authors AFIEDLER)
	   (input   "A list of lines of the current proof.")
	   (value   "The maximum of the values of these lines."))
  (apply #'max (mapcar #'alb~line-value lines)))

(defun alb~show-line-values ()
  (declare (edited  "27-JAN-1994 11:48")
	   (authors AFIEDLER)
	   (input   "None.")
	   (effect  "Shows the values of the lines of the current proof.")
	   (value   "Undefined."))
  (misc~maphash #'(lambda (key value) (omega~output "~1& Line ~S: ~S~%" key value))
		(alb=line-values-hash-table (alb~proof-hash-tables
					     pds*current-proof-plan))))


;;; real-just-hash-table




;;; proof-rule-environment

(defun alb~rule-environment (proof)
  (declare (edited  "23-JUN-1997")
	   (authors Afiedler)
	   (input   "A proof.")
	   (value   "Its environment."))
  (alb=proof-rule-environment (alb~proof-hash-tables proof)
			       #+old(misc~gethash (keim~name proof) alb*proof-hash-tables)))



