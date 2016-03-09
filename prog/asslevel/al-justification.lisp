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


(mod~defmod ALJ 
            :uses (alb alx infer just keim mod node pds pdsj pdsn)
            :documentation "Assertion level justifcations."
            :exports (
                      
                      alj~add-or-replace
                      alj~create
                      alj~find
                      alj~method
                      alj~p
                      alj~premises
                      ))


(defun alj~create (premises &optional (status "unexpanded") substitutions outline-pattern
			    constraints old-just)
  (declare (edited  "13-NOV-1997")
	   (authors Afiedler)
	   (input   "A list of proof nodes, a status, a list of substitutions, and a list"
		    "for the outline-pattern.") 
	   (effect  "A new closed assertion level justification is created.")
	   (value   "The new justification."))
  (let ((new-just (pdsj~closed-just-create (infer~find-method 'assertion) premises nil status
					   substitutions outline-pattern constraints)))
    (when old-just
      (setf (pdsj~control new-just)
	    (pdsc~create (pdsj~reasons old-just)
			 (pdsj~sponsors old-just)
			 (pdsj~unsponsors old-just))))
    new-just))

(defun alj~p (just)
  (declare (edited  "30-JUN-1997" "01-DEC-1996")
	   (authors Afiedler Afiedler)
	   (input   "A justification.")
	   (value   "T, iff JUST is an assertion level justification."))
  (string= (alx~rule-symbol just) (alx~rule-symbol (infer~find-method 'assertion))))

(defmethod pdsj~assertion-just-p ((just pdsj+justification))
  (alj~p just))


(defgeneric alj~method (obj)
  (declare (edited  "01-JUL-1997")
	   (authors Afiedler)
	   (input   "A proof node.")
	   (value   "The assertion, if OBJ is justified by an assertion, otherwise its"
		    "justifying rule."))
  (:method ((node pdsn+node))
	   (let ((just (node~justification node)))
	     (if (alj~p just)
		 (keim~name (car (just~premises just)))
	       (alx~rule-symbol just))))
  )

(defgeneric alj~premises (obj)
  (declare (edited  "01-JUL-1997")
	   (authors Afiedler)
	   (input   "A proof node.")
	   (value   "The premises of OBJ without the applied assertion."))
  (:method ((node pdsn+node))
	   (let* ((just (node~justification node))
		  (premises (just~premises just)))
	     (if (alj~p just)
		 (cdr premises)
	       premises))))


(defun alj~find (node)
  (declare (edited  "13-NOV-1997")
	   (authors Afiedler)
	   (input   "A pds node.")
	   (value   "A assertion level justification for NODE, if it exists,"
		    "otherwise NIL."))
  (labels ((test (just)
		 (if (alj~p just)
		     just
		   (pdsj~above just))))
    (test (pdsn~most-abstract-just node))))

(defun alj=best (just1 just2)
  (declare (edited  "02-JUL-1997" "03-DEC-1996")
	   (authors Afiedler Afiedler)
	   (input   "At least two justifications.")
	   (value   "The best justification."))
  (cond ((keim~equal just1 just2) just1)
	((and (alj~p just1) (alj~p just2))
	 (flet ((just-value (just)
			      (let ((premises (just~premises just)))
				(/ (reduce #'+ (mapcar #'alb~line-value premises))
				   (length premises)))))
	     (if (> (just-value just1) (just-value just2))
		 just2
	       just1)))
	(t just2) ;fuers erste
	))

(defun alj~add-or-replace (node just)
  (declare (edited  "13-NOV-1997")
	   (authors Afiedler)
	   (input   "A node and a justification.")
	   (effect  "Inserts the assertion level justification JUST as the actual"
		    "justification of NODE possibly replacing a worse assertion level"
		    "justification. If the NODE's actual justification was open, it is"
		    "deleted.")
	   (value   "Undefined."))
  (let ((alj (alj~find node)))
    (if alj
	(setf (node~justification node)
	      (pdsj~replace-justification-in-chain! alj (alj=best alj just)))
      (pds~insert-actual-justification node just))))
