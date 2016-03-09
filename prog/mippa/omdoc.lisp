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
;; This file contains (preliminary) OMDOC specification for the MIPPA interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :omega)


(defun om~document (obj stream)
    (format stream "~&<?xml version=1.0><omdoc>")
    (om~print obj stream)
    (format stream "</omdoc>"))
 


(defmethod om~print ((obj pos+position) stream)
    (format stream "~&<position><FMP>")
    (dolist (x (pos~number-list obj)) (format stream "<OMI>~A</OMI>" x))
    (format stream "</FMP></position>"))

(defmethod om~print ((obj subst+substitution) stream)
    (format stream "~&<substitution><domain>")
    (om~print (subst~domain obj) stream)
    (format stream "~&</domain><codomain>")
    (om~print (subst~codomain obj) stream)
    (format stream "</codomain></substitution>"))

(defmethod om~print ((obj mapp+mapping) stream)
    (format stream "~&<mapping><domain>")
    (om~print (mapp~domain obj) stream)
    (format stream "~&</domain><codomain>")
    (om~print (mapp~codomain obj) stream)
    (format stream "</codomain></mapping>"))

(defmethod om~print ((obj node+node) stream)
    (format stream "~&<ndline xref=\"~A\"/>" (keim~name obj)))

(defmethod om~print ((obj meth+mapping) stream)
    (format stream "~&<methodmapping><FMP>")
    (om~print (meth~mapp-mapp obj) stream)
    (format stream "~&</FMP><FMP>")
    (om~print (meth~mapp-subst obj) stream)
    (format stream "</FMP></methodmapping>"))




(defmethod om~print ((obj mippa+method-matching) stream)
    (format stream "~&<mippasuggestion><method xref=\"~A\"/>"
	    (mippa~matching-method obj))
    (om~print (mippa~matching-mapping obj) stream)
    (format stream "</mippasuggestion>"))




(defmethod om~print ((obj meth+method) stream)
  (let* ((theory (keim~name (meth~theory obj)))
	   (docu     (mapcan #'(lambda (man) (when (equal (car man) 'documentation) (cdr man)))
			     (meth~manual obj)))
	   (author   (mapcan #'(lambda (man) (when (equal (car man) 'author) (cdr man)))
			     (meth~manual obj)))
	   (examples (mapcan #'(lambda (man) (when (equal (car man) 'examples) (cdr man)))
			     (meth~manual obj)))
	   (prems  (mapcar #'(lambda (conc)  (list (meth~node-sign conc) (keim~name conc)))
			   (meth~premises obj)))
	   (concs  (mapcar #'(lambda (conc)  (list (meth~node-sign conc) (keim~name conc)))
			   (meth~conclusions obj)))
	   (decl-cnt  (mapcar #'(lambda (dec) (list
					       (keim~name dec)
					       (mapcar #'keim~name (pdsn~hyps dec))
					       (node~formula dec)
					       (if (node~justification dec)
						   (cons 
						    (ohlp=parse-html-string
						     (string (keim~name (just~method (node~justification dec)))))
						    (if (listp (just~premises (node~justification dec)))
							(mapcan #'(lambda (line) (when line (list (keim~name line))))
								(just~premises (node~justification dec)))
						      (just~premises (node~justification dec))))
						 "")))
			      (meth~declarative-content obj)))
	   (out-action (mapcar #'(lambda (act) (list (mapcar #'(lambda (ac)
								 (list
								  (keim~name ac)
								  (mapcar #'keim~name
									  (meth=action-support-nodes ac))))
							     (meth=action-pattern-actions act))
						     (mapcar #'keim~name 
							     (meth=action-pattern-supported-nodes act))))
							      (meth~outline-actions obj)))
	   (out-comp   (meth~outline-computations obj))
	   (param      (meth~parameters obj))      
	   (appl-cond  (ohlp=applcond2list (meth~application-condition obj))))      
      (with-output-to-string
	(stream)
	(format stream "~&<method name=\"~A\">" (keim~name obj))
	(format stream "~&<theory xref=\"~A\"/>" theory)
       (when  prems
	(format stream "~&<premises>")
	(dolist (prem prems) (format stream "~&<premise name=\"~A\" sign=\"~A\"/>" (second prem) (first prem)))
	(format stream "~&</premises>"))
       (when  concs
	(format stream "~&<conclusions>")
	(dolist (conc concs) (format stream "~&<conclusion name=\"~A\" sign=\"~A\"/>" (second conc) (first conc)))
	(format stream "~&</conclusions>"))
       (when  param
	(format stream "~&<parameters>")
	(dolist (para param) (om~print para stream))
	(format stream "~&</parameters>")))))
 



;(format str "<TR>~%")
;	      (format str "<TD>Parameters:</TD>")
;	      (format str "<TD>") (dolist (para param) (format str "~A " para))
;	                                               (format str "</TD>~%")
;	      (format str "</TR>~%"))
;       (when  appl-cond
;	      (format str "<TR>~%")
;	      (format str "<TD>Application-Condition:</TD>")
;	      (format str "<TD>")
;	      (format str "~A" (ohlp=list2html-tabs appl-cond))
;              (format str "</TD>~%")
;	      (format str "</TR>~%"))
;       (progn (format str "<TR>~%")
;	      (format str "<TD>Declarative Content:</TD>")
;	      (format str "<TD>") (format str "<TABLE>~%")
;	                          (dolist (dec decl-cnt)
;				    (format str "<TR> <TD><FONT COLOR=\"~A\">(~A)</FONT></TD>
;                                                      <TD>~A</TD> <TD>|-</TD> <TD>~A</TD>
;                                                      <TD>~A</TD> </TR>~%"
;					    (or (Car (mapcan #'(lambda (line) (when
;										  (eq (first dec) (car (last line)))
;										(list (car line))))
;							     (union prems concs)))
;						"#000000")
;					    (first dec)
;					    (cons "<FONT FACE=\"Symbol\"> &#68;</FONT>"  (second dec)) 
;					    (ohlp~html-formula (third dec))
;					    (fourth dec)))
;				  (format str "</TABLE>~%")	  
;	      (format str "</TD>~%")
;	      (format str "</TR>~%"))        
;       (when  out-action
;	      (format str "<TR>~%")
;	      (format str "<TD>Outline-Action:</TD>")
;	      (format str "<TD>")
;	                (do* ((actions out-action (rest actions))
;			      (br "" "<BR>")
;			      (action (car actions) (car actions)))
;			    ((null actions))
;			  (format str "~A Apply " br)
;			  (do* ((act (car action) (rest act))
;				(and "" "and")
;				(ac (car act) (car act)))
;                              ((null act))
;			    (format str " ~A ~A ~A " and (car ac) (cadr ac)))
;                          (format str "to ")
;                          (do* ((act (cadr action) (rest act))
;                               (and "" "and")
;			       (ac  (car act) (car act)))
;                              ((null act))
;                            (format str " ~A ~A" and  ac ))
;			  (format str "."))
;              (format str "</TD>~%")
;	      (format str "</TR>~%"))
;       (when  out-comp
;	      (format str "<TR>~%")
;	      (format str "<TD>Outline-Computation:</TD>")
;	      (format str "<TD>")
;	      (do* ((comps out-comp (rest comps))
;		    (br "" "<BR>")
;		    (comp (car comps) (car comps)))
;		  ((null comps))
;		(format str "~A ~A" br comp))
;              (format str "</TD>~%")
;	      (format str "</TR>~%"))
;       (format str "</TABLE>~%")
;; table ends here
;       (when docu
;	  (format str "<P><H3>Description:</H3>")
;	  (format str "~A~%" (car docu)))
;       (when examples
;(format str "<P><H3>The methods is used for the following problems:</H3>")
;	  (format str "~A~%" (car examples)))
;       (when author
;	  (format str "<P><DIV ALIGN=right><FONT SIZE=-2>~A</FONT></DIV>" (car author))))))
