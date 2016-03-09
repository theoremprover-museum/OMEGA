;;; -*- Mode: KEIM; Base: 10; Syntax: Common-lisp; Package: OMEGA -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 2001 by AG Siekmann, Fachbereich Informatik,             ;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; file: om-content.lisp
;; creation date: 02.02.2001
;;
;; author: Juergen Zimmer
;; e-mail: (jzimmer@ags.uni-sb.de)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file contains translation functions for OpenMath content
;; of KQML messages. Especially functions for translating POST formulas
;; to OpenMath.
;;
;; also look at   
;;    keim-3/prog/theory/openmath.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :omega)


#{\section{{\sc OpenMath} Input and Output for {\sc KQML}}
Certain POST objects have a special {\sc OpenMath} representation#}

(defvar omcont*symbol-cd-mapping nil)
(setf omcont*symbol-cd-mapping 
      '((PLUS  . ("plus" "arith1"))
	(MINUS . ("minus" "arith1"))
	(TIMES . ("times" "arith1"))
	(POWER . ("power" "arith1"))
	(ABSVAL . ("abs" "arith1"))
	
	(=        . ("eq" "relation1"))
	(LESS     . ("lt" "relation1"))
	(GREATER  . ("gt" "relation1"))
	)
      )

(defvar omcont*default-content-dictionary "arith1")
(defvar omcont*dtd-path "http://www.mathweb.org/omdoc/dtd/omdoc.dtd")

(defgeneric omcont~print (object stream)
  (declare (edited  "27-JAN-1993 17:12")
	   (authors RICHTS)
	   (input   "An KEIM-object and a stream (or NIL or T).")
	   (effect  "The OpenMath Representation of OBJECT is printed. It is printed without any formatting like linefeeds etc.  If STREAM is NIL, then
a string containing the output will be produced; if STREAM is T, then
output will be to *standard-output*.")
	   (value   "Like format, a string if STREAM is NIL or NIL else."))
					;  (:argument-precedence-order object stream)
  (:method :around (object stream)
	   (cond ((null stream)
		  (with-output-to-string (string-stream)
					 (omcont~print object string-stream)))
		 ((eq t stream)
		  (omcont~print object *standard-output*))
		 (t (call-next-method))))
  (:method ((object t) stream)
	   (post~print object stream))
  (:method ((object-list list) stream)
	   (when object-list
	     (omcont~print (first object-list) stream)
	     (mapc #'(lambda (object)
		       (omcont~print object stream))
		   (rest object-list)))))

(defun omcont~string (object)
  (declare (edited  "27-JAN-1993 17:12")
	   (authors RICHTS)
	   (input   "An KEIM-object.")
	   (effect  "Prints the OpenMath representation of OBJECT to a string.") 
	   (value   "The OpenMath form of OBJECT in a string."))
  (with-output-to-string (intern-stream)
    (omcont~print object intern-stream)))

(defun omcont~pprint (object stream)
  (declare (edited  "27-JAN-1993 17:12")
	   (authors RICHTS)
	   (input   "An KEIM-object and a stream (or NIL or T).")
	   (effect  "Pretty prints the OpenMath representation of OBJECT to STREAM. If STREAM is NIL, *standard-output* is used. If STREAM is T, *terminal-io* is used.")
	   (value   "No values."))
  (pprint (read-from-string (omcont~string object)) stream))


(defmethod omcont~print ((obj data+constant) stream)
  (let* ((cd-pair (omcont=defined-in-name obj))
	 (symb (first cd-pair))
	 (cd (second cd-pair))
	 (name (omcont=canonicalize-name symb)))
    (format stream "<OMS cd=\"~A\" name=\"~A\"/>" cd name)))

(defmethod omcont~print ((obj data+variable) stream)
  (format stream "<OMV name=\"~A\"/>" (omcont=canonicalize-name (keim~name obj))))

(defmethod omcont~print ((num term+number) stream)
  (omcont~print (keim~name num) stream)
  )

(defmethod omcont~print((num integer) stream)
  (format stream "<OMI>")
  (if (< num 0)   
      (format stream "~A~A" "~" (abs num))
    (format stream "~A" num))
  (format stream "</OMI>")
  )

(defmethod omcont~print ((obj data+appl) stream)
  (format stream "<OMA>")
  (omcont~print (data~appl-function obj) stream)
  (omcont~print (data~appl-arguments obj) stream)
  (format stream "</OMA>"))

(defmethod omcont~print ((obj data+abstr) stream)
  (format stream "<OMBIND><OMS cd=\"mltt\" name=\"lambda\"/><OMBVAR>")
  (omcont~print (data~abstr-range obj) stream)
  (format stream "</OMBVAR>")
  (omcont~print (data~abstr-domain obj) stream)
  (format stream "</OMBIND>"))

(defmethod omcont~print ((obj data+schema) stream)
  (format stream "<OMBIND><OMS cd=\"POST\" name=\"all-types\"/><OMBVAR>")
  (omcont~print (data~schema-domain obj) stream)
  (format stream "</OMBVAR>")
  (omcont~print (data~schema-range obj) stream)
  (format stream "</OMBIND>"))

#|specialization for types |#

(defmethod omcont~print ((type type+func) stream)
  (format stream "<OMA><OMS cd=\"mltt\" name=\"funtype\"/>")
  (omcont~print (data~abstr-c-domain type) stream)
  (omcont~print (data~abstr-c-range type) stream)
  (format stream "</OMA>"))

#| OpenMath output of TERMS |#

(defgeneric omcont~print-declaration (object stream)
  (declare (edited  "27-JAN-1993 17:12")
	   (authors RICHTS)
	   (input   "An KEIM-object and a stream (or NIL or T).")
	   (effect  "The OpenMath Representation of the declaration of OBJECT is printed.")
	   (value   "Like in format a string if STREAM is NIL or NIL else."))
  (:method ((object t) stream)
      (declare (ignore stream key-list))
    (error "(KEIM) omcont~~print: There is no OpenMath-Representation for the declaration of ~A. (Or at least no method printing it.)" object))
  (:method ((object-list list) stream)
      (when object-list
	(omcont~print-declaration (first object-list) stream)
	(mapc #'(lambda (object)
		  (omcont~print-declaration object stream))
	      (rest object-list)))))

(defmethod omcont~print-declaration ((variable term+variable) stream)
  (format stream "<OMATTR><OMATP><OMS cd=\"mltt\" name=\"type\"/>~A</OMATP><OMV name=\"~A\"/></OMATTR>"
	  (omcont~string (data~annotation variable))
	  (omcont=canonicalize-name (keim~name variable))))

(defmethod omcont~print ((obj term+abstr) stream)
  (format stream "<OMBIND><OMS cd=\"mltt\" name=\"lambda\"/><OMBVAR>")
  (omcont~print-declaration (data~abstr-domain obj) stream)
  (format stream "</OMBVAR>")
  (omcont~print (data~abstr-range obj) stream)
  (format stream "</OMBIND>"))


(defun omcont=canonicalize-id (symbol-or-string)
  (declare (edited  "24-SEPTEMBER-1999")
	   (authors Kohlhase)
	   (input   "A symbol or string.")
	   (value   "Every symbol that is not allowed in XML ID fields is replaced by an unproblematic string."))
  (let ((string (format nil "~A" symbol-or-string)))
    ;;check the first char if it is not an alpha char that is different from the chars tested in the do-loop
    (when  (or (integerp (read-from-string (string (elt string 0))))
	       (eq (elt string 0) #\-)) (setf string (concatenate 'string "ID" string)))
    (do ((i 0 (1+ i))
	 (resstring ""
		    (let ((char (elt string i)))
		      (cond 
			((eq char #\&) (concatenate 'string resstring "AND"))
			((eq char #\<) (concatenate 'string resstring "LESS"))
			((eq char #\>) (concatenate 'string resstring "GREATER"))
			((eq char #\=) (concatenate 'string resstring "EQUAL"))
			((eq char #\+) (concatenate 'string resstring "PLUS"))
			((eq char #\*) (concatenate 'string resstring "TIMES"))
			(T (concatenate 'string resstring (string char)))))))
	 ((= i (length string)) resstring))))

(defun omcont=canonicalize-name (symbol-or-string)
  (declare (edited  "11-OCT-1999")
	   (authors Pollet)
	   (input   "A symbol or string.")
	   (value   "A string where every character that is not allowed in the"
		    "XML NAME field is replaced by an unproblematic string."))
  (let ((string (format nil "~A" symbol-or-string)))
    ;;check the first char if it is not an alpha char that is different from the chars tested in the do-loop
    (when (eq (elt string 0) #\-) (setf string (concatenate 'string "ID" string)))
    (do ((i 0 (1+ i))
	 (resstring ""
		    (let ((char (elt string i)))
		      (cond 
		       ((eq char #\<) (concatenate 'string resstring "LESS"))
		       (T (concatenate 'string resstring (string char)))))))
	((= i (length string)) resstring))))

;; does not do anything about metadata, since there are no slots in the data structure.

(defmethod omcont~print ((obj th+def) stream)
  (let ((id  (omcont=canonicalize-id (keim~name obj)))
	(thy (omcont=defined-in-name obj)))
    (format stream
	    "~&<symbol id=\"~A\" kind=\"object\"><commonname>~A</commonname></symbol>"
	    id
	    (keim~name obj))
    (format stream
	    "~&<definition id=\"def-~A\" item=\"~A\" type=\"simple\">"
	    id id)
    (format stream "~&<CMP>~A</CMP>"
	    (omcont=canonicalize-name (help~help-string obj)))
    (format stream "~&<FMP logic=\"POST\">~%<OMOBJ>")
    (omcont~print  (th~ass-formula obj) stream)
    (format stream "</OMOBJ></FMP>~%</definition>")
    (omcont=print-private (th~definition-constant obj) (th~ass-theory obj) stream)))


(defmethod omcont~print ((obj th+axiom) stream)
  (let ((id (omcont=canonicalize-id (keim~name obj))))
    (format stream "~&<axiom id=\"~A\">" id)	    
    (format stream "~&<CMP>~A</CMP>"
	    (omcont=canonicalize-name (help~help-string obj)))
    (format stream "~&<FMP><OMOBJ>")
    (omcont~print  (th~ass-formula obj) stream)
    (format stream "</OMOBJ></FMP>~%</axiom>")))

(defmethod omcont~print ((obj prob+problem) stream)
  (let* ((name (keim~name obj))
	 (help  (omcont=canonicalize-name (help~help-string obj)))
	 (id (omcont=canonicalize-id name))
	 (thy (th~ass-theory obj))
	 (thy-name (omcont=canonicalize-id (keim~name  thy)))
	 (status (if (prob~proven-p obj) "theorem"  "conjecture"))
	 (local-constants  (omcont=env-objects-of-class (prob~environment obj) '(term+constant term+schema))))
    (format stream "~&<assertion id=\"~A\" type=\"~A\"  theory=\"~A\">" id status thy-name)	    
    (mapc #'(lambda (const) (omcont=print-symbol const nil stream)) local-constants)
    (when help (format stream "<CMP>~A</CMP>~%" help))
    (format stream "<FMP>~%")
    (mapcar #'(lambda (ass)
		(omcont=assconcl "assumption" ass obj stream))
	    (prob~assumptions obj))
    (omcont=assconcl "conclusion" (prob~conclusion obj) obj stream)
    (format stream "</FMP>~%")
    (format stream "</assertion>~%")))

(defun omcont=assconcl-id (node theo)
  (let ((nodename (keim~name node))
	(theoname (keim~name theo)))
    (if (equal nodename theoname)
	(omcont=canonicalize-id (format nil "~A-CONC" theoname))
      (omcont=canonicalize-id (format nil "~A-~A" theoname nodename)))))

(defun omcont=assconcl (type obj theo stream &optional hyp?)
  (format stream "~&<~A id=\"~A\">"
	  type
	  (omcont=assconcl-id obj theo))
  (when hyp? (format stream "~&<FMP>"))
  (format stream "~&<OMOBJ>")
  (omcont~print  (node~formula obj) stream)
  (format stream "</OMOBJ>~%")
  (when hyp? (format stream "~&</FMP>"))
  (format stream "</~A>" type))

(defun omcont=print-symbol (obj theo stream)
  (let* ((const (if (data~schema-p obj)
		    (data~schema-range obj)
		  obj))
	 (id (omcont=canonicalize-id (keim~name const))))
    (format stream
	    "~&<symbol id=\"~A\" kind=\"~A\" scope=\"~A\">"
	    id
	    (if (type~p obj) "type" "object")
	    (if theo "global" "local"))
    (format stream "~&<commonname>~A</commonname>" (keim~name const))
    (cond
     ((term~constant-p const)
      (progn (format stream "~&<type system=\"POST\"><OMOBJ>")
	     (omcont~print  (term~type obj) stream)
	     (format stream "</OMOBJ></type>")))
     ((type~constructor-p const)
      (let* ((type (type~variable-create 'anytype))
	     (typeschema (do ((buildtype type (data~appl-create type (list buildtype)))
			      (num (1- (type~constructor-arity obj)) (1- num)))
			     ((zerop num) buildtype))))
	       (format stream "~&<type system=\"POST\"><OMOBJ>")
	       (omcont~print  typeschema stream)
	       (format stream "</OMOBJ></type>"))))
    (format stream "~&</symbol>~%")
    (when theo (omcont=print-private const theo stream))))

(defun omcont=print-signature (obj stream)
  (format stream
	  "~&<signature id=\"TD-~A\" item=\"~A\" system=\"sorted\"/>~%"
	  (omcont=canonicalize-id obj)(omcont=canonicalize-id obj)))

(defun omcont=print-import (obj for stream)
  (declare (edited  "11-NOV-1999")
	   (authors Kohlhase)
	   (input   "A theory, a canonicalized name of the importing theory, and a stream.")
	   (effect  "Prints the import statement for importing OBJ into FOR  to stream."))
  (let ((id (omcont=canonicalize-id (keim~name obj))))
    (format stream "~&<imports id=\"~A.impfrom.~A\" fromcont=\"~A\" type=\"global\">" for id id)
    (format stream "~&<morphism id=\"~A.impfrom.~A.morphism\"/>~%</imports>" for id)))

(defun omcont=print-private (obj theo stream)
  (if (data~schema-p obj) (omcont=print-private (data~schema-range obj) theo stream)
  (let ((type (cond  ((sort~sort-of-pred obj (th~senv theo)) "sort")
		     ((logic~connective-p obj :theory theo)
		      (format nil "connective-type  ~A" (logic~connective-type obj :theory theo))))))
    (when type
      (let ((id (omcont=canonicalize-id (keim~name obj)))
	    (th (omcont=canonicalize-id (keim~name theo))))
	(format stream "~&<private id=\"annotation-for-~A\" item=\"~A\" theory=\"~A\" type=\"annotation\">"
		id id th)
	(format stream "<data href=\"don't know what to put here\">")
	(format stream "(~A)" type)
	(format stream "</data>")
	(format stream "</private>~%"))))))

(defun omcont=dc-date ()
  (declare (ignore sec min hour))
  (multiple-value-bind (sec min hour day month year) (get-decoded-time)
    (format nil "~A-~A-~A" year month day)))

(defun omcont=env-objects-of-class (env class)
  (let ((classlist (if (listp class) class (list class))))
    (mapcar #'(lambda (key) (env~lookup-object key env))
	(env~classes-keys env classlist nil))))
	 
(defmethod omcont~print ((obj th+theory) stream)
  (declare (edited  "10-JAN-2000")
	   (authors Pollet)
	   (input   "A theory and a stream.")
	   (effect  "Prints the theory in OMDOC-format to stream.")
	   (value   "unspecified."))
  (let* ((title      (format nil "The Theory ~A" (keim~name obj)))
	 (authors    (list "The OMEGA system"))
	 (date       (omcont=dc-date))
	 (id         (omcont=canonicalize-id (keim~name obj)))
	 (imports    (th~imports obj))
	 (env        (th~env obj))
	 (typecostrs (omcont=env-objects-of-class env 'type+constructor))                ;type-constructors
	 (typeconsts (omcont=difference (omcont=env-objects-of-class env 'type+constant)
				    typecostrs))                                     ;type-constants
	 (defis      (th~definitions obj))                                           ; the definitions 
	 (constants  (omcont=env-objects-of-class env '(term+constant term+schema)))
	 (defconsts  (mapcar #'th~definition-constant defis))
	 (loc-syms   (omcont=difference constants defconsts))                            ; the constants without definintion
	 (axioms     (th~axioms obj))                                                ; the axioms
	 (assumps    (omcont=difference (th~assumptions obj) (append defis axioms)))     ; the theorems
	 (probs      (th~problems obj))                                              ; the problems
         (termdecls  (mapcar #'sort~td-theorem
			     (sort~env-termdecls (th~senv obj) :recursive nil))))    ; the termdeclarations 
    (format stream "~&~%<theory id=\"~A\">" id)
    (omcont~print-dc-metadata stream :title title :authors authors :date date)
    (format stream "~&<commonname>~A</commonname>" (help~help-string obj))
    (mapc #'(lambda (thy) (omcont=print-import thy id stream)) imports)
    (mapc #'(lambda (tconst) (omcont=print-symbol tconst obj stream)) typeconsts)
    (mapc #'(lambda (tcostr) (omcont=print-symbol tcostr obj stream)) typecostrs)
    (mapc #'(lambda (const) (omcont=print-symbol const obj stream)) loc-syms)
    (mapc #'(lambda (ass) (omcont~print ass stream))
	  (omcont=sort-by-substruct defis))
    (mapc #'(lambda (ass) (omcont~print ass stream)) axioms)
    (mapc #'(lambda (td) (omcont=print-signature td stream)) termdecls)
    (mapc #'(lambda (ass) (omcont~print ass stream)) assumps)
    (mapc #'(lambda (ass) (omcont~print ass stream)) probs))
  (format stream "~&</theory>")
)


(let ((loaded nil)) ;;local variable for printing of the theory

  (defun omcont~print-theory (theory stream &key (parent nil))
  (declare (edited  "10-JAN-2000")
	   (authors Pollet)
	   (input   "The theory-name or theory itself, a stream and a key for parent theories.")
	   (effect  "Prints a theory (and all of it's parents, when key is set to T) to stream.")
	   (value   "unspecified."))
  (let* ((obj     (if (th~p theory) theory (th~find-theory theory)))
	 (title   (if parent
		      (format nil "The Theory ~A with all parent-theories" (keim~name obj))
		    (format nil "The Theory ~A" (keim~name obj))))
	 (authors (list "The OMEGA system"))
	 (date    (omcont=dc-date))
	 (id      (omcont=canonicalize-id (keim~name obj))))
    (setf loaded nil)
    (omcont~print-omdoc-declaration stream)
    (omcont~print-omdoc-prefix stream (omcont=canonicalize-id (concatenate 'string id "-THEORY")))
    (omcont~print-dc-metadata stream :title title :authors authors :date date)
    (if parent
	(omcont=print-theories obj stream)
      (omcont~print obj stream) )
    (omcont~print-omdoc-postfix stream)))

  (defun omcont=print-theories (theory stream)
    (push  (keim~name theory) loaded)
    (dolist (parent (th~imports theory))
      (unless (member (keim~name parent) loaded)
	(omcont=print-theories parent stream)))
    (omcont~print theory stream))
);LET ENDS HERE      
      
  
#|general functions for OMDoc printing|#

(defun omcont~print-OMDoc-declaration (stream)
  (declare (edited  "6-SEPTEMBER-1999")
	   (authors Kohlhase)
	   (input   "A stream (or NIL or T).")
	   (effect  "The OMDoc document declaration.")
	   (value   "Like in format a string if STREAM is NIL or NIL else."))
  (format stream "~&<?xml version=\"1.0\"?>")
;  (format stream "~&<!DOCTYPE omdoc SYSTEM \"http://www.ags.uni-sb.de/~~omega/www/projects/openmath/omdoc/omdoc.dtd\" []>")
  (format stream "~&<!DOCTYPE omdoc SYSTEM \"~A\" []>" omcont*dtd-path)
  (format stream "~&<!--  This file is generated by the OMEGA system, do not edit  -->"))

(defun omcont~print-OMDoc-prefix (stream ident)
  (declare (edited  "6-SEPTEMBER-1999")
	   (authors Kohlhase)
	   (input   "A stream (or NIL or T) and an identifier string.")
	   (effect  "The OMDoc document prefix is printed.")
	   (value   "Like in format a string if STREAM is NIL or NIL else."))
  (format stream "~&<omdoc id=\"~A\">" ident))

(defun omcont~print-OMDoc-postfix (stream)
  (declare (edited  "6-SEPTEMBER-1999")
	   (authors Kohlhase)
	   (input   "A stream (or NIL or T).")
	   (effect  "The OMDoc document postfix is printed.")
	   (value   "Like in format a string if STREAM is NIL or NIL else."))
  (format  stream "~&</omdoc>~%"))

(defun omcont~print-dc-metadata (stream &key (title nil) (authors nil) (date nil))
  (declare (edited  "6-SEPTEMBER-1999")
	   (authors Kohlhase)
	   (input   "A stream (or NIL or T) and a list of keyword parameters conforming
                     to the Dublin Core Metadata scheme")
	   (effect  "The OMDoc metadata is printed conforming the metadata.dtd.")
	   (value   "Like in format a string if STREAM is NIL or NIL else."))
  (format  stream "~&~%<metadata>")
  (if title (format stream "~&<Title>~A</Title>" title))
  (mapc #'(lambda (aut)
	    (format stream "~&<Creator role=\"aut\">~A</Creator>" aut))
	authors)
  (if date (format stream "~&<Date>~A</Date>" date))
  (format  stream "~&</metadata>"))
   
   
;; todo: some method for identifying the cd for methods.
(defmethod omcont~print ((obj just+justification) stream)
  (format stream
	  "~&<method><OMSTR mid=\"~A\"/>"
	  (omcont=canonicalize-name (keim~name (just~method obj))))
  (mapcar #'(lambda (para)
	      (format stream "~&<parameter>")
	      (cond ((th~definition-p para)
		     (format stream "~A" (keim~name (th~definition-constant para))))
		    ((th~assumption-p para)
		     (format stream "~A" (keim~name para)))
		    ((null para))
		    (t (format stream "<OMOBJ><OMV name=\"~A\"/></OMOBJ>" (omcont=canonicalize-name para))))
	      (format stream "~&</parameter>"))
	  (pdsj~parameters obj))
  (format stream "~&</method>"))


;; todo
;; type~o mu"s prop werden, und type~i mu"s ind werden

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions for printing the PDS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod omcont~print ((obj prob+proof) stream)
  (omcont=set-all-nodes-to-most-abstract-just obj)
  (let* ((concnode (prob~proof-root obj))
	 (assnodes (pdsn~hyps concnode)))
    (omcont=rec-print-proof obj concnode assnodes stream
			(omcont=canonicalize-id (keim~name obj))
			(omcont=canonicalize-id (keim~name (prob~proof-problem obj))))))
    
(defun omcont=rec-print-proof (pds conc prems stream &optional (ident nil) (item nil))
  (declare (edited  "29-SEP-1999")
	   (authors Pollet Kohlhase)
	   (input   "A PDS, the conclusion and premises of the subproof,"
		    "a stream for printing, and optionally an identification-"
		    "name for the proof, and an an item-name that identifies"
		    "the derivation the proof corresponds to.")
	   (effect  "Prints the subproof prems |- conc in OMDoc-format to stream,"
		    "(including hypotheses that are new to the subproof) and calls"
		    "itself recursively for every expandable justification for a"
		    "subproof.")
	   (value   "nil"))
  (let*  ((new-hyps (omcont=difference (omcont=collect-hyps-until conc prems) prems))
	  (nodes (append (omcont=difference (omcont=collect-prems-until conc prems) (union new-hyps prems)) (list conc)))
	  (problem (prob~proof-problem pds))
	  (theory (prob~theory pds)))
    ;;
    ;(format stream "~%Debug_______Nodes ~A" nodes)
    ;(format stream "~%Debug_______Prems ~A" prems)
    ;(format stream "~%Debug_______Conc  ~A" conc)
    ;;
    (format stream "~&<proof id=\"~A\" item=\"~A\" theory=\"~A\">"
	    ident item  (omcont=canonicalize-id (keim~name theory)))

;; print the hyps that are new to the subproof
    (when new-hyps                                        
      (dolist (hyp new-hyps)
	(omcont=assconcl "hypothesis" hyp problem stream T)))

;; print the nodes of the proof
    (dolist (obj nodes)                                
      (let* ((premises (pdsn~just-premises obj)) ;
	     (id (omcont=assconcl-id obj problem)))
	(if (eq obj conc)
	    (format stream "~&<conclude id=\"~A-step-~A\">" id (omcont=level conc))
	  (progn (format stream "~&<derive id=\"~A-step\">" id)
		 (format stream "<FMP>")
		 (omcont=assconcl  "conclusion" obj problem stream)
		 (format stream "</FMP>")))
	(omcont~print (node~justification obj) stream)

;; print the prems of the derive-step
	(mapcar #'(lambda (prem)                          
		    (if (th~assumption-method-p (node~justification prem))
		    (format stream "~&<premise href=\"~A:~A\"/>"
			    (omcont=canonicalize-id (keim~name (th~ass-theory (th~find-assumption (keim~name prem) theory))))
			    (omcont=canonicalize-id (keim~name (th~find-assumption (keim~name prem) theory))))
		    (format stream "~&<premise href=\"~A\"/>" (omcont=assconcl-id prem problem))))
		premises)

;; recursive method-proof expansion
	(let* ((just (node~justification obj))           
	       (below (pdsj~below just)))
	  (when below
	    (let ((new-prems (union new-hyps (union (just~premises just) prems))))
	      (setf (node~justification obj) below)
	      (omcont=rec-print-proof
	       pds obj new-prems stream
	       	(if (eq obj conc)
		    (format nil "~A-proof-~A" id (1- (omcont=level conc)))
		  (format nil "~A-proof" id))
	       	(if (eq obj conc)
		    (format nil "~A-step-~A" id (1- (omcont=level conc)))
		  (format nil "~A-step" id))))))
;; recursion ends here

	(if (eq obj conc)
	    (format stream "</conclude>")
	  (format stream "</derive>"))))
    (format stream "</proof>")))

(defun omcont=collect-hyps-until (node &optional (premises nil))
  (declare (edited  "29-SEP-1999")
	   (authors Pollet)
	   (input   "A node and optionally a list of pemise-nodes")
	   (effect  "none")
	   (value   "All hyps that appear inside the proof prems |- node."))
  (remove-duplicates
   (append (pdsn~hyps node)
	   (mapcan #'(lambda (line)
		       (unless (member line premises)
			 (omcont=collect-hyps-until line premises)))
		   (pdsn~just-premises node)))))

(defun omcont=collect-prems-until (node &optional (premises nil))
  (declare (edited  "29-SEP-1999")
	   (authors Pollet)
	   (input   "A node and optionally a list of pemise-nodes")
	   (effect  "none")
	   (value   "The transitive closure of premises of the node"
		    "that are conclusions of the premises in the list."))
  (remove-duplicates
   (append (pdsn~just-premises node)
	   (mapcan #'(lambda (line)
		       (unless (member line premises)
			 (omcont=collect-prems-until line premises)))
		   (pdsn~just-premises node)))))

(defun omcont=set-all-nodes-to-most-abstract-just (pds)
  (declare (edited  "29-SEP-1999")
	   (authors Pollet)
	   (input   "A PDS.")
	   (effect  "Sets all justifications of the nodes of the PDS to"
		    "the most abstract justification.")
	   (value   "not determined"))
  (do* ((nodes (list (prob~proof-root pds))
	       (omcont=difference (mapcan #'pdsn~just-all-premises nodes) all-nodes))
	(all-nodes nodes (append nodes all-nodes)))
      ((null nodes))
    (dolist (node nodes)
      (when (pdsj~above (node~justification node))
	(setf (node~justification node)(pdsn~most-abstract-just node))))))

(defun omcont=difference (list1 list2)
  (declare (edited  "29-SEP-1999")
	   (authors Pollet)
	   (input   "Two lists")
	   (effect  "none")
	   (value   "All elements of list1 that are not elements of list2."))
  (mapcan #'(lambda (elem)
	      (unless (member elem list2) (list elem)))
	  list1))

(defun omcont=level (node)
  (declare (edited  "29-SEP-1999")
	   (authors Pollet)
	   (input   "A node")
	   (effect  "none")
	   (value   "An integer that specifies the levels of justs above current just."))
  (let ((node-just (node~justification node)))
    (do ((just node-just (pdsj~above just))
	  (n 0 (1+ n)))
	((null just) n))))

(defun omcont=defined-in-name (thing)
  (let* ((assoc-cd (assoc (keim~name thing) omcont*symbol-cd-mapping))
	(adsfasd (omega~message "mapping ~A to ~A" thing assoc-cd))
	)
    (if assoc-cd 
	(cdr assoc-cd)
      (list (keim~name thing)
	    omcont*default-content-dictionary)))
  )
;;;;;;;;;Testing

(defun omcont=test-theories (file)
  (with-open-file (str file :direction :output :if-exists :supersede)
                  (omcont~print-theory 'base str :parent t)))

(defun omcont~greater-func (a b)
  (> a b))

(defun omcont~smaller-func (a b)
  (< a b))


(defun omcont=sort-by-substruct (defis &optional result)
  (if defis
      (let* ((min (Car defis))
	     dummy
	     (newlist (mapcar #'(lambda (defi) (if (data~substruct-positions
						    (th~definition-constant min)
						    (th~ass-formula defi)
						    :test #'data~schema-equal)
						   (progn (setq dummy min)
							  (setq min defi)
							  dummy) defi))
			      (cdr defis))))
	(omcont=sort-by-substruct newlist (push min result)))
     result))
       
    
				 
	 
	


