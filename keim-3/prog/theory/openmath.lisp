;; -*- syntax: common-lisp; package: keim; base: 10; mode: lisp -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
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

(in-package "KEIM")

(mod~defmod om :uses (post th)
                 :documentation "All KEIM objects have OpenMath representations, this module has the input/output functions."
		 :exports (
			   om~print-dc-metadata
			   om~env-objects-of-class
			   om~print
			   om~string
			   om~print-declaration
			   om~print-omdoc-declaration
			   om~print-omdoc-prefix
			   om~print-omdoc-postfix
			   om~print-theory
                           om~conjecture2omdoc
			   )
		 )

#|
TODO:
   rewrite the whole stuff
   add metadata 'depends-on' for definitions (and proofs)
   write OMDoc parser
|#

#{\section{{\sc OpenMath} input and output}
All KEIM objects have an {\sc OpenMath} representation#}

(defvar om*dtd-path "http://www.mathweb.org/omdoc/dtd/omdoc.dtd")

; (lam       . ("lambda" "fns1"))
; (all-types . ("all-types" "simpletypes"))
; (          . ("funtype" "simpletypes"))
; (          . ("type" "simpletypes"))

(defvar om*object-cd-mapping 
      '((implies . ("implies" "pl0"))
	(and     . ("and" "pl0"))
	(or      . ("or" "pl0"))
	(not     . ("not" "pl0"))
	(equiv   . ("equivalent" "pl0"))
	(not     . ("not" "pl0"))
	(forall  . ("forall" "sthol"))
	(exists  . ("exists" "sthol"))
	(true    . ("true" "truthval"))
	(false   . ("false" "truthval"))
	))

(defvar om*type-cd-mapping 
      '((i   . ("ind" "ind"))
	(o   . ("bool" "truthval"))))
	

(defgeneric om~print (object stream)
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
					 (om~print object string-stream)))
		 ((eq t stream)
		  (om~print object *standard-output*))
		 (t (call-next-method))))
  (:method ((object t) stream)
	   (post~print object stream))
  (:method ((object-list list) stream)
	   (when object-list
	     (om~print (first object-list) stream)
	     (mapc #'(lambda (object)
		       (om~print object stream))
		   (rest object-list)))))

(defun om~string (object)
  (declare (edited  "27-JAN-1993 17:12")
	   (authors RICHTS)
	   (input   "An KEIM-object.")
	   (effect  "Prints the OpenMath representation of OBJECT to a string.") 
	   (value   "The OpenMath form of OBJECT in a string."))
  (with-output-to-string (intern-stream)
    (om~print object intern-stream)))

(defun om~pprint (object stream)
  (declare (edited  "27-JAN-1993 17:12")
	   (authors RICHTS)
	   (input   "An KEIM-object and a stream (or NIL or T).")
	   (effect  "Pretty prints the OpenMath representation of OBJECT to STREAM. If STREAM is NIL, *standard-output* is used. If STREAM is T, *terminal-io* is used.")
	   (value   "No values."))
  (pprint (read-from-string (om~string object)) stream))


(defmethod om~print ((obj term+constant) stream)
  (let* ((special-object (assoc (keim~name obj) om*object-cd-mapping :test #'string-equal))
	 (cd (or (third special-object)(om=defined-in-name obj)))
	 (name (or (second special-object)(om=canonicalize-id (keim~name obj)))))
    (format stream "<om:OMS cd=\"~A\" name=\"~A\"/>" cd name)))

(defmethod om~print ((obj type+constant) stream)
  (let* ((special-object (assoc (keim~name obj) om*type-cd-mapping :test #'string-equal))
	 (cd (or (third special-object)(om=defined-in-name obj)))
	 (name (or (second special-object)(om=canonicalize-id (keim~name obj)))))
    (format stream "<om:OMS cd=\"~A\" name=\"~A\"/>" cd name)))

(defmethod om~print ((obj term+number) stream)
  (let ((num (keim~name obj)))
    (cond ((integerp num)
	   (format stream "<om:OMI>~A</om:OMI>" num))
	  ((floatp num)
	   (format stream "<om:OMF dec=\"~A\"/>" num))
	  ((rationalp num)
	   (format stream "<om:OMF dec=\"~A\"/>" (float num)))
	  (T
	   (error "unknown type ~A" obj)))))
	  

(defmethod om~print ((obj data+variable) stream)
  (format stream "<om:OMV name=\"~A\"/>" (om=canonicalize-name (keim~name obj))))


(defmethod om~print ((obj data+appl) stream)
  (format stream "<om:OMA>")
  (om~print (data~appl-function obj) stream)
  (om~print (data~appl-arguments obj) stream)
  (format stream "</om:OMA>"))

(defmethod om~print ((obj data+abstr) stream)
  (format stream "<om:OMBIND><om:OMS cd=\"fns1\" name=\"lambda\"/><om:OMBVAR>")
  (om~print (data~abstr-range obj) stream)
  (format stream "</om:OMBVAR>")
  (om~print (data~abstr-domain obj) stream)
  (format stream "</om:OMBIND>"))

(defmethod om~print ((obj data+schema) stream)
  (format stream "<om:OMBIND><om:OMS cd=\"simpletypes\" name=\"all-types\"/><om:OMBVAR>")
  (om~print (data~schema-domain obj) stream)
  (format stream "</om:OMBVAR>")
  (om~print (data~schema-range obj) stream)
  (format stream "</om:OMBIND>"))

#|specialization for types |#

(defmethod om~print ((type type+func) stream)
  (format stream "<om:OMA><om:OMS cd=\"simpletypes\" name=\"funtype\"/>")
  (om~print (data~abstr-c-domain type) stream)
  (om~print (data~abstr-c-range type) stream)
  (format stream "</om:OMA>"))

#| OpenMath output of TERMS |#

(defgeneric om~print-declaration (object stream)
  (declare (edited  "27-JAN-1993 17:12")
	   (authors RICHTS)
	   (input   "An KEIM-object and a stream (or NIL or T).")
	   (effect  "The OpenMath Representation of the declaration of OBJECT is printed.")
	   (value   "Like in format a string if STREAM is NIL or NIL else."))
  (:method ((object t) stream)
      (declare (ignore stream key-list))
    (error "(KEIM) om~~print: There is no OpenMath-Representation for the declaration of ~A. (Or at least no method printing it.)" object))
  (:method ((object-list list) stream)
      (when object-list
	(om~print-declaration (first object-list) stream)
	(mapc #'(lambda (object)
		  (om~print-declaration object stream))
	      (rest object-list)))))

(defmethod om~print-declaration ((variable term+variable) stream)
  (format stream "<om:OMATTR><om:OMATP><om:OMS cd=\"simpletypes\" name=\"type\"/>~A</om:OMATP><om:OMV name=\"~A\"/></om:OMATTR>"
	  (om~string (data~annotation variable))
	  (om=canonicalize-name (keim~name variable))))

(defmethod om~print ((obj term+abstr) stream)
  (format stream "<om:OMBIND><om:OMS cd=\"lambda-calc\" name=\"lambda\"/><om:OMBVAR>")
  (om~print-declaration (data~abstr-domain obj) stream)
  (format stream "</om:OMBVAR>")
  (om~print (data~abstr-range obj) stream)
  (format stream "</om:OMBIND>"))


(defun om=canonicalize-id (symbol-or-string)
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
			;((eq char #\=) (concatenate 'string resstring "EQUAL"))
			((eq char #\+) (concatenate 'string resstring "PLUS"))
			((eq char #\*) (concatenate 'string resstring "TIMES"))
			(T (concatenate 'string resstring (string char)))))))
	 ((= i (length string)) resstring))))


(defun om=canonicalize-content (symbol-or-string)
  (let ((string (format nil "~A" symbol-or-string)))
    ;;check the first char if it is not an alpha char that is different from the chars tested in the do-loop
    (when  (or (integerp (read-from-string (string (elt string 0))))
	       (eq (elt string 0) #\-)) (setf string (concatenate 'string "ID" string)))
    (do ((i 0 (1+ i))
	 (resstring ""
		    (let ((char (elt string i)))
		      (cond 
			((eq char #\&) (concatenate 'string resstring "&amp;"))
			((eq char #\<) (concatenate 'string resstring "&lt;"))
			((eq char #\>) (concatenate 'string resstring "&gt;"))
			(T (concatenate 'string resstring (string char)))))))
	 ((= i (length string)) resstring))))

(defun om=canonicalize-name (symbol-or-string)
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

(defmethod om~print ((obj th+def) stream)
  (let ((id   (om=canonicalize-id (keim~name obj)))
	(thy  (om=defined-in-name obj)))
    (format stream
	    "~&<definition id=\"def-~A\" for=\"~A\" type=\"simple\">"
	    id id)
    (format stream "~&<CMP>~A</CMP>"
	    (om=canonicalize-name (help~help-string obj)))
    (format stream "~&<FMP logic=\"POST\">~%<om:OMOBJ>")
    (om~print  (th~ass-formula obj) stream)
    (format stream "</om:OMOBJ></FMP>~%</definition>")
    (om=print-private (th~definition-constant obj) (th~ass-theory obj) stream)))


(defmethod om~print ((obj th+axiom) stream)
  (let ((id (om=canonicalize-id (keim~name obj))))
    (format stream "~&<axiom id=\"~A\">" id)	    
    (format stream "~&<CMP>~A</CMP>"
	    (om=canonicalize-name (help~help-string obj)))
    (format stream "~&<FMP><om:OMOBJ>")
    (om~print  (th~ass-formula obj) stream)
    (format stream "</om:OMOBJ></FMP>~%</axiom>")))

(defmethod om~print ((obj prob+problem) stream)
  (let* ((name (keim~name obj))
	 (help (help~help-string obj))
	 (id (om=canonicalize-id name))
	 (thy (th~ass-theory obj))
	 (thy-name (om=canonicalize-id (keim~name  thy)))
	 (status (if (prob~proven-p obj) "theorem"  "conjecture"))
	 (local-typeconstants  (om~env-objects-of-class (prob~environment obj) '(type+constant)))
	 (local-constants  (om~env-objects-of-class (prob~environment obj) '(term+constant term+schema))))
    (format stream "~&<assertion id=\"~A\" type=\"~A\"  theory=\"~A\">" id status thy-name)	    
    (mapc #'(lambda (const) (om=print-symbol const nil stream)) local-typeconstants)
    (mapc #'(lambda (const) (om=print-symbol const nil stream)) local-constants)
    (when help (format stream "<CMP>~A</CMP>~%" (om=canonicalize-name help)))
    (format stream "<FMP>~%")
    (mapcar #'(lambda (ass)
		(om=assconcl "assumption" ass obj stream))
	    (prob~assumptions obj))
    (om=assconcl "conclusion" (prob~conclusion obj) obj stream)
    (format stream "</FMP>~%")
    (format stream "</assertion>~%")))

(defun om=assconcl-id (node theo)
  (let ((nodename (keim~name node))
	(theoname (keim~name theo)))
    (if (equal nodename theoname)
	(om=canonicalize-id (format nil "~A-CONC" theoname))
      (om=canonicalize-id (format nil "~A-~A" theoname nodename)))))

(defun om=assconcl (type obj theo stream &optional hyp?)
  (labels ((the-id (thing)
		   (om=assconcl-id thing theo)))
    (format stream "~&<~A id=\"~A\"" type (om=assconcl-id obj theo))
    (if hyp? (progn
	       (format stream " discharged-in=\"~A~{,~A~}\">" (the-id (car hyp?)) (mapcar #'the-id (rest hyp?)))
	       (format stream "~&  <FMP>"))
      (format stream ">")))
  (format stream "~&<om:OMOBJ>")
  (om~print  (node~formula obj) stream)
  (format stream "</om:OMOBJ>~%")
  (when hyp? (format stream "~&  </FMP>"))
  (format stream "~&</~A>" type))

(defun om=assconjecture (type obj theory-name name stream &optional hyp?)
  (format stream "~&<~A id=\"~A\" type=\"conjecture\" theory=\"~A\">" type name
          theory-name)
  (format stream "~&  <FMP>")
  (format stream "~&<om:OMOBJ>")
  (om~print  (node~formula obj) stream)
  (format stream "</om:OMOBJ>~%")
  (format stream "~&  </FMP>")
  (format stream "~&</~A>" type))

(defun om=print-symbol (obj theo stream)
  (let* ((const (if (data~schema-p obj)
		    (data~schema-range obj)
		  obj))
	 (id (om=canonicalize-id (keim~name const))))
    (format stream
	    "~&<symbol id=\"~A\" kind=\"~A\" scope=\"~A\">"
	    id
	    (if (type~p obj) "type" "object")
	    (if theo "global" "local"))
    (format stream "~&  <commonname>~A</commonname>" (om=canonicalize-content (keim~name const)))
    (cond
     ((term~constant-p const)
      (progn (format stream "~&  <type system=\"simpletypes\"><om:OMOBJ>")
	     (om~print  (term~type obj) stream)
	     (format stream "  </om:OMOBJ></type>")))
     ((type~constructor-p const)
      (let* ((type (type~variable-create 'anytype))
	     (typeschema (do ((buildtype type (data~appl-create type (list buildtype)))
			      (num (1- (type~constructor-arity obj)) (1- num)))
			     ((zerop num) buildtype))))
	       (format stream "~&  <type system=\"simpletypes\"><om:OMOBJ>")
	       (om~print  typeschema stream)
	       (format stream "</om:OMOBJ></type>"))))
    (format stream "~&</symbol>~%")
    (when theo (om=print-private const theo stream))))

(defun om=print-termdecl (obj theo stream)
  (setf bla obj)
  (format stream
	  "~&<private id=\"TD-~A\" for=\"~A\" theory=\"~A\" type=\"termdecl\">
<data href=\"don't know what to put here\"/></private>~%"
	  (om=canonicalize-id obj)(om=canonicalize-id obj) theo))

(defun om=print-import (obj for stream)
  (declare (edited  "11-NOV-1999")
	   (authors Kohlhase)
	   (input   "A theory, a canonicalized name of the importing theory, and a stream.")
	   (effect  "Prints the import statement for importing OBJ into FOR  to stream."))
  (let ((id (om=canonicalize-id (keim~name obj))))
    (format stream "~&<imports id=\"~A.impfrom.~A\" from=\"~A\" type=\"global\">" for id id)
    (format stream "~&<morphism id=\"~A.impfrom.~A.morphism\"/>~%</imports>" for id)))

(defun om=print-private (obj theo stream)
  (if (data~schema-p obj) (om=print-private (data~schema-range obj) theo stream)
  (let ((type (cond  ((sort~sort-of-pred obj (th~senv theo)) "sort")
		     ((logic~connective-p obj :theory theo)
		      (format nil "connective-type  ~A" (logic~connective-type obj :theory theo))))))
    (when type
      (let ((id (om=canonicalize-id (keim~name obj)))
	    (th (om=canonicalize-id (keim~name theo))))
	(format stream "~&<private id=\"annotation-for-~A\" for=\"~A\" theory=\"~A\" type=\"annotation\">"
		id id th)
	(format stream "<data href=\"don't know what to put here\">")
	(format stream "(~A)" type)
	(format stream "</data>")
	(format stream "</private>~%"))))))

(defun om=dc-date ()
  (declare (ignore sec min hour))
  (multiple-value-bind (sec min hour day month year) (get-decoded-time)
    (format nil "~A-~A-~A" year month day)))

(defun om~env-objects-of-class (env class)
  (let ((classlist (if (listp class) class (list class))))
    (mapcar #'(lambda (key) (env~lookup-object key env))
	(env~classes-keys env classlist nil))))
	 
(defmethod om~print ((obj th+theory) stream)
  (declare (edited  "10-JAN-2000")
	   (authors Pollet)
	   (input   "A theory and a stream.")
	   (effect  "Prints the theory in OMDOC-format to stream.")
	   (value   "unspecified."))
  (let* ((title      (format nil "The Theory ~A" (keim~name obj)))
	 (authors    (list "The OMEGA group"))
	 (source     "The OMEGA theory library: http://www.ags.uni-sb.de/~omega/soft/omega/omegaindex/theories")
	 (date       (om=dc-date))
	 (type       "Dataset")
	 (format     "application/omdoc+xml")
	 (rights     "Copyright by the OMEGA group.")
	 (id         (om=canonicalize-id (keim~name obj)))
	 (imports    (th~imports obj))
	 (env        (th~env obj))
	 (typecostrs (om~env-objects-of-class env 'type+constructor))                ;type-constructors
	 (typeconsts (om=difference (om~env-objects-of-class env 'type+constant)
				    typecostrs))                                     ;type-constants
	 (defis      ;(th~definitions obj)
	   (reverse                                                                  ;the defis (hack for the right order)
	    (mapcan #'(lambda (thing) (when  (and (th~find-assumption (car thing) obj)
						  (th~definition-p (th~find-assumption (car thing) obj)))
					(list (th~find-assumption (car thing) obj))))
		    (keim::env=locals (th~env obj)))))
	 (constants  (om~env-objects-of-class env '(term+constant term+schema)))
	 (defconsts  (mapcar #'th~definition-constant defis))
	 (loc-syms   (om=difference constants defconsts))                            ; the constants without definintion
	 (axioms     (th~axioms obj))                                                ; the axioms
	 (assumps    (om=difference (th~assumptions obj) (append defis axioms)))     ; the theorems
	 (probs      (th~problems obj))                                              ; the problems
         (termdecls  (mapcar #'sort~td-theorem
			     (sort~env-termdecls (th~senv obj) :recursive nil))))    ; the termdeclarations 
    (format stream "~&~%<theory id=\"~A\">" id)
    (om~print-dc-metadata stream :title title :authors authors :date date :source source :type type :rights rights :format format)
    (format stream "~&<commonname>~A</commonname>" (om=canonicalize-content (help~help-string obj)))
    (mapc #'(lambda (thy) (om=print-import thy id stream)) imports)
    (mapc #'(lambda (tconst) (om=print-symbol tconst obj stream)) typeconsts)
    (mapc #'(lambda (tcostr) (om=print-symbol tcostr obj stream)) typecostrs)
    (mapc #'(lambda (const) (om=print-symbol const obj stream)) loc-syms)
    (mapc #'(lambda (const) (om=print-symbol (th~definition-constant const) obj stream)) defis)
    (mapc #'(lambda (ass) (om~print ass stream)) defis)
    (mapc #'(lambda (ass) (om~print ass stream)) axioms)
    (mapc #'(lambda (td) (om=print-termdecl td id stream)) termdecls)
    (mapc #'(lambda (ass) (om~print ass stream)) assumps)
    (mapc #'(lambda (ass) (om~print ass stream)) probs))
  (format stream "~&</theory>")
)


(let ((loaded nil)) ;;local variable for printing of the theory

  (defun om~print-theory (theory stream &key (parent nil))
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
	 (date    (om=dc-date))
	 (id      (om=canonicalize-id (keim~name obj))))
    (setf loaded nil)
    (om~print-omdoc-declaration stream)
    (om~print-omdoc-prefix stream (om=canonicalize-id (concatenate 'string id "-THEORY")))
    (om~print-dc-metadata stream)
    (if parent
	(om=print-theories obj stream)
      (om~print obj stream) )
    (om~print-omdoc-postfix stream)))

  (defun om=print-theories (theory stream)
    (push  (keim~name theory) loaded)
    (dolist (parent (th~imports theory))
      (unless (member (keim~name parent) loaded)
	(om=print-theories parent stream)))
    (om~print theory stream))
);LET ENDS HERE      


(defun om~print-conjecture (node theory name stream)
  (declare (edited  "16-JUN-2005")
           (authors jzimmer)
           (input   "An open psdn+node.")
           (effect  "Creates an OMDoc with the node as a conjecture.")
           (value   "unspecified."))
  (let* ((title   (format nil "The problem ~A" (keim~name node)))
         (authors (list "The OMEGA system"))
         (date    (om=dc-date))
         (id      (om=canonicalize-id name))
         (theory-name (concatenate (om=canonicalize-id (keim~name theory))))
         (just (node~justification node))
         (conj-p (infer~open-p (just~method just)))
         )
    (if conj-p
        (progn
          (setf loaded nil)
          (om~print-omdoc-declaration stream)
          (om~print-omdoc-prefix stream (om=canonicalize-id (concatenate 'string id "-problem")))
          (om~print-dc-metadata stream)
          (format stream "~%~%<theory id=\"~A\">~%" theory-name)
          (om=assconjecture "assertion" node theory-name (om=canonicalize-id (concatenate 'string id "-no-defined-symbols")) stream)
;;;          (om~print node stream)
          (format stream "~%</theory>~%")
          (om~print-omdoc-postfix stream))
      (format t "The given node is not an open node. Can't translate it into an
OMDoc conjecture"))))

(defun om~conjecture2omdoc (node theory name)
  (with-output-to-string (intern-stream)
			 (om~print-conjecture node theory name intern-stream)))

  
#|general functions for OMDoc printing|#

(defun om~print-OMDoc-declaration (stream)
  (declare (edited  "6-SEPTEMBER-1999")
	   (authors Kohlhase)
	   (input   "A stream (or NIL or T).")
	   (effect  "The OMDoc document declaration.")
	   (value   "Like in format a string if STREAM is NIL or NIL else."))
  (format stream "~&<?xml version=\"1.0\"?>")
;  (format stream "~&<!DOCTYPE omdoc SYSTEM \"http://www.ags.uni-sb.de/~~omega/www/projects/openmath/omdoc/omdoc.dtd\" []>")
;;   (format stream "~&<!DOCTYPE omdoc SYSTEM \"~A\" []>" om*dtd-path)
  (format stream "~&<!--  This file is generated by the OMEGA system, do not edit  -->"))

(defun om~print-OMDoc-prefix (stream ident)
  (declare (edited  "6-SEPTEMBER-1999")
	   (authors Kohlhase)
	   (input   "A stream (or NIL or T) and an identifier string.")
	   (effect  "The OMDoc document prefix is printed.")
	   (value   "Like in format a string if STREAM is NIL or NIL else."))
  (format stream "~&<omdoc id=\"~A\"" ident)
  (format stream "~&  xmlns=\"http://www.mathweb.org/omdoc\"")
  (format stream "~&  xmlns:om=\"http://www.openmath.org/OpenMath\"")
  (format stream "~&  xmlns:cc=\"http://creativecommons.org/ns\"")
  (format stream "~&  xmlns:dc=\"http://purl.org/DC\"")
  (format stream "~&  xmlns:m=\"http://www.w3.org/1998/Math/MathML\"")
  (format stream "~&  version=\"1.2\"") 
  (format stream "~&  xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"")
  (format stream "~&  xsi:schemaLocation=\"http://www.mathweb.org/omdoc")
  (format stream "~&                       http://www.mathweb.org/omdoc/xsd/omdoc.xsd\">")
  )
  
(defun om~print-OMDoc-postfix (stream)
  (declare (edited  "6-SEPTEMBER-1999")
	   (authors Kohlhase)
	   (input   "A stream (or NIL or T).")
	   (effect  "The OMDoc document postfix is printed.")
	   (value   "Like in format a string if STREAM is NIL or NIL else."))
  (format  stream "~&</omdoc>~%"))

(defun om~print-dc-metadata (stream &key (title nil) (authors nil) (date nil) (source nil) (rights nil) (type nil) (format nil))
  (declare (edited  "6-SEPTEMBER-1999")
	   (authors Kohlhase)
	   (input   "A stream (or NIL or T) and a list of keyword parameters conforming
                     to the Dublin Core Metadata scheme")
	   (effect  "The OMDoc metadata is printed conforming the metadata.dtd.")
	   (value   "Like in format a string if STREAM is NIL or NIL else."))
  (format  stream "~&~%<metadata>")
  (when title (format stream "~&  <Title>~A</Title>" title))
  (mapc #'(lambda (aut)
	    (format stream "~&  <Creator role=\"aut\">~A</Creator>" aut))
	authors)
  (when date (format stream "~&  <Date>~A</Date>" date))
  (when source (format stream "~&  <Source>~A</Source>" source))
  (when type (format stream "~&  <Type>~A</Type>" type))
  (when format (format stream "~&  <Format>~A</Format>" format))
  (when rights (format stream "~&  <Rights>~A</Rights>" rights))
  (format  stream "~&</metadata>"))
   
   
;; todo: some method for identifying the cd for methods.
(defmethod om~print ((obj just+justification) stream)
  (let* ((meth (just~method obj))
	 (class (string (class-name (class-of meth)))))
    (format stream
	    "~&  <method xref=\"omega:~A:~A:~A\">"
 	    (om=canonicalize-name (keim~name (infer~theory meth)))
	    (subseq class (1+ (position #\+ class)))
	    (om=canonicalize-name (keim~name meth)))
  (mapcar #'(lambda (para)
	      (format stream "~&")
	      (cond ((th~definition-p para)
		     (format stream "~A" (keim~name (th~definition-constant para))))
		    ((th~assumption-p para)
		     (format stream "~A" (keim~name para)))
		    ((null para))
		    (t (format stream "<om:OMOBJ><om:OMV name=\"~A\"/></om:OMOBJ>" (om=canonicalize-name para))))
	      (format stream "~&"))
	  (pdsj~parameters obj))
  (format stream "</method>")))


;; todo
;; type~o mu"s prop werden, und type~i mu"s ind werden

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions for printing the PDS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod om~print ((obj prob+proof) stream)
  (om=set-all-nodes-to-most-abstract-just obj)
  (let* ((concnode (prob~proof-root obj))
	 (assnodes (pdsn~hyps concnode))
	 (env (pds~environment obj))
	 (local-constants  (om~env-objects-of-class env
						    '(type+variable type+constant term+constant term+variable)))
	 (prelude (with-output-to-string (mystream)
					 (mapc #'(lambda (thing)
						   (om=print-symbol thing nil mystream))
					       local-constants)))
	 (proof-id (om=canonicalize-id (keim~name obj)))
	 (privs (om=rec-print-proof obj concnode assnodes stream proof-id
				    (om=canonicalize-id (keim~name (prob~proof-problem obj))) prelude)))
    ;;here comes the additional stuff for the reconstruction of the PDS, stored in private-tags
    (format stream "~{~A~}"
	    (cons
	     (om=proof-private (format nil "~A-~A" proof-id 'assumps) (prob~proof-assumptions obj) :for proof-id :type "assumptions")
	     (cons
	      (om=proof-private (format nil "~A-~A" proof-id 'conc) (prob~proof-root obj) :for proof-id :type "conclusion")
	      privs)))))

(defun om=proof-private (id obj &key (for nil) (pto "OMEGA") (type 'justification))
  (with-output-to-string (mystream)
			 (format mystream "~%<private id=\"~A\"" id)
			 (when for (format mystream " for=\"~A\"" for))
			 (when pto (format mystream " pto=\"~A\"" pto))
			 (when type (format mystream " type=\"~A\"" type))
			 (format mystream "><data>~A</data></private>"
				 (cond ((string-equal type 'justification)
					(format nil "\"~A\" ~:A (~{\"~A\" ~})"
						(pdsj~status obj)(pdsj~subst obj)(pdsj~outline-pattern obj)))
				       ((string-equal type 'conclusion)
					(keim~name obj))
				       ((string-equal type 'assumptions)
					(format nil "~{~A ~}" (mapcar #'keim~name obj)))))))
  
(defun om=rec-print-proof (pds conc prems stream &optional (ident nil) (for nil) (prelude nil))
  (declare (edited  "29-SEP-1999")
	   (authors Pollet Kohlhase)
	   (input   "A PDS, the conclusion and premises of the subproof,"
		    "a stream for printing, and optionally an identification-"
		    "name for the proof, and an an for-name that identifies"
		    "the derivation the proof corresponds to.")
	   (effect  "Prints the subproof prems |- conc in OMDoc-format to stream,"
		    "(including hypotheses that are new to the subproof) and calls"
		    "itself recursively for every expandable justification for a"
		    "subproof.")
	   (value   "A string with the collected <private>-decls for each proof-node."))
  (let*  ((new-hyps (om=difference (om=collect-hyps-until conc prems) prems))
	  (nodes (append (om=difference (om=collect-prems-until conc prems) (union new-hyps prems)) (list conc)))
	  (problem (prob~proof-problem pds))
	  (theory (prob~theory pds))
	  (privates))
    ;;
    ;(format stream "~%Debug_______Nodes ~A" nodes)
    ;(format stream "~%Debug_______Prems ~A" prems)
    ;(format stream "~%Debug_______Conc  ~A" conc)
    ;;
    (format stream "~%<proof id=\"~A\" for=\"~A\" theory=\"~A\">"
	    ident for  (om=canonicalize-id (keim~name theory)))
    (when prelude (format stream "~&~A" prelude)) ;;for things like local constants...
    
;; print the hyps that are new to the subproof
    (when new-hyps                                        
      (dolist (hyp new-hyps)
	(om=assconcl "hypothesis" hyp problem stream
		     (remove-if #'(lambda (nod) (member hyp (pdsn~hyps nod)))
				(mapcar #'pdsc~an-node (pdsj~other-reasons (node~justification hyp)))))
	(push (om=proof-private
	       (format nil "~A-hyp" (om=assconcl-id hyp problem))
	       (node~justification hyp)
	       :for (om=assconcl-id hyp problem)) privates)))

;; print the nodes of the proof
    (dolist (obj nodes)                                
      (let* ((premises (pdsn~just-premises obj)) ;
	     (id (om=assconcl-id obj problem)))
	(if (eq obj conc)
	    (progn 
	      (format stream "~&  <conclude id=\"~A-step-~A\">" id (om=level conc))
	      (push (om=proof-private
		     (format nil "~A-just-~A" id (om=level conc)) 
		     (node~justification obj)
		     :for (format nil "~A-step-~A" id (om=level conc)))
		     privates))
	  (progn (format stream "~%~%<derive id=\"~A-step\">" id)
		 (format stream "~&  <FMP>")
		 (om=assconcl  "conclusion" obj problem stream)
		 (format stream "~&  </FMP>")
		 (push (om=proof-private
			(format nil "~A-just" id)
			(node~justification obj)
			:for (format nil "~A-step" id))
		       privates)))
	  
	(om~print (node~justification obj) stream)

;; print the prems of the derive-step
	(mapcar #'(lambda (prem)                          
		    (if (th~assumption-method-p (node~justification prem))
		    (format stream "~&  <premise xref=\"~A:~A\"/>"
			    (om=canonicalize-id (keim~name (th~ass-theory (th~find-assumption (keim~name prem) theory))))
			    (om=canonicalize-id (keim~name (th~find-assumption (keim~name prem) theory))))
		    (format stream "~&  <premise xref=\"~A\"/>" (om=assconcl-id prem problem))))
		premises)

;; recursive method-proof expansion
	(let* ((just (node~justification obj))           
	       (below (pdsj~below just)))
	  (when below
	    (let ((new-prems (union new-hyps (union (just~premises just) prems))))
	      (setf (node~justification obj) below)
	      (append (om=rec-print-proof
		       pds obj new-prems stream
		       (if (eq obj conc)
			   (format nil "~A-proof-~A" id (1- (om=level conc)))
			 (format nil "~A-proof" id))
	       	(if (eq obj conc)
		    (format nil "~A-step-~A" id (1- (om=level conc)))
		  (format nil "~A-step" id)))
		      privates))))
;; recursion ends here

	(if (eq obj conc)
	    (format stream "</conclude>")
	  (format stream "~&</derive>"))))
    (format stream "</proof>")
    privates))



(defun om=collect-hyps-until (node &optional (premises nil))
  (declare (edited  "29-SEP-1999")
	   (authors Pollet)
	   (input   "A node and optionally a list of pemise-nodes")
	   (effect  "none")
	   (value   "All hyps that appear inside the proof prems |- node."))
  (remove-duplicates
   (append (pdsn~hyps node)
	   (mapcan #'(lambda (line)
		       (unless (member line premises)
			 (om=collect-hyps-until line premises)))
		   (pdsn~just-premises node)))))

(defun om=collect-prems-until (node &optional (premises nil))
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
			 (om=collect-prems-until line premises)))
		   (pdsn~just-premises node)))))

(defun om=set-all-nodes-to-most-abstract-just (pds)
  (declare (edited  "29-SEP-1999")
	   (authors Pollet)
	   (input   "A PDS.")
	   (effect  "Sets all justifications of the nodes of the PDS to"
		    "the most abstract justification.")
	   (value   "not determined"))
  (do* ((nodes (list (prob~proof-root pds))
	       (om=difference (mapcan #'pdsn~just-all-premises nodes) all-nodes))
	(all-nodes nodes (append nodes all-nodes)))
      ((null nodes))
    (dolist (node nodes)
      (when (pdsj~above (node~justification node))
	(setf (node~justification node)(pdsn~most-abstract-just node))))))

(defun om=difference (list1 list2)
  (declare (edited  "29-SEP-1999")
	   (authors Pollet)
	   (input   "Two lists")
	   (effect  "none")
	   (value   "All elements of list1 that are not elements of list2."))
  (mapcan #'(lambda (elem)
	      (unless (member elem list2) (list elem)))
	  list1))

(defun om=level (node)
  (declare (edited  "29-SEP-1999")
	   (authors Pollet)
	   (input   "A node")
	   (effect  "none")
	   (value   "An integer that specifies the levels of justs above current just."))
  (let ((node-just (node~justification node)))
    (do ((just node-just (pdsj~above just))
	  (n 0 (1+ n)))
	((null just) n))))

(defun om=defined-in-name (thing)
  (if (th~defined-in thing)
      (keim~name (th~defined-in thing))
    "local"))



(defun om=test-theories (file)
  (with-open-file (str file :direction :output :if-exists :supersede)
                  (om~print-theory 'base str :parent t)))

(defun om~greater-func (a b)
  (> a b))

(defun om~smaller-func (a b)
  (< a b))


;;;;;;;;;Testing

#|
(defun om=write-all-theories (dir)
(progn ;(setf *theory-registry* "/home/pollet/afranke/theo/")
  (let ((theolist '(
base
calculus
curry-howard
divisibility
economy
exl-nr-th
field
function
generic
glist
group
group1
group2
group3
group4
group5
group6
hua
if-res-calc
integer
learn
limit
lin-res
linguistic
lock-res
loop
lueneburg
magma
measure
metric
monoid
morphism
natural
neumann-bernays-goedel
nic
ordered-ring
per-sorts
polynomial
poset
post
prex
prime
proof-theory
prop-res
propositional-logic
puzzle
quasigroup
rational
real
relation
resolution
ring
semigroup
sequences
size
struct
taut-res
topology
tps
typed-set
zermelo-fraenkel
zmz
		    )))
  (dolist (theo theolist)
      (when (probe-file (car (keim::th=compose-file-name theo)))
          (clrhash keim::th*theory-hash-table)
          (clrhash keim::th*loaded)
	(th~require-completely theo)
	(th~load-problems theo)
	(with-open-file (str (make-pathname :directory dir
					    :name (string-downcase theo)
					    :type "omdoc")
			     :direction :output :if-exists :supersede)
			(format t "Writing ~A" theo)
			(om~print-theory theo str :parent nil)))))))

 

(defun om=test-proof (file)
  (with-open-file (str file :direction :output :if-exists :supersede)
                  (om~print-omdoc-declaration str)
                  (om~print-omdoc-prefix str "agatha")
                  (om~print-dc-metadata str :title "Aunt Agatha" :authors '("The OMEGA system") :date "1999")
                  (om~print (prob~find-problem 'ilf-181) str)
                  (om~print (prob~find-proof 'ilf-181-1) str)
                  (om~print-omdoc-postfix str)
))

(defun om=test-proof (file)
  (with-open-file (str file :direction :output :if-exists :supersede)
                  (om~print-omdoc-declaration str)
                  (om~print-omdoc-prefix str "Example")
                  (om~print-dc-metadata str :title "A Simple Example" :authors '("The OMEGA system") :date "2002")
                  (om~print (prob~find-problem 'my) str)
                  (om~print (prob~find-proof 'my-2) str)
                  (om~print-omdoc-postfix str)
))

(defun om=test-proof (file)
  (with-open-file (str file :direction :output :if-exists :supersede)
                  (om~print-omdoc-declaration str)
                  (om~print-omdoc-prefix str "agatha")
                  (om~print-dc-metadata str :title "Aunt Agatha" :authors '("The OMEGA system") :date "1999")
                  (om~print (prob~find-problem 'sfor) str)
                  (om~print (prob~find-proof 'sfor-1) str)
                  (om~print-omdoc-postfix str)
))

(defun om=test-theory (file)
  (with-open-file (str file :direction :output :if-exists :supersede)
                  (om~print-theory ;'struct
                                   ;'generic
                                   'base
                                   ;'natural
                                   ;'post
                   str :parent nil)))


(setf om*dtd-path "/home/pollet/MATHWEB/omdoc/dtd/omdoc.dtd")


	
(defun om=write-groups (dir)
(let ((theolist '(
 generic 
 base 
 nic
 post
 typed-set 
 function relation struct morphism
 poset natural integer 
 magma quasigroup loop semigroup monoid group
 group1 group2 group3 group4 group5 group6
                  )))
  (dolist (theo theolist)
    (progn
      (th~load-problems theo)
      (with-open-file (str (make-pathname :directory dir
                                        :name (string-downcase theo)
                                        :type "xml")
                         :direction :output :if-exists :supersede)
                    (format t "Writing ~A" theo)
                    (om~print-theory theo str :parent nil))))))

;; write tps theory (jzimmer)
(defun om=test-theory (file)
 (with-open-file (str file :direction :output :if-exists :supersede)
                 (om~print-theory 'tps str :parent nil)))

(defun om=write-groups (dir)
(let ((theolist '(
 generic 
 base 
 nic
 post
 typed-set 
 function relation struct morphism
 poset natural integer 
 magma quasigroup loop semigroup monoid group
 group1 group2 group3 group4 group5 group6
                  )))
      (with-open-file (str (make-pathname :directory dir
                                        :name "allgroups"
                                        :type "xml")
			         :direction :output :if-exists :supersede)
			   (dolist (theo theolist)
			     (progn
			       (format t "Writing ~A" theo)
			       (om~print (th~find-theory theo) str))))))


(defun om=test-proof (file)
  (with-open-file (str file :direction :output :if-exists :supersede)
                  (om~print-omdoc-declaration str)
                  (om~print-omdoc-prefix str "omdoc-proof")
                  (om~print-dc-metadata str :title "A simple proof in OmDoc" :authors '("Martin Pollet") :date "2003")
                  (om~print (prob~find-problem 'prop-and-comm) str)
                  (om~print (prob~find-proof 'prop-and-comm-1) str)
                  (om~print-omdoc-postfix str)
))

|#




