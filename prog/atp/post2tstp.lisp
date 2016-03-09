;;; -*- syntax: common-lisp; package: OMEGA; base: 10; mode: LISP -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     MathWeb                                          ;;
;;     AG Siekmann/FB Informatik                                            ;;
;;     Universitaet des Saarlandes                                          ;;
;;     Bau 36, 4. Stock                                                     ;;
;;     D-66041 Saarbruecken                                                 ;;
;;     Germany                                                              ;;
;;   electronic mail: jzimmer@mathweb.org                                   ;;
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

(defvar tstp*symbol-cd-mapping nil)
(defvar tstp*infix-symbols nil)
(defvar tstp*quantors nil)
(defvar tstp*cnf-conjecture nil)

(setf tstp*symbol-cd-mapping 
      '(
	(=       . "=")
	(false   . "$false")
	)
      )

(setf tstp*infix-symbols '(=))

(setf tstp*quantors '(FORALL EXISTS))



(defun tstp~prob2xtstp (object)
  (declare (edited  "27-JAN-1993 17:12")
	   (authors jzimmer)
	   (input   "An KEIM-object.")
	   (effect  "Prints the OpenMath representation of OBJECT to a string.") 
	   (value   "The XML TSTP form of OBJECT in a string."))
  (with-output-to-string (intern-stream)
			 (tstp~res2xtstp object intern-stream)))

(defgeneric tstp~res2xtstp (object stream)
  (:method ((object t) stream)
	   (declare (ignore stream key-list))
	   (error "(KEIM) om~~print: There is no OpenMath-Representation for the declaration of ~A. (Or at least no method printing it.)" object))
  (:method ((object-list list) stream)
	   (when object-list
	     (tstp~cnf2xtstp (first object-list) stream)
	     (mapc #'(lambda (object)
		       (tstp~res2xtstp object stream))
		   (rest object-list)))))

;; <formula language="cnf" name="3" type="initial">
;;   <conn-1 symbol="not">
;;    <pred name="left_zero">
;;     <func name="h"/>
;;     <func name="phi">
;;      <func name="f_left_zero"/>
;;     </func>
;;    </pred>
;;   </conn-1>
;;   <source>
;;    <file-source filename="Unknown"/>
;;   </source>
;;  </formula>

(defmethod tstp~res2xtstp ((obj cl+clause) stream)
  (let* ((name (string-downcase (tstp=canonicalize-name (keim~name obj))))
	 (literals (cl~literals obj))
	 (num (length literals)))
    (format stream "<formula language=\"cnf\" name=\"~A\" type=\"axiom\">~%" name)
    (cond ((= num 1)
	   (tstp~res2xtstp (first literals) stream))
	  ((= num 2)
	   (format stream "<conn-2 symbol=\"or\">~%")
	   (tstp~res2xtstp (first literals) stream)
	   (tstp~res2xtstp (second literals) stream)
	   (format stream "</conn-2>~%"))
	  ((> num 2)
	   (format stream "<conn-n symbol=\"or\">~%")
	   (mapc #'(lambda (object)
		     (tstp~res2xtstp object stream))
		 literals)
	   (format stream "</conn-2>~%"))
	  (T nil))
    (format stream "<source>~%")
    (format stream "<file-source filename=\"Unknown\"/>~%")
    (format stream "</source>~%")
    (format stream "</formula>~%")
    ))

(defmethod tstp~res2xtstp ((obj lit+literal) stream)
  (let ((atom (lit~atom obj))
	(polarity (lit~polarity obj)))
    (unless polarity
      (format stream "<conn-1 symbol=\"not\">~%"))
    (tstp~res2xtstp atom stream)
    (unless polarity
      (format stream "</conn-1>~%"))
    ))

(defmethod tstp~res2xtstp ((obj data+constant) stream)
  (format stream "<func name=\"~A\"/>" (tstp=translate-name (keim~name obj) t))
  )

(defmethod tstp~res2xtstp ((obj data+variable) stream)
  (format stream "<var name=\"~A\"/>" (tstp=canonicalize-name (keim~name obj))))

;; (defmethod tstp~res2xtstp ((obj data+appl) stream)
;;   (let* ((func (data~appl-function obj))
;; 	 (args (data~appl-arguments obj))
;; 	 )
;;     (if (and (term~primitive-p func)
;;              (member (keim~name func) tstp*quantors))
;;         (let ((abstr  (first (data~appl-arguments obj)))
;;               )
;;           (if (eq (keim~name func) 'FORALL)
;;               (format stream "! [")
;;             (format stream "? ["))
;;           (tstp~res2tstp (data~abstr-domain abstr) stream)
;;           (format stream "] : ")
;;           (omcont~print (data~abstr-range abstr) stream)
;;           )
;;       (progn 
;;     (format stream "~A(" (tstp=translate-name (keim~name func) t))
;;     (mapc #'(lambda (object)
;; 	      (tstp~res2tstp object stream)
;; 	      (when (> num 1)
;; 		(format stream ", ")
;; 		(setq num (- num 1)))
;; 	      )
;; 	  args)
;;     (format stream ")"))
;;       )))

#|specialization for types |#

(defmethod tstp~res2xtstp ((type type+func) stream)
  )


;; does not do anything about metadata, since there are no slots in the data structure.
(defmethod tstp~res2xtstp ((obj th+def) stream)
  (let ((id (tstp=canonicalize-id (keim~name obj)))
	(thy (om=defined-in-name obj)))
    (format stream
	    "~&<symbol Ident=\"~A\" type=\"~A\"/>"
	    id
	    (if (type~p obj) "type" "object"))    
    (format stream
	    "~&<definition Ident=\"def-~A\" item=\"~A\" type=\"simple\">"
	    id id)
    (format stream "~&<CMP>~A</CMP>" (help~help-string obj))
    (format stream "~&<FMP logic=\"POST\">~%<OMOBJ>"))
  (tstp~res2xtstp  (th~ass-formula obj) stream)
  (format stream "</OMOBJ></FMP>~%</definition>"))


(defmethod tstp~res2xtstp ((obj th+axiom) stream)
  (let ((id (tstp=canonicalize-id (keim~name obj))))
    (format stream "~&<axiom Ident=\"~A\">" id)	    
    (format stream "~&<CMP>~A</CMP>" (help~help-string obj))
    (format stream "~&<FMP><OMOBJ>")
    (tstp~res2xtstp  (th~ass-formula obj) stream)
    (format stream "</OMOBJ></FMP>~%</axiom>")))

(defmethod tstp~res2xtstp ((obj res+proof) stream)
  (let* ((name (keim~name obj))
	 (clauses  (res~proof-clauses obj))
	 )
    (omega~message "name ~A" name)
    (omega~message "clauses ~A" clauses)
    (format stream "<!DOCTYPE tstp PUBLIC \"-//TPTP//DTD TSTP V1.0//EN\" \"http://www.tptp.org/tstp2xml/dtd/tstp-v01.dtd\">~%")
    (format stream "<tstp>~%")
    (format stream "<comment>~%")
    (format stream "~S~%" obj)
    (format stream "/<comment>~%")
    (mapc #'(lambda (object)
	      (tstp~res2xtstp object stream))
	  clauses)
    (format stream "</tstp>~%")
    ))

;;;;;;;;;;***********************************************************
;;;;;;;;;;           pure PROLOG style TSTP
;;;;;;;;;;***********************************************************
(defun tstp~prob2tstp (object)
  (declare (edited  "27-JAN-1993 17:12")
	   (authors jzimmer)
	   (input   "An KEIM-object.")
	   (effect  "Prints the OpenMath representation of OBJECT to a string.") 
	   (value   "The XML TSTP form of OBJECT in a string."))
  (setf tstp*cnf-conjecture nil)
  (with-output-to-string (intern-stream)
			 (tstp~res2tstp object intern-stream)))

(defgeneric tstp~res2tstp (object stream)
  (:method ((object t) stream)
	   (declare (ignore stream key-list))
	   (error "There is no res2tstp method for the translation of ~A." object))
  (:method ((object-list list) stream)
	   (when object-list
	     (tstp~res2tstp (first object-list) stream)
	     (mapc #'(lambda (object)
		       (tstp~res2tstp object stream))
		   (rest object-list)))))

;; <formula language="cnf" name="3" type="initial">
;;   <conn-1 symbol="not">
;;    <pred name="left_zero">
;;     <func name="h"/>
;;     <func name="phi">
;;      <func name="f_left_zero"/>
;;     </func>
;;    </pred>
;;   </conn-1>
;;   <source>
;;    <file-source filename="Unknown"/>
;;   </source>
;;  </formula>

(defmethod tstp~res2tstp ((obj pdsn+node) stream)
  (let* ((name (string-downcase (tstp=canonicalize-name (keim~name obj))))
	 (formula (node~formula obj))
         (just (node~justification obj))
         (conjecture (infer~open-p (just~method just)))
         )
    (if conjecture
	(format stream "~%fof(~a, conjecture,~%   (" name)
      (format stream "~%fof(~a, axiom,~%   (" name))
    (tstp~res2tstp formula stream)
    (format stream "),~%   unknown, []).~%")
    ))


(defmethod tstp~res2tstp ((obj cl+clause) stream)
  (let* ((name (string-downcase (tstp=canonicalize-name (keim~name obj))))
	 (literals (cl~literals obj))
	 (num (length literals)))
    (if (slot-boundp obj 'keim::mark)
        (if (lab~mark obj)
            (progn ;; (format t "mark: ~A~%" (lab~mark obj))
              (if tstp*cnf-conjecture
                  (format stream "~%cnf(~a, axiom,~%   (" name)
                (progn (format stream "~%cnf(~a, negated_conjecture,~%(" name)
                       (setf tstp*cnf-conjecture t))))
          (format stream "~%cnf(~a, axiom,~%   (" name))
      (format stream "~%cnf(~a, axiom,~%   (" name))
    (mapc #'(lambda (object)
              (tstp~res2tstp object stream)
              (when (> num 1)
                (format stream "~%   | ")
                (setq num (- num 1)))
              )
          literals)
    (format stream "),~%   unknown, []).~%")
    ))

(defmethod tstp~res2tstp ((obj lit+literal) stream)
  (let ((atom (lit~atom obj))
	(polarity (lit~polarity obj)))
    (unless polarity
      (format stream "~~"))
    (tstp~res2tstp atom stream)
    ))

(defmethod tstp~res2tstp ((obj data+constant) stream)
;;  (omega~message "const ~A" obj)
  (let* (
	 (name (tstp=translate-name (keim~name obj) t))
	 )
;;    (omega~message "~%mapping ~A to ~A" (keim~name obj) name)
    (format stream "~A" name)))

(defmethod tstp~res2tstp ((obj data+variable) stream)
;;  (format t "variable: ~A" obj)
  (let* (
	 (name (string-upcase (tstp=canonicalize-name (keim~name obj))))
	 )
;;    (omega~message "~%mapping ~A to ~A" (keim~name obj) name)
    (format stream "~A" name)))

(defmethod tstp~res2tstp ((obj data+appl) stream)
  (let* ((func (data~appl-function obj))
	 (args (data~appl-arguments obj))
	 (num (length args))
	 )
    (if (term~primitive-p func)
        (if (member (keim~name func) tstp*quantors)
            (let ((abstr  (first (data~appl-arguments obj)))
                  )
              (if (eq (keim~name func) 'FORALL)
                  (format stream "! [")
                (format stream "? ["))
              (tstp=list2tstp (data~abstr-domain abstr) stream)
              (format stream "] : ")
              (tstp~res2tstp (data~abstr-range abstr) stream)
              )
          (if (eq (keim~name func) 'AND)
              (progn 
                (tstp~res2tstp (first args) stream)
                (format stream " & ")
                (tstp~res2tstp (second args) stream)
                )
            (if (eq (keim~name func) 'OR)
                (progn 
                  (tstp~res2tstp (first args) stream)
                  (format stream " | ")
                  (tstp~res2tstp (second args) stream)
                  )
              (if (eq (keim~name func) 'IMPLIES)
                  (progn 
                    (format stream "(")
                    (tstp~res2tstp (first args) stream)
                    (format stream " => ")
                    (tstp~res2tstp (second args) stream)
                    (format stream ")")
                    )
              (if (eq (keim~name func) 'EQUIV)
                  (progn 
                    (tstp~res2tstp (first args) stream)
                    (format stream " <=> ")
                    (tstp~res2tstp (second args) stream)
                    )
                (if (eq (keim~name func) 'NOT)
                    (progn 
                      (format stream " ~(")
                      (tstp~res2tstp (first args) stream)
                      (format stream " )")
                      )
                  (if (eq (keim~name func) '=)
                      (progn 
                      (format stream "(")
                      (tstp~res2tstp (first args) stream)
                      (format stream " = ")
                      (tstp~res2tstp (second args) stream)
                      (format stream ")")
                      )
                    (progn (format stream "~A(" (tstp=translate-name (keim~name func) t))
                           (mapc #'(lambda (object)
                                     (tstp~res2tstp object stream)
                                     (when (> num 1)
                                       (format stream ", ")
                                       (setq num (- num 1)))
                                     )
                                 args)
                           (format stream ")")))
                  ))))))
      (format t "Complex or variable functors for applications are not allowed in first-order logic!")
      )))

(defun tstp=list2tstp (object-list stream)
  (when object-list
    (tstp~res2tstp (first object-list) stream)
    (when (rest object-list)
      (format stream ", ")
      (tstp=list2tstp (rest object-list) stream))))

  
(defmethod tstp~res2tstp ((obj data+abstr) stream)
  (format stream " 'OMBVAR'(")
  (omcont~print (data~abstr-range obj) stream)
  (format stream ")")
  (omcont~print (data~abstr-domain obj) stream)
  (format stream ")"))

#|specialization for types |#

(defmethod tstp~res2tstp ((type type+func) stream)
  (format t "res2tstp not implemented for ~A " type)
  )

#| TSTP output of TERMS |#


;; does not do anything about metadata, since there are no slots in the data structure.
(defmethod tstp~res2tstp ((obj th+def) stream)
  (format t "res2tstp not implemented for ~A " obj)
  (let ((id (tstp=canonicalize-id (keim~name obj)))
	(thy (om=defined-in-name obj)))
    (format stream
	    "~&<symbol Ident=\"~A\" type=\"~A\"/>"
	    id
	    (if (type~p obj) "type" "object"))    
    (format stream
	    "~&<definition Ident=\"def-~A\" item=\"~A\" type=\"simple\">"
	    id id)
    (format stream "~&<CMP>~A</CMP>" (help~help-string obj))
    (format stream "~&<FMP logic=\"POST\">~%<OMOBJ>"))
  (tstp~res2tstp  (th~ass-formula obj) stream)
  (format stream "</OMOBJ></FMP>~%</definition>"))


(defmethod tstp~res2tstp ((obj th+axiom) stream)
  (format t "res2tstp not implemented for ~A " obj)
  (let ((id (tstp=canonicalize-id (keim~name obj))))
    (format stream "~&<axiom Ident=\"~A\">" id)	    
    (format stream "~&<CMP>~A</CMP>" (help~help-string obj))
    (format stream "~&<FMP><OMOBJ>")
    (tstp~res2tstp  (th~ass-formula obj) stream)
    (format stream "</OMOBJ></FMP>~%</axiom>")))

(defmethod tstp~res2tstp ((obj res+proof) stream)
  (let* ((name (keim~name obj))
	 (clauses  (res~proof-clauses obj))
	 )
;;    (omega~message "name ~A" name)
;;    (omega~message "clauses ~A" clauses)
    (format stream "%----------------------------------------------------------------~%")
    (format stream "% Auto-generated TSTP file for problem: ~A~%~%" (string-downcase (tstp=canonicalize-id name)))
    (mapc #'(lambda (object)
	      (tstp~res2tstp object stream))
	  clauses)
    ))



(defun tstp=canonicalize-id (symbol-or-string)
  (declare (edited  "24-SEPTEMBER-1999")
	   (authors Kohlhase)
	   (input   "A symbol or string.")
	   (value   "Every symbol that is not allowed in XML ID fields is replaced by an unproblematic string."))
  (let ((string (format nil "~A" symbol-or-string)))
    ;;check the first char if it is not an alpha char that is different from the chars tested in the do-loop
    ;;    (when (eq (elt string 1) #\-) (setf string (concatenate 'string "ID" string)))
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
		       ((eq char #\-) (concatenate 'string resstring "_"))
		       (T (concatenate 'string resstring (string char)))))))
	((= i (length string)) resstring))))

(defun tstp=canonicalize-name (symbol-or-string)
  (declare (edited  "11-OCT-1999")
	   (authors Pollet)
	   (input   "A symbol or string.")
	   (value   "A string where every character that is not allowed in the"
		    "XML NAME field is replaced by an unproblematic string."))
  (let ((string (format nil "~A" symbol-or-string)))
    ;;check the first char if it is not an alpha char that is different from the chars tested in the do-loop
;;    (when (eq (elt string 1) #\-) (setf string (concatenate 'string "ID" string)))
    (do ((i 0 (1+ i))
	 (resstring ""
		    (let ((char (elt string i)))
		      (cond 
			((eq char #\<) (concatenate 'string resstring "LESS"))
			((eq char #\-) (concatenate 'string resstring "_"))
			(T (concatenate 'string resstring (string char)))))))
	 ((= i (length string)) resstring))))

(defun tstp=translate-name (name down)
  (let* ((assoc-sym (assoc (intern name) tstp*symbol-cd-mapping))
	 )
    (if assoc-sym 
	(if down
	    (string-downcase (tstp~lisp2prolog (tstp=canonicalize-name (cdr assoc-sym))))
	  (string-upcase (tstp~lisp2prolog (tstp=canonicalize-name (cdr assoc-sym)))))
      (if down
	  (string-downcase (tstp~lisp2prolog (tstp=canonicalize-name name)))
        (string-upcase (tstp~lisp2prolog (tstp=canonicalize-name name))))))
      )

;;; str - string
;;; alist - is an alist of (<character> . <string>)
;;; example,
;;; (juergenize-string "foo-bar" '((#\- . "_")))
;;; give "foo_bar"
(defun tstp=replace-in-string (str alist)
 (let ((ret ""))
   (dotimes (i (length str) ret)
     (let ((c (aref str i)))
    (let ((a (assoc c alist)))
      (if a
          (setq ret (format nil "~d~d" ret (cdr a)))
        (setq ret (format nil "~d~d" ret c))))))))


(defun tstp~lisp2prolog (name &optional use-gh)
  "Transforms lisp NAME, a string, to its Prolog counterpart, and inserts it."
  (let ((transforms '((#\- . "_")
                      (#\= . "_eq_")
                      (#\? . "_p_")
                      (#\! . "_x_")
                      (#\> . "_to_")
                      (#\< . "_lt")
                      (#\> . "_gr")
                      (#\@ . "at"))))
    (let* ((prolog-string (tstp=replace-in-string name transforms))
           (first-letter (aref prolog-string 0)))
;;      (format t "first letter: ~S~%" first-letter)
      (if (eq first-letter #\_)
          (format nil "const~A" prolog-string)
        prolog-string))))


