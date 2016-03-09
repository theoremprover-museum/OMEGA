;; -*- Package: KEIM; Syntax: Common-lisp; Mode: LISP -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1998 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     KEIM Project                                                         ;;
;;     AG Siekmann/FB Informatik                                            ;;
;;     Universitaet des Saarlandes                                          ;;
;;     Postfach 1150                                                        ;;
;;     D-66041 Saarbruecken                                                 ;;
;;     Germany                                                              ;;
;;   electronic mail: keim@ags.uni-sb.de                                    ;;
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
  

;;;
;;; ALLGEMEINE ANMERKUNGEN ZU DEN KORRESPONDENZTABELLEN FUER FUNKTIONEN:

;;; Shared-Symbols gibt es nicht mehr, daher entfallen saemtliche Funktionen,
;;;        die mit shsym~ beginnen.
;;; Gleiches gilt fuer den Modul poly. Ab sofort wird Polymorphie durch die
;;;        Schema-klassen realisiert.
;;; Die Aufteilung der Terme auf die Module sym, appl, abstr entfaellt.
;;; Funktionen, die fuer Typen und Terme gleichermassen definiert sind,
;;;        werden im neuen, allgemeineren, Modul data definiert.
;;;        Dies schliesst auch die meisten Funktionen aus dem top Modul,
;;;        das es ebenfalls nicht mehr gibt, ein.
;;; Diese Zusammenlegung ergibt, dass es keine getrennten Datenstrukturen fuer
;;; (Term-)Substitutionen und Typsubstitutionen mehr zu geben braucht. Daher
;;; verschwinden die entsprechenden Funktionen aus type!
;;;
;;; Saemtliche Funktionen mit Ausrufezeichen am Ende werden verschwinden.
;;; Entweder werden die entsprechenden Effekte durch die neuen Copy-Funktionen
;;; erzielt, oder es gibt setf-Methoden, oder ein keyword destructive.
;;;
;;;
;;; Eine erste (indeterministische) Heuristik zum Auffinden von Funktionen ist:
;;;            --------------------
;;;                Ersetze:                         durch:
;;;                ----------------------------------------------------------
;;;                term~, sym~, appl~, abstr~       term~ oder data~
;;;                type~                            type~ , data~ oder subst~
;;;                top~                             term~ oder data~
;;;
;;; Der Modul termc wird an gewohnter Stelle verschwinden, ebenso Teile aus fo.
;;; Beides gelangt dann irgendwo in den theory Modul.
;;;
;;; Analoges gilt natuerlich fuer die Klassennamen.
;;;
;;;

; aus term-basic:

; term+top                      --> data+top
; term~reader                   --> data~reader
; term~defgeneric               --> data~defgeneric
; term+term                     bleibt
; term~type                     bleibt
; term~set-type!                --> (setf term~type)
; term~set-term-binding!        ???
; term~term-binding             ???
; term~label                    --> data~label
; term~set-label!               --> (setf data~label)
; term~plist                    --> data~plist
; term~set-plist!               --> (setf data~plist)
; term~copy                     --> data~copy
; term~copy-with-properties     ???
; term~p                        bleibt
; term~general-p                ???
; term~set-term                 ???
; term~termlist-p               ???
; term~nested-termlist-p        ???
; term~top                      --> data+top
; term~subterms                 --> data~substructs
; term~binding                  --> data~binding
; term~set-binding!             --> (setf data~binding)
; term~equal                    --> data~equal
; term~equal-p                  --> data~equal-p
; term~equal-p-ab               --> data~alpha-equal-p
; term~=equal-p-ab-aux          entfaellt
; term~equal-ab                 --> data~alpha-equal
; term~=equal-ab-aux            entfaellt
; term~subterm-positions        --> data~substruct-positions
; term~positions                --> data~positions
; term~position                 --> data~position
; term~at-position              --> data~struct-at-position
; term~env-lookup               --> post~read-object (with indicator :existing-term)
; term~env-lookup-new           ???
; term~read                     --> post~read-object (with indicator :existing-term)
; term~read-poly                entfaellt
; term~poly-subterm-find        entfaellt
; term~read-variable            --> term~variable-create
; term~read-constant            --> term~constant-create 
; term~env-enter-constant       --> term~create-primitive-in-environment bzw. post~read-object (with indicator :constant) 
; term~env-enter-variable       --> term~create-primitive-in-environment bzw. post~read-object (with indicator :variable)

; aus symbol:

; shsym+sharedsymbol            entfaellt
; shsym~create                  entfaellt
; shsym~type                    entfaellt
; shsym~binding                 entfaellt
; shsym~set-binding!            entfaellt
; shsym~label                   entfaellt
; shsym~set-label!              entfaellt
; shsym~p                       entfaellt

; aus sym:

; sym+sym                       --> term+primitive
; sym+var                       --> term+variable
; sym+const                     --> term+constant
; sym~symbol-binding            --> data~binding
; sym~set-symbol-binding!       --> (setf data~binding)
; sym~shared-symbol             entfaellt
; sym~set-shared-symbol!        entfaellt
; sym~p                         --> term~primitive-p
; sym~name                      --> keim~name
; sym~plist                     --> data~plist
; sym~label                     --> data~label
; sym~set-label!                --> (setf data~label)
; sym~variable-p                --> term~variable-p
; sym~variable-list-p           ???
; sym~constant-p                --> term~constant-p
; sym~read-variable             --> use term~read-term-of-class
; sym~env-enter-variable        --> term~create-primitive-in-environment
; sym~env-enter-constant        --> term~create-primitive-in-environment
; sym~constant-create           --> term~constant-create
; sym~variable-create           --> term~variable-create
; sym~rename-var                ???

; aus sym+num:

; sym+num                       --> term+number
; sym~constant-create           --> term~constant-create
; sym~number-p                  --> term~number-p

;; usw

(in-package :keim)

(mod~defmod TERM 
            :uses (bind data env keim post subst type)
            :documentation "Datastructures with the basic functionaltiy of typed terms."
            :exports (
		      ;; CLASSES
		      
		      term+abstr
		      term+appl
		      term+complex
		      term+constant
		      term+number
		      term+primitive
		      term+schema
		      term+term
		      term+variable
		      
		      ;; Creating and Access
		      
		      term~abstr-create
		      term~abstr-p
		      term~appl-create
		      term~appl-p
		      term~apply
		      term~complex-p
		      term~constant-create
		      term~constant-p
		      term~number-p
		      term~p
		      term~primitive-p
		      term~schema-create
		      term~schema-p
		      term~type
		      term~variable-p
		      term~variable-create
		      
		      ;; Equalities
		      
		      term~alpha-equal
		      term~alpha-match
		      term~alpha-unify
		      term~alpha-copy
		      term~unify
		      term~taco-equal
		      
		      ;; Others
		      term~type-unexpanded
		      term~schema-close
		      term~create-primitive-in-environment

		      term~type-variables-rec
		      term~free-type-variables
		      term~bound-type-variables

		      term~variables
		      term~bound-variables
		      term~free-variables 

		      term~generate-new-name
		      term~generate-term-primitive-with-new-name

		      term~get-type-variables
		      term~not-free-occuring
		      term~free-occ-positions
		      term~polymorphic
		      term~read-term-of-class
		      
		      ))

;; =========================================================================
;;                               Classes
;; =========================================================================

;; ------------ Superclass for terms
(eval-when (load compile eval) 
  (defclass term+term (data+object data+struct)
    ()
    (:documentation "The class of all terms in KEIM.")))

;; ------------ Primitive
(eval-when (load compile eval) 
  (defclass term+primitive (term+term data+primitive)
    ()
    (:documentation "The class of primitive (non complex) terms.")))

(eval-when (load compile eval) 
  (defclass term+constant (term+primitive data+constant)
    ()
    (:documentation "The class of term constants.")))

(eval-when (load compile eval)
  (defclass term+number (term+constant)
    ()
    (:documentation "The class of all numbers of type num")))
  
(eval-when (load compile eval) 
  (defclass term+variable (term+primitive data+variable)
    ()
    (:documentation "The class of complex variable.")))

;; ------------ Complex 
(eval-when (load compile eval) 
  (defclass term+complex (term+term data+complex)
    ()
    (:documentation "The class of complex terms.")))

(eval-when (load compile eval) 
  (defclass term+schema (term+term data+schema)
    ()
    (:documentation "The class of polymorphic terms.")))
  
(eval-when (load compile eval) 
  (defclass term+appl (term+complex data+appl)
    ()
    (:documentation "The class of term applications.")))
  
(eval-when (load compile eval) 
  (defclass term+abstr (term+complex data+abstr) 
    ()
    (:documentation "The class of constant types e.g. the type of individuals.")))


;; =========================================================================
;;                               Create Funktionen 
;; =========================================================================


;; ------------- Primitive
(data~defgeneric term~variable-create (symbol (type))
  (declare (edited  "21-NOV-1997")
	   (authors Gebhard)
	   (input   "A symbol and a type.")
	   (effect  "Creates a new object, representing a term variable.")
	   (value   "A new created termvariable"))
  (:method (symbol (type type+type))
	   (when (not (symbolp symbol))
	     (error "~A is not a symbol" symbol))
	   (let ((tvar (data~variable-create 'term+variable :name symbol)))
	     (setf (data~annotation tvar) type)
	     tvar)))

(data~defgeneric term~constant-create (symbol (type))
  (declare (edited  "21-NOV-1997")
	   (authors Gebhard)
	   (input   "A symbol and a type")
	   (effect  "Creates a new object, representing a term constant.")
	   (value   "The new constant"))
  (:method (symbol (type type+type))
	   (let ((tconst (data~constant-create 'term+constant :name symbol)) )
	     (setf (data~annotation tconst) type)
	     (setf (data~constant-origin tconst) tconst)
	     tconst))
  (:method ((number number) (type type+type))
	   (let ((tnum (data~constant-create 'term+number :name number)) )
	     (setf (data~annotation tnum) type)
	     (setf (data~constant-origin tnum) tnum)
	     tnum)))


;; ------------- C O M P L E X
(defun term~polymorphic (datum)
  (declare (edited  "08-DEC-1997")
	   (authors Gebhard)
	   (input   "A term")
	   (effect  "None")
	   (value   "T if term has kappa-bound type-variables, else nil"))
  (typep datum 'term+schema))


(defun term=update-subtypes (datum subst)
  (declare (edited  "01-SEP-1998")
	   (authors Gebhard)
	   (input   "A datum and a substitution")
	   (effect  "While craeting terms, there may occur wrong typed"
		    "subterms. These terms are correced by applying the subst")
	   (value   "The new typed term."))
  (if (subst~empty-p subst) 
      datum
    (term==update-subtypes datum subst)))

;; This is a very intern function of the term-nmoduls,because it describes
;; states of an term WHILE being created.
(data~defgeneric term==update-subtypes ((datum) subst)
  (declare (edited  "24-NOV-1997")
	   (authors Gebhard)
	   (input   "A term and a (type-)substitution.")
	   (effect  "The subst is applied to all types of terms subterms.")
	   (value   "The new-typed term."))
  (:method ((datum data+primitive) subst)
	   (setf (data~annotation datum)
		 (subst~apply subst (data~annotation datum)))
	   datum)
  (:method ((datum data+appl) subst)
	   (setf (data~annotation datum)
		 (subst~apply subst (data~annotation datum)))
	   (term==update-subtypes (data~appl-function datum) subst)
	   (term==update-subtypes (data~appl-arguments datum) subst)
	   datum)
  (:method ((datum data+abstr) subst)
	   (setf (data~annotation datum)
		 (subst~apply subst (data~annotation datum)))
	   (term==update-subtypes (data~abstr-range datum) subst)
	   (term==update-subtypes (data~abstr-domain datum) subst)
	   datum)
  (:method ((datum data+schema) subst)
	   (setf (data~annotation datum)
		 (subst~apply subst (data~annotation datum)))
	   (term==update-subtypes (data~schema-range) subst)
	   datum)
  (:method ((datum list) subst)
	   (mapcar #'(lambda (x) (term==update-subtypes x subst)) datum)))
	   
;; This is a very intern function of the term-modul, because it describes
;; states of an term WHILE being created.
(data~defgeneric term=set-type ((datum) subst)
  (declare (edited  "07-NOV-1997")
	   (authors Gebhard)
	   (input   "A just created datum.")
	   (effect  "The typ of the datum is computed, actualised.")
	   (value   "The typed datum."))
  (:method ((datum data+primitive) subst)
	   (declare (ignore subst))
	   (format t "KEIM: term=set-type, Primitives not need this function"))
  (:method ((datum data+appl) subst)
	   (let* ((func-type (data~annotation
			      (term=update-subtypes
			       (data~appl-function datum)
			       subst)))
		  (arg-types (mapcar 'data~annotation
				     (term=update-subtypes
				      (data~appl-arguments datum)
				      subst)))
		  (restype (type~apply func-type arg-types
				       :substitution T))
		  (back-subst (subst~idem (cadr restype))))

	     ;; (format t "~%~%IN TERM=SET-TYPE, RESTTYPE: ~A" restype)
	     (setf (data~annotation datum) (car restype))
	     (dolist (x (cons (data~appl-function datum)
			      (data~appl-arguments datum)))
	       (when (intersection 
		      (term~free-type-variables x) 
		      (subst~domain back-subst))
		 (term=update-subtypes x back-subst)))
	     
	     ;; the call of term~free-type-variables can be very costly;
	     ;; especially if we compose terms recursively, such that this
	     ;; function is evaluated again and again on the same terms and subterms!
	     ;; a better solution would be to provide terms with an additonal slot
	     ;; containing the free variables somewhere hidden in the term!
	     ;; Mayby these slots could be set by one application of term~free-type-variables!
	     ;; Afterwards we would not have to apply the function again and again!
	     
	     ;;;; (term=update-subtypes datum (subst~idem (cadr restype)))  Ma waeret nie!
	     (subst~remove-components
	       (subst~domain back-subst)
	       back-subst)

	     ;; (format t "~%The Back-Type is: ~A" (data~annotation datum))
	     
	     datum))
 (:method ((datum data+abstr) subst)
	  (let* ((dom-types (mapcar #'(lambda (x)
					(if (data~schema-p x)
					    (data~schema-range
					     (data~annotation x))
					  (data~annotation x)))
				    (data~abstr-domain datum)))
		 (ran-type  (if (data~schema-p (data~abstr-range datum))
				(data~schema-range
				 (data~annotation (data~abstr-range datum)))
			      (data~annotation (data~abstr-range  datum)))))
	    (setf (data~annotation datum)
		  (type~func-create dom-types ran-type :mode :n-normalize))
	    (when subst (term=update-subtypes datum subst))
	    datum))
 (:method ((datum data+schema) subst)
	  (declare (ignore subst))
	  (setf (data~annotation datum)
		(type~schema-create (data~annotation (data~schema-range datum))
				    :domain (data~schema-domain datum)
				    :rename nil))
	  datum))

;; ------------ Applikation
(defun term~appl-create (function arguments &key destructive
				                 (mode :n-normalize)
						 (tnormalize t))
  (declare (edited  "01-SEP-1998")
	   (authors Gebhard)
	   (input   "A function, its arguments, and some keywords"
		    " -- destructive: should func and args be destructivly"
		    "                 changed for normalizing."
		    " -- mode: normalize the new created datum."
		    "          default: n-normalform"
		    " -- tnormalize: should the type of term be n-normlized"
		    "          default: yes")
	   (effect  "None")
	   (value   "The new created term application"))
  (let* ((func1 (if (data~schema-p function)
		    (data~schema-range
		     (data~copy function :downto nil))
		  function))
	 (args1 (mapcar #'(lambda (x)
			    (if (data~schema-p x)
				(data~schema-range
				 (data~copy x :downto nil))
			      x))
			arguments))
	 (term (data~appl-create func1
				 args1
				 :destructive destructive
				 :mode mode)))

    ;; (format t "~% WE ARE HERE:AFTER THE DATA-APPL: TYPE: ~A" (data~annotation term))

    (if tnormalize
	(progn
	  (setf (data~annotation term)
		(data~n-normalize (data~annotation term)
				  :share-vars t))
	  term)
      term)))

(defmethod data~appl-create :around
  ((function term+term) arguments &key destructive mode)
  (declare (edited  "22-APR-1995")
	   (authors Kohlhase)
	   (input   "")
	   (effect  "")
	   (value   ""))
  (declare (ignore destructive mode arguments))
  
  (let* ((new-thing (change-class
		     (term=set-type
		      (call-next-method)
		      (subst~create nil nil))
		     'term+appl)))
    
    new-thing))


;; ======================== S C H E M A T A
;; Schemata werde nach folgenden Regeln aufgebaut:
;; -- kappas sind die polymorphen Typevariablen
;; -- Der Term wird mit neuen Typvariablen fuer die kappas ausgestattet
;; -- Die neuen kappas kommen in den kappa Slot

(defun term~schema-create (term &key kappas destructive
				            (mode :n-normalize)
				            (tnormalize t))
  (if (and (or (null kappas)
	       (every 'type~variable-p kappas))
	   (not (term~schema-p term)))
      (let* ((dom  (if kappas
		       kappas
		     (term~type-variables-rec term))))
	(data~schema-create term dom :destructive destructive :mode mode))
    (error "Kappas has to be list of type-vars, term not a schema")))

(defun term~schema-close (term &key destructive
			       (mode :n-normalize)
			       (tnormalize t))
  (declare (edited  "10-FEB-1999")
	   (authors Ameier)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "If term is a schema: a schema with all free type variables (including the already"
		    "schemated variables) of the range of the term as domain and the range of the term"
		    "as range."
		    "If term is not a schema: a schema with all free type variables of the term as domain"
		    "and the term as range."))

  (let* ((range (if (term~schema-p term)
		    (data~schema-range term)
		  term))
	 (type-variables (term~type-variables-rec range)))

    (if type-variables
	(term~schema-create range
			    :kappas type-variables
			    :destructive destructive
			    :mode mode
			    :tnormalize tnormalize)
      range)))

(defmethod data~schema-create :around
  ((datum term+term) kappa &key destructive mode)
  (declare (edited  "03-NOV-1998")
	   (authors gebhard)
	   (input   )
	   (effect  )
	   (value   ))
  (declare (ignore destructive mode kappa))
  (change-class (term=set-type (call-next-method)
			       (subst~create nil nil))
		'term+schema))


(defun term~abstr-create (domain range &key destructive
				            (mode :n-normalize)
					    (tnormalize t))
  (declare (edited  "01-SEP-1998")
	   (authors Gebhard)
	   (input   "A list of variables (domain) an a term (range)"
		    "For further explanation see term~appl-create")
	   (effect  "See data~~abstr-create")
	   (value   "the new crated term abstraction"))
 (if (every #'term~variable-p domain)
     (let* ((new-ran (if (data~schema-p range)
			 (data~schema-range
			  (data~copy range :downto nil))
		       range))
	    (term (data~abstr-create domain new-ran
				     :destructive destructive
				     :mode mode)))
       (if tnormalize
	   (progn
	     (setf (data~annotation term)
		   (data~n-normalize (data~annotation term) :share-vars t))
	     term)
	 term))
   (error "data~~abstr-create: Elements in the domain have to be variables and range is not allowed to be a schema.")))


(defmethod data~abstr-create :around (domain (range term+term) &key destructive
	                                                            mode)
  (declare (edited  "22-APR-1995")
	   (authors Kohlhase)
	   (input   )
	   (effect  )
	   (value   ))
  (declare (ignore destructive mode domain))
  (change-class (term=set-type (call-next-method)
			       (subst~create nil nil))
		'term+abstr))



;; =========================================================================
;;                               Printing
;; =========================================================================

(defvar print*term-appearance 'short
  "affects all the print functions; recognized values are: 'short, 'verbose, 'constants, 'variables, 'abstr")

;;; The meaning is the following:
;;; short:        only the names of primitives are given
;;; verbose:      all type information is supplied
;;; constants:    type information is only supplied for constants
;;; variables:    type information is only supplied for variables
;;; abstrs:       type information is only supplied for abstractions
;;; appls:        type information is only supplied for applications
;;; abstrs&appls: type information is only supplied for abstractions and appls

(defmethod print-object ((datum term+term) stream)
  (format stream "|Some term|"))

;; ----------- Primitive
(defmethod print-object ((sym term+primitive) stream)
  (declare (edited  "02-AUG-1991 21:44")
	   (authors SORGE RICHTS))
  (case print*appearance
    ((verbose)
     (print-types t sym stream
		  (format nil "~:[unnamed (!) Primitive~;~:*Primitive ~S~]"
			  (keim~name sym))))
    (t (print-types t sym stream (format nil "~(~A~)" (keim~name sym))))))

;; ----------- Variables
(defmethod print-object ((sym term+variable) stream)
  (declare (edited  "02-AUG-1991 21:44")
	   (authors SORGE RICHTS))
  (case print*appearance
    ((short&discriminate discriminate)
     (print-types 'variables sym stream
		  (format nil "~:@(~A~)" (keim~name sym))))
    ((verbose)
     (print-types 'variables sym stream
		  (format nil "~:[unnamed (!) Variable~;~:*Variable ~S~]"
			  (keim~name sym))))
    (t (print-types 'variables sym stream
		    (format nil "~(~A~)" (keim~name sym))))))


;; ----------- Constants
(defmethod print-object ((sym term+constant) stream)
  (declare (edited  "02-AUG-1991 21:44")
	   (authors SORGE RICHTS))
  (case print*appearance
    ((verbose)
     (print-types 'constants sym stream 
		  (format nil "~:[unnamed (!) Constant~;~:*Constant ~S~]"
			  (keim~name sym))))
    (t (print-types 'constants sym stream
		    (format nil "~(~A~)" (keim~name sym))))))


;; --------------------------- C O M P L E X 

;; --------- Applikationen
(defmethod print-object ((appl term+appl) stream)
  (declare (edited  "02-AUG-1991 21:44")
	   (authors SORGE RICHTS))
  (print-types '(appls appls&abstrs abstrs&appls) appl stream
	       (with-output-to-string
		(str)
		(cond
		 ((or (null *print-length*) 
		      (> *print-length* (length (data~appl-arguments appl))))
		  (format str "(~A~{ ~A~})" (data~appl-function appl) 
			  (data~appl-arguments appl)))
		 (t (format str "(~A~{ ~A~} ...)"
			    (data~appl-function appl)
			    (subseq (data~appl-arguments appl)
				    0
				    (1- *print-length*))))))))

;; ---------- Abstraktionen
(defmethod print-object ((abstr term+abstr) stream)
  (declare (edited  "17-NOV-1994")
           (authors SORGE Fehrer RICHTS))
  (print-types '(abstrs appls&abstrs abstrs&appls) abstr stream
	       (with-output-to-string
		(str)
		(format str "[lam")
		(map nil #'(lambda (x) (format str " ~A" x))
		     (data~abstr-domain abstr))
		(format str ".~A]" (data~abstr-range abstr)))))

;; ---------- Abstraktionen
(defmethod print-object ((scheme term+schema) stream)
  (declare (edited  "17-NOV-1994")
           (authors SORGE Fehrer RICHTS))
  (print-types '(abstrs appls&abstrs abstrs&appls) scheme stream
	       (with-output-to-string
		(str)
		(format str "<kap")
		(map nil #'(lambda (x) (format str " ~A" x))
		     (data~schema-domain scheme))
		(format str ".~A>" (data~schema-range scheme)))))

(defun print-types (type sym stream output)
  (let ((app  print*term-appearance))
    (format stream "~A" output)
     (when (find app (if (atom type)
			 (cons type '( verbose))
		       (append type '( verbose))))
       (format stream ":~A"  (data~annotation sym)))))





;; =========================================================================
;;                            Transparenz und Tests
;; =========================================================================

(defun term~type (term-obj)
  (declare (edited  "01-SEP-1998")
	   (authors Gebhard)
	   (input   "A term")
	   (effect  "None")
	   (value   "The terms type"))
  ;; erweitern um umformungs optionen
  (data~annotation term-obj))

(defun (setf term~type) (new-type term)
  (setf (data~annotation term) new-type))

(defun term~type-unexpanded (term-obj)
  (declare (edited  "20-FEB-2001")
	   (authors Afiedler)
	   (input   "A term.")
	   (value   "The unexpanded type of TERM-OBJ."))
  (data=annotation term-obj)
  )

(defun term~constant-p (thing)
  (declare (edited  "22-APR-1995")
	   (authors Kohlhase)
	   (input   "An object." )
	   (effect  "None.")
	   (value   "T, iff ."))
  (typep thing 'term+constant))

(defun term~number-p (thing)
  (declare (edited  "20-JAN-1998")
	   (authors Sorge)
	   (input   "An object." )
	   (effect  "None.")
	   (value   "T, iff object is of type TERM+NUMBER."))
  (typep thing 'term+number))

(defun term~variable-p (thing)
  (declare (edited  "22-APR-1995")
	   (authors Kohlhase)
	   (input   "An object." )
	   (effect  "None.")
	   (value   "T, iff ."))
  (typep thing 'term+variable))

(defun term~appl-p (thing)
  (declare (edited  "22-APR-1995")
	   (authors Kohlhase)
	   (input   "An object." )
	   (effect  "None.")
	   (value   "T, iff ."))
  (typep thing 'term+appl))

(defun term~abstr-p (thing)
  (declare (edited  "22-APR-1995")
	   (authors Kohlhase)
	   (input   "An object." )
	   (effect  "None.")
	   (value   "T, iff ."))
  (typep thing 'term+abstr))

(defun term~schema-p (thing)
  (declare (edited  "22-APR-1995")
	   (authors Kohlhase)
	   (input   "An object." )
	   (effect  "None.")
	   (value   "T, iff ."))
  (typep thing 'term+schema))

(defun term~primitive-p (thing)
  (declare (edited  "22-APR-1995")
	   (authors Kohlhase)
	   (input   "An object." )
	   (effect  "None.")
	   (value   "T, iff ."))
  (typep thing 'term+primitive))

(defun term~complex-p (thing)
  (declare (edited  "22-APR-1995")
	   (authors Kohlhase)
	   (input   "An object." )
	   (effect  "None.")
	   (value   "T, iff ."))
  (typep thing 'term+complex))

(defun term~p (thing)
  (declare (edited  "22-APR-1995")
	   (authors Kohlhase)
	   (input   "An object." )
	   (effect  "None.")
	   (value   "T, iff ."))
  (typep thing 'term+term))


;; =========================================================================
;;                            Andere Funktionen 
;; =========================================================================

(defun term~apply (term1 term2 &key substitution)
  (let* ((term3  (if (listp term2) term2 (list term2)))
	 (result (multiple-value-bind (res-data res-subst)
		    (bind~with-bindings
		     ((let ((normalized (beta~head-normalize-with-args-and-bindings
					nil
					term1
					term3
					;;bindables
					nil)))
			(bind~insert-bindings! normalized :local T)))
		     :insert nil)
		  (list res-data res-subst))))
    (when (term~schema-p (car result))
      (setf (data~schema-domain (car result))
	    (data~schema-domain (data~annotation (car result)))))
    (if substitution result (car result))))

(defun term~free-variables (term)
  (declare (edited  "10-FEB-1998")
	   (authors Gebhard)
	   (input   "A Term")
	   (effect  "None")
	   (value   "list of all variables of term, that have no bound"
		    "occurence in term."))
  (set-difference (term~variables term) (term~bound-variables term)))

(data~defgeneric term~variables ((term))
  (declare (edited  "05-DEC-1997")
	   (authors Gebhard)
	   (input   "A Term")
	   (effect  "None")
	   (value   "A list of all (term)variables (bound or free) of the term"))
  (:method ((term term+variable)) (list term))
  (:method ((term term+constant)) nil)
  (:method ((term term+abstr))
	   (remove-duplicates
	    (append (data~abstr-domain term)
		    (term~variables (data~abstr-range term)))))
  (:method ((term term+appl))
	   (remove-duplicates
	    (append (mapcan 'term~variables
			    (data~appl-arguments term))
		    (term~variables (data~appl-function term)))))
  (:method ((term term+schema))
	   (term~variables (data~schema-range term))))


(data~defgeneric term~bound-variables ((term))
  (declare (edited  "10-FEB-1998")
	   (authors Gebhard)
	   (input   "A Term ")
	   (effect  "None")
	   (value   "The list of all (term)vars, having bound occurence in term."))
  (:method ((term term+variable)) nil)
  (:method ((term term+constant)) nil)
  (:method ((term term+abstr))
	   (remove-duplicates
	    (append (data~abstr-domain term) 
		    (term~bound-variables (data~abstr-range term)))))
  (:method ((term term+appl))
	   (remove-duplicates
	    (append (mapcan 'term~bound-variables (data~appl-arguments term))
		    (term~bound-variables (data~appl-function term)))))
  (:method ((term term+schema))
	   (term~bound-variables (data~schema-range term))))

(data~defgeneric term~free-type-variables ((term))
  (declare (edited  "26-JUN-1998")
	   (authors Ameier)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "The free type variables used in the term"
		    "(the type-vars without the kappa-bound type-vars.)"))
  (:method ((term term+primitive))
	   (term~type-variables-rec term))
  (:method ((term term+appl))
	   (remove-duplicates
	    (apply 'append (mapcar #'term~free-type-variables
				   (cons (data~appl-function term)
					 (data~appl-arguments term))))))
  (:method ((term term+abstr))
	   (remove-duplicates
	    (apply 'append (mapcar #'term~free-type-variables
				   (cons (data~abstr-range term)
					 (data~abstr-domain term))))))
  (:method ((term term+schema))
	   (set-difference (term~free-type-variables (data~schema-range term))
			   (data~schema-domain term)))
  (:method ((term list))
	   (remove-duplicates
	    (apply 'append (mapcar #'term~free-type-variables term)))))

;; Vorsicht diese Funktion berechnet alle Typvariablen, die in einem Term
;; Vorkommen  und nicht nur die, die 'frei' in ihm vorkommen
(data~defgeneric term~type-variables-rec ((term))
  (declare (edited  "26-MAR-1998")
	   (authors Gebhard)
	   (input   "A term")
	   (effect  "None")
	   (value   "A list of all type-variables occuring (bound or free) in this term."))
  (:method ((term term+primitive))
	   (type~variables (data~annotation term)))
  (:method ((term term+appl))
	   (term~type-variables-rec (cons (data~appl-function term)
				      (data~appl-arguments term))))
  (:method ((term term+abstr))
	   (term~type-variables-rec (cons (data~abstr-range term)
				      (data~abstr-domain term))))
  (:method ((term term+schema))
	   (remove-duplicates (append (data~schema-domain term) (term~type-variables-rec (data~schema-range term)))))
  (:method ((termlist list))
	   (remove-duplicates (mapcan #'(lambda (x) (term~type-variables-rec x))
				      termlist))))

(defun term~bound-type-variables (term)
  (declare (edited  "03-FEB-1999")
	   (authors Ameier)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "All type-variables that are bound somewhere in the term."))
  (set-difference (term~type-variables-rec term) (term~free-type-variables term)))
  

(defun term~get-type-variables (env)
  (declare (edited  "10-FEB-1998")
	   (authors Ameier)
	   (input   "An environment.")
	   (effect  "None.")
	   (value   "A list of all objects of class type+variable in the environment."))
  (let* ((type-vars (mapcar #'(lambda (key)
				(env~lookup-object key env))
			    (env~class-keys env 'type+variable))))
    (remove-if #'(lambda (type-var)
		   (find type-var term*not-usable-type-vars))
	       type-vars)))


(defun term~not-free-occuring (termvar datum &optional boundvars)
  (term=not-free-occuring termvar
			  datum
			  :boundvars boundvars
			  :followed nil))

;; neue Version: die zyklischen Bindungen nicht nachlaufen soll
(data~defgeneric term=not-free-occuring (termvar (datum) &key boundvars
						              followed)
  (declare (edited  "21-JAN-1998")
	   (authors Gebhard)
	   (input   "A term-var and a datum")
	   (effect  "None")
	   (value   "True if the variable doesn't occur free in datum"))
  (:method (termvar (datum term+abstr) &key boundvars followed)
	   (let ((new-bound (append (data~abstr-domain datum) boundvars)))
	     (term=not-free-occuring termvar
				     (data~abstr-range datum)
				     :boundvars new-bound
				     :followed followed)))
  (:method (termvar (datum term+appl) &key boundvars followed)
	   (and (term=not-free-occuring termvar (data~appl-arguments datum)
					:boundvars boundvars
					:followed followed)
		(term=not-free-occuring termvar (data~appl-function datum)
					:boundvars boundvars
					:followed followed)))
  (:method (termvar (datum term+constant) &key boundvars followed)
	   (declare (ignore termvar boundvars followed))
	   t)
  (:method (termvar (datum term+variable) &key boundvars followed)
	   (if (and (bind~binding datum)
		    (not (find datum followed :test #'data~equal)))
	       (term=not-free-occuring termvar
				       (bind~binding datum)
				       :boundvars boundvars
				       :followed (cons datum followed)) 
	     (or (find termvar boundvars :test #'data~equal)
		 (not (eq termvar datum)))))
  (:method (termvar (datum term+schema)  &key boundvars followed)
	   (term=not-free-occuring termvar
				   (data~schema-range datum)
				   :boundvars boundvars
				   :followed followed))
  (:method (termvar (datum list) &key boundvars followed)
	   (every #'(lambda (x) (term=not-free-occuring termvar x
							:boundvars boundvars
							:followed followed))
		  datum)))

(defun term~free-occ-positions (term var)
  (declare (edited  "06-MAR-1998")
	   (authors Gebhard)
	   (input   "A term and a variable")
	   (effect  "None")
	   (value   "The list of all positions where var is free occuring"
		    "in term."))
  (term=free-occ-positions term var (pos~empty) nil))

(data~defgeneric term=free-occ-positions ((term) var act-pos act-bound)
  (declare (edited  "06-MAR-1998")
	   (authors Gebhard)
	   (input   "A term and a termvariable.")
	   (effect  "None")
	   (value   "The list of positions where occurs free in the term"))
  (:method ((term term+constant) var act-pos act-bound)
	   (declare (ignore var act-pos act-bound))
	   nil)
  (:method ((term term+variable) var act-pos act-bound)
	   (cond ((find var act-bound :test `eq) nil)
		 ((eq term var) (list act-pos))
		 (t nil)))
  (:method ((term term+appl) var act-pos act-bound)
	   (let* ((i -1)
		  (result (mapcan #'(lambda (subterm)
				      (incf i)
				      (term=free-occ-positions
				       subterm
				       var
				       (pos~add-end i act-pos)
				       act-bound))
				  (cons (data~appl-function term)
					(data~appl-arguments term)))))
	     result))
  (:method ((term term+abstr) var act-pos act-bound)
	   (term=free-occ-positions (data~abstr-range term)
				    var
				    (pos~add-end 0 act-pos)
				    (append (data~abstr-domain term) act-bound)))
  (:method ((term term+schema) var act-pos act-bound)
	   (term=free-occ-positions (data~schema-range term)
				    var
				    (pos~add-end 0 act-pos)
				    act-bound)))




;; =========================================================================
;;                    Data Methoden
;; =========================================================================
 (defmethod data~replacement-check-p ((struct1 term+term) (struct2 term+term))
  (declare (edited  "29-JAN-1998")
	   (authors Ameier)
	   (input   "Two terms.")
	   (effect  "None.")
	   (value   "T if the two terms have equal types."))
  (data~equal (data~annotation struct1) (data~annotation struct2)))


;; ========================  data=replace-fv

(defmethod data=replace-fv ((datum term+constant)
			    bound domain codomain destructive downto test)
  (let* ((old-type (data~copy (data~annotation datum)
			      :downto (list 'data+primitive)))
	 (new-type (data=replace-fv
		    (term~type datum)
		    bound domain codomain destructive downto test)))
    (if (keim~equal old-type new-type)
	datum
      (let* ((new-constant (term~constant-create (keim~name datum) new-type)))
	(setf (data~constant-origin new-constant)
	      (data~constant-origin datum))
	new-constant))))

(defmethod data=replace-fv ((datum term+variable)
			    bound domain codomain destructive downto test)  
  (setf (data~annotation datum)
	(data=replace-fv (term~type datum)
			 bound domain codomain 't downto test))
  (let* ((pos (position datum domain :test test)))
    (cond ((or (find datum bound :test test)
	       (null pos))
	   ;; -> nicht ersetzen !
	   (if (or destructive
		   (eq downto data*all-classes-keyword)
		   (some #'(lambda (x) (typep datum x)) downto))
	       datum
	     (data~copy datum)))
	  (pos
	   ;; -> ersetzen 
	   (nth pos codomain))
	  (t (error "data~~replace-fv: This message shopuldn't appear!")))))


(defmethod data=replace-fv ((datum term+appl)
			    bound domain codomain destructive downto test)
  (if (or destructive
	  (eq downto data*all-classes-keyword)
	  (some #'(lambda (x) (typep datum x)) downto))
      (progn
	(setf (data~appl-function datum)
	      (data=replace-fv (data~appl-function datum)
				 bound domain codomain 't downto test))
	  (setf (data~appl-arguments datum)
		(data=replace-fv (data~appl-arguments datum)
				 bound domain codomain 't downto test))
	  (setf (data~annotation datum)
		(data=replace-fv (data~annotation datum)
				 bound domain codomain destructive downto
				 test))
	  datum)
    (let* ((new-fun (data=replace-fv
		     (data~appl-function datum)
		     bound domain codomain destructive downto test))
	   (new-args (data=replace-fv
		      (data~appl-arguments datum)
		      bound domain codomain  destructive downto test))
	   (new-type (data=replace-fv
		      (data~annotation datum)
		      bound domain codomain destructive downto test)))
      (make-instance (class-of datum)
		     :function new-fun
		     :arguments new-args 
		     :status (data=status datum)
		     :annotation new-type
		     ))))


(defmethod data=replace-fv ((datum term+abstr)
			    bound domain codomain destructive downto test)
  
  (let* ((new-bound (append bound (mapcan #'(lambda (x)
					      (when (data~variable-p x) (list x)))
					  (data~abstr-domain datum)))))
    (if (or destructive
	    (eq downto data*all-classes-keyword)
	    (some #'(lambda (x) (typep datum x)) downto))
	(progn
	  (setf (data~abstr-range datum)
		(data=replace-fv (data~abstr-range datum)
				 new-bound domain codomain 't downto test))
	  (setf (data~abstr-domain datum)
		(data=replace-fv (data~abstr-domain datum)
				 new-bound domain codomain 't downto test))
	  (setf (data~annotation datum)
		(data=replace-fv (data~annotation datum)
				 new-bound domain codomain destructive downto
				 test))
	  datum)
      (let* ((new-ran (data=replace-fv (data~abstr-range datum)
					 new-bound domain codomain destructive downto test))
	     (new-dom (data=replace-fv (data~abstr-domain datum)
				       new-bound domain codomain destructive downto test))
	     (new-type (data=replace-fv (data~annotation datum)
					new-bound domain codomain destructive downto test)))
	
	(make-instance (class-of datum)
		       :range new-ran 
		       :domain new-dom
		       :status (data=status datum)
		       :annotation new-type)))))



(defmethod data=replace-fv ((datum term+schema)
			    bound domain codomain destructive downto test)
  
  (let* ((new-bound (append bound (mapcan #'(lambda (x)
					      (when (data~variable-p x) (list x)))
					  (data~schema-domain datum)))))
    (if (or destructive
	    (eq downto data*all-classes-keyword)
	    (some #'(lambda (x) (typep datum x)) downto))
	(progn
	  (setf (data~schema-range datum)
		(data=replace-fv (data~schema-range datum)
				 new-bound domain codomain 't downto test))
	  (setf (data~schema-domain datum)
		(data=replace-fv (data~schema-domain datum)
				 new-bound domain codomain 't downto test))
	  (setf (data~annotation datum)
		(data=replace-fv (data~annotation datum)
				 new-bound domain codomain destructive downto
				 test))
	  datum)
      (let* ((new-ran (data=replace-fv (data~schema-range datum)
					 new-bound domain codomain destructive downto test))
	     (new-dom (data=replace-fv (data~schema-domain datum)
				       new-bound domain codomain destructive downto test))
	     (new-type (data=replace-fv (data~annotation datum)
					new-bound domain codomain destructive downto test)))
	
	(make-instance (class-of datum)
		       :datum new-ran 
		       :domain new-dom
		       :status (data=status datum)
		       :annotation new-type)))))

;; =========================================================================
;; data=replace-at-position-methods
;; =========================================================================

(defmethod data=replace-at-position ((datum term+appl)
				     position struct destructive downto)
  (let ((index (pos~first position))
	(appl-length (list-length (data~appl-arguments datum))))
    (cond ((or (minusp index) (> index appl-length))
	   (error "data=replace-at..: The position is not valid for ~A" datum))
	  ((and (zerop index) (pos~empty-p (pos~rest position)))
	   (let ((matcher (type~alpha-match (if (term~schema-p struct)
						(data~annotation (data~schema-range struct))
					      (data~annotation struct))
					    (data~annotation (data~appl-function datum)))))
	     (if matcher
		 (if (or destructive
			 (eq downto data*all-classes-keyword)
			 (some #'(lambda (x) (typep datum x)) downto))
		     (progn
		       (setf (data~appl-function datum)
			     (subst~apply matcher (if (term~schema-p struct)
						      (data~schema-range struct)
						    struct)))
		       datum)
		   (make-instance (class-of datum)
				  :function  (subst~apply matcher (if (term~schema-p struct)
								      (data~schema-range struct)
								    struct))
				  :arguments (data~copy
					      (data~appl-arguments datum)
					      :downto downto)
				  :annotation (data~copy
					       (data~annotation datum)
					       :downto (list 'data+variable))
				  :status (data=status datum)))
	       (error "Some of the structs are incompatible: ~A ~A."
		      (data~appl-function datum)
		      struct))))
	  ((zerop index)
	   (if (or destructive
		   (some #'(lambda (x) (typep datum x)) downto))
	       (progn
		 (setf (data~appl-function datum)
		       (data=replace-at-position
			(data~appl-function datum) (pos~rest position)
			struct 't downto))
		 datum)
	     (make-instance (class-of datum)
			    :function (data=replace-at-position
				       (data~appl-function datum)
				       (pos~rest position)
				       struct destructive downto)
			    :arguments (data~copy (data~appl-arguments datum)
						  :downto downto)
			    :annotation (data~copy
					 (data~annotation datum)
					 :downto (list 'data+variable))
			    :status (data=status datum))))
	  (t
	   (if (or destructive
		   (eq downto data*all-classes-keyword)
		   (some #'(lambda (x) (typep datum x)) downto))
	       (progn 
		 (setf (data~appl-arguments datum)
		       (data=replace-at-position
			(data~appl-arguments datum)
			(pos~add-front (- index 1) (pos~rest position))
			struct 't downto))
		 datum)
	     (make-instance (class-of datum)
			    :function (data~copy (data~appl-function datum)
						 :downto downto)
			    :arguments (data=replace-at-position
					(data~appl-arguments datum)
					(pos~add-front (- index 1)
						       (pos~rest position))
					struct destructive downto)
			    :annotation (data~copy (data~annotation datum)
						   :downto (list
							    'data+variable))
			    :status (data=status datum)))))))



(defmethod data=replace-at-position ((datum term+abstr)
				     position struct destructive downto)

  (cond ((not (zerop (pos~first position)))
	 (error "data=replace-at..: Position is not valid for datum ~A" datum))
	((pos~empty-p (pos~rest position))
	 (let ((matcher (type~alpha-match (if (term~schema-p struct)
					      (data~annotation (data~schema-range struct))
					    (data~annotation struct))					  
					  (data~annotation (data~abstr-range datum)))))
	   (if matcher
	       (if (or destructive
		       (eq downto data*all-classes-keyword)
		       (some #'(lambda (x) (typep datum x)) downto))
		   (progn
		     (setf (data~abstr-range datum)
			   (subst~apply matcher (if (term~schema-p struct)
						    (data~schema-range struct)
						  struct)))
		     datum)
		 (make-instance (class-of datum)
				:range (subst~apply matcher (if (term~schema-p struct)
								(data~schema-range struct)
							      struct))
				:domain (data~copy (data~abstr-domain datum)
						   :downto downto)
				:annotation (data~copy (data~annotation datum)
						       :downto
						       (list 'data+variable))
				:status (data=status datum)))
	     (error "Some of the structs are incompatible: ~A ~A."
		    struct
		    (data~abstr-range datum)))))
	(t
	 (if (or destructive
		 (eq downto data*all-classes-keyword)
		 (some #'(lambda (x) (typep datum x)) downto))
	     (progn
	       (setf (data~abstr-range datum)
		     (data=replace-at-position
		      (data~abstr-range datum) (pos~rest position) struct 't downto))
	       datum)
	   (make-instance (class-of datum)
			  :range (data=replace-at-position
				  (data~abstr-range datum)
				  (pos~rest position) struct destructive downto)
			  :domain (data~copy (data~abstr-domain datum)
					     :downto downto)
			  :annotation (data~copy (data~annotation datum)
						 :downto (list 'data+variable))
			  :status (data=status datum))))))


(defmethod data=replace-at-position ((datum term+schema)
				     position struct destructive downto)
  (cond ((not (zerop (pos~first position)))
	 (error "data=replace-at..: Position is not valid for datum ~A" datum))
	((pos~empty-p (pos~rest position))
	 (let ((matcher (type~alpha-match (data~annotation struct)
					  (data~annotation
					   (data~schema-range datum)))))
	   (if matcher
	       (if (or destructive
		       (eq downto data*all-classes-keyword)
		       (some #'(lambda (x) (typep datum x)) downto))
		   (progn
		     (setf (data~schema-range datum)
			   (subst~apply matcher struct))
		     datum)
		 (make-instance (class-of datum)
				:datum (subst~apply matcher struct) 
				:domain (data~copy (data~schema-domain datum)
						   :downto downto)
				:annotation (data~copy (data~annotation datum)
						       :downto
						       (list 'data+variable))
				:status (data=status datum)))
	     (error "Some of the structs are incompatible: ~A ~A."
		    struct
		    (data~schema-range datum)))))
	(t
	 (if (or destructive
		 (eq downto data*all-classes-keyword)
		 (some #'(lambda (x) (typep datum x)) downto))
	     (progn
	       (setf (data~schema-range datum)
		     (data=replace-at-position
		      (data~schema-range datum) (pos~rest position) struct 't downto))
	       datum)
	   (make-instance (class-of datum)
			  :datum (data=replace-at-position
				  (data~schema-range datum)
				  (pos~rest position) struct destructive downto)
			  :domain (data~copy (data~schema-domain datum)
					     :downto downto)
			  :annotation (data~copy (data~annotation datum)
						 :downto (list 'data+variable))
			  :status (data=status datum))))))




;; =========================================================================
;; data=replace-structs
;; ========================================================================

(defmethod data=replace-structs ((datum term+primitive)
				 struct-list1 struct-list2
				 destructive downto replacers-downto test)
  (let* ((assoc-struct (data~assoc datum struct-list1 struct-list2 test))
	 (assoc-copy (if assoc-struct
			 (data~copy assoc-struct
				    :downto (if (data~schema-p assoc-struct)
						nil
					      replacers-downto)
				    :explode nil :preserve :all-classes)
		       nil)))
    (if assoc-copy
	(let ((match (type~alpha-match (if (term~schema-p assoc-copy)
					   (data~annotation (data~schema-range assoc-copy))
					 (data~annotation assoc-copy))
				       (data~annotation datum))))
	  (if match
	      (subst~apply match (if (term~schema-p assoc-copy)
				     (data~schema-range assoc-copy)
				   assoc-copy))
	    (error "Some of the structs are incompatible: ~A ~A."
		   assoc-struct datum)))
      (if (or destructive
	      (eq downto data*all-classes-keyword)
	      (some #'(lambda (x) (typep datum x)) downto)) 
	  datum
	(data~copy datum :downto downto)))))
  

(defmethod data=replace-structs ((datum term+appl)
				 struct-list1 struct-list2
				 destructive downto replacers-downto test)
  (let* ((assoc-struct (data~assoc datum struct-list1 struct-list2 test))
	 (assoc-copy (if assoc-struct
			 (data~copy assoc-struct
				    :downto (if (data~schema-p assoc-struct)
						nil
					      replacers-downto)
				    :explode nil :preserve :all-classes)
		       nil)))
    (if assoc-copy
	(let ((match (type~alpha-match (if (term~schema-p assoc-copy)
					   (data~annotation (data~schema-range assoc-copy))
					 (data~annotation assoc-copy))
				       (data~annotation datum))))
	  (if match
	      (subst~apply match (if (term~schema-p assoc-copy)
				     (data~schema-range assoc-copy)
				   assoc-copy))
	    (error "Some of the structs are incompatible: ~A ~A."
		   assoc-struct datum)))
      (if (or destructive
	      (eq downto data*all-classes-keyword)
	      (some #'(lambda (x) (typep datum x)) downto))
	  (progn
	    (setf (data~appl-function datum)
		  (data=replace-structs (data~appl-function datum)
					struct-list1 struct-list2 't
					downto replacers-downto test))
	    (setf (data~appl-arguments datum)
		  (data=replace-structs (data~appl-arguments datum)
					struct-list1 struct-list2 't
					downto replacers-downto test))
	    datum)
	(make-instance (class-of datum)
		       :function (data=replace-structs
				  (data~appl-function datum)
				  struct-list1 struct-list2
				  destructive downto replacers-downto test)
		       :arguments (data=replace-structs
				   (data~appl-arguments datum)
				   struct-list1 struct-list2
				   destructive downto replacers-downto test)
		       :annotation (data~copy (data~annotation datum)
					      :downto (list 'data+variable))
		       :status (data=status datum))))))


(defmethod data=replace-structs ((datum term+abstr)
				 struct-list1 struct-list2 destructive
				 downto replacers-downto test)
  (let* ((assoc-struct (data~assoc datum struct-list1 struct-list2 test))
	 (assoc-copy (if assoc-struct
			 (data~copy assoc-struct
				    :downto (if (data~schema-p struct)
						nil
					      replacers-downto)
				    :explode nil :preserve :all-classes)
		       nil)))
    (if assoc-copy
	(let ((match (type~alpha-match (if (term~schema-p assoc-copy)
					   (data~annotation (data~schema-range assoc-copy))
					 (data~annotation assoc-copy))
				       (data~annotation datum))))
	  (if match
	      (subst~apply matcher (if (term~schema-p assoc-copy)
				       (data~schema-range assoc-copy)
				     assoc-copy))
	    (error "Some of the structs are incompatible: ~A ~A."
		   assoc-struct datum)))
      (if (or destructive
	      (eq downto data*all-classes-keyword)
	      (some #'(lambda (x) (typep datum x)) downto))
	  (progn
	    (setf (data~abstr-range datum)
		  (data=replace-structs (data~abstr-range datum)
					struct-list1 struct-list2 't
					downto replacers-downto test)) 
	    (setf (data~abstr-domain datum)
		  (data=replace-structs (data~abstr-domain datum)
					struct-list1 struct-list2 't
					downto replacers-downto test))
	    datum)
	(make-instance (class-of datum)
		       :range (data=replace-structs
			       (data~abstr-range datum)
			       struct-list1 struct-list2
			       destructive downto replacers-downto test)
		       :domain (data=replace-structs
				(data~abstr-domain datum)
				struct-list1 struct-list2
				destructive downto replacers-downto test)
		       :annotation (data~copy (data~annotation datum)
					      :downto (list
						       'data+variable))
		       :status (data=status datum))))))


(defmethod data=replace-structs ((datum term+schema)
				 struct-list1 struct-list2 destructive
				 downto replacers-downto test)
  (let* ((assoc-struct (data~assoc datum struct-list1 struct-list2 test))
	 (assoc-copy (if assoc-struct
			 (data~copy assoc-struct
				    :downto replacers-downto
				    :explode nil :preserve :all-classes)
		       nil)))
    (if assoc-copy
	(let ((match (type~alpha-match (data~annotation new-assoc)
				       (data~annotation datum))))
	  (if match
	      (subst~apply matcher new-assoc)
	    (error "Some of the structs are incompatible: ~A ~A."
		   assoc-struct datum)))
      (if (or destructive
	      (eq downto data*all-classes-keyword)
	      (some #'(lambda (x) (typep datum x)) downto))
	  (progn
	    (setf (data~schema-range datum)
		  (data=replace-structs (data~schema-range datum)
					struct-list1 struct-list2 't
					downto replacers-downto test)) 
	    (setf (data~schema-domain datum)
		  (data=replace-structs (data~schema-domain datum)
					struct-list1 struct-list2 't
					downto replacers-downto test))
	    datum)
	(make-instance (class-of datum)
		       :datum (data=replace-structs
			       (data~schema-range datum)
			       struct-list1 struct-list2
			       destructive downto replacers-downto test)
		       :domain (data=replace-structs
				(data~schema-domain datum)
				struct-list1 struct-list2
				destructive downto replacers-downto test)
		       :annotation (data~copy (data~annotation datum)
					      :downto (list
						       'data+variable))
		       :status (data=status datum))))))







;; ============================ subst~idem-rec

(defmethod subst~idem-rec ((appl term+appl) subst)
  (let ((new-func (subst~idem-rec (data~appl-function appl) subst)))
    (when new-func
      (let ((new-args (mapcar #'(lambda (arg)
				  (subst~idem-rec arg subst))
			      (data~appl-arguments appl))))
	(unless (some #'null new-args)
	  (term~appl-create new-func new-args :mode :conserve))))))

(defmethod subst~idem-rec ((abstr term+abstr) subst)
  (let* ((binder (data~abstr-domain abstr))
	 (old-labels (mapcar #'data~label binder))
	 (new-abstr))
    (unwind-protect
	(progn
	  (mapc #'(lambda (x) (setf (data~label x) x)) binder)
	  (let ((new-range (subst~idem-rec (data~abstr-range abstr) subst)))
	    (when new-range
	      (setf new-abstr (term~abstr-create binder new-range)))))
      (mapc #'(lambda (x y) (setf (data~label x) y)) binder old-labels))
    new-abstr))

(defmethod subst~idem-rec ((scheme term+schema) subst)
  (let* ((binder (data~schema-domain scheme))
	 (old-labels (mapcar #'data~label binder))
	 (new-scheme))
    (unwind-protect
	(progn
	  (mapc #'(lambda (x) (setf (data~label x) x)) binder)
	  (let ((new-datum (subst~idem-rec (data~schema-range scheme) subst)))
	    (when new-datum
		(setf new-scheme (term~schema-create new-datum :kappas binder)))))  
      (mapc #'(lambda (x y) (setf (data~label x) y)) binder old-labels))
    new-scheme))

;; =========================================================================
;;                        POST-anschluss und Environments
;; =========================================================================

(defun term=possibly-all-types (type-construct env)
  (declare (edited  "24-MAR-1998")
	   (authors Ameier)
	   (input   "A type-construct and an environment.")
	   (effect  "None.")
	   (value   "If the type-construct is a list and the first element is"
		    "'all-types' the hole list is read as a all-type "
		    "construct: '(all-types (type-vars) type)'"
		    "otherwise the construct is directly read as simple type."
		    "Multiple-value:"
		    "First: the type"
		    "Second: the all-quantified type-variables."))
  (cond ((or (symbolp type-construct)
	     (listp (first type-construct))
	     (not (string= (string (first type-construct)) "ALL-TYPES")))
	 (values (post~read-object type-construct env :existing-type) nil))
	(t ;; -> type-construct = (all-types ... )
	 (let* ((new-type-vars (second type-construct))
		(enter-vars (post~read-object new-type-vars env
					      :type-variables-multiple))
		(new-type (post~read-object (third type-construct) env
					    :existing-type)))
	   (mapcar #'(lambda (var)
		       (env~remove var env))
		   new-type-vars)
	   (values new-type enter-vars)))))

(defmethod post~read-object ((vars list)
			     (env env+environment) 
			     (indicator (eql :variables)))
  (mapcar #'(lambda (var)
	      (post~read-object var env :variable))
	  vars))

(defmethod post~read-object ((var cons) (env env+environment) 
			    (indicator (eql :variable)))
  (when (null (post~all-types-checker (second var)))
    (error "~%During reading variable ~A, it is only allowed to use 'all-types' at the top of a type" var))
  
  (let* ((type (post~read-object (second var) env :existing-type-arity-check))
	 ;; see post~read-object with indicator :existing-type-arity-check why this indicator is necessary here (instead of :existing-type)
	 (new-term (term~create-primitive-in-environment (first var)
							 (data~n-normalize type :destructive t)
							 'term+variable env)))
    new-term))

(defmethod post~read-object ((vars list) (env env+environment) 
			     (indicator (eql :variables-multiple)))
  (mapcar #'(lambda (var)
	      (post~read-object var env :variable-multiple))
	  vars))

(defmethod post~read-object ((var cons) (env env+environment) 
			    (indicator (eql :variable-multiple)))
  (when (null (post~all-types-checker (second var)))
    (error "~%During reading variable-multiple ~A, it is only allowed to use 'all-types' at the top of a type" var))
  
  (let* ((type (post~read-object (second var) env :existing-type-arity-check))
	 ;; see post~read-object with indicator :existing-type-arity-check why this indicator is necessary here (instead of :existing-type)
	 (new-term (term~create-primitive-in-environment (first var)
							 (data~n-normalize type :destructive t)
							 'term+variable env :allow-multi 't)))
    new-term))

(defmethod post~read-object ((consts list) (env env+environment) 
			    (indicator (eql :constants)))
  (mapcar #'(lambda (const)  
	      (post~read-object const env :constant))
	  consts))

(defmethod post~read-object ((const cons) (env env+environment) 
			    (indicator (eql :constant)))
  (when (null (post~all-types-checker (second const)))
    (error "~%During reading constant ~A, it is only allowed to use 'all-types' at the top of a type" const))
  
  (let* ((type (post~read-object (second const) env :existing-type-arity-check))
	 ;; see post~read-object with indicator :existing-type-arity-check why this indicator is necessary here (instead of :existing-type)
	 (new-term (term~create-primitive-in-environment (first const)
							 (data~n-normalize type :destructive t)
							 'term+constant env)))
    new-term))

(defgeneric term~create-primitive-in-environment (symbol type class env &key (allow-multi nil))
  (declare (edited  "15-NOV-1996")
	   (authors Ameier)
	   (input   "A symbol, a type, a term primitive class, an environment"
		    "and a keyword allow-multi.")
	   (effect  "If there is already an object in the environment with the"
		    " key symbol, but an other class, error."
		    "If there is already an object in the environment with the"
		    "key symbol and the same class, but an other type"
		    "(annotation), error."
		    "If there is already an object in the environment with the"
		    "same symbol and the same class, this object is returned."
		    "Otherwise or if it is allow to define terms multiple in"
		    "the environment, a term-object of the class is created,"
		    "added to the environment and returned.")
	   (value   "See effect."))
  (:method ((symbol symbol) (type type+type) (class (eql 'term+variable)) (env env+environment) &key (allow-multi nil)) 
	   (let* ((thing (env~lookup-object symbol env))
		  (class-thing (class-name (class-of thing))))
	     (cond ((or allow-multi (null thing))
		    (let* ((real-type (if (type~schema-p type)
					  (data~schema-range type)
					type))
			   (type-kappas (if (type~schema-p type)
					    (data~schema-domain type)
					  nil))
			   (term-object (term~variable-create symbol real-type))
			   (term-object-ii (if (type~schema-p type)
					       (term~schema-create term-object :kappas type-kappas)
					     term-object)))
		      (env~enter symbol term-object-ii env)
		      term-object-ii))
		   ((and thing (not (equal class-thing class)))
		    (error "Can't declare ~S as a term-object of class ~A, because it already exists in the environment as a ~A. "
			   symbol class class-thing))
		   ((not (keim~equal (data~annotation thing) type))
		    (error "Can't declare ~A as a term of type ~A, because it already exists in the environment with type ~S." 
			   symbol type (data~annotation thing)))
		   (t
		    thing))))
  (:method ((symbol symbol) (type type+type) (class (eql 'term+constant)) (env env+environment) &key (allow-multi nil))
	   (let* ((thing (env~lookup-object symbol env))
		  (class-thing (class-name (class-of thing))))
	     (cond ((null thing)		    
		    (let* ((real-type (if (type~schema-p type)
					  (data~schema-range type)
					type))
			   (type-kappas (if (type~schema-p type)
					    (data~schema-domain type)
					  nil))
			   (term-object (term~constant-create symbol real-type))
			   (term-object-ii (if (type~schema-p type)
					       (term~schema-create term-object :kappas type-kappas)
					     term-object)))
		      (env~enter symbol term-object-ii env)
		      term-object-ii))
		   ((and thing (not (equal class-thing class)))
		    (error "Can't declare ~S as a term-object of class ~A, because it already exists in the environment as a ~A. "
			   symbol class class-thing))
		   ((not (keim~equal (data~annotation thing) type))
		    (error "Can't declare ~A as a term of type ~A, because it already exists in the environment with type ~S." 
			   symbol type (data~annotation thing)))
		   (t
		    thing)))))

(defun term~read-term-of-class (term env class)
  (declare (edited  "17-FEB-1998")
	   (authors Ameier)
	   (input   "The post-representation of a term, the environment, the term should be read about"
		    "and a class symbol.")
	   (effect  "None.")
	   (value   "The term is read and if it is of the input-class it is returned, otherwise an"
		    "error is produced."))
  (let* ((readed-term (post~read-object term env :existing-term)))
    (if (typep readed-term class)
	readed-term
      (error "~A is not of type ~A." readed-term class))))

(defvar term*not-usable-type-vars nil)

(defmethod post~read-object (term (env env+environment)
				  (indicator (eql :existing-term-closed)))

  (let* ((term (post~read-object term env :existing-term)))

    (term~schema-close term)))

(defmethod post~read-object (term (env env+environment)
				  (indicator (eql :existing-term)))
  (cond ((symbolp term)
	 
	 ;; primitiver Term -> aus Environment lesen

	 (let* ((env-obj (term=env-lookup-symbol term env)))
	   (when (null env-obj)
	     (warn "Tried to read ~A as primitive term, but it was not definined in environment!"))
	   env-obj))
	
	((and (not (listp (first term)))
	      (string= (string (first term)) "GROUNDED"))

	 ;; zu groundender Term

	 (let* ((ground-types (mapcar #'(lambda (type-list)
					  (let* ((type (post~read-object type-list env :existing-type)))
					    (if (type~type-variables type)
						(error "~% Type ~A contains type-variables but has to be ground.")
					      type)))
				      (second term)))
		(term-to-ground (post~read-object (third term) env :existing-term)))

	   (if (null (data~schema-p term-to-ground))
	       term-to-ground
	     (let* ((domain (data~schema-domain term-to-ground))
		    (min-length (min (length domain) (length ground-types)))
		    (domain-vars (subseq domain 0 min-length))
		    (according-ground-types (subseq ground-types 0 min-length))
		    (rest-domain-vars (subseq domain min-length))
		    (new-term (data~replace-free-variables (data~schema-range term-to-ground) domain-vars according-ground-types)))

	       (if (null rest-domain-vars)
		   new-term
		 (term~schema-create new-term :kappas rest-domain-vars))))))
	
	((and (not (listp (first term)))
	      (string= (string (first term)) "ALL-TYPES"))

	 ;; Kappa Term
	 
	 (let* ((syn-type-variables (rest (butlast term)))
		(rest-term (first (last term)))
		(type-vars (post~read-object syn-type-variables env :type-constants-multiple))
		(the-term (post~read-object rest-term env :existing-term))                         
		(new-term (if (data~schema-p the-term) (data~schema-range the-term) the-term)))    ;; (all-types aa const) MP


	   ;; type-vars werden zunaechst als constanten eingelesen, damit sie waehrend des Termaufbaus nicht gebunden werden koennen
	   ;; jetzt aber werden wir sie in type-variablen umaendern !!

	   (mapcar #'(lambda (type-var)
		       (change-class type-var 'type+variable))
		   type-vars)
	   
	   ;; rauswerfen der type-variables aus dem environment
	   (mapcar #'(lambda (var)
		       (env~remove var env))
		   syn-type-variables)

	   (term~schema-create new-term
			       :kappas type-vars)
	   ))
	((and (not (listp (first term)))
	      (string= (string (first term)) "LAM"))

	 ;; Lambda Abstraktion
	 
	 (let* ((vars-des (rest (reverse (rest (reverse term)))))
		(normal-vars (remove-if-not #'(lambda (var-decl)
						(= (length var-decl) 2))
					    vars-des))
		(ss-vars (remove-if-not #'(lambda (var-decl)
					    (and (= (length var-decl) 3)
						 (not (equal (third var-decl) :lookup))))
					vars-des))
		(lookup-vars (remove-if-not #'(lambda (var-decl)
						(and (= (length var-decl) 3)
						     (equal (third var-decl) :lookup)))
					    vars-des))
		
		(variables (post~read-object normal-vars env :variables-multiple))
		(ss-variables (post~read-object ss-vars env :ss-variables))
		(lookup-variables (post~read-object lookup-vars env :lookup-variables))
		(type-var-subst (if (env~lookup-object 'type-var-subst env)
				    (env~lookup-object 'type-var-subst env)
				  (subst~create nil nil)))
		(scope (post~read-object (first (last term)) env :existing-term))
		(new-term (ignore-errors (term~abstr-create (append variables ss-variables lookup-variables) scope
							    :mode :n-normalize
							    :tnormalize 't  
							    ))))
	   (mapcar #'(lambda (var)
		       (env~remove (first var) env))
		   normal-vars)
	   
	   (if new-term
	       new-term
	     (error "POST ERROR: Creation of term abstraction with variables ~A and range ~A failed!"
		    (append variables ss-variables lookup-variables)
		    scope))
	   ))
	(t
	 
	 ;; Application
	 
	 (let* ((type-var-subst (if (env~lookup-object 'type-var-subst env)
				    (env~lookup-object 'type-var-subst env)
				  (subst~create nil nil)))
		(sub-terms (mapcar #'(lambda (sub-term)
				       (post~read-object sub-term env :existing-term))
				   term))
		(new-term (ignore-errors (term~appl-create (first sub-terms) (rest sub-terms)
							   :mode :n-normalize
							   :tnormalize 't 
							   ))))

	   (if new-term
	       new-term
	     (error "POST ERROR: Creation of application with function ~A and arguments ~A failed!~%Maybe there is a problem with the types of the function: ~A~%and the types of the arguments: ~A?" (first sub-terms) (rest sub-terms) (term~type (first sub-terms)) (mapcar #'term~type (rest sub-terms))))))))


(defmethod post~read-object ((vars list)
			     (env env+environment) 
			     (indicator (eql :lookup-variables)))
  (mapcar #'(lambda (var)
	      (post~read-object var env :lookup-variable))
	  vars))
  
(defmethod post~read-object ((var cons) (env env+environment) 
			     (indicator (eql :lookup-variable)))
  (let* ((type (post~read-object (second var) env :existing-type))
	 (symbol (first var))
	 (lookup-object (env~lookup-object symbol env)))
    (cond ((not (keim~equal (term~type lookup-object) type))
	   (error "In post~read-object, lookup-variable, object in environment and given type are not keim~equal."))
	  (t
	   lookup-object))))

(defun term=env-lookup-symbol (term env)
  (declare (authors nesmith)
	   (input  "A Lisp symbol representing a term and an environment.")
	   (effect "none")
	   (value  "Looks up the symbol in the environment.  If it maps to a term, the term is returned, otherwise an error is signaled."))
  (let ((obj (env~lookup-object term env)))
    (cond ((not obj) 
	   (post~error "~A is not an existing term." term))
	  ((not (term~p obj))
	   (post~error "~A is declared in the environment as a ~A, 
not as a term." term (class-of obj)))
	  (t obj)))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Post-Output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defmethod post~print ((sym term+primitive) stream)
  (format stream "~A" (keim~name sym)))

(defmethod post~print ((application term+appl) stream)
  (declare (edited  "27-JAN-1993 17:37")
	   (authors RICHTS)
	   (input   "A application, a stream and a list of 
                     keyword parameters.")
	   (effect  "(<function> <argument>*)")
	   (value   "Like format."))
  (format stream "(")
  (post~print (data~appl-function application) stream)
  (mapcar #'(lambda (arg)
	      (format stream " ")
	      (post~print arg stream))
	  (data~appl-arguments application))
  (format stream ")"))

(defmethod post~print ((abstraction term+abstr) stream)
  (declare (edited  "27-JAN-1993 17:37")
	   (authors RICHTS)
	   (input   "A abstraction, a stream and a list of keyword parameters.")
	   (effect  "(lam (<bound-var> <type>) <scope>)")
	   (value   "Like format."))
  (let ((bound-variable (data~abstr-domain abstraction))
	(scope (data~abstr-range abstraction)))
    (format stream "(lam ")
    (post~print-declaration bound-variable stream)
    (format stream " ")
    (post~print scope stream)
    (format stream ")")))

(defmethod post~print ((schema term+schema) stream)
   (let ((domain (data~schema-domain schema))
	 (range (data~schema-range schema)))
     (format stream "(all-types")
     (mapcar #'(lambda (type-var)
		 (format stream " ~A " (keim~name type-var)))
	     domain)
     (post~print range stream)
     (format stream ")")))


(defmethod post~print-declaration ((variable term+variable) stream)
  (format stream "(~A " (keim~name variable))
  (post~print (term~type-unexpanded variable) stream)
  (format stream ")"))

(defmethod post~print-declaration ((constant term+constant) stream)
  (format stream "(~A " (keim~name constant))
  (post~print (term~type-unexpanded constant) stream)
  (format stream ")"))

(defmethod post~print-declaration ((schema term+schema) stream)
  (let* ((domain (data~schema-domain schema))
	 (range (data~schema-range schema)))
    (if (typep range 'term+primitive)
	(progn
	  (format stream "(~A "  (keim~name range))
	  (post~print (term~type schema) stream)
	  (format stream ")"))
      (error "~%In post~print-declaration! range of schema has to be a term+primitive"))))


(defmethod env~post-print (key (var term+variable) stream)
  (declare (ignore key))
  (format stream "~&(variables (~A " (keim~name var))
  (post~print (data~annotation var) stream)
  (format stream "))~%")
  (values))

(defmethod env~post-print (key (con term+constant) stream)
  (declare (ignore key))
  (format stream "~&(constants (~A " (keim~name con))
  (post~print (data~annotation con) stream)
  (format stream "))~%")
  (values))

(defmethod env~post-print (key (ap term+appl) stream)
  (declare (ignore key stream))
  (values))

(defmethod env~post-print (key (abstr term+abstr) stream)
  (declare (ignore key stream))
  (values)) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; alpha-equal/match/unify
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(data~defgeneric term~alpha-equal ((term1) (term2) &key subst additional-bind)
  (declare (edited  "30-JAN-1998")
	   (authors Gebhard)
           (input   "Two terms. Subst is a pre-executed Substituion;"
		    "additional-bind is a list of additionally bindable"
		    "primitives.")
           (effect  "None")
           (value   "A subst if the terms are alpha equal."))
  (:method ((term1 term+term) (term2 term+term) &key (subst (subst~create nil nil)) additional-bind)
	   (multiple-value-bind
	       (success subst)
	       (bind~with-bindings
		((when (subst~p subst)
		   (subst~bind-substitution subst))
		 (term=alpha-equal term1 term2 additional-bind)))
	     (when success
	       (subst~idem subst)))))

(data~defgeneric term~alpha-match ((term1) (term2) &key subst)
  (declare (edited  "22-JAN-1998")
	   (authors Gebhard)
	   (input   "Two terms. Subst is a pre-executed Substituion")
	   (effect  "None.")
	   (value   "A subst if the terms are matchable, else nil"))
  (:method ((term1 term+term) (term2 term+term) &key (subst (subst~create nil nil)))
	   (let ((bindables (append (term~free-variables term1)
				    (term~free-type-variables term1))))
	     (multiple-value-bind
		 (success subst)
		 (bind~with-bindings
		  ((when (subst~p subst) (subst~bind-substitution subst))
		   (term=alpha-equal term1 term2 bindables)))
	       (when success
		 (subst~idem subst))))))

(data~defgeneric term~alpha-unify ((term1) (term2) &key subst)
  (declare (edited  "22-JAN-1998")
	   (authors Gebhard)
	   (input   "Two terms. Subst is a pre-executed Substituion")
	   (effect  "None")
	   (value   "A substitution if unifyable, else nil"))
  (:method ((term1 term+term) (term2 term+term) &key (subst (subst~create nil nil)))
	   (let ((bindables (append (term~free-variables term1)
				    (term~free-variables term2)
				    (term~free-type-variables term1)
				    (term~free-type-variables term2))))
	     (multiple-value-bind (success subst)
		 (bind~with-bindings
		  ((when (subst~p subst) (subst~bind-substitution subst))
		   (term=alpha-equal term1 term2 bindables)))
	       (when success (subst~idem subst))))))

(defun term~unify (term1 term2)
  (declare (edited  "16-MAR-1998")
	   (authors Ameier)
	   (input   "two terms.")
	   (effect  "None.")
	   (value   "A mgu if the two terms are unifiable, otherwise nil."))
  (term~alpha-unify term1 term2))
   
     
(data~defgeneric term=alpha-equal ((term1) (term2) bdbl)
  (declare (edited  "30-JAN-1998")
           (authors Gebhard)
           (input   "jede menge")
           (effect  "none")
           (value   "True if term1 and term2 are alpha-equal."))
  (:method ((term1 term+constant) (term2 term+constant) bdbl)
	   ;; (format t "~% alpha-equal: TERM+KONST TERM+KONST")
	   (cond ((bind~binding term1)
		  (term=alpha-equal (bind~binding term1) term2 bdbl))
		 ((bind~binding term2)
                   (term=alpha-equal term1
				     (bind~binding term2)
				     bdbl))
		 ((data~equal (data~constant-origin term1)
			      (data~constant-origin term2))
		  (let ((lev-res (type~alpha-equal (data~annotation term1)
						   (data~annotation term2)
						   bdbl)))
		    (when lev-res
		      (subst~bind-subst lev-res bdbl))))
		 ((find term1 bdbl)
		   (let ((lev-res (type~alpha-equal (data~annotation term1)
						    (data~annotation term2)
						    bdbl)))
		     (when lev-res
		       (progn 
			 (bind~bind term1 term2)
			 (subst~bind-subst lev-res bdbl)))))
		 ((find term2 bdbl)
		  (let ((lev-res (type~alpha-equal (data~annotation term1)
						   (data~annotation term2)
						   bdbl)))
		    (when lev-res
		      (progn 
			(bind~bind term2 term1)
			(subst~bind-subst lev-res bdbl)))))
		 (t nil)))
  (:method ((term1 term+variable) (term2 term+variable) bdbl)
	   ;; (format t "~% alpha-equal: TERMVAR TERMVAR")
	   (cond  ((eq term1 term2) t)
		  ((bind~binding term1)
                   (term=alpha-equal (bind~binding term1) term2
				     bdbl))
                  ((bind~binding term2)
                   (term=alpha-equal term1 (bind~binding term2)
				     bdbl))
                  ((data~equal term1 term2)
                   (let ((lev-res (type~alpha-equal (data~annotation term1)
						    (data~annotation term2)
						    bdbl)))
		     (when lev-res
		       (subst~bind-subst lev-res bdbl))))
		  ((find term1 bdbl)
		   (let ((lev-res (type~alpha-equal (data~annotation term1)
						    (data~annotation term2)
						    bdbl)))
		     (when lev-res
		       (progn 
			 (bind~bind term1 term2)
			 (subst~bind-subst lev-res bdbl)))))
		  ((find term2 bdbl)
		   (let ((lev-res (type~alpha-equal (data~annotation term1)
						    (data~annotation term2)
						    bdbl)))
		     (when lev-res
		       (progn 
			 (bind~bind term2 term1)
			 (subst~bind-subst lev-res bdbl)))))
		  (t nil)))
  (:method ((term1 term+variable) (term2 term+term) bdbl)
	   ;; (format t "~% alpha-equal: TERMVAR TERM+TERM")
	   (cond ((bind~binding term1)
                   (term=alpha-equal (bind~binding term1) term2
				     bdbl))
                  ((and (find term1 bdbl) (term~not-free-occuring term1 term2))
		   (let ((lev-res (type~alpha-equal (data~annotation term1)
						    (data~annotation term2)
						    bdbl)))
		     (when lev-res
		       (progn 
			 (bind~bind term1 term2)
			 (subst~bind-subst lev-res bdbl)))))
                  (t nil)))
  (:method ((term1 term+term) (term2 term+variable) bdbl)
	   ;; (format t "~% alpha-equal: TERM+TERM TERM+VAR")
	   (cond ((bind~binding term2)
                   (term=alpha-equal term1 (bind~binding term2)
				     bdbl))
                  ((and (find term2 bdbl) (term~not-free-occuring term2 term1))
		   (let ((lev-res (type~alpha-equal (data~annotation term1)
						    (data~annotation term2)
						    bdbl)))
		     (when lev-res
		       (progn 
			 (bind~bind term2 term1)
			 (subst~bind-subst lev-res bdbl)))))
 		  (t nil)))
  (:method ((term1 term+appl) (term2 term+appl) bdbl)
	   ;; (format t "~% alpha-equal: TERM+APPL TERM+APPL")
	   (let ((argl1 (length (data~appl-arguments term1)))
		 (argl2 (length (data~appl-arguments term2))))
	     (cond ((< argl1 argl2)
		    (let ((new-fun2 (term~appl-create
				     (data~appl-function term2)
				     (subseq (data~appl-arguments term2)
					     0 (- argl2 argl1))))
			  (new-arg2 (last (data~appl-arguments term2) argl1)))
		      (term=alpha-equal (cons (data~appl-function term1)
					      (data~appl-arguments term1))
					(cons new-fun2 new-arg2)
					bdbl)))
		   ((< argl2 argl1)
		    (let ((new-fun1 (term~appl-create
				     (data~appl-function term1)
				     (subseq (data~appl-arguments term1) 
					     0 (- argl1 argl2))))
			  (new-arg1 (last (data~appl-arguments term1) argl2)))
		      (term=alpha-equal (cons new-fun1 new-arg1)
					(cons (data~appl-function term2)
					      (data~appl-arguments term2))
				        bdbl)))
		   ((= argl1 argl2)
		    (term=alpha-equal (cons (data~appl-function term1)
					    (data~appl-arguments term1))
				      (cons (data~appl-function term2)
					    (data~appl-arguments term2))
				      bdbl))
		   (t nil))))
  (:method ((term1 term+abstr) (term2 term+abstr) bdbl)
	   ;; (format t "~% alpha-equal: TERM+ABSTR TERM+ABSTR")
	   (let ((doml1 (length (data~abstr-domain term1)))
		 (doml2 (length (data~abstr-domain term2))))
	     (cond ((= doml1 doml2)

		    ;; first make a check of the domain-variables (to get for instance type matchings etc.)
		    ;; the one domain is bindable, the other one not.
		    ;; This will cause that the variables of the one domain are afterwards bound to the
		    ;; variables of the other domain. 
		    (term=alpha-equal (data~abstr-domain term1)
				      (data~abstr-domain term2)
				      (data~abstr-domain term1))

		    ;; Then make a alpha-equal on the ranges of the abstraction.
		    ;; Thereby, NONE OF THE DOMAIN VARIABLES has to be bindable, since they are already matched.
		    ;; Indeed, if now some of the domain-variables are bindable this will cause failures
		    ;; AMEIER
		    (term=alpha-equal (data~abstr-range term1)
				      (data~abstr-range term2)
				      bdbl))
		   
		   ((< doml1 doml2)
		    (let ((newran2 (term~abstr-create
				    (last (data~abstr-domain term2)
					  (- doml2 doml1))
				    (data~abstr-range term2)))
			  (newdom2 (subseq (data~abstr-domain term2) 0 doml1)))

		      (term=alpha-equal (data~abstr-domain term1)
					newdom2
					(data~abstr-domain term1))

		      (term=alpha-equal (data~abstr-range term1)
					newran2
					bdbl)))
		   
		   ((< doml2 doml1) 
		    (let ((newran1 (term~abstr-create
				    (last (data~abstr-domain term1)
					  (- doml1 doml2))
				    (data~abstr-range term1)))
			  (newdom1 (subseq (data~abstr-domain term1) 0 doml2)))

		      (term=alpha-equal newdom1
					(data~abstr-domain term2)
					newdom1)
		      
		      (term=alpha-equal newran1
					(data~abstr-range term2)
					bdbl)))
		   (t nil))))
  (:method ((term1 term+schema) (term2 term+schema) bdbl)
	   
	   (let ((lev-res (type~alpha-equal (data~annotation term1)
	   				    (data~annotation term2)
	   				    bdbl)))
	     (when lev-res
	       (progn 
	   	 (subst~bind-subst lev-res (append (data~schema-domain term1)
	   					   (data~schema-domain term2)
	   	 				   bdbl))
	   	 (term=alpha-equal (data~schema-range term1)
	   			   (data~schema-range term2)
	   			   bdbl)))))
  (:method ((term1 term+term) (term2 term+term) bdbl)
	   (declare (ignore bdbl))
	   ;;(format t "~% alpha-equal:  F E H L E R F A L L ~% ")
	   nil)
  (:method ((term1 data+struct) (term2 data+struct) bdbl)
	   (declare (ignore bdbl))
           (warn ";;;ERROR:Term=alpha-equal, only pally it on term+...!")
           nil)
  (:method ((terme1 list) (terme2 list) bdbl)
	   ;; (format t "~% alpha-equal: L I S T E N ~%")
	   (every #'(lambda (x y) (term=alpha-equal x y bdbl))
		  terme1 terme2)))

(defmethod data~alpha-copy ((datum term+term) renaming)
  (term~alpha-copy datum renaming))


(defgeneric term~alpha-copy (datum renaming)
  (declare (edited  "14-JUL-1998")
           (authors Ameier)
           (input   "A datum and the current renaming (former bound vars and their image)")
           (effect  "Nil")
           (value   "Multiple-value:"
		    "First: The alpha-renamed datum."
		    "Second: The updated renamings list."
		    "Remark: All constants in the scope of a schame-type variable with this type variable in type"
		    "        are replaced by new ones with updated type. Same thing happens to free variables occuring"
		    "        in the datum. Only term primitives with types without schemated type-variables are keeped."
		    "        The new free variables will be eq, the constants will only be keim/data~equal."
		    "        To free type-variables nothing is happened."))

  (:method ((datum term+constant) renaming)
	   ;;  -> checke ob der Typ der Konstante irgendwelche Type-variablen enthaelt, die
	   ;; auch in dem renaming als linke Seiten vorkommen
	   ;; -> wenn ja -> neue Constante mit neuem Typ, origin der neuen constante = origin der input constanten,
	   ;;    renaming NICHT veranedert 
	   ;; -> wenn nein -> input constante zurueck
	   
	   (let* ((type-vars (type~variables (term~type datum))))

	     (if (some #'(lambda (type-var)
			   (assoc type-var renaming))
		       type-vars)
		 
		 (let* ((new-const (term~constant-create (keim~name datum)
							 (first (data=copy (data~annotation datum) renaming
									   :FIX :ALL-CLASSES NIL NIL '(DATA+PRIMITIVE))))))
		   (setf (data~constant-origin new-const) (data~constant-origin datum))
		   (values new-const
			   renaming))

	       (values datum
		       renaming))))
  (:method ((datum term+variable) renaming)
           (if (assoc datum renaming)
	       ;; -> vorher gebundene Variable oder vorher bereits vorkommende Variable
	       ;; -> bereits vorher ersetzt -> benutze Ersetzungs term
               (values
		(rest (assoc datum renaming))
		renaming)
	     ;; -> hier neu vorkommende freie Variable -> checke ob der Typ der Variable irgendwelche Type-variablen enthaelt, die
	     ;; auch in dem renaming als linke Seiten vorkommen
	     ;; -> wenn ja -> neue Variable mit neuem Typ, renaming veraendert
	     ;; -> wenn nein -> gib diese Variable zurueck, renaming nicht veraendert	     
	     (let* ((type-vars (type~variables (term~type datum))))

	       (if (some #'(lambda (type-var)
			     (assoc type-var renaming))
			 type-vars)
		   
		   (let* ((new-variable (term~variable-create (keim~name datum)
							      (first (data=copy (data~annotation datum) renaming
										:FIX :ALL-CLASSES NIL NIL '(DATA+PRIMITIVE))))))
		     
		     (values
		      new-variable
		      (cons (cons datum new-variable)
			    renaming)))

		 (values
		  datum
		  renaming)))))
  
  (:method ((datum term+appl) renaming)

	   (multiple-value-bind
	       (back-list back-renaming)
	       (term~alpha-copy (cons (data~appl-function datum) (data~appl-arguments datum)) renaming)
	     
	     (values
	      (term~appl-create (first back-list)
				(rest back-list))
	      back-renaming)))
  (:method ((datum term+abstr) renaming)
           (let* ((bound-vars (data~abstr-domain datum))
                  (new-vars (mapcar #'(lambda (bound-var)
                                        (make-instance (class-of bound-var)
                                                       :name (data=copy-new-name)
                                                       :binding (data~binding bound-var)
                                                       :binding-table (data~binding-table bound-var)
                                                       :annotation (first (data=copy (data~annotation bound-var) renaming
										     :FIX :ALL-CLASSES NIL NIL '(DATA+PRIMITIVE)))      
                                                       :status T))						       
                                    bound-vars))
                  (new-renaming (pairlis bound-vars
					 new-vars
					 renaming)))
	     
	     (multiple-value-bind
		 (back-range back-renaming)
		 (term~alpha-copy (data~abstr-range datum) new-renaming)
	       
	       (values 
		(term~abstr-create new-vars back-range)
		back-renaming))))
  
  (:method ((datum term+schema) renaming)
	   (let* ((kappa-bound-type-vars (data~schema-domain datum))
		  (new-type-vars (mapcar #'(lambda (bound-type-var)
					     (make-instance (class-of bound-type-var)
							    :name (data=copy-new-name)
							    :status T
							    ))
					 kappa-bound-type-vars))
		  (new-renaming (pairlis kappa-bound-type-vars
					 new-type-vars
					 renaming)))
	     
	     (multiple-value-bind
		 (back-range back-renaming)
		 (term~alpha-copy (data~schema-range datum) new-renaming)

	       (values
		(term~schema-create back-range :kappas new-type-vars)
		back-renaming))))
  (:method ((datum list) renaming)
	   (if datum
	       (multiple-value-bind
		   (back-first-element back-renaming)
		   (term~alpha-copy (first datum) renaming)
		 (multiple-value-bind
		     (back-rest-elements back-rest-renaming)
		     (term~alpha-copy (rest datum) back-renaming)

		   (values
		    (cons back-first-element back-rest-elements)
		    back-rest-renaming)))
	     (values
	      nil
	      renaming))))	 


;; =========================================================================
;; Functions for TERM+NUMBER  (especially equality)
;; =========================================================================

(defmethod data~equal ((term1 term+number) (term2 term+number))
  (= (keim~name term1) (keim~name term2)))

(defmethod term~alpha-equal ((term1 term+number) (term2 term+number) &key subst additional-bind)
  (= (keim~name term1) (keim~name term2)))

(defmethod term=alpha-equal ((term1 term+number) (term2 term+number)
			     bdbl)
  (declare (ignore bdbl))
  (= (keim~name term1) (keim~name term2)))

(defmethod data~equal-p ((term1 term+number) (term2 term+number)
			 &key mode destructive)
  (declare (ignore mode destructive))
  (= (keim~name term1) (keim~name term2)))

(defmethod keim~equal ((term1 term+number) (term2 term+number))
  (equal (keim~name term1) (keim~name term2)))

(defmethod post~read-object ((term number) (env env+environment) (indicator (eql :existing-term)))
  (let ((type (env~lookup-object 'num env)))
    (if type
	(term~constant-create term type)
      (post~error "The type-constant declaration num is missing!"))))


;; =========================================================================
;;    Termspezifishe Methoden aus dem Beta Modul (dessen generischen
;;          Funktionen meist welche die von hop ererbt wurden
;; =========================================================================

(defmethod beta=contract ((term term+appl) &key destructive)
  (let* ((ab        (data~appl-function term))
	 (ab-dom    (data~abstr-domain ab))
	 (arg       (first (data~appl-arguments term)))
	 (new-scope (if (data~abstr-p ab)
			(data~replace-free-variables
			 (data~abstr-range ab)
			 (list (first ab-dom))
			 (list arg)
			 :destructive destructive)
		      ab))
	 (new-ab    (if (cdr ab-dom)
			(term~abstr-create (cdr ab-dom) new-scope)
		      new-scope)))
    (if (cdr (data~appl-arguments term))
	(term~appl-create new-ab (cdr (data~appl-arguments term)))
      new-ab)))

(defmethod beta~contains-redex-p ((datum term+abstr))
  (beta~contains-redex-p (data~abstr-range datum)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MISC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun term~generate-term-primitive-with-new-name (symbol type class env)
  (declare (edited  "20-MAR-1998")
	   (authors Ameier)
	   (input   "A symbol, a type, a class and an environment.")
	   (effect  "A new element of class CLASS is created and added into "
		    "the environment (CClass has to be a subclass of"
		    "term+primitive). If there is not already an element with"
		    "name SYMBOL in the environment the name of the new"
		    "element will be SYMBOL, otherwise it will be SYMBOLn"
		    "where n is the first natural number, that SYMBOLn is not"
		    "in the environment.")
	   (value   "The new created object."))
  (when (null (subtypep class 'term+primitive))
    (error "~A should be a subclass of term+primitive."))
  (let* ((name (term~generate-new-name symbol env)))
    (term~create-primitive-in-environment name type class env)))

(defun term~generate-new-name (symbol env)
  (declare (edited  "20-MAR-1998")
	   (authors Ameier)
	   (input   "A symbol and an environment.")
	   (effect  "None.")
	   (value   "A symbol, if there is no element in the environment with"
		    "name SYMBOL, SYMBOL itself, otherwise a SYMBOLN, where n"
		    "is the first natural number, that SYMBOLN is not in the"
		    "environment."))
  (let ((n 1)
	(new-symbol))
    (loop
     (setq new-symbol (intern (format nil "~A~A" symbol n)
			      (find-package :keim)))
     (when (null (env~lookup-object new-symbol env))
       (return))
     (incf n))
    new-symbol))


(data~defgeneric term~taco-equal ((term1) (term2))
  (declare (edited  "24-OCT-2000")
	   (authors Theiss)
           (input   "Two terms.")
           (effect  "None")
           (value   "A subst if the terms are probably equal."))
  (:method ((a term+term) (b term+term))
	   (let* ((a1 (if (term~constant-p a) (data~constant-origin a) a))
		  (b1 (if (term~constant-p b) (data~constant-origin b) b))
		  (a2 (if (term~schema-p a1) (data~schema-range a1) a1))
		  (b2 (if (term~schema-p b1) (data~schema-range b1) b1)))
	     (term~alpha-equal a2 b2)))
  (:method ((a list) (b list))
           (if (null a)
               (null b)
             (and (term~taco-equal (car a) (car b))
                  (term~taco-equal (cdr a) (cdr b)))))
  (:method (a b)
           (equal a b)))



