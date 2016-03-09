;;; -*- Mode: LISP; Base: 10; Syntax: Common-lisp; Package: KEIM -*-
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

(mod~defmod mapp :uses (keim mod data)
            :documentation "mapping in fundamental data structures."
            :exports (
                      mapp+mapping
                      mapp~create
                      mapp~domain
                      mapp~codomain
                      mapp~p
                      mapp~empty-p
                      mapp~list-p
                      mapp~insert-component
                      mapp~insert-component!
                      mapp~get-component
                      mapp~remove-component
                      mapp~remove-component!
                      mapp~remove-components
                      mapp~remove-components!
                      mapp~restrict-mapping
                      mapp~restrict-mapping!
                      mapp~delete-duplicates
                      mapp~delete-duplicates!
                      )
            )


#{
\section{Mappings}
This module provides elementary functions for mappings. Mappings come in several forms
like substitutions for instance. Essentially they map from a finite domain in a codomain.
#}


(eval-when (load compile eval)
(defclass mapp+mapping (keim+object)
  ((domain :initarg :domain :initform nil :accessor mapp~domain)
   (codomain :initarg :codomain :initform nil :accessor mapp~codomain))
  (:documentation "The class of mappings.")))

(defun mapp~create (domain codomain &optional copy)
  (declare (edited  "25-APR-1996 18:32")
	   (authors GKLEIN)
           (input   "A list of variables and a list of things. Optional a flag.")
           (effect  "None.")
           (value   "The new mapping with the domain DOMAIN and the codomain CODOMAIN."
                    "If COPY = T the input-lists are copied.")
           (example "(x y) (a (f a)) --> {(X --> A) (Y --> (F A))}"))
  (cond
   ((not (= (length domain) (length codomain))) (error "domain and codomain of a mapping have to be of matching length!"))
   (t
    (if copy
	(make-instance 'mapp+mapping :domain (copy-list domain) :codomain (copy-list codomain))
      (make-instance 'mapp+mapping :domain domain :codomain codomain)))))

#{
{\tt mapp~create} does not check if the codomain things are of the same category as the things in domain.
If we specialize mappings to substitutions of terms, we must obey the appropiate classes of domain and codomain
members.
#}

(defmethod print-object ((mapping mapp+mapping) stream)
  (declare (edited  "05-NOV-1991 12:22")
           (authors GKLEIN RICHTS)
           (input   )
           (effect  )
           (value   )
           (special *print-length*))
  (cond ((mapp~empty-p mapping)
         (format stream "{}"))
        (t
         (format stream "{(~A --> ~A)" (car (mapp~domain mapping)) (car (mapp~codomain mapping)))
         (do ((i (if  *print-length* (1- *print-length*) -1) (1- i))
              (domain-tail (cdr (mapp~domain mapping)) (cdr domain-tail))
              (codomain-tail (cdr (mapp~codomain mapping)) (cdr codomain-tail)))
             ((or (null domain-tail) (zerop i))
              (if domain-tail
                  (format stream " ...}")
                  (format stream "}")))
           (format stream " (~A --> ~A)" (car domain-tail) (car codomain-tail))))))

;;;
;;;
;;;


(defun mapp~p (object)
  (declare (edited  "04-NOV-1991 11:01")
           (authors RICHTS)
           (input   "A Lisp object.")
           (effect  "None.")
           (value   "True, iff OBJECT is a mapping.")
           (example "{(X --> A) (Y --> (F A))}-->T"))
  (typep object 'mapp+mapping))

(defmethod keim~copy ((mapping mapp+mapping) &key (explode :all-classes) share preserve downto)
  (declare (edited  "25-APR-1996 18:46")
	   (authors GKLEIN AMEIER)
           (input   "A mapping.")
           (effect  "None.")
	   (value   "The copied mapping."
		    "Warning: Which subparts of the mapping are shared depends on the"
		    "         keyword argument downto."))
  (if (or (eq downto :all-classes) (some #'(lambda (x) (typep mapping x)) downto))
      mapping
    (mapp~create (keim~copy (mapp~domain substitution)
			    :explode explode
			    :share share
			    :preserve preserve
			    :downto downto)
		 (keim~copy (mapp~codomain substitution)
			    :explode explode
			    :share share
			    :preserve preserve
			    :downto downto))))


(defun mapp~empty-p (mapping)
  (declare (edited  "25-APR-1996 18:46")
	   (authors GKLEIN)
           (input   "A mapping.")
           (effect  "None.")
           (value   "True iff MAPPING is empty.")
           (example "{(X --> A) (Y --> (F A))}-->NIL"
                     "{}--> NIL"))
  (null (mapp~domain mapping)))

(defmethod keim~equal ((mapp1 mapp+mapping) (mapp2 mapp+mapping))
  (declare (edited  "04-NOV-1991 11:02")
	   (authors GKLEIN RICHTS)
	   (input   "Two mappings.")
	   (effect  "None.")
	   (value   "True iff the two mappings are equal."))
  (let ((domain1 (mapp~domain mapp1))
        (domain2 (mapp~domain mapp2)))
    (and (eq (class-of mapp1) (class-of mapp2))
	 (= (length domain1) (length domain2))
	 (every #'(lambda (variable)
                    (member variable domain2 :test #'keim~equal))
                domain1)
         (every #'(lambda (variable)
                    (keim~equal (mapp~get-component variable mapp1)
				(mapp~get-component variable mapp2)))
                domain1))))

(defun mapp~list-p (object)
  (declare (edited  "04-NOV-1991 11:08")
           (authors RICHTS)
           (input   "OBJECT could be any lisp object.")
           (value   "True iff OBJECT is a (possible empty) list of mappings.")
           (example "({(X --> F) (Y --> (F A))} {}-->T"
                     "(a b)"))
  (and (listp object)
       (or (null object)
           (and (mapp~p (car object)) (mapp~list-p (cdr object))))))
;;;
;;;
;;;


(defgeneric mapp~insert-component (dom-thing codom-thing mapping)
  (declare (edited  "04-NOV-1991 11:08")
           (authors GKLEIN RICHTS)
           (input   "A thing of the domain, a thing of the codomain and a mapping (which may be nil).")
           (effect  "None.")
           (value   "A new mapping where [DOM-THING --> CODOM-THING] is added at the front of MAPPING.")
           (remark  "There is no check if the added component [DOM-THING --> CODOM-THING] is in the domain"
                    "and codomain. Also occurrences of DOM-THING in the codomain terms are not substituted.")
           (example "Z B {(X --> F) (Y --> (F A))} -->"
                     "{(Z --> B) (X --> F) (Y --> (F A))}"))
  (:method (dom-thing codom-thing (mapping mapp+mapping))
	   (let ((new-mapping (mapp~create (cons dom-thing (copy-list (mapp~domain mapping)))
					   (cons codom-thing (copy-list (mapp~codomain mapping))))))
	     new-mapping)))


(defun mapp~insert-component! (dom-thing codom-thing mapping)
  (declare (edited  "18-APR-1995" "04-NOV-1991 11:09")
           (authors GKLEIN Fehrer RICHTS)
           (input   "A thing of the domain, a thing of the codomain and a mapping.")
           (effect  "The pair [DOM-THING --> CODOM-THING] is added at the front of the domain and codomain lists"
                    "of MAPPING.")
           (value   "The changed MAPPING.")
           (remark  "There is no check if the added component [DOM-THING --> CODOM-THING] is in the domain and codomain."
                    "Also occurences of DOM-THING in the codomain terms are not substituted.")
           (example "X B {(Z --> F) (Y --> (F X))} -->"
                     "{(X --> B) (Z --> F) (Y --> (F X))}"))
  (let ((new-domain (cons dom-thing (mapp~domain mapping)))
        (new-codomain (cons codom-thing (mapp~codomain mapping))))
    (setf (mapp~domain mapping) new-domain)
    (setf (mapp~codomain mapping) new-codomain)
    mapping))

;;;
;;;
;;;


(defun mapp~get-component (dom-thing mapping)
  (declare (edited  "04-NOV-1991 11:13")
           (authors GKLEIN RICHTS prckln)
           (input   "A thing and a mapping.")
           (effect  "None.")
           (value   "The codomain term corresponding to DOM-THING or NIL if there is none.")
           (example "X {(X --> F) (Y --> (F A))} --> F"))
  (some #'(lambda (var term)
            (if (keim~equal var dom-thing) term nil))
        (mapp~domain mapping)
        (mapp~codomain mapping)))

(defun mapp~remove-component (dom-thing mapping)
  (declare (edited  "04-NOV-1991 11:13")
           (authors RICHTS gk)
           (input   "A thing and a mapping.")
           (effect  "None.")
           (value  "A new mapping without DOM-THING and its codomain term.")
           (example "X {(X --> F) (Y --> (F A))} --> {(Y --> (F A))}"))
  (mapp~remove-component! dom-thing (keim~copy mapping :downto (list 'data+variable))))

(defun mapp~remove-component! (dom-thing mapping)
  (declare (edited  "04-NOV-1991 11:14")
           (authors RICHTS gk)
           (input   "A variable and a mapping.")
           (effect  "If DOM-THING is in the domain of the mapping, the component of the mapping"
                    "is removed destructively from the domain- and codomain-list.")
           (value  "MAPPING without DOM-THING and its codomain term.")
           (example "X {(X --> F) (Y --> (F A))} --> {(Y --> (F A))}"))
  (labels ((mapp=delete (domain codomain)
             (cond ((null domain) (values nil nil))
                   ((keim~equal dom-thing (car domain))
                    (values (cdr domain) (cdr codomain)))
                   (t (multiple-value-bind (domain-tail codomain-tail)
                          (mapp=delete (cdr domain) (cdr codomain))
                        (setf (cdr domain) domain-tail
                              (cdr codomain) codomain-tail)
                        (values domain codomain))))))
    (multiple-value-bind (domain-tail codomain-tail)
        (mapp=delete (mapp~domain mapping) (mapp~codomain mapping))
      (setf (mapp~domain mapping) domain-tail)
      (setf (mapp~codomain mapping) codomain-tail)))
  mapping)

(defun mapp~remove-components (dom-things mapping)
  (declare (edited  "04-NOV-1991 11:13")
           (authors RICHTS gk)
           (input   "A list of things and a mapping.")
           (effect  "None.")
           (value   "A new mapping without DOM-THINGS and its codomain terms.")
           (example "X Y {(X --> F) (Y --> (F A))} --> {}"
                     "NIL {(X --> F) (Y --> (F A))} --> {(X --> F) (Y --> (F A))}"))
  (mapp~remove-components! dom-things (keim~copy mapping :downto (list 'data+variable))))

(defun mapp~remove-components! (dom-things mapping)
  (declare (edited  "04-NOV-1991 11:14")
           (authors GKLEIN RICHTS gk)
           (input   "A list of things and a mapping.")
           (effect  "if a thing in the domain of MAPPING is in DOM-THINGS,"
                    "it is removed destructively from the domain and also its codomain term.")
           (value  "MAPPING without DOM-THINGS and its codomain terms.")
           (example "X Y {(X --> F) (Y --> (F A))} --> {}"))
  (labels ((mapp=delete (domain codomain)
             (if (null domain)
                 (values nil nil)
               (multiple-value-bind (domain-tail codomain-tail)
                   (mapp=delete (cdr domain) (cdr codomain))
                 (cond ((member (car domain) dom-things :test #'keim~equal)
                        (values domain-tail codomain-tail))
                       (t (setf (cdr domain) domain-tail
                                (cdr codomain) codomain-tail)
                         (values domain codomain)))))))
    (multiple-value-bind (domain-tail codomain-tail)
        (mapp=delete (mapp~domain mapping) (mapp~codomain mapping))
      (setf (mapp~domain mapping) domain-tail)
      (setf (mapp~codomain mapping) codomain-tail)))
  mapping)


(defun mapp~restrict-mapping (mapping things)
  (declare (edited  "05-NOV-1991 10:25")
           (authors GKLEIN RICHTS)
           (input   "A mapping and a list of things.")
           (effect  "None.")
           (value   "A new mapping where the domain is the intersection of the domain of MAPPING and THINGS"
                    "and the codomain contains the corresponding terms of MAPPING.")
           (example "{(X --> A) (Y --> (F A))} (X Z) -->{(X --> A)}"))
  (let ((new-mapping (mapp~create nil nil)))
    (mapc #'(lambda (var term)
              (when (member var things)
                (mapp~insert-component! var term new-mapping)))
          (mapp~domain mapping) (mapp~codomain mapping))
    (if (eq (class-of mapping) (class-of new-mapping))
	new-mapping
      (change-class new-mapping (class-of mapping)))))


(defun mapp~restrict-mapping! (mapping things)
  (declare (edited  "05-NOV-1991 10:26")
           (authors GKLEIN RICHTS)
           (input   "A mapping and a list of things.")
           (effect  "The things in the domain of MAPPING which don't occur in THINGS and their"
                    "corresponding terms in the codomain are destructively deleted from these lists.")
           (value   "The changed MAPPING.")
           (example "mapp --> {(X --> A) (Y --> (F A))}"
                     "{(X --> A) (Y --> (F A))} (X Z) -->{(X --> A)}"
                     "mapp --> {(X --> A)}, please notice the destructive changes made to mapp"))
  (do ((domain-tail (mapp~domain mapping) (cdr domain-tail)))
      ((or (null domain-tail) (member (car domain-tail) things :test #'keim~equal)))
    (setf (mapp~domain mapping) (cdr (mapp~domain mapping)))
    (setf (mapp~codomain mapping) (cdr (mapp~codomain mapping))))
  (do ((domain-tail (mapp~domain mapping))
       (codomain-tail (mapp~codomain mapping)))
      ((null (cdr domain-tail)))
    (cond ((member (cadr domain-tail) things :test #'keim~equal)
           (pop domain-tail)
           (pop codomain-tail))
          (t (pop (cdr domain-tail))
             (pop (cdr codomain-tail)))))
  mapping)


(defun mapp~delete-duplicates (mapping)
  (declare (edited  "05-NOV-1991 10:35")
           (authors GKLEIN RICHTS ohlbach)
           (input   "A mapping.")
           (effect  "None.")
           (value   "A new mapping with all pairs of MAPPING without that of the form (x --> x).")
           (example "{(X --> X) (Y --> (F A))} --> {(Y --> (F A))}"))
  (let ((new-mapping (mapp~create nil nil)))
    (do ((domain-tail (mapp~domain mapping) (cdr domain-tail))
         (codomain-tail (mapp~codomain mapping) (cdr codomain-tail)))
        ((null domain-tail))
      (unless (keim~equal (car domain-tail) (car codomain-tail))
        (mapp~insert-component! (car domain-tail) (car codomain-tail) new-mapping)))
    (if (eq (class-of mapping) (class-of new-mapping))
	new-mapping
      (change-class new-mapping (class-of mapping)))))

(defun mapp~delete-duplicates! (mapping)
  (declare (edited  "05-NOV-1991 10:37")
           (authors RICHTS)
           (input   "A mapping.")
           (effect  "Identical domain - codomain pairs (x --> x) are deleted in the mapping.")
           (value   "The changed MAPPING.")
           (example "mapp --> {(X --> X) (Y --> (F A))}"
                     "{(X --> X) (Y --> (F A))} --> {(Y --> (F A))}"
                     "mapp {(Y --> (F A))}, please notice the destructive changes made to mapp"))
  (do ((domain-tail (mapp~domain mapping) (cdr domain-tail))
       (codomain-tail (mapp~codomain mapping) (cdr codomain-tail)))
      ((or (null domain-tail) (not (keim~equal (car domain-tail) (car codomain-tail)))))
    (setf (mapp~domain mapping) (cdr (mapp~domain mapping)))
    (setf (mapp~codomain mapping) (cdr (mapp~codomain mapping))))
  (do ((domain-tail (mapp~domain mapping))
       (codomain-tail (mapp~codomain mapping)))
      ((null (cdr domain-tail)))
    (cond ((keim~equal (cadr domain-tail) (cadr codomain-tail))
           (pop (cdr domain-tail))
           (pop (cdr codomain-tail)))
          (t (pop domain-tail)
             (pop codomain-tail))))
  mapping)

;;;
;;;
;;;

(defun mapp~lookup-mappings (object mappings)
  (declare (edited  "26-APR-1996 15:07")
	   (authors GKLEIN)
	   (input   "An object and a list of mappings.")
	   (effect  "None.")
	   (value   #{
		    If OBECT does not occur in the domain of some mapping in the list of MAPPINGS,
		    then NIL will be returned.
		    Otherwise two values will be returned:
		    \begin{enumerate}
		    \item The codomain object of OBJECT from the first mapping in MAPPINGS.
		    \item this mapping
		    #}))
  (let ((context (first mappings)))
    (when context
      (let ((result (mapp~get-component object context)))
	(cond ((null result)
	       (mapp~lookup-mappings object (cdr mappings)))
	      (t
	       (values result context)))))))






