;;; -*- syntax: common-lisp; package: keim; base: 10; mode: keim -*-
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
;;   electronic mail: keim@ags.uni-sb.de                                     ;;
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



(mod~defmod SSTERM 
            :uses (bind data env keim subst term type)
            :documentation "Syntactically sorted Terms"
            :exports (ssterm+syntactic-sort
                      
		      term+syn-sorted-term
		      term+syn-sorted-var
		      
		      ssterm~add-ssort
                      ssterm~alpha-match
                      ssterm~get-ssort
                      ssterm~sort
                      ssterm~ssort-create
                      ssterm~var-create
                      ssterm~var-p
                      
                      term~ssterm-free
                      term~ssterm-on-top-level

                      ssterm*ssort-env))

;;; The following functions are internal in other modules and should not be used:
;;; (bind=bind bind=current-context bind=current-stack term=copy-and-polytype-rename)


	
;; Folgendes gilt es zu beachten:
;; ==============================
;; Um die Matching auf dieser Ebene zu realisieren musste
;; 1) an manchen Stellen darauf verzichtet werden komplett durchgetypte Terme zu haben,
;;    d.h. insbesondere replace Funktioenen durften keine Typkompatibilitaet testen
;; 2) die bedingung, dass in substitutionen oder Bindings nur structs gebunden werden
;;    durften musste aufgehobenb werden, damit man bsp-weise Metavariablen der Sorte
;;    :varlist an eine LISTE von VAriablen binden kann und trotzdem Substitutionen beutzen
;;    kann. (Dies wird im weiteren wohl (mindestens) noch weitere (subst~apply) Probeleme
;;    verursachen).

;; Um bisher vorhandene, prima mit den neuen Termen kooperierende Funktioenen mit tollen
;; Tests nicht vollkommen ueber den Haufen zu werfen, wurden diejenigen Funktioenen in den
;; verschiedenen Modulen die stoerten (mit leichtveraendertem Namen), in den jeweiligen
;; Modulen in neuer (d.h. leicht abgeaenderter Form)zusaetlich existieren.
;; Betroffen sind: data~replace-free-variabels ---> data~replace-free-variables-ntc (NoTypCheck)
;;                                                                                   - -  -
;;                 subst~create                ---> subst~create-nso            (NotStructsOnly)
;;                 subst~add-component         ---> subst~add-component-nso
;;                 subst~compose-substitution  ---> subst~compose-subst-nso
;;                 subst~apply                 ---> subst~apply-nso
;; Die zusaetzlichen Methoden fuer
;;                                                  bind=bind
;;                                                  term=copy-andpolytype-rename
;; damit man mit den Bindingskontexten auch Bindungen verwalten kann, die nicht auf
;; structs zeigen (bleiben in diesem File stehen).

;; Alle anderen Funktioenen stehen in den jeweiligen Files, gekennzeichenet durch folgenden
;; Text


;;
;;========================================================================================
;;                SYNTACTICALLY SORTED TERMS SPECIFIC FUNCTIONS
;;                   (mit aeusserster Vorsicht zu geniessen)
;; =======================================================================================


;; ---------------------------------------------------------------------------------------------------------------

;; ============================================================
;; DIE BISHER AEUSSERST SIMPLE KLASSE DER SORTEN
;; ============================================================


;; Sorten sind nichts weiter als Namen, die fuer die jeweilige Sorte stehen
(eval-when (load compile eval) 
  (defclass ssterm+syntactic-sort (keim+name) 
    ()
    (:documentation "A (dummy-)class for syntactically sorted terms.")))


;;---------------- Einige Funktionen zum Zugriff und Behandeln von Sorten

(defun ssterm~ssort-create (symbol)
  (declare (edited  "19-MAR-1998")
	   (authors Gebhard)
	   (input   "A symbol")
	   (effect  "Create a new sort, idendified by its name, the symbol.")
	   (value   "The new (sort)object"))
  (make-instance 'ssterm+syntactic-sort :name symbol))

;; Probleme damit dem env einen Namen zu geben 
;; und ein help bei der env~create Funktion ???
;; Ein globales Environment zum Verwalten der Sorten 
(defvar ssterm*ssort-env (env~create))


;; Standart Sorten, die immer vorhanden sind
;; Vorsicht: kein automatische Test, ob eine Sorte (Name) schon vorhanden ist
(env~enter 'var       (ssterm~ssort-create 'var)         ssterm*ssort-env)
(env~enter 'varlist   (ssterm~ssort-create 'varlist)     ssterm*ssort-env)
(env~enter 'metavar   (ssterm~ssort-create 'metavar)     ssterm*ssort-env)
(env~enter 'const     (ssterm~ssort-create 'const)       ssterm*ssort-env)
(env~enter 'constlist (ssterm~ssort-create 'constlist)   ssterm*ssort-env)
(env~enter 'term      (ssterm~ssort-create 'term)        ssterm*ssort-env) 
(env~enter 'termlist  (ssterm~ssort-create 'termlist)    ssterm*ssort-env)
(env~enter 'problem   (ssterm~ssort-create 'problem)     ssterm*ssort-env)


(defun ssterm~add-ssort (symbol)
  (declare (edited  "19-MAR-1998")
	   (authors Gebhard)
	   (input   "A symbol")
	   (effect  "Creates a new ssort, and enters it into the ssort-env")
	   (value   "The new sort-object"))
   (env~enter symbol (ssterm~ssort-create symbol) ssterm*ssort-env)
   (ssterm~get-ssort symbol))


(defun ssterm~get-ssort (symbol)
  (declare (edited  "19-MAR-1998")
	   (authors Gebhard)
	   (input   "A symbol")
	   (effect  "None")
	   (value   "The sort object, if a sort with Name symbol exists or nil"))
  (env~lookup-object symbol ssterm*ssort-env))


;; ================================
;; DIE KLASSEN FUER SORTIERTE TERME
;; ================================

;; ---- Allgemein `gesortete` Terme
(eval-when (load compile eval) 
  (defclass term+syn-sorted-term (term+term) 
    ((ssort :initform nil 
	    :initarg  :ssort
	    :accessor ssterm=sort))
    (:documentation "The superclass of all syntactically sorted terms.")))

;; ---- Syn. sort. Variablen 
(eval-when (load compile eval) 
  (defclass term+syn-sorted-var (term+syn-sorted-term term+variable) 
    ()
    (:documentation "A class for syntactically sorted variables.")))

;; ================================

;;; ---------------  Zugriffs und einfache Funktionen zu sortierten Termen

(defun ssterm~var-p (something)
  (typep something 'term+syn-sorted-var))

(defun ssterm-p (something)
  (typep something 'term+syn-sorted-term))

(defun ssterm~var-create (symbol sort &optional type)
  (let ((ssvar (data~variable-create 'term+syn-sorted-var :name symbol)))
    (setf (ssterm~sort ssvar) sort)
    (if type
	(progn
	  (setf (data~annotation ssvar) type))
	  
      (progn 
	(setf (data~annotation ssvar) (type~variable-create (gensym "ssv")))))
	
    ssvar))
	 
(data~defgeneric ssterm~sort ((ssvar))
  (:method ((ssvar data+struct))
	   nil)
  (:method ((ssvar term+syn-sorted-term))
           (ssterm=sort ssvar)))

(data~defgeneric (setf ssterm~sort) (sort (datum)) 
  (declare (edited  "17-NOV-1994" )
           (authors Fehrer )
           (input   "A list of data and a datum." )
           (effect  "the domain (binder) of the datum is set to the new domain (a list!)" )
           (value   "the changed datum"))
  (:method (sort (datum data+struct))
	   (declare (ignore sort))
           (error "Only term+syn-sorted-terms have sorts"))
  (:method (sort (datum term+syn-sorted-term))
           (setf (ssterm=sort datum) sort)))


;; ===========================================================================
;;
;; Wegen der Abhaengigkeiten hier stehende Funktionen und Methoden
;;
;; ===========================================================================


(data~defgeneric term~ssterm-free ((term))
  (declare (edited  "12-MAR-1998")
	   (authors Gebhard)
	   (input   "A term")
	   (effect  "None")
	   (value   "T if term has no subterm of class term+syn-sorted-term, else nil."))
  (:method ((term term+primitive))
	   (not (ssterm~var-p term)))
  (:method ((term term+appl))
	   (and (term~ssterm-free (data~appl-function term))
		(every #'term~ssterm-free (data~appl-arguments term))))
  (:method ((term term+abstr))
	   (and (term~ssterm-free (data~abstr-range term))
		(every #'term~ssterm-free (data~abstr-domain term))))
  (:method ((term term+syn-sorted-var)) nil))



(data~defgeneric term~ssterm-on-top-level ((term))
  (declare (edited  "13-MAR-1998")
	   (authors Gebhard)
	   (input   "A term")
	   (effect  "none")
	   (value   "True if the term contains a ssterm on the top level, else nil"
		    "This Funtion doesn't test term itself"))
  (:method ((term term+primitive))
	   (ssterm-p term))
  (:method ((term term+appl))
	   (some #'ssterm-p (cons (data~appl-function term) (data~appl-arguments term))))
  (:method ((term term+abstr))
	   (some #'ssterm-p (cons (data~abstr-range term) (data~abstr-domain term)))))

(defmethod bind=bind ((var data+primitive) (thing list) (key (eql nil)))
	   (let ((context (bind=current-context)))
	     (if context
		 (progn
		   (subst~insert-component! var thing context)
		   (setf (data~binding var) (list context thing (bind=current-stack)))
		   nil)
	       (error "you can bind variables only in binding contexts"))))
;;
;;--------------------------------------------------------------------------------------------------


;; Einige Funktioenen um Listen von Substitutionen zu behandeln
;; ============================================================
;; Nur als Mapping abkuerzungen 

(defun ssterm=substs-add-comps (subst subst-list &key destructive)
  (declare (edited  "19-MAR-1998")
	   (authors Gebhard)
	   (input   "A substitution and a lists of substitutions")
	   (effect  "Depends on keyword")
	   (value   "All subsitutions in subst-list are enlarged with subst"))
  (mapcar #'(lambda (x) (subst~compose-subst-nso x subst :destructive destructive))
	  subst-list))

(defun ssterm=substs-add-comp (var term subst-list &key destructive)
  (declare (edited  "19-MAR-1998")
	   (authors Gebhard)
	   (input   "A variable, a term and a list of substitutions")
	   (effect  "Depends on keyword")
	   (value   "Every substitution in the list is enlarged by the ver-term-pair."))
  (mapcar #'(lambda (x) (subst~add-component-nso var term x :destructive destructive))
	  subst-list))

(defun ssterm=substs-product (substs sub-substs &key destructive)
  (declare (edited  "19-MAR-1998")
	   (authors Gebhard)
	   (input   "A lists of Substs and a list of enlargement substs")
	   (effect  "Depends on keyword")
	   (value   "The list of all substitutions of substs, where every element of substs "
		    "is enlarged by every element of sub-substs."))
  (mapcan #'(lambda (y) (ssterm=substs-add-comps y substs :destructive destructive)) sub-substs )
  ;;(mapcan #'(lambda (x) (mapcan #'(lambda (y) (ssterm=substs-add-comps y x)) substs ))
  ;; 	  sub-substs)
  )


;; ==============================================================
;;                 MATCHING UND ALPHA GLEICHHEIT
;; ==============================================================
;; Im folgenden sind die NAmensgebeungen der Funktionen bis auf weiteres, noch nicht alzu
;; ernst zu nehmen

(defun ssterm~alpha-match (term1 term2 &key assocs add-bind (subst (subst~create nil nil)))
  (declare (edited  "19-MAR-1998")
	   (authors Gebhard)
	   (input   "Two terms and a list of associative Funktionsymbols (normally forall and exists)")
	   (effect  "Shouldn't have any extern effects")
	   (value   "A list of substitutions matching the two terms."))
  (let ((bdbl (append  (data~free-variables term1) (term~type-variables-rec term1)))
	(asso (mapcar #'(lambda (obj) (if (data~schema-p obj)
					  (data~schema-range obj) obj)) assocs)))
    ;; Spaeter sollte wohl (term~type-varibales...) add-bind ersetzt werden, in dem
    ;; dann die Typevariablen stehen, die von der Methode gebunden werden.
    (multiple-value-bind (result)
	(bind~with-bindings  
	 ((subst~bind-substitution subst)
	  (ssterm=alpha-equal term1 term2 bdbl asso nil 
			      (list subst)
			      nil nil -1))
	 :insert nil
	 :build-subst nil)
      result)))


(defun ssterm=ae-list-prepare (arg-list le1 le2 &optional prevars)
  (let ((vorne (subseq arg-list 0 le1))
        (hint  (append prevars (last arg-list (- le2 le1)))))
    (append vorne  (list hint))))


(data~defgeneric ssterm=alpha-equal ((te1) (te2) bdb assocs actass substs termtyp prevars fargs)
  (declare (edited  "13-MAR-1998")
           (authors Gebhard)
           (input   "Two terms, and some other information (einiges kann wohl weg).")
           (effect  "Shouldn't have extern effects")
           (value   "A list of substitutions, making the terms alpha-equal, where the"
                    "elements of bdb can be used in the  substs."))
  (:method ((te1 term+primitive) (te2 term+primitive) bdb assocs actass substs termtyp prevars fargs)
           (declare (ignore prevars termtyp actass assocs))
	   (multiple-value-bind
	       (succ subst)
	       (bind~with-bindings
		(;(term~alpha-equal te1 te2 :additional-bind bdb)))
		 (term=alpha-equal te1 te2 bdb)))
	     (if succ
		 (progn
		   (subst~bind-substitution subst)
		   (ssterm=substs-add-comps subst substs :destructive t)
		   substs)
	       nil)))
  (:method ((te1 term+appl) (te2 term+appl) bdb assocs actass substs termtyp prevars fargs)
           (declare (ignore prevars))
	   (cond ((term~ssterm-free te1)
		  (multiple-value-bind (succ subst)
		      (bind~with-bindings
		       (;(term~alpha-equal te1 te2 :additional-bind bdb)))
			(term=alpha-equal te1 te2 bdb)))
		    (if succ
			(progn
			  (subst~bind-substitution subst)
			  (ssterm=substs-add-comps subst substs :destructive t)
			  substs)
		      nil)))
		 ((and (find (data~appl-function te1) assocs :test 'data~equal)
		       (ssterm~var-p (car (data~abstr-domain (car (data~appl-arguments te1)))))
		       (eq (ssterm~sort (car (data~abstr-domain (car (data~appl-arguments te1)))))
			   (ssterm~get-ssort 'varlist)))
		  (if (data~equal (data~appl-function te1) (data~appl-function te2))
		      (let ((new-substs
			     (ssterm=alpha-equal (car (data~appl-arguments te1))
						 (car (data~appl-arguments te2))
						 bdb assocs
						 (data~appl-function te1)
						 (list (subst~create nil nil)) :abstr
						 nil 0)))
			(ssterm=substs-product substs new-substs))
		    nil))
		 ((find (ssterm~sort (car (last (data~appl-arguments te1)))) 
			(mapcar #'ssterm~get-ssort '(constlist varlist termlist)))
		  (let ((subsubsts (progn 
				     (when (< fargs 0) (incf fargs))
				     (if (= (length (data~appl-arguments te2))
					    (+ fargs (length (data~appl-arguments te1))))
					 (let* ((fixargs (length (data~appl-arguments te1))) 
						(nf2     (if (zerop fargs)
							     (data~appl-function te2)
							   (term~appl-create
							    (data~appl-function te2)
							    (subseq (data~appl-arguments te2) 0 fargs)
							    :mode :n-normalize)))               
						(na2 (subseq (data~appl-arguments te2)
							     fargs (- (+ fixargs fargs) 1)))    
						(nr2 (last (data~appl-arguments te2)
							   (- (length (data~appl-arguments  te2))
							      (- (+ fargs  fixargs) 1)))))
					   (multiple-value-bind (subst1) ;; hier ware vorher zwei parameter
					       (bind~with-bindings
						((ssterm=alpha-equal (cons (data~appl-function	te1)
									   (list (data~appl-arguments te1)))
								     (append (list nf2) na2 (list nr2))
								     bdb assocs actass
								     (list (subst~create nil nil))
								     :appl-args prevars fargs))
						:insert nil :build-subst nil)
					     subst1))
				       (let* ((fixargs (length (data~appl-arguments te1)))
					      (nf2     (if (zerop fargs)
							   (data~appl-function te2)
							 (term~appl-create (data~appl-function te2)
									   (subseq (data~appl-arguments te2)
										   0 fargs)
									   :mode :n-normalize)))
					      (na2 (subseq (data~appl-arguments te2)
							   fargs (- (+ fixargs fargs) 1)))      
					      (nr2 (last (data~appl-arguments te2)
							 (- (length (data~appl-arguments te2))
							    (- (+ fargs fixargs) 1))))
					      (new-subsubsts1 (multiple-value-bind (subst1)
								  (bind~with-bindings
								   ((ssterm=alpha-equal
								     (cons
								      (data~appl-function te1)
								      (list (data~appl-arguments te1)))
								     (append (list nf2) na2 (list nr2))
								     bdb assocs actass
								     (list (subst~create nil nil))
								     :appl-args prevars fargs))
								   :insert nil :build-subst nil)
								subst1)) 
					      (new-subsubsts2 (ssterm=alpha-equal
							       te1 te2 bdb assocs actass
							       (list (subst~create nil nil))
							       termtyp prevars (+ 1 fargs))))
					 (append new-subsubsts1 new-subsubsts2))))))
		    (if subsubsts
			(ssterm=substs-product substs subsubsts)
		      subsubsts)))
		 ((< (length (data~appl-arguments te1)) (length (data~appl-arguments te2)))
		  (let* ((lete2 (length (data~appl-arguments te2)))
			 (lete1 (length (data~appl-arguments te1)))
			 (fuarg (subseq (data~appl-arguments te2) 0 (- lete2 lete1)))
			 (rargs (last   (data~appl-arguments te2) lete1))
			 (func1 (term~appl-create (data~appl-function te2) fuarg :mode :n-normalize)))
		    (ssterm=alpha-equal (cons (data~appl-function te1) (data~appl-arguments te1))
					(cons func1 rargs)
					bdb assocs nil substs :appl-args
					prevars -1)))
		 (t
		  (let ((res (ssterm=alpha-equal (data~appl-function te1) (data~appl-function te2)
						 bdb assocs actass substs :appl-func nil -1)))
		    (when res
		      (ssterm=alpha-equal (data~appl-arguments te1) (data~appl-arguments te2)
					  bdb assocs actass substs :appl-args nil -1))))))
  (:method ((te1 term+abstr) (te2 term+abstr) bdb assocs actass substs termtyp prevars fargs)
           (declare (ignore termtyp))
	   (cond ((term~ssterm-free te1)
		  (multiple-value-bind (succ subst)
		      (bind~with-bindings
		       (;(term~alpha-equal te1 te2 :additional-bind bdb)))
			(term=alpha-equal te1 te2 bdb)))
		    (if succ
			(progn
			  (subst~bind-substitution subst)
			  (ssterm=substs-add-comps subst substs :destructive t)
			  substs)
		      nil)))
		 ((and actass (data~equal (data~appl-function (data~abstr-range te2))
					  actass))
		  (let* ((new-sub1
			  (multiple-value-bind (subst1) 
			      (bind~with-bindings
			       ((ssterm=alpha-equal (append (data~abstr-domain te1) (list (data~abstr-range te1)))
						    (append
						     (ssterm=ae-list-prepare
						      (data~abstr-domain te2)
						      (- (length (data~abstr-domain te1)) 1)
						      (length (data~abstr-domain te2))
						      prevars)
						     (list (data~abstr-range te2)))
						    (append (data~abstr-domain te1) bdb)
						    assocs nil (list (subst~create nil nil))
						    :quant prevars -1))
			       :insert nil :build-subst nil)
			    subst1))
			 (new-sub2 (ssterm=alpha-equal te1
						       (first (data~appl-arguments (data~abstr-range te2)))
						       (append (data~abstr-domain te1) bdb)
						       assocs actass
						       (list (subst~create nil nil))
						       :quant
						       (append prevars (data~abstr-domain te2)) -1))
			 (subs3 (if new-sub1 (ssterm=substs-product substs new-sub1)
				  nil))
			 (subs4 (if new-sub2 (ssterm=substs-product substs new-sub2)
				  nil)))
		    (when (or new-sub1 new-sub2) (append subs3 subs4))))
		 (actass
		  (let ((new-subs (ssterm=alpha-equal
				   (append (data~abstr-domain te1) (list (data~abstr-range te1)))
				   (append (ssterm=ae-list-prepare
					    (data~abstr-domain te2)
					    (- (length (data~abstr-domain te1)) 1)
					    (length (data~abstr-domain te2))
					    prevars)
					   (list (data~abstr-range te2)))
				   (append (data~abstr-domain te1) bdb) assocs nil
				   (list (subst~create nil nil)) :quant nil -1)))
		    (if new-subs
			(ssterm=substs-product substs new-subs)
		      nil)))
		 ((and (ssterm~var-p (car (last (data~abstr-domain te1))))
		       (eq (ssterm~sort (car (last (data~abstr-domain te1))))
			   (ssterm~get-ssort 'varlist)))
		  (ssterm=alpha-equal (append (data~abstr-domain te1) (list (data~abstr-range te1)))
				      (append
				       (ssterm=ae-list-prepare
					(data~abstr-domain te2)
					(- (length (data~abstr-domain te1)) 1)
					(length (data~abstr-domain te2)))
				       (list (data~abstr-range te2)))
				      (append (data~abstr-domain te1) bdb)
				      assocs actass substs :abstr prevars -1))
		 ((= (length (data~abstr-domain te1)) (length (data~abstr-domain te2)))
		  (ssterm=alpha-equal (append (data~abstr-domain te1) (list (data~abstr-range te1)))
				      (append (data~abstr-domain te2) (list (data~abstr-range te2)))
				      (append (data~abstr-domain te1) bdb)
				      assocs actass substs :abstr prevars -1))))                         
  (:method ((te1 list) (te2 list) bdb assocs actass substs termtyp prevars fargs)
	   (if te1
               (let ((mt (first te1)))
		   (if (and (typep mt 'term+syn-sorted-var)
			    (find (ssterm~sort mt) (list (ssterm~get-ssort 'termlist)
							 (ssterm~get-ssort 'varlist)))
			    (eq termtyp :appl-args))
		       (if (rest te1)
			   (error ";;;ssterm=alpha-equal: Not considered case!.")
			 (ssterm=alpha-equal mt te2 bdb assocs actass substs
					     termtyp prevars -1))
		     (let ((res
			    (ssterm=alpha-equal mt (first te2) bdb assocs
						actass substs termtyp prevars -1)))
		       (if (and res (rest te1))
			   (ssterm=alpha-equal (rest te1) (rest te2) bdb
					       assocs actass res termtyp prevars -1)
			 res))))))
  (:method ((te1 term+syn-sorted-var) (te2 t) bdb assocs actass substs termtyp prevars fargs)
           (let ((ssvar-sort (keim~name (ssterm~sort te1))))
             (cond ((bind~binding te1)
                    (ssterm=alpha-equal (bind~binding te1) te2 bdb assocs actass
                                        substs termtyp prevars fargs))
                   ((or
                     (and (listp te2) (eq ssvar-sort 'varlist) (every 'term~variable-p te2))
                     (and (listp te2) (eq ssvar-sort 'termlist)(every 'term~p te2)))
		    
		    (ssterm=substs-add-comp te1 te2 substs :destructive t)
		    (bind~bind te1 te2)
                    substs)
                   ((or (and (term~p te2)            (eq ssvar-sort 'term))
                        (and (eq ssvar-sort 'var)    (term~variable-p te2))
                        (and (eq ssvar-sort 'metavar)(typep te2 'meta+variable))
                        (and (eq ssvar-sort 'const)  (term~constant-p te2)))
                    (let ((te1-type (data~annotation te1)))
                      (if (find (type~list) (type~symbols te1-type))
                          (progn
                            (ssterm=substs-add-comp te1 te2 substs :destructive t)
                            (bind~bind te1 te2)
                            substs)
                       (when t
			 (ssterm=substs-add-comp te1 te2 substs :destructive t)
			 (bind~bind te1 te2)
			 substs))))
                   (t
		    nil))))
  (:method ((te1 term+term) (te2 term+term) bdb assocs actass substs termtyp
            prevars fargs)
           (declare (ignore bdb assocs actass substs termtyp prevars fargs))
           nil
           )
  ;; TWO NEW CASES ADDED BY AMEIER:
  (:method ((te1 term+term) (te2 null) bdb assocs actass substs termtyp
            prevars fargs)
           (declare (ignore bdb assocs actass substs termtyp prevars fargs))
           nil
           )
  (:method ((te1 null) (te2 term+term) bdb assocs actass substs termtyp
            prevars fargs)
           (declare (ignore bdb assocs actass substs termtyp prevars fargs))
           nil
           )
  (:method ((te1 data+struct) (te2 data+struct) bdb assocs actass substs termtyp
            prevars fargs)
           (declare (ignore bdb assocs actass substs termtyp prevars fargs))
           (error "ssterm=alpha-match: This Function is not defined on data+structure, only on terms.")))

;; post-syntax

(defmethod post~read-object ((vars list)
			     (env env+environment) 
			     (indicator (eql :ss-variables)))
  (mapcar #'(lambda (var)
	      (post~read-object var env :ss-variable))
	  vars))
  
(defmethod post~read-object ((var cons) (env env+environment) 
			    (indicator (eql :ss-variable)))
  (let* ((type (data~n-normalize (post~read-object (second var) env :existing-type) :destructive t))
	 (sort (ssterm~get-ssort (third var)))
	 (lookup (env~lookup-object (first var) env)))
    (if lookup
	(cond ((typep lookup 'term+syn-sorted-var)
	       (let* ((lookup-type (term~type lookup))
		      (lookup-sort (ssterm~sort lookup)))
		 (if (and (data~equal lookup-type type)
			  (eq lookup-sort sort))
		     lookup
		   (error "~A exists already in the environment with other sort or type." (first var)))))
	      ((and (data~schema-p lookup)
		    (typep (data~schema-range lookup) 'term+syn-sorted-var))
	       (let* ((lookup-type (term~type lookup))
		      (lookup-sort (ssterm~sort (data~schema-range lookup))))
		 (if (and (data~equal lookup-type type)
			  (eq lookup-sort sort))
		     lookup
		   (error "~A exists already in the environment with other sort or type." (first var)))))
	      (t
	       (error "~A exists already in the environment and is not of type term+syn-sorted-var" (first var))))
      (let* ((real-type (if (type~schema-p type)
			    (data~schema-range type)
			  type))
	     (type-kappas (if (type~schema-p type)
			      (data~schema-domain type)
			    nil))
	     (term-object (ssterm~var-create (first var) sort type))
	     (term-object-ii (if (type~schema-p type)
				 (term~schema-create term-object :kappas type-kappas)
			       term-object)))
	(env~enter (first var) term-object-ii env)
	term-object-ii))))


