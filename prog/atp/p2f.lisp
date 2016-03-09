;;; -*- syntax: common-lisp; package: OMEGA; base: 10; mode: LISP -*-
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

;; Die Veraenderung in Bezug auf p2f.lisp liegt darin, dass hier sobald eine Variable als Functor auftaucht sofort die max-order eins
;; HOEHER als die Ordnung der Variable gesetzt wird (wenn sie nicht sowieso schon hoeher war).
;; Dies verursacht eine Uebersetzung der Variablen in appl-Terme
;; Zum Vergleich mit p2f.lisp betrachte z.b. ~ameier/examples/hines-ex/ex-ii.post
;;
;; Leider gibt es im MOment noch zwei eklatante Probleme:
;; 1. Die definitions Expansion laeuft schief!
;;    Sie ~ameier/examples/hines-ex/ex-ii.post : doppelt auftretende Variablen muessten renamed werden !
;;
;; -> Kann ich heute nicht mehr nachvollziehen, AMEIER=MISTER-KEIM-3
;;
;; 2. Durch die in keim benutzte verzwickte Behandlung von Polymorphismus ist es notwendig (um transcl hinzubekommen), dass die
;;    keys in der hash-table die Namen der Zeichen sind (zwei verschieden getypte Dingens einer Konstanten sindnicht mehr eq!).
;;    Dies fuehrt aber zu Shit, falls Variablen oefter benuetzt werden.
;;
;; -> SOLLTE GEHEN, DA HEUTE KONSTANTEN AN IHR ORIGIN GEBUNDEN WERDEN, AMEIER=MISTER-KEIM-3 
;;
;; Sind diese Sachen erst repariert, besteht begruendete Hoffnung, dass alle Examples aus dem Verzeichnis 
;; ~ameier/examples/hines-ex/ex-ii.post laufen
;;
;; Momentan laufen mit diesem File: transcl?KEIM-3 sowie ex-i.post, ex-ii.post und ex-iv.post
;;
;;
;; ameier




;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                   Module                                 ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;

(in-package :omega)



(mod~defmod P2F 
            :uses (data env keim logic res subst term termix type)
            :documentation "Translating higher-order res-proofs to first-order."
            :exports (
                      
                      p2f~translate
                      
                      p2f*codomain
                      p2f*constant-counter
                      p2f*domain
                      p2f*orders-arities
                      p2f*orders-arities-hash
                      p2f*orders-arities-max))





(defvar p2f*constant-counter 0)

(defvar p2f*domain nil)

(defvar p2f*codomain nil)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                              Orders and Arities                          ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;

(defstruct p2f*orders-arities  
  (hash (make-hash-table))
  (max 0))

(defgeneric p2f=type-order (type)
  (declare (edited  "30-SEP-1996")
	   (authors Hess AMEIER)
	   (input   "A type in its n-normal-form.")
	   (effect  "None.")
	   (value   "The order of the type."))
  (:method ((type type+primitive))
	   0)
  (:method ((type type+complex))
	   (+ 1 (multiple-value-call #'max
				     (values-list
				      (cons (p2f=type-order
					     (data~n-range type))
					    (mapcar #'p2f=type-order
						    (data~n-domain type))))))))

		
;; die fuer Konstanten und Variablen in die Hashtable eingetragenen Paare bestehen immer aus:
;;   Ordnung des TYpes (bei Konstanten des Types der origin-constant, bei Variablen des direkt vorkommenden Types)
;; + Minimale vorkommende Stelligkeit 

(defgeneric p2f=set-orders-arities (term orders-arities &key (arity 0) (functor nil))
  (declare (edited  "30-SEP-1996")
	   (authors Hess AMEIER)
	   (input   "A term and a struct with slot hash and max.")
	   (effect  "Enters, if not already in hash-table, all constants and"
		    "variables of term as keys with coresponding orders and"
		    "arities as values in hash-table.")
	   (value   nil))
  (:method ((term term+constant) orders-arities &key (arity 0) (functor nil))
	   ;; eine Konstante kann durchaus in einem Problem mit verschiedenen Typen auftreten (z.b kappa aa = (o aa aa)),
	   ;; gehe also immer ueber die origin der Konstante
	   (declare (ignore functor))
	   (let* ((orig-const (data~constant-origin term)))
	     (if (not (gethash orig-const (p2f*orders-arities-hash orders-arities)))
		 ;; constante hat noch keinen Eintrag in hash-table -> berechne zuerst Ordnung des Types,
		 ;; update eventuell die maximale Ordnung, trage dann ein paar aus Ordung und vorkommender Stelligkeit
		 ;; in die Hashtable ein
		 (let ((order (p2f=type-order (term~type orig-const))))
		   (when (> order (p2f*orders-arities-max orders-arities))
		     (setf (p2f*orders-arities-max orders-arities)
			   order))
		   (setf (gethash
			  orig-const
			  (p2f*orders-arities-hash orders-arities))
			 (cons order arity)))
	       ;; constante het bereits einen Eintrag in der Hashtable ->
	       ;; falls die nun vorliegende Arity kleiner ist als die bisher eingetragene ->
	       ;; trage die neue ein.
	       (when (< arity (cdr (gethash
				    orig-const
				    (p2f*orders-arities-hash orders-arities))))
		 (setf (gethash orig-const (p2f*orders-arities-hash orders-arities))
		       (cons (car (gethash
				   orig-const
				   (p2f*orders-arities-hash orders-arities)))
			     arity))))))
  (:method ((term term+variable) orders-arities &key (arity 0) (functor nil))
	   ;; eine Variable tritt in einem Problem nur mit einem Typ auf !
	   ;; aber: FALLS VARIABLE FUNCTOR IST MUSS TERM UMGESCHRIEBEN WERDEN
	   ;;       => alle Terme dieser Ordnung sollten umgeschrieben werden
	   ;;       => order eins hoeher falls Variable als functor gebraucht !
	   (cond ((not (gethash term (p2f*orders-arities-hash orders-arities)))
		  ;; variable hat noch keinen Eintrag in hash-table -> berechne zuerst Ordnung des Types,
		  ;; update eventuell die maximale Ordnung, trage dann ein paar aus Ordung und vorkommender Stelligkeit
		  ;; in die Hashtable ein
		  (let* ((order (p2f=type-order (term~type term)))
			 (add-order (if functor
					(+ 1 order)
				      order)))
		    (when (> add-order (p2f*orders-arities-max orders-arities))
		      (setf (p2f*orders-arities-max orders-arities) add-order))
		    (setf (gethash
			   term
			   (p2f*orders-arities-hash orders-arities))
			  (cons order arity))))
		 ((and functor 
		       (< arity (cdr (gethash
				      term
				      (p2f*orders-arities-hash orders-arities)))))
		  ;; variable hat bereits einen Eintrag in der Hashtable und ist als functor benutzt
		  ;; -> update mit order +1
		  ;; falls die nun vorliegende Arity kleiner ist als die bisher eingetragene ->
		  ;; trage die neue ein.
		  (let* ((order (+ 1 (p2f=type-order (term~type term)))))
		    (when (> order (p2f*orders-arities-max orders-arities))
		      (setf (p2f*orders-arities-max orders-arities) order))
		    (when (< arity (cdr (gethash
					 term
					 (p2f*orders-arities-hash orders-arities))))
		      (setf (gethash term (p2f*orders-arities-hash orders-arities))
			    (cons (car (gethash
					term
					(p2f*orders-arities-hash orders-arities)))
				  arity)))))
		 (t
		  ;; variable hat bereits einen Eintrag in der Hashtable ->
		  ;; falls die nun vorliegende Arity kleiner ist als die bisher eingetragene ->
		  ;; trage die neue ein.
		  (when (< arity (cdr (gethash
				       term
				       (p2f*orders-arities-hash orders-arities))))
		    (setf (gethash term (p2f*orders-arities-hash orders-arities))
			  (cons (car (gethash
				      term
				      (p2f*orders-arities-hash orders-arities)))
				arity))))))
  (:method ((term term+appl) orders-arities &key (arity 0) (functor nil))
	   (declare (ignore arity))
	   (declare (ignore functor))
	   (let ((func (data~appl-function term))
		 (args (data~appl-arguments term)))
	     (dolist (subterm args)
	       (p2f=set-orders-arities subterm orders-arities))
	     (when (not (or (logic~universal-quantification-p term)
			    (logic~existential-quantification-p term)
			    (logic~negation-p term)
			    (logic~conjunction-p term)
			    (logic~disjunction-p term)
			    (logic~equivalence-p term)
			    (logic~implication-p term)))
	       (p2f=set-orders-arities func orders-arities
				       :arity (length args)
				       :functor 't))))
  (:method ((term term+abstr) orders-arities &key (arity 0) (functor nil))
	   (declare (ignore arity))
	   (declare (ignore functor))
	   (let ((vars (data~abstr-domain term))
		 (scope (data~abstr-range term)))
	     
	     (p2f=set-orders-arities scope orders-arities)))
  (:method ((term term+schema) orders-arities &key (arity 0) (functor nil))
	   (declare (ignore arity))
	   (declare (ignore functor))

	   (p2f=set-orders-arities (data~schema-range term) orders-arities)))


(defun p2f=all-orders-arities (terms &optional (orders-arities
						(make-p2f*orders-arities)))
  (declare (edited  "01-OCT-1996")
	   (authors Hess)
	   (input   "A list of terms.")
	   (effect  )
	   (value   "Hash-table with pairs (sym|order) and maximum order."))
  (progn (dolist (x terms) (p2f=set-orders-arities x orders-arities))
	 orders-arities))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                 New Symbols                              ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;

(defun p2f=symbol-from-types (type1 type2)
  (declare (edited  "01-JUL-1996")
	   (authors Hess)
	   (input   "Two types.")
	   (effect  "None.")
	   (value   "A symbol representing the type's structure."))
  (intern (string-upcase (concatenate 'string "at_"
				      (p2f=symbol-rec type1)
                                      "_"
				      (p2f=symbol-rec type2)))))

(defun p2f=symbol-rec (type &optional (stream nil))
  (declare (edited  "01-JUL-1996")
	   (authors Hess)
	   (input   "")
	   (effect  "")
	   (value   ""))
  (if (type~primitive-p type)
      (format stream "~S" type)
    (format stream "~{~A~}~S"
	    (mapcar #'(lambda (domain)
                        (format nil "~A_to_" (p2f=symbol-rec domain)))
                    (data~n-domain type))
	    (data~n-range type))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                   Main                                   ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;

(defgeneric p2f=translate-term (term orders env)
  (declare (edited  "01-OCT-1996")
	   (authors Hess)
	   (input   "A probably higher-order term, a struct"
		    "and an environment.")
	   (effect  "Enters if needed the new applications in"
		    "the environment.")
	   (value   "The translated first-order term."))
  (:method ((term term+constant) orders env)
	   (declare (ignore env orders))
	   term)

  (:method ((term term+variable) orders env)
	   (declare (ignore env orders))
	   term)
	       
  (:method ((term term+appl) orders env)

	   (let* ((func (data~appl-function term))
		  (type (term~type func))
		  (args (data~appl-arguments term))
		  (hash-term (if (term~variable-p func)
				 func
			       (data~constant-origin func))))
	     
;;;             (format t "every: ~A ~A" func (gethash hash-term (p2f*orders-arities-hash orders)))
	     
	     (cond ((or (logic~universal-quantification-p term)
			(logic~existential-quantification-p term)
			(logic~negation-p term)
			(logic~conjunction-p term)
			(logic~disjunction-p term)
			(logic~equivalence-p term)
			(logic~implication-p term))
		    ;; standart case

		    ;; (format t "~% The normal logical connectives case")
		    (term~appl-create func
				      (mapcar #'(lambda (x)
						  (p2f=translate-term x orders env))
					      args)))
		   ((string= (if (stringp (keim~name func))
				 (keim~name func)
			       (string (keim~name func)))
			     "=")
		    
		    ;; Is the application symbol an equation -> no replacement of the =, because otherway otter
		    ;; get a symbol, it can't interpret as equality and so can't use for paramodulation
		    ;; perhaps this part should be replaced by Leibnitz-equality or something like that

		    ;;(format t "~% = Special Part")
		    (term~appl-create func
				      (list (p2f=translate-term (first args) orders env)
					    (p2f=translate-term (second args) orders env))))
		   ((and (not (term~variable-p func))
			 (< (cdr (gethash hash-term (p2f*orders-arities-hash orders)))
			    (length args)))
		    ;; die Behandlung von Variablen als Funktoren geschieht im default Fall (order < max-order)
		    
		    ;;(format t "~% Too many args and no function part")
		    ;; (error "jsd")
		    (let* ((min-arity (cdr (gethash
					    hash-term
					    (p2f*orders-arities-hash orders))))
			   (min-args (butlast args (- (length args) min-arity)))
			   (inner (p2f=translate-term
				   (if (= min-arity 0)
				       func
				     (term~appl-create func min-args))
				   orders env)))

		      (term~appl-create (term~create-primitive-in-environment
					 (p2f=symbol-from-types (term~type inner)
								(term~type term))
					 (type~abstract
					  (data~n-range type)
					  (cons (term~type inner)
						(nthcdr
						 min-arity
						 (data~n-domain type))))
					 'term+constant
					 env)
					(cons inner
					      (mapcar
					       #'(lambda (x)
						   (p2f=translate-term x orders env))
					       (nthcdr min-arity args))))))
		   ((= (car (gethash hash-term (p2f*orders-arities-hash orders)))
		       (p2f*orders-arities-max orders))

		    ;; (format t "Order = max-order")
		    (term~appl-create (p2f=translate-term func orders env)
				      (mapcar
				       #'(lambda (x)
					   (p2f=translate-term x orders env))
				       args)))
		   (t

		    ;; (format t "Order lesser than max-order")
		    ;; (error "asd")
		    (term~appl-create (term~create-primitive-in-environment
				       (p2f=symbol-from-types type (term~type term))
				       (type~abstract
					(data~n-range type)
					(cons type (data~n-domain type)))
				       'term+constant
				       env)
				      (cons (p2f=translate-term func orders env)
					    (mapcar
					     #'(lambda (x)
					    (p2f=translate-term x orders env))
					     args)))))))
  (:method ((term term+abstr) orders env)
	   (let ((vars (data~abstr-domain term))
		 (scope (data~abstr-range term)))
	     (term~abstr-create vars
				(p2f=translate-term scope orders env))))
  (:method ((term term+schema) orders env)
	   (let* ((vars (data~schema-domain term))
		  (range (data~schema-range term)))

	     (term~schema-create (p2f=translate-term range orders env)
				 :kappas vars))))

(defun p2f~translate (res-proof)
  (declare (edited  "08-JUL-1996")
	   (authors Hess AMEIER)
	   (input   "A probably higher-order resolution proof with all its formulas"
		    "in n-normal-form. A keyword free-variables, to deceide"
		    "whether existing free variables should be interpreted"
		    "as forall-quantified (forall, also default) or"
		    "exists-quantified (exists).")
	   (effect  "The conclusion and the assumptions of the resolution proof"
		    "are translated into f.o. logic.")
	   (value   "The changed resolution proof."))
  
  ;; (omega~message "Translating from higher- to first-order logic.")

  (let* ((env (res~proof-environment res-proof))	 
	 (all-terms (cons (termix~term (res~proof-conclusion res-proof))
			  (mapcar #'termix~term (res~proof-assumptions res-proof))))
	 (all-orders-arities (p2f=all-orders-arities all-terms)))

    (dolist (x (res~proof-assumptions res-proof))
      (setf (termix~term x)
	    (p2f=translate-term
	     (termix~term x)
	     all-orders-arities
	     env)))
    (setf (termix~term (res~proof-conclusion res-proof))
	  (p2f=translate-term
	   (termix~term (res~proof-conclusion res-proof))
	   all-orders-arities
	   env))
    (fresh-line)
    (p2f=replace-free-variables! res-proof env)
    res-proof))


(defun p2f=replace-free-variables! (res-proof env)
  (declare (edited  "05-MAR-1998")
	   (authors Ameier)
	   (input   "A resolution proof and an environment.")
	   (effect  "All free-variables that occur in the conclusion and the assumptions"
		    "of the resolution proof are replaced by new constants. These new constants"
		    "are added to the environment of the resolution proof."
		    "Remark: The constants are replaced back by the according old variables"
		    "        in the modul f2p.")
	   (value   "Undefined."))
  (let* ((nodes (cons (res~proof-conclusion res-proof) (res~proof-assumptions res-proof)))
	 (free-variables (remove-duplicates (apply 'append (mapcar #'data~free-variables nodes)))))
					       
    (do* ((rest-free-variables free-variables (rest rest-free-variables))
	  (domain nil)
	  (codomain nil))
	((null rest-free-variables)
	 (progn
	   (setq p2f*domain domain)
	   (setq p2f*codomain codomain)
	   (mapcar #'(lambda (node)
		       (setf (termix~term node)
			     (data~replace-structs (termix~term node)
						   domain
						   codomain
						   :downto '(data+primitive))))
		   nodes)))
      
      (let* ((head-var (first rest-free-variables))
	     (type (term~type head-var))
	     (new-const (term~generate-term-primitive-with-new-name 'const- type 'term+constant env)))
	
	;; update replacement-domain
	(setq domain (cons head-var domain)) 
	
	;; update replacement-codomain
	(setq codomain (cons new-const codomain))))))

	
