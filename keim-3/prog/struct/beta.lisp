; -*- syntax: common-lisp; package: keim; base: 10; mode: lisp -*-
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


;; In diesen Modul sind einige Funktionen des frueheren hop-Moduls
;; eingegangen: Daraus ergab sich die Notwendigkeit folgende Umbenennungen
;; durchzufuehren:

;; hop~beta-contract         --->   beta~contract
;;
;; hop~beta-expand           --->   beta~expand
;;
;; hop~beat-redex-p          --->   beta~redex-p
;; 
;; hop~beta-normform-p       --->   beta~normform-p
;;
;; hop~contains-beta-redex-p --->   beta~contains-redex-p



(mod~defmod beta :uses (keim mod tree data)
            :exports (
	              beta~head-normalize
		      beta~normalize
		      beta~head-normalize-with-args
		      beta~head-normalize-with-args-and-bindings
		      beta~drop-abstractions
		      beta~compute-with-local-bindings

		      beta~matrix
		      beta~binder
		      
		      beta~redex-p
		      beta~contract
		      beta~expand
		      beta~normform-p
		      beta~contains-redex-p
		      
		      )
            )


#{\subsection{Head beta reduction}

Let $A=(\lambda X_1,\ldots, X_k ((\lambda Y_1,\ldots,Y_l.M) N_1\ldots N_n))$ be data structure, 
then we call the beta reduction to 
\begin{itemize}
\item $(\lambda X_1,\ldots, X_k ([N_1/Y_1],\ldots,[N_l/Y_l]M) N_{n-l}\ldots N_n))$ if $l\leq n$ and to 
\item $(\lambda X_1,\ldots, X_k ((\lambda Y_{l-n},\ldots,Y_l.[N_1/Y_1],\ldots[N_n/Y_n]M)))$ if $n\leq l$ 
\end{itemize}
a head reduction step. A head normal form is a data structure from which no head reduction steps are possible.
#}
 

(defgeneric beta~normalize (datum)
  (declare (edited  "25-MAR-1996" "21-MAR-1996 16:38")
	   (authors kk)
	   (input   "A datum.")
	   (effect  "None.")
	   (value   "A complete beta normalization."))
  (:method ((term data+variable))
	   (if (bind~binding term)
	       (beta~normalize (bind~binding term))
	     term))
  (:method ((term data+constant))
           term)
  (:method ((term data+appl))
	   (let* ((hnf-copi (data~alpha-copy term nil)) 
		  (hnf (beta~head-normalize hnf-copi)))    ;;; klappt so nicht, chris
	     (if (data~appl-p hnf)
		 (data~appl-create (beta~normalize (data~appl-function hnf))
				   (mapcar #'beta~normalize (data~appl-arguments hnf)))
	       (beta~normalize hnf))))
  (:method ((term data+abstr))
           (let* ((binder (data~abstr-domain term))
		  (scope (data~abstr-range term))
		  (new-scope (beta~normalize scope)))
             (data~abstr-create binder
				new-scope)))
  (:method ((term data+schema))
	   (let* ((binder (data~schema-domain term))
		  (scope (data~schema-range term))
		  (new-scope (beta~normalize scope)))
             (data~schema-create new-scope binder))))




(defmethod beta~head-normalize (data)
  (declare (edited  "3-4-1995")
	   (authors kohlhase)
	   (input   "A data")
	   (effect  "{\vb DATA} is destructively beta-normalized using"
		    "and respecting the binding situation.")
	   (value   "The normalized data."))
  (beta~head-normalize-with-args nil data nil)) 

(defmethod beta~head-normalize ((data data+constant))
  (declare (edited  "3-4-1995")
	   (authors kohlhase)
	   (input   "A data")
	   (effect  "{\vb DATA} is destructively beta-normalized using"
		    "and respecting the binding situation.")
	   (value   "The normalized data."))
  data)

(defmethod beta~head-normalize ((data data+variable))
  (declare (edited  "3-4-1995")
	   (authors kohlhase)
	   (input   "A data")
	   (effect  "{\vb DATA} is destructively beta-normalized using"
		    "and respecting the binding situation.")
	   (value   "The normalized data."))
  (if (bind~binding data)
      (beta~head-normalize (bind~binding data))
    data))

(defmethod beta~head-normalize ((data data+appl))
  (declare (edited  "3-4-1995")
	   (authors kohlhase)
	   (input   "A data")
	   (effect  "{\vb DATA} is destructively beta-normalized using"
		    "and respecting the binding situation.")
	   (value   "The normalized data."))
  (let ((function (data~appl-function data)))
        (if (or (data~constant-p function)
		(and (data~variable-p function)
		     (not (bind~binding function))))
	    data
	  ;; else
	  (beta~head-normalize-with-args nil data nil))))   ;; HHGG bvars mit nil initialisiert

;; (defparameter *global-beta-counter* 0) ;; just for me -kk-

(defun beta~head-normalize-with-args (bin data args)   ;; HHGG nur bvars eingefuegt
  (declare (edited  "3-4-1995")
	   (authors GKLEIN kohlhase)
	   (input   "A data and a list of arguments")
	   (effect  "the application of {\vb DATA} to {\vb ARGS} is destructively beta-normalized"
		    "using and respecting the binding situation.")
	   (value   "The normalized data."))
  ;; (incf *global-beta-counter*)
  (values (bind~with-bindings
	   ((let ((nterm (beta~head-normalize-with-args-and-bindings
                          bin data args)))
	      (bind~insert-bindings! nterm :local T)))
	   :insert nil)))

;; Prinzipliell kann es in dieser Funktion passieren, dass bei der Reduktion  polymorphe
;; strukturen in den Argumenten (args) oder sogar in den domainen (bin) vorkommen.
;; in diesem Fall muessen die entsprechenden Faelle diese Tatsachen entdecken, die
;; Kappagebundenen Variablen in den also-bind-slot schieben, und mit den eigentlichen
;; datas der polymnorphen  Strukturen Weiterechnen.

;; BEMERKUNG:
;; 1.) Eventuell ist obige Bemerkung hinfaellig !
;; 2.) Ich (AM) habe im data+constant und data+variable Fall Aenderungen vorgenommen, so dass keine dort keine
;;     Schemas mehr erzeugt werden !
;;     Die erzeugung von Schemas ergab immer dann Probleme, wenn term~appl- oder term~abstr-creates gemacht wurden bei denen die
;;     Terme irgendwelche Typ-variablen enthielten. Dies fuehrte naemlich stets dazu, dass die entstehenden applikationen bzw.
;;     Abstraktionen geschemata-Typen erhielten, was schlicht und ergreifend falsch ist !
;;     Die andere Moeglichkeit waere gewesen im term-modul abfragen einzufuegen, dass statt den typ-schemas nur die ranges benutzt
;;     werden. Solltn mit dieser Loesung hier Probleme entstehen schlage ich vor genau darauf zurueckzugreifen.
;;    Gruss AM


(data~defgeneric beta~head-normalize-with-args-and-bindings (bin (data) args &optional also-bind)
  (declare (edited  "21-APR-1995" "3-4-1995")
	   (authors Fehrer kohlhase)
	   (input   "A data structure and lists of bindings , bound-variables and arguments")
	   (effect  "The application of {\vb DATA} to {\vb ARGS} is destructively beta-normalized"
		    "using and respecting the binding situation.")
	   (value   "The normalized data."))
  (:method (bin (data data+constant) args &optional also-bind)
	   (let* ((scope (if args
			     (data~appl-create data args)
			   data))
		  (datum (if bin
			     (data~abstr-create bin scope) 
			   scope)))

	     ;; ohne Kappas ?
	     ;; einfach nur:
	     datum))
	     	     
	     ;;(if also-bind
	     ;;	 (data~schema-create datum also-bind)
	     ;;datum)))
  (:method (bin (data data+schema) args &optional also-bind)
	   (beta~head-normalize-with-args-and-bindings
	    bin (data~schema-range data) args (remove-duplicates
					       (append (data~schema-domain data)
						       also-bind))))
  (:method (bin (data data+variable) args &optional also-bind)
	   (if (bind~binding data)
	       (beta~head-normalize-with-args-and-bindings bin (bind~binding data) args)
	     (let* ((scope (if args
			       (data~appl-create data args)
			     data))
		    (datum (if bin
			       (data~abstr-create bin scope) 
			     scope)))
	       ;;(if also-bind
	       ;;	   (data~schema-create datum also-bind)
	       ;; datum))))
	       
	       datum)))
  (:method (bin (data data+appl) args  &optional also-bind)
	   (beta~head-normalize-with-args-and-bindings 
	         bin
		 (data~appl-n-function data)
		 (if (data~appl-n-arguments data)
		     (append (data~appl-n-arguments data) args)
		   args)))
  (:method (bin (data data+abstr) args  &optional also-bind)
	   (if args
	       (let* ((data+also (data~alpha-copy (list data also-bind) nil))
		      (cdata    (car data+also))
		      (also-bind1 (second data+also))
		      ;;(cdata (data~copy data :downto (list 'data+primitive)))
		      ;; Die data~alpha-copy Version war die in 3.0 am besten funktionierende
		      ;; (cdata (data~alpha-copy data nil)) 
		      (binder (data~abstr-n-domain cdata))
		      (minl (min (length binder) (length args)))
		      (prebin (subseq binder 0 minl))
		      (preargs (subseq args 0 minl))
		      (binding-vars (if (typep data 'type+type)
					also-bind1
				      (append prebin also-bind1))))
		 
		 (unless
		     (data~alpha-equal prebin preargs nil nil binding-vars)
		   (error "error: The head reduction in ~A is blocked. ~%" cdata))
		 
		 (beta~head-normalize-with-args-and-bindings 
		  bin
		  (beta~drop-abstractions cdata minl)
		  (subseq args minl)
		  also-bind1))
	     ;; else
	     (beta~head-normalize-with-args-and-bindings
	      (if bin
		  (append bin (data~abstr-n-domain data))
		(data~abstr-n-domain data))
	      (data~abstr-n-range data) 
	      nil
	      also-bind))))

(data~defgeneric beta~drop-abstractions ((data) num)
  (declare (edited  "3-4-1995")
	   (authors kohlhase)
	   (input   "A data structure and a nuber")
	   (effect  "Error, iff the length of the n-domain of {\vb DATA} is less than {\vb NUM}")
	   (value   "If {\vb DATA} is an abstraction, such that the n-domain"
		    "has length greater then {\vb NUM}, then a new abstraction,"
		    "where the first {\vb NUM} elements of the binder have been dropped,"
		    "else the n-range of {\vb DATA}."))
  (:method ((data data+struct) num)
	   (if (eq num 0)
	       data
	     (keim~error "Cannot drop ~A elements of the binder of ~A" num data)))
  (:method ((data data+abstr) num)
	   (let* ((binder (data~abstr-domain data))
		  (scope (data~abstr-range data))
		  (len (length binder)))
	     (cond ((< len num)        
		    (beta~drop-abstractions scope (- num len)))
		   ((equal num len)
		    scope)
		   ((< num len)
		    (data~abstr-create (subseq binder num) scope
				       ))))))  ;;HHGG

;(defmacro beta~compute-with-local-bindings (&body forms)
;  (declare (edited  "21-APR-1995" "3-4-1995")
;           (authors GKLEIN Fehrer kohlhase)
;           (input   "A body of forms")
;           (effect  "This macro causes the computation to proceed using the binding mechanism.")
;           (value   "The value of the last form as computed within the binding context."))
;; `(unwind-protect
;;               (let ((val (progn (top~new-binding-list!)
;;                                 ,@forms)))
;;                 (if (data~struct-p val) (top~insert-bindings! val)
;;                   val))
;;             (top~unbind-binding-list!))
;  `(let ((result nil))
;     (bind~with-bindings ((let ((val (progn ,@forms)))
;                            (if (data~struct-p val)
;                                (setf result (top~insert-bindings! val))
;                              (setf result val)))))
;     result))

#{\subsection{Head et al}

Let $A$ be a data structure in head normal form, then $A$ is an abstraction of the form
$(\lambda X_1,\ldots,X_k (h B_1\ldots B_n))$, where $h$ is a primitive data structure and $k,n\geq0$.
We call the list $(X_1\ldots X_k)$ of variables the {\bf binder}, the subformula $(h B_1\ldots B_n)$
the {\bf matrix} and the primitive $h$ the {\bf head} of $A$. For data structures that are nore head normal
forms we first head normalize to compute the binder, matrix, and head.#}

(defun beta~matrix (data) 
  (declare (edited  "27-JAN-1992 9:55")
	   (authors Kohlhase)
	   (input   "DATA is a data.")
	   (effect  "Depending on the destructivity, {\\vb DATA} is head normalized in the process.")
	   (value  #{\begin{enumerate}
		   \item 1. The matrix of {\vb DATA}
		   \item 2. The binder of {\vb DATA}
		   \item 3. The head of {\vb DATA}.
		   \end{enumerate}#}))
  (let* ((hnf (beta~head-normalize data))
	 (binder (when (data~abstr-p hnf)
		   (data~abstr-n-domain hnf)))
	 (matrix (if (data~abstr-p hnf)
		     (data~abstr-n-range hnf)
		   hnf))
	 (head (data~top matrix)))
    (values matrix binder head)))

(defun beta~binder (data)
  (declare (edited  "27-JAN-1992 9:55")
	   (authors Kohlhase)
	   (input   "DATA is a data.")
	   (effect  "Depending on the destructivity, {\\vb DATA} is head normalized in the process.")
	   (value  "1. The binder of {\vb DATA}"
		    "2. The matrix of {\vb DATA}"
		    "3. The head of {\vb DATA}."))
  (let* ((hnf (beta~head-normalize data))
	 (binder (when (data~abstr-p hnf)
		   (data~abstr-n-domain hnf)))
	 (matrix (if (data~abstr-p hnf)
		     (data~abstr-n-range hnf)
		   hnf))
	 (head (data~top matrix)))
    (values binder matrix head)))

(defun beta~head (data)
  (declare (edited  "27-JAN-1992 9:55")
	   (authors Kohlhase)
	   (input   "DATA is a data.")
	   (effect  "Depending on the destructivity, {\\vb DATA} is head normalized in the process.")
	   (value  "1. The head of {\vb DATA}"
		    "2. The binder of {\vb DATA}"
		    "3. The matrix of {\vb DATA}."))
  (let* ((hnf (beta~head-normalize data))
	 (binder (when (data~abstr-p hnf)
		   (data~abstr-n-domain hnf)))
	 (matrix (if (data~abstr-p hnf)
		     (data~abstr-n-range hnf)
		   hnf))
	 (head (data~top matrix)))
    (values head binder matrix)))
				       
  
#{\subsection{Beta normalization}



this needs treatment of structure and destructivity #}


(defgeneric data~beta-normalize (data)
  (declare (edited  "3-4-1995")
	   (authors kohlhase)
	   (input   "A data")
	   (effect  "{\\vb DATA} is destructively beta-normalized using and respecting the binding situation.")
	   (value   "The normalized data."))
  (:method ((data data+struct))
	   (multiple-value-bind (matrix binder top) (beta~matrix data)
	     (let* ((new-args (mapcar #'(lambda (arg)
					  (data~beta-normalize arg))
				      (data~args matrix)))
		    (new-matrix (if new-args
				    (data~appl-create top new-args)
				  top))
		    (new-data (if binder
				  (data~abstr-create binder new-matrix)
				new-matrix)))
	       new-data)))
  (:method ((subst subst+substitution))
	   (subst~create (subst~domain subst)
			 (mapcar #'data~beta-normalize (subst~codomain subst)))))


;;;;;;;;;;;;;; ===============================================

;;;;;;;;;;;;;; Additional beta-normalization related Functions
;;;;;;;;;;;;;;              retrieved from the modul HOP

;;;;;;;;;;;;;; ===============================================

(data~defgeneric beta~redex-p ((datum))
  (declare (edited  "20-AUG-1991 9:55" "11-MAR-1998")
	   (authors Kohlhase Gebhard)
	   (input   "A datum")
	   (effect  "None.")
	   (value   "T iff  Datum is of the form (([x].A)B)")
	   (example "([x] (P x) a) --> t"))
  (:method ((term data+constant))
	   nil)
  (:method ((term data+variable)) nil)
  (:method ((term data+abstr)) nil)
  (:method ((term data+appl))
	   (data~abstr-p (data~appl-function term))))


(defun beta~contract (term &key (pos (pos~empty)) destructive)
  (declare (edited  "4-Feb-1993 9:55")
	   (authors Kohlhase)
	   (input   "TERM is a term and POS is a valid position in TERM. POS defaults to the empty position.")
	   (effect  "None.")
	   (value   "If the subterm of TERM at POS is a beta-redex of the form (([x].A)B)"
		    " then a new term is returned where the subterm at POS is changed to [B/x]A,"
		    "otherwise TERM.")
	   (example "([X].(P X) C) --> (P C)"
		    "([X].(X C) P) --> (P C)"
		    "([X].[Y].(G X Y) C D) --> ([Y].(G C Y) D)"
		    "([X].[Y].(G X Y) C D)  (1) --> ([Y].(G C Y) D)"
		    "([X].[Y].(G X Y) C D)  (1 0) --> ([X].[Y].(G X Y) C D)"))
  (let ((atpos (data~struct-at-position term pos)))
    (if (beta~redex-p atpos)  
        (if (pos~empty-p pos)
	    (beta=contract atpos :destructive destructive)
	  (data~replace-at-position term pos (beta=contract atpos :destructive destructive)))
      term)))


(data~defgeneric beta=contract ((term) &key destructive)
  (declare (edited  "27-JAN-1992 9:55")
	   (authors Kohlhase)
	   (input   "TERM is a term.")
	   (effect  "None.")
	   (value   "If TERM is a beta-redex of the form (([x].A)B)"
		    " then a new term is [B/x]A, is returned, otherwise TERM."))
  (:method ((term data+constant) &key destructive)
	   (declare (ignore destructive))
	   term)
  (:method ((term data+variable) &key destructive)
	   (declare (ignore destructive))
	   term)
  (:method ((term data+abstr) &key destructive)
	   (declare (ignore destructive))
	   term))


(data~defgeneric beta~expand ((term) positions)
  (declare (edited  "31-Maerz-1998")
	   (authors KohlhaseChris)
	   (input   "A term and a list of positions")
	   (effect  "none")
	   (value   "The beta-expansion, lam x TERM' B, where B is the subterm"
		    "of TERM at position POS and TERM' is TERM, where B is"
		    "replaced by a bound variable x."))
  (:method ((term data+struct) positions)
	   (if (null positions)
	       term 
	     (let* ((subterms (mapcar #'(lambda (pos) (data~struct-at-position term pos))
				      positions))
		    (sub (if (every #'(lambda (s) (data~equal s (first subterms)))
				    (rest subterms))
			     (first subterms)
			   (error "cannot beta-abstract: subterms at positions (~A) differ in ~A"
				  positions term)))			    
		    (type (data~annotation sub))
		    (bvar (term~variable-create 'X type)))
					;(data~variable-create sub :name "X" type)))
	       (data~appl-create
		(data~abstr-create
		 bvar
		 (if (cdr positions)
		     (data~replace-at-positions term positions bvar)
		   (data~replace-at-position term (car positions) bvar)))
                (list sub))))))


(defun beta~normform-p (datum)
  (declare (edited  "12-MAR-1998" "27-JAN-1992 9:55")
	   (authors Gebhard Kohlhase)
	   (input   "A datum, a term or a type")
	   (effect  "None.")
	   (value   "T iff datum is in beta-normal form, i.e. datum does not contain beta-redices.")
	   (example "([X].(Q X) C) --> nil"
		    "[X].(F (F X)) --> t"))
  (not (beta~contains-redex-p datum)))


(data~defgeneric beta~contains-redex-p ((datum))
  (declare (edited  "12-MAR-1998" "27-JAN-1991 9:55")
	   (authors Gebhard Kohlhase)
	   (input   "A datum, is a term or a type.")
	   (effect  "None.")
	   (value   "T iff DATUM contains a beta-redex")
	   (example "([X].(Q X) C) --> t"
		    "(FORALL [X].(Q X)) --> nil"))
  (:method ((datum data+constant)) nil)
  (:method ((datum data+variable)) nil)
  (:method ((datum data+appl))
	   (let ((fun (data~appl-function datum)))
	     (if (data~abstr-p fun)
		 t
	      (some #'beta~contains-redex-p (cons fun (data~appl-arguments datum)))))))












