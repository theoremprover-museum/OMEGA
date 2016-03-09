;;; -*- Package: KEIM; Syntax: Common-lisp; Mode: LISP -*-
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

(in-package :keim)

;; Dieser Modul enthaelt die Funktionen aus dem frueheren hop-Modul. Allerdings hat es sich
;; ergeben, dass vor der Umstellung schon einige hop~-Funktioenen unabdingbar waren und
;; deshalb fruehzeitig in den - schon vorher existierenden - beta-Modul eingepasst worden
;; sind.

;; hop~beta-contract         --->   beta~contract
;;
;; hop~beta-expand           --->   beta~expand
;;
;; hop~beta-redex-p          --->   beta~redex-p
;; 
;; hop~beta-normform-p       --->   beta~normform-p
;;
;; hop~contains-beta-redex-p --->   beta~contains-redex-p

;; Die Idee, den nun neu entstehenden Lamda-Modul hierarchisch hinter dem beta-Modul
;; stehend einzuordnen, ist naheliegend.

;; Insgesamt sollten saemtliche Funktionen in ihrer ur-Wirkung erhatlen bleiben, und auch
;; die prinzipielle Funktionsweise der Funktioenen wird moeglicherweise uebernommen werden.

;; Moduldefinition des Alten Hop-moduls
;(mod~defmod hop :uses (abstr appl mod pos sym term top type poly)
;            :documentation "Higher-order functionality of terms."
;            :exports (
;                      hop~matrix                       --->    lam~matrix
;                      hop~binder                       --->    lam~binder
;                      hop~rename-bound-variables       --->    ersatzlos gestrichen 
;                      hop~rename-bound-variables-aux   --->    ersatzlos gestrichen
;                      hop~beta-redex-p                 --->    beta-Modul 
;                      hop~beta-normform-p              --->    beta-Modul
;                      hop~contains-beta-redex-p        --->    beta-Modul 
;                      hop~beta-contract                --->    beta-Modul 
;                      hop~rename-top-variable          --->    zZ ersatzlos gestrichen
;                      hop~beta-contract!               --->    beta-Modul 
;                      hop~beta-normform                --->    beta~normalize
;                      hop~beta-normform!               --->    zZ ersatzlos gestrichen???
;                      hop~eta-redex-p                  --->    lam~eta-redex-p               
;                      hop~eta-longform-p               --->    lam~eta-longform-p
;                      hop~eta-expand                   --->    lam~eta-expand
;                      hop~eta-expand-1                 --->    zZ ersatzlos gestrichen 
;                      hop~eta-longform                 --->    lam~eta-longform
;                      hop~eta-contract                 --->    lam~eta-contract
;                      hop~beta-equal-p                 --->    lam~beta-equal-p
;                      hop~eta-equal-p                  --->    lam~eta-equal-p
;                      hop~alpha-equal-p                --->    term~alpha-equal
;                                              durch    --->    lam~alpha-equal-p 
;                      hop~lambda-equal-p               --->    lam~equal-p
;		       hop~beta-expand                  --->    beta-Modul 
;                      )
;            )



(mod~defmod LAM 
            :uses (beta data mod term type)
            :documentation "Higher-order functionality of terms, completing the beta module."
            :exports (
                      lam~alpha-equal-p
                      lam~beta-equal-p
                      lam~binder
                      lam~equal-p
                      lam~eta-contract
                      lam~eta-equal-p
                      lam~eta-expand
                      lam~eta-longform
                      lam~eta-longform-p
                      lam~eta-redex-p
                      lam~matrix
                      ))

#{\section{Higher-Order Term Operations}\label{mod:hop}
In this module the basic
procedure for manipulating $\lambda$-terms in Church's simple theory of
types are defined. In particular procedures for forming normal forms
and testing equality with respect to higher-order transformations are
given.
#}

#{\subsection{Selectors}#}
(defun lam~matrix (term)
  (declare (edited  "02-APR-1998" "27-JAN-1992 9:55")
	   (authors Gebhard Kohlhase)
	   (input   "TERM is a ~term.")
	   (effect  "None.")
	   (value  "1. The matrix of the term, i.e. if TERM is of the form ([X1]..[XN].M)"
		    "   where M is not an abstraction, the matrix is M;"
		    "2. The binder of the term, i.e. (X1 .. XN).")
	   (example "[X].[Y].[Z].(G (F X) (G (F Y) Z)) -->"
                    "(G (F X) (G (F Y) Z)); "
                    "(X Y Z)"
		    " "
		    "(FORALL [A].(A C)) -->"
		    "(FORALL [A].(A C))"
		    "NIL"))
  (values (data~n-range term) (data~n-domain term)))


(defun lam~binder (term)
  (declare (edited  "27-JAN-1992 9:55")
	   (authors Kohlhase)
	   (input   "TERM is a term.")
	   (effect  "None.")
	   (value  "1. The binder of the term, i.e. if TERM is of the form ([X1]..[XN].M)"
		    "   where M is not an abstraction, the binder is (X1 .. XN);"
		    "2. The matrix of the term, i.e. M.")
	   (example "[X].[Y].[Z].(G (F X) (G (F Y) Z)) -->"
                    "(X Y Z)"
                    "(G (F X) (G (F Y) Z))"
		    " "
		    "(FORALL [A].(A C)) -->"
		    "NIL"
		    "(FORALL [A].(A C))"))
  (multiple-value-bind (matrix binder)
      (lam~matrix term)
    (values binder matrix)))


;#{\subsection{Alpha-Conversion}#}
;
;(defun lam~rename-bound-variables (term)
;  (declare (edited  "20-AUG-1991 9:55")
;           (authors Kohlhase)
;           (input   "TERM is any term.")
;           (effect  "None.")
;           (value   "A term where the variables bound by the binder of TERM and all other occurences"
;                    "of these variable in the scope of TERM are substituted by new variables.")
;           (example "[X].[Y].[Z].(G (F X) (G (F Y) Z)) --> [X1].[X2].[X3].(G (F X1) (G (F X2) X3))"))
;  (or (lam~rename-bound-variables-aux term) term))
;
;(term~defgeneric hop~rename-bound-variables-aux ((term))
;  (declare
;   (authors nesmith)
;   (input   "TERM is any term.")
;   (effect  "None.")
;   (value   "A term wherein the variables of all abstractions have been renamed, if there are any;
;If TERM contains no abstractions, returns nil, otherwise returns a new term with as much sharing
;as possible with the input TERM.")
;   (example "[X].[Y].[Z].(G (F X) (G (F Y) Z)) --> [X1].[X2].[X3].(G (F X1) (G (F X2) X3))"))
;  (:method ((term term+abstr))
;    (let* ((oldscope (data~abstr-range term))
;           (newscope (lam~rename-bound-variables oldscope)))
;      (lam~rename-top-variable 
;       (if (eq oldscope newscope)
;           term
;           (data~abstr-create (data~abstr-domain term) newscope)))))
;  (:method ((term sym+sym))
;    nil)
;  (:method ((term appl+appl))
;    (let* ((oldfun (appl~function term))
;           (newfun (hop~rename-bound-variables oldfun))
;           (oldargs (appl~arguments term))
;           (newargs (mapcar #'hop~rename-bound-variables oldargs)))
;      (unless (and (eq oldfun newfun) (every #'eq oldargs newargs))
;        (appl~create newfun newargs)))))


; Funktionen aus hop von denen noch nicht offensichtlich ist ob sie
; uebernommen werden muessen oder nicht

;(defun hop=subst-term-var-rename (term var inwff)
;  (or (hop=subst-term-var-rename-aux term var inwff) inwff))

;(term~defgeneric hop=subst-term-var-rename-aux ((term) (var) (wff))
; (declare
;  (authors nesmith)
;  (input "A TERM to be substituted for a VAR in a WFF.")
;  (effect "none")
;  (value "nil if no substitution was made, otherwise a new wff, sharing as much
;of the previous structure as possible is returned"))
; (:method ((term term+term) (var poly+polyvar) (wff poly+polyvar))
;   (if (keim~equal (sym~shared-symbol var) (sym~shared-symbol wff)) term nil))
; (:method ((term term+term) (var sym+sym) (wff sym+sym))
;   (if (term~equal-p var wff) term nil))
; (:method ((term term+term) (var sym+sym) (wff abstr+abstr))
;   (cond ((term~equal-p (abstr~bound-variable wff) var)
;          nil)
;         ((member (abstr~bound-variable wff) (top~all-unbound-symbols term) :test #'term~equal-p)
;          (if (member var (top~all-unbound-symbols (abstr~scope wff)) :test #'term~equal-p)
;              (hop=subst-term-var-rename-aux term var (hop~rename-top-variable wff))
;              nil))
;         (t (let ((newwff (hop=subst-term-var-rename-aux term var (abstr~scope wff))))
;              (if newwff 
;                  (abstr~create (abstr~bound-variable wff) newwff)
;                  nil)))))
; (:method ((term term+term) (var sym+sym) (wff appl+appl))
;   (let* ((oldfun (appl~function wff))
;          (newfun (or (hop=subst-term-var-rename-aux term var (appl~function wff))
;                      oldfun))
;          (oldargs (appl~arguments wff))
;          (newargs (mapcar #'(lambda (x) (or (hop=subst-term-var-rename-aux term var x) x))
;                           oldargs)))
;     (unless (and (eq newfun oldfun) (every #'eq oldargs newargs))
;       (appl~create newfun newargs)))))
;
;(defun hop~rename-top-variable (abstr)
;  (let ((newvar (sym~rename-var (abstr~bound-variable abstr))))
;    (abstr~create newvar 
;                  (hop=subst-term-var-rename newvar 
;                                             (abstr~bound-variable abstr)
;                                             (abstr~scope abstr)))))


;; Zunaechst mal ausgelassen, die Funktionen hop~beta-normform
;;                                    und    hop~beta-normform!


#{\subsection{Eta-equal}
The functions concerning eta-equality are already specified, but not yet implemented.#}

;; Vorsicht einige Aenderungen haben hier vielleicht Fehler eingeschlaeusst.
(data~defgeneric lam~eta-redex-p ((term))
  (declare (edited  "20-Juli-1998" "20-AUG-1991 9:55")
	   (authors Chris)
	   (input   "TERM is a term or nested termlist.")
	   (effect  "None.")
	   (value   "A iff TERM is of the form ([X1...Xn].(AXn...X1)), nil otherwise.")
	   (example "Let Q have type (O I), P have type (O I I), then
\\begin{itemize}
\\item (hop~eta-redex-p Q) -> nil
\\item (hop~eta-redex-p [X].(Q X) ) -> Q
\\item (hop~eta-redex-p [X].(P X X) ) -> nil
\\item (hop~eta-redex-p [X Y].(P Y X) ) -> P
\\end{itemize}")
)
    (:method ((term term+term))
      nil)
    (:method ((abstr term+abstr))
     (let* ((norm-term (beta~normalize abstr))
	    (vars      (data~abstr-domain norm-term))
	    (scope     (data~abstr-range  norm-term))
	    (args-of-scope (when (term~appl-p scope) (data~appl-arguments scope)))
	    (head-of-scope (when (term~appl-p scope) (data~appl-function scope)))
	    (possible1     (<= (length vars) (length args-of-scope)))
	    (vars-counterargs (when possible1
				(subseq args-of-scope
					(- (length args-of-scope) (length vars)))))
	    (rest-args (when possible1
			 (subseq args-of-scope
				 0 (- (length args-of-scope) (length vars)))))
	    (possible2 (when possible1
			 (every #'(lambda (x y) (keim~equal x y))
				vars
				vars-counterargs)))
	    (possible3 (when possible2
			 (null (intersection
				vars
				(mapcan #'data~free-variables
					(cons head-of-scope rest-args))
				:test #'term~alpha-equal)))))
       (when possible3
	 (data~appl-create head-of-scope rest-args)))))



(data~defgeneric lam~eta-longform ((term))
 (declare
  (authors nesmith)
  (input "A term")
  (value "The term with all subterms in eta-longform.")
  (example "Let R have type (O (O I I)), Q have type (O I I), then
\\begin{itemize}
\\item (hop~eta-longform Q) -> [E1].[E2].(Q E1 E2)
\\item (hop~eta-longform R) -> [E3].(R E3)
\\item (hop~eta-longform (R Q)) -> (R [E4].[E5].(Q E4 E5))
\\end{itemize}"))
 (:method ((term term+abstr))
   (multiple-value-bind (matrix binder) (lam~matrix term)
     (lam~eta-expand
      (let ((newterm (lam~eta-longform matrix)))
        (dolist (oldvar (reverse binder) newterm)
          (setq newterm (term~abstr-create (list oldvar) newterm)))))))
 (:method ((term term+primitive))
   (lam~eta-expand term))
 (:method ((term term+appl))
   (let* ((args (data~appl-arguments term))
	  (new-args (mapcar #'lam~eta-longform
			    args)))
     (if (every #'eq new-args args)
	 (lam~eta-expand term)
       (lam~eta-expand (term~appl-create (data~appl-function term)
					 new-args))))))


(data~defgeneric lam~eta-contract ((term))
  (declare (edited  "20-AUG-1991 9:55")
           (authors chris)
           (input   "TERM is a term")
           (effect  "None.")
           (value   "If TERM is an eta-redex, then the result of contracting it
is returned, otherwise the TERM itself is returned.")
           (example "Let P be of type (O I), Q of type (O I I), then
\\begin{itemize}
\\item (hop~eta-contract [X].(P X) ) -> P
\\item (hop~eta-contract [X].(Q X X) ) -> [X].(Q X X)
\\end{itemize}" ))
  (:method ((term term+abstr))
    (let ((result (lam~eta-redex-p term)))
      (if result
	  (lam~eta-contract result)
        (term~abstr-create
	 (list (data~abstr-c-domain term))
	 (lam~eta-contract (data~abstr-c-range term))))))
  (:method ((term term+appl))
	   (term~appl-create
	    (lam~eta-contract (data~appl-function term))
	    (mapcar #'lam~eta-contract (data~appl-arguments term))))
  (:method ((term term+primitive))
	   term))


(data~defgeneric lam~eta-longform-p ((term))
  (declare (edited  "20-AUG-1991 9:55")
	   (authors nesmith)
	   (input   "TERM is a term or nested termlist.")
	   (effect  "None.")
	   (value   "T iff TERM is in eta-long form, i.e. the matrix of TERM has"
		    "elementary type and all arguments of the scope are in eta-long form.")
	   (example  "Let Q have type (O I I), then
\\begin{itemize}
\\item (hop~eta-longform-p Q) -> nil
\\item (hop~eta-longform-p [X].[Y].(Q X Y) ) -> t
\\end{itemize}"))
  (:method ((term term+primitive))
    (type~primitive-p (term~type term)))
  (:method ((term term+appl))
    (and (type~primitive-p (term~type term))
	 (lam~eta-longform-p (data~appl-function term))
	 (every #'lam~eta-longform-p (data~appl-arguments term))))
  (:method ((term term+abstr))
    (let ((matrix (lam~matrix term)))
      (and (type~primitive-p (term~type matrix))
	   (if (data~appl-p matrix)
	       (every #'lam~eta-longform-p
		      (data~appl-arguments matrix))
	     t)))))


(data~defgeneric lam~eta-expand ((term))
  (declare (edited  "20-AUG-1991 9:55")
	   (authors nesmith)
	   (input   "TERM is a term")
	   (effect  "None.")
	   (value   "If the matrix M TERM is of type (i1,..,in)->i,"
		    " then a new term is returned where the matrix is ([X1]..[Xn].(M X1 .. Xn)),"
		    "otherwise TERM (M has elementary type).")
	   (example "Let R have type (O (O I I)), Q have type (O I I), then
\\begin{itemize}
\\item (hop~eta-expand Q) -> [E1].[E2].(Q E1 E2)
\\item (hop~eta-expand (R Q)) -> (R Q)
\\end{itemize}" ))
    (:method ((term term+term))
      (multiple-value-bind (matrix binder) (lam~matrix term)
	  (let ((type (term~type matrix)))
	    (if (type~func-p type)
		(let* ((newvar (term~variable-create (gensym "E") (data~c-domain type)))
		       (newterm (term~abstr-create (list newvar) (term~appl-create matrix (list newvar)))))
		  (lam~eta-expand
		   (dolist (oldvar (reverse binder) newterm)
		     (setq newterm (term~abstr-create (list oldvar) newterm)))))
	      term)))))



#{\subsection{Equality predicates}
#}

(data~defgeneric lam~beta-equal-p ((term1) (term2))
  (declare (edited  "20-AUG-1991 9:55")
	   (authors nesmith)
	   (input   "TERMi are terms.")
	   (effect  "None.")
	   (value   "T iff TERMi are beta-equal, i.e. TERMi have the same beta-normal form."))
    (:method ((term1 term+term) (term2 term+term))
      (term~alpha-equal (beta~normalize term1) (beta~normalize term2))))


(data~defgeneric lam~eta-equal-p ((term1) (term2))
  (declare (edited  "20-AUG-1991 9:55")
	   (authors nesmith)
	   (input   "TERMi are terms.")
	   (effect  "None.")
	   (value   "T iff TERMi are eta-equal, e.g. TERMi have the same eta-long form."))
    (:method ((term1 term+term) (term2 term+term))
      (term~alpha-equal (lam~eta-longform term1) (lam~eta-longform term2))))


(data~defgeneric lam~alpha-equal-p ((term1) (term2))
  (declare (edited  "20-AUG-1991 9:55")
	   (authors nesmith)
	   (input   "TERMi are terms.")
	   (effect  "None.")
	   (value   "T iff TERMi are alpha-equal, i.e. TERM1 can be obtained"
                    "from TERM2 by renaming of bound variables."))
    (:method ((term1 term+term) (term2 term+term))
      (term~alpha-equal term1 term2)))


(data~defgeneric lam~equal-p ((term1) (term2))
  (declare (edited  "20-AUG-1991 9:55")
	   (authors nesmith)
	   (input   "TERMi are terms.")
	   (effect  "None.")
	   (value   "T iff TERMi are alpha-beta-eta-equal."))
    (:method ((term1 term+term) (term2 term+term))
      (term~alpha-equal
       (lam~eta-longform (beta~normalize term1))
       (lam~eta-longform (beta~normalize term2)))))


		  
	     
