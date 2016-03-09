;;; -*- syntax: common-lisp; package: keim; base: 10; mode: lisp -*-
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


(mod~defmod subst :uses (keim mod data mapp)
            :documentation "substitution in fundamental data structures."
            :exports (
                      subst+substitution
                      subst~create
                      subst~domain
                      subst~codomain
                      subst~p
                      subst~empty-p
                      subst~list-p
                      subst~insert-component
                      subst~insert-component!
                      subst~add-component
;                     subst~add-component!                     -> benutze subst~add-component
;                     subst~add-component!!                    -> benutze subst~add-component
                      subst~get-component
                      subst~remove-component
                      subst~remove-component!
                      subst~remove-components
                      subst~remove-components!
                      subst~apply
;                     subst~apply!                             -> benutze subst~apply
;                     subst~apply!!                            -> benutze subst~apply
                      subst~apply-and-rename
;                     subst~apply-and-rename!                  -> benutze subst~apply-and-rename
;                     subst~apply-and-rename!!                 -> benutze subst~apply-and-rename 
                      subst~compose-substitution
		      subst~compose-subst-nso
;                     subst~compose-substitution!              -> benutze subst~compose-substitution 
;                     subst~compose-substitution!!             -> benutze subst~compose-substitution
                      subst~disjoint-compose-substitution
;                     subst~disjoint-compose-substitution!     -> benutze subst~disjoint-compose-substitution
;                     subst~disjoint-compose-substitution!!    -> benutze subst~disjoint-compose-substitution
                      subst~restrict-substitution
                      subst~restrict-substitution!
                      subst~delete-duplicates
                      subst~delete-duplicates!
                      subst~bind-substitution
;                     subst~create-binding-substitution        -> ???????????????????????????????????
		      subst~idem
		      subst~idem-rec ;;;; liess sich nicht vermeiden; DEF
		      subst~reduce
		      subst~read
		      subst~read-renaming
		      )
            )


#{
\section{Substitutions}
This module provides elementary functions for substitutions.
#}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Class-def + Accessors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (load compile eval)
(defclass subst+substitution (mapp+mapping)
  ()
  (:documentation "The class of substitutions."))

(defun subst~create (domain codomain &optional copy)
  (declare (edited  "04-NOV-1991 11:00")
           (authors GKLEIN RICHTS)
           (input   "A list of variables and a list of terms. Optional a flag.")
           (effect  "None.")
           (value   "The new substitution with the domain DOMAIN and the codomain CODOMAIN."
                    "If COPY = T the input-lists are copied.")
           (example "(x y) (a (f a)) --> {(X --> A) (Y --> (F A))}"))
  (let ((mapping (mapp~create domain codomain copy)))
    (cond
     ((notevery #'(lambda (item) (typep item 'data+variable)) domain)
      (error "the domain of a substitution has to consist of variables!"))
     ((notevery #'(lambda (item) (typep item 'data+struct)) codomain)
      (error "the codomain of a substitution may contain only data structures!"))
     (t
      (change-class mapping 'subst+substitution)))))
)

(defun subst~domain (substitution)
  (declare (edited  "26-APR-1996 10:08")
	   (authors GKLEIN)
           (input   "A substitution.")
           (effect  "None.")
           (value   "The domain of the substitution, i.e. the list of variables that will be subtituted.")
           (example "{(X --> A) (Y --> (F A))}-->(X Y)"))
  (mapp~domain substitution))

(defun subst~codomain (substitution)
  (declare (edited  "26-APR-1996 10:09")
	   (authors GKLEIN)
           (input   "A substitution.")
           (effect  "None.")
           (value   "The codomain of the substitution, i.e. the list of terms that will be substituted for variables.")
           (example "{(X --> A) (Y --> (F A))}-->(A (F A))"))
  (mapp~codomain substitution))

(defsetf subst~domain (substitution) (domain)
  `(setf (mapp~domain ,substitution) ,domain))

(defsetf subst~codomain (substitution) (codomain)
  `(setf (mapp~codomain ,substitution) ,codomain))

(defun subst~p (object)
  (declare (edited  "04-NOV-1991 11:01")
           (authors RICHTS)
           (input   "A Lisp object.")
           (effect  "None.")
           (value   "True, iff OBJECT is a substitution.")
           (example "{(X --> A) (Y --> (F A))}-->T"))
  (typep object 'subst+substitution))

(defun subst~empty-p (substitution)
  (declare (edited  "26-APR-1996 10:11")
	   (authors GKLEIN)
           (input   "A substitution.")
           (effect  "None.")
           (value   "True iff SUBSTITUTION is empty.")
           (example "{(X --> A) (Y --> (F A))}-->NIL"
                     "{}--> NIL"))
  (mapp~empty-p substitution))


(defun subst~list-p (object)
  (declare (edited  "04-NOV-1991 11:08")
           (authors RICHTS)
           (input   "OBJECT could be any lisp object.")
           (value   "True iff OBJECT is a (possible empty) list of substitutions.")
           (example "({(X --> F) (Y --> (F A))} {}-->T"
                     "(a b)"))
  (and (listp object)
       (or (null object)
           (and (subst~p (car object)) (subst~list-p (cdr object))))))


;;;;;;;;;;;;;;;;;;;;;;;;;
;; Changing components 
;;;;;;;;;;;;;;;;;;;;;;;;;


(defun subst~insert-component (variable term substitution)
  (declare (edited  "04-NOV-1991 11:08")
           (authors GKLEIN RICHTS)
           (input   "A variable, a term and a substitution (which may be nil).")
           (effect  "None.")
           (value   "A new substitution where [VARIABLE --> TERM] is added at the front of SUBSTITUTION.")
           (remark  "There is no check if the added component [VARIABLE --> TERM] is in the domain"
                    "and codomain. Also occurrences of VARIABLE in the codomain terms are not substituted.")
           (example "Z B {(X --> F) (Y --> (F A))} -->"
                     "{(Z --> B) (X --> F) (Y --> (F A))}"))
  (if substitution
      (mapp~insert-component variable term substitution)
    (mapp~insert-component variable term (subst~create nil nil))))


(defmethod mapp~insert-component :around (variable term (substitution subst+substitution))
  (declare (ignore variable term))
  (change-class (call-next-method) 'subst+substitution))



(defun subst~insert-component! (variable term substitution)
  (declare (edited  "18-APR-1995" "04-NOV-1991 11:09")
           (authors GKLEIN Fehrer RICHTS)
           (input   "A variable, a term and a substitution.")
           (effect  "The pair [VARIABLE --> TERM] is added at the front of the domain and codomain lists"
                    "of SUBSTITUTION.")
           (value   "The changed SUBSTITUTION.")
           (remark  "There is no check if the added component [VARIABLE --> TERM] is in the domain and codomain."
                    "Also occurences of VARIABLE in the codomain terms are not substituted.")
           (example "X B {(Z --> F) (Y --> (F X))} -->"
                     "{(X --> B) (Z --> F) (Y --> (F X))}"))
  (mapp~insert-component! variable term substitution))

(defun subst~add-component (variable term substitution
				     &key (destructive nil) (replacers-downto '(data+struct)) (downto '(data+primitive)) (test #'eq))
  (declare (edited  "04-NOV-1991 11:10")
           (authors RICHTS AMEIER)
           (input   "A variable, a term and a substitution (which may be nil).")
           (effect  "Depends on the keyword arguments DESTRUCTIVE and DOWNTO."
		    "If DESTRUCTIVE is true the codomain is changed destructively."
		    "If DESTRUCTIVE is nil, but DOWNTO is a list of Clos-Classes,"
		    "all objects of this classes are shared. So if some substructs are"
		    "of one of this shared objects, the replacement is also done"
		    "destructively on them.")
           (value   "A new substitution where the pair [VARIABLE --> TERM] is added"
                    "at the front of the domain and codomain lists"
                    "of SUBSTITUTION and is applied as a substitution to the codomain list."
		    "Remark: Which structs are shared depends also on the keyword argument REPLACERS-DOWNTO."
		    "        A copy of TERM is made one time, using data~copy with"
		    "        REPLACERS-DOWNTO as downto argument and :all-classes as preserve argument."
		    "        This (possibly new created) struct is then used for the replacement.")
	   (example "X B {(X --> F) (Y --> (F X))}"
		    "{(X --> B) (Y --> (F B)) (X --> F)}"))
  (subst~compose-substitution (subst~create (list variable) (list term))
			      substitution
			      :destructive destructive
			      :replacers-downto replacers-downto
			      :downto downto
			      :test test))

(defun subst~get-component (variable substitution)
  (declare (edited  "04-NOV-1991 11:13")
           (authors GKLEIN RICHTS prckln)
           (input   "A variable and a substitution.")
           (effect  "None.")
           (value   "The codomain term corresponding to VARIABLE or NIL if there is none.")
           (example "X {(X --> F) (Y --> (F A))} --> F"))
  (mapp~get-component variable substitution))

(defun subst~remove-component (variable substitution)
  (declare (edited  "04-NOV-1991 11:13")
           (authors GKLEIN RICHTS)
           (input   "A variable and a substitution.")
           (effect  "None.")
           (value  "A new substitution without VARIABLE and its codomain term.")
           (example "X {(X --> F) (Y --> (F A))} --> {(Y --> (F A))}"))
  (mapp~remove-component variable substitution))

(defun subst~remove-component! (variable substitution)
  (declare (edited  "04-NOV-1991 11:14")
           (authors GKLEIN RICHTS)
           (input   "A variable and a substitution.")
           (effect  "If VARIABLE is in the domain of the substitution, the component of the substitution"
                    "is removed destructively from the domain- and codomain-list.")
           (value  "SUBSTITUTION without VARIABLE and its codomain term.")
           (example "X {(X --> F) (Y --> (F A))} --> {(Y --> (F A))}"))
  (mapp~remove-component! variable substitution))

(defun subst~remove-components (variables substitution)
  (declare (edited  "04-NOV-1991 11:13")
           (authors GKLEIN RICHTS)
           (input   "A list of variables and a substitution.")
           (effect  "None.")
           (value   "A new substitution without VARIABLES and its codomain terms.")
           (example "X Y {(X --> F) (Y --> (F A))} --> {}"
                     "NIL {(X --> F) (Y --> (F A))} --> {(X --> F) (Y --> (F A))}"))
  (mapp~remove-components variables substitution))

(defun subst~remove-components! (variables substitution)
  (declare (edited  "04-NOV-1991 11:14")
           (authors RICHTS)
           (input   "A list of variables and a substitution.")
           (effect  "if a variable in the domain of SUBSTITUTION is in VARIABLES,"
                    "it is removed destructively from the domain and also its codomain term.")
           (value  "SUBSTITUTION without VARIABLES and its codomain terms.")
           (example "X Y {(X --> F) (Y --> (F A))} --> {}"))
  (mapp~remove-components! variables substitution))


#{
\subsection{Operations with substitutions} 
#}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operations with substitutions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun subst~apply (substitution object &key (destructive nil) (replacers-downto '(data+struct))
				 (downto '(data+primitive)) (test #'eq))
  (declare (edited  "04-NOV-1991 11:14")
           (authors RICHTS AMEIER)
           (input   "A substitution and a struct.")
           (effect  "Depends on the keyword arguments DESTRUCTIVE and DOWNTO."
		    "If DESTRUCTIVE is true the input struct is changed destructively."
		    "If DESTRUCTIVE is nil, but DOWNTO is a list of Clos-Classes,"
		    "all objects of this classes are shared. So if some substructs are"
		    "of one of this shared objects, the replacement is also done destructively"
		    "on them.")
           (value   "The new object with its terms instantiated by SUBSTITUTION."
		    "Remark: Which structs are shared depends also on the keyword argument REPLACERS-DOWNTO."
		    "        A copy of all structs of the codomain of the substitution is made one time, using"
		    "        data~copy with REPLACERS-DOWNTO as downto argument and :all-classes as preserve"
		    "        argument. This (possibly new created) structs are then used for the replacement.")
	   (example "{(X --> A) (Y --> B)} (Q X (G X Y)) --> (Q A (G A B))"))
  (data~replace-free-variables object (subst~domain substitution) (subst~codomain substitution)
			       :destructive destructive
			       :downto downto
			       :replacers-downto replacers-downto
			       :test test))

;; Man beachte mit subst~apply und type-variablen folgende Probleme:
;;
;; Sobald man frei type-variablen hat gehen die Problme los:
;; Variablen mit type-variablen drin verhalten sich leicht anders als constanten mit type-variablen drin. Waehrend bei
;; Constanten mit type-variablen drin irgendwo in einem Environment eine Schema-Version liegen muss, die mittels des
;; data~origin Slots von der Constante aus erreicht werden kann, und von der man dann einfach eine weitere Instanz
;; herstellt, haben Variablen kein ORIGIN!
;;
;; Dies fuehrt im Zusammenhang mit subst~apply zu folgenden Problemen:
;; 
;; 1.) Es kann zu destruktiven Ersetzungen an = Veraenderungen von Variablen mit Type-variablen im Typ kommen!
;;     Beispiel: Subst~apply auf (q x:aa) - wobei x:aa eine freie Variable ist - und in substitution kommt aa->i vor!
;;               Resultat: (q x:i) aber auch der original Term ist DESTRUKTIV veraendert, naemlich das x:i !!!
;;     Dies kann dazu fuehren, dass der Original-Term falsch getypt wird, uberhaupt kann das dazu fuehren, dass man
;;     extrem ekelhaftre Fehler erhaelt, die einem naemlich Sachen zerschiessen ohne dass man es merkt.
;;     Solche Fehler sind nur schwer bis gar nicht nachzuvollziehen!
;;
;; 2.) Eine Substitution kann eventuell nicht ganz idempotent gemacht werden, z.B. {(z:i --> y:hh) (hh --> i)} 
;;     Eigentlich braeuchte man {(z:i --> y:i) (hh --> i)}, wuerde man das jedoch machen wuerde man y destruktive
;;     veraendern (was man nicht will siehe 1.)). Dies kann dazu fuehren, dass gewisse subst~applies ziemlichen
;;     Mist erzeugen, z.B. (SUBST~APPLY {(z:i --> y:hh) (hh --> i)} (q:[i i] z:i):i
;;     erzeugt den vollkommen falsch getypten Term (q:[i i] y:hh):i
;;
;;
;;
;; Die folgende Funktion subst~apply-separate behebt alle diese Probleme, allerdings zu einem gewissen Preis ...
;;
;; Es werden zuerst von allen polymorphen Variablen in dem Object UND der Codmain der Substitution Kopien mit gleichem
;; Namen und gleichem Typ (also mit Typ-Variablen) erzeugt! Diese sind aber NICHT MEHR EQ ZU DEN ORIGINALEN!!!!!
;; Dann werden diese neuen Term-Variablen fuer die alten in der Substitution und im Object eingestetz, dann wird erst
;; die eigentliche Substitution ausgefuehrt!
;;
;; Allerdings bezahlt man dafuer mit folgendem Preis:
;; Die entstehenden Terme enthalten Term-Variablen, die nicht mehr EQ sind zu den Originalen (und auch einen anderen Typ haben),
;; sondern nur noch gleich heissen

(defun subst~apply-separate (substitution object &key (destructive nil) (replacers-downto '(data+struct))
				 (downto '(data+primitive)) (test #'eq))
   (multiple-value-bind
      (type-domain type-codomain other-domain other-codomain)
      (data=filter-type-substitution (subst~domain substitution) (subst~codomain substitution))

    (let* ((free-variables (append (data~free-variables object)
				   (apply #'append (mapcar #'data~free-variables (subst~codomain substitution)))))
	   (free-variables-with-type-domain (remove-if-not #'(lambda (fv)
							       (and (term~variable-p fv)
								    (intersection type-domain (term~free-type-variables fv))))
							   free-variables)))
      (if (null free-variables-with-type-domain)
	  (subst~apply substitution
		       object
		       :destructive destructive
		       :replacers-downto replacers-downto
		       :downto downto
		       :test test)
	(let* ((new-free-variables (mapcar #'(lambda (fv)
					       (term~variable-create (keim~name fv) (term~type fv)))
					   free-variables-with-type-domain))
	       (new-other-codomain (data~replace-free-variables other-codomain
								free-variables-with-type-domain
								new-free-variables))
	       (new-other-domain (data~replace-free-variables other-domain
							      free-variables-with-type-domain
							      new-free-variables))
	       (new-object1 (data~replace-free-variables object
							 free-variables-with-type-domain
							 new-free-variables
							 :destructive destructive
							 :downto downto
							 :replacers-downto replacers-downto
							 :test test))
	       (new-object2 (data~replace-free-variables new-object1
							 (append type-domain new-other-domain)
							 (append type-codomain (data~replace-free-variables new-other-codomain
													    type-domain
													    type-codomain
													    )))))
	  
	  ;; The followin step is necesary since without this step on the new variables the type-substitution would not be
	  ;; applied what could cause that the resulting terms are typed incorrectly
	  ;; e.g., 
	  ;; {(y:i --> x:jj) (jj --> i)} on (q:[i i] y:i):i
	  ;; would result in (q:[i i] x:jj):i
	  (mapcar #'(lambda (fv)
		      (setf (term~type fv)
			    (subst~apply substitution
					 (term~type fv)
					 :downto '(data+primitive))))
		  new-free-variables)
	  
	  new-object2)))))
    
					    
	  
  

(defun subst~apply-and-rename (substitution object &key (destructive nil) (replacers-downto '(data+struct))
					    (downto '(data+primitive)) (test #'eq))
  (declare (edited  "04-NOV-1991 11:14")
           (authors RICHTS Ameier)
           (input   "A substitution and a struct.")
           (effect  "Depends on the keyword arguments DESTRUCTIVE and DOWNTO."
		    "If DESTRUCTIVE is true the input datum is changed destructively."
		    "If DESTRUCTIVE is nil, but DOWNTO is a list of Clos-Classes,"
		    "all objects of this classes are shared. So if some substructs"
		    "are of one of this shared objects the replacement is also done"
		    "destructively on them.")
	   (value   "Multiple-Value:"
		    "First: The new struct with its terms instantiated by SUBSTITUTION"
		    "       and all free variables renamed."
		    "Second: The renaming substitution."
		    "Remark: Which structs are shared depends also on the keyword argument REPLACERS-DOWNTO."
		    "        A copy of all structs of the codomain of the substitution is made one time, using"
		    "        data~copy with REPLACERS-DOWNTO as downto argument and :all-classes as preserve"
		    "        argument. This (possibly new created) structs are then used for the replacement.")
	   (example "{(X --> A)} (Q X (G X Y)) --> (Q A (G A x-1)) {(Y --> x-1)}"))
  (multiple-value-bind
      (new-object domain codomain)
      (data~replace-free-variables-and-rename object (subst~domain substitution) (subst~codomain substitution)
					      :destructive destructive
					      :replacers-downto replacers-downto
					      :downto downto
					      :test test)
    (values new-object (subst~create domain codomain))))

(defun subst~compose-substitution (outer-substitution inner-substitution
						      &key (destructive nil) (replacers-downto '(data+struct))
						      (downto '(data+primitive)) (test #'eq))
  (declare (edited  "04-NOV-1991 11:19")
           (authors RICHTS AMEIER)
           (input   "Two substitutions.")
	   (effect  "Depends on the keyword arguments DESTRUCTIVE and DOWNTO."
		    "If DESTRUCTIVE is true the input codomain structs of the INNER-SUBSTITUTION are"
		    "changed destructively. If DESTRUCTIVE is nil, but downto contains some Clos-Classes,"
		    "all objects of this classes are shared. So if some substructs of the INNER-SUBSTITUTION"
		    "are of one of this shared objects, the replacement is also done destructively on them.")
           (value   "OUTER-SUBSTITUTION is applied to the codomain of INNER-SUBSTITUTION and all domain-variables of"
                    "OUTER-SUBSTITUTION not in the domain of INNER-SUBSTITUTION are added with their codomain-terms:"
                    "The new substitution."
		    "REMARK: In order to obtain an idempotent substitution the codomain of OUTER-SUBSTITUTION"
                    "        and the domain of INNER-SUBSTITUTION must be variable disjoint."
		    "Remark: Which structs are shared depends also on the keyword argument REPLACERS-DOWNTO."
		    "        A copy of all structs of the codomain of the OUTER-SUBSTITUTION is made one time, using"
		    "        data~copy with REPLACERS-DOWNTO as downto argument and :all-classes as preserve"
		    "        argument. This (possibly new created) structs are then used for the replacement and"
		    "        are added (together with there according domain variables) as new pairs.")
	   (example "{(Z --> A)} {(X --> A) (Y --> (F Z))} --> {(Z --> A) (Y --> (F A)) (X --> A)}"
		    "{(X --> A) (Y --> (F Z))} {(Z --> A)} --> {(Y --> (F Z)) (X --> A) (Z --> A)}"))
  (let* ((shared-outer-substitution (subst~create (subst~domain outer-substitution)
						  (mapcar #'(lambda (codo-term)
							      (data~copy codo-term
									 :downto
									 replacers-downto
									 :explode nil
									 :preserve :all-classes))
							  (subst~codomain outer-substitution))))
	 (new-substitution (subst~apply shared-outer-substitution
					inner-substitution
					:destructive destructive
					:replacers-downto '(data+struct)
					:downto downto
					:test test)))
    (mapc #'(lambda (var term)
              (unless (member var (subst~domain new-substitution) :test #'data~equal)
		(subst~insert-component! var term new-substitution)))
          (subst~domain shared-outer-substitution)
	  (subst~codomain shared-outer-substitution))
    new-substitution))
   
(defun subst~disjoint-compose-substitution (outer-substitution inner-substitution
							       &key (destructive nil) (replacers-downto '(data+struct))
							       (downto '(data+primitive)) (test #'eq))
  (declare (edited  "04-NOV-1991 11:22")
           (authors RICHTS gk AMEIER)
           (input   "Two substitutions the domains of which are disjoint.")
	   (effect  "Depends on the keyword arguments DESTRUCTIVE and DOWNTO."
		    "If DESTRUCTIVE is true the input codomain structs of the INNER-SUBSTITUTION are"
		    "changed destructively. If DESTRUCTIVE is nil, but downto contains some Clos-Classes,"
		    "all objects of this classes are shared. So if some substructs of the INNER-SUBSTITUTION"
		    "are in one of this shared objects, the replacement is also done destructively.")
           (value   "OUTER-SUBSTITUTION is applied to the codomain of INNER-SUBSTITUTION and the domain and codomain of"
                    "OUTER-SUBSTITUTION is added to the resulting substitution:"
                    "A new substitution."
		    "REMARK: In order to obtain an idempotent substitution the codomain of OUTER-SUBSTITUTION"
                    "        and the domain of INNER-SUBSTITUTION must be variable disjoint."
		    "Remark: Which structs are shared depends also on the keyword argument REPLACERS-DOWNTO."
		    "        A copy of all structs of the codomain of the OUTER-SUBSTITUTION is made one time, using"
		    "        data~copy with REPLACERS-DOWNTO as downto argument and :all-classes as preserve"
		    "        argument. This (possibly new created) structs are then used for the replacement and"
		    "        are added (together with there according domain variables) as new pairs.")
	   (example "{(X --> F) (Y --> (F A))} {(Z --> C)} -->{(Z --> C) (X --> F) (Y --> (F A))}"))  
  (when (intersection (subst~domain outer-substitution) (subst~domain inner-substitution) :test test)
    (error "Substitution ~A and Substitution ~A should be disjoint." outer-substitution inner-substitution))
  (let* ((shared-outer-substitution (subst~create (subst~domain outer-substitution)
						  (mapcar #'(lambda (codo-term)
							      (data~copy codo-term
									 :downto replacers-downto
									 :preserve :all-classes
									 :explode nil))
							  (subst~codomain outer-substitution))))
	 (new-substitution (subst~apply shared-outer-substitution
					inner-substitution
					:destructive destructive
					:replacers-downto '(data+struct)
					:downto downto
					:test test)))
    (setf (subst~domain new-substitution) (append (subst~domain new-substitution) (subst~domain shared-outer-substitution)))
    (setf (subst~codomain new-substitution) (append (subst~codomain new-substitution) (subst~codomain shared-outer-substitution)))
    new-substitution))

(defun subst~restrict-substitution (substitution variables)
  (declare (edited  "05-NOV-1991 10:25")
           (authors GKLEIN RICHTS)
           (input   "A substitution and a list of variables.")
           (effect  "None.")
           (value   "A new substitution where the domain is the intersection of the domain of SUBSTITUTION and VARIABLES"
                    "and the codomain contains the corresponding terms of SUBSTITUTION.")
           (example "{(X --> A) (Y --> (F A))} (X Z) -->{(X --> A)}"))
  (mapp~restrict-mapping substitution variables))


(defun subst~restrict-substitution! (substitution variables)
  (declare (edited  "05-NOV-1991 10:26")
           (authors GKLEIN RICHTS)
           (input   "A substitution and a list of variables.")
           (effect  "The variables in the domain of SUBSTITUTION which don't occur in VARIABLES and their"
                    "corresponding terms in the codomain are destructively deleted from these lists.")
           (value   "The changed SUBSTITUTION.")
           (example "subst --> {(X --> A) (Y --> (F A))}"
                     "{(X --> A) (Y --> (F A))} (X Z) -->{(X --> A)}"
                     "subst --> {(X --> A)}, please notice the destructive changes made to subst"))
  (mapp~restrict-mapping! substitution variables))


(defun subst~delete-duplicates (substitution)
  (declare (edited  "05-NOV-1991 10:35")
           (authors RICHTS ohlbach)
           (input   "A substitution.")
           (effect  "None.")
           (value   "A new substitution with all pairs of SUBSTITUTION without that of the form (x --> x).")
           (example "{(X --> X) (Y --> (F A))} --> {(Y --> (F A))}"))
  (mapp~delete-duplicates substitution))


(defun subst~delete-duplicates! (substitution)
  (declare (edited  "05-NOV-1991 10:37")
           (authors GKLEIN RICHTS)
           (input   "A substitution.")
           (effect  "Identical domain - codomain pairs (x --> x) are deleted in the substitution.")
           (value   "The changed SUBSTITUTION.")
           (example "subst --> {(X --> X) (Y --> (F A))}"
                     "{(X --> X) (Y --> (F A))} --> {(Y --> (F A))}"
                     "subst {(Y --> (F A))}, please notice the destructive changes made to subst"))
  (mapp~delete-duplicates! substitution))



;;;;;;;;;;;;;;;;;;;;;;;
;; bindings
;;;;;;;;;;;;;;;;;;;;;;;


(defun subst~bind-substitution (subst)
  (declare (edited  "09-MAR-1994 20:47")
           (authors RICHTS)
           (input   "A substitution.")
           (effect  "The variables in the domain of SUBST are bound to the terms in the"
                    "codomain of SUBST in the current binding context.")
           (value   "Undefined."))
  (mapc #'(lambda (var datum)
	    (bind~bind var datum))
	(subst~domain subst) (subst~codomain subst)))



;;; obsolete: Instead use the bind module facilities!
#+gklein(defun subst~create-binding-substitution (&optional (recursiv T))
  (declare (edited  "24-AUG-1994" "07-NOV-1991 12:01")
           (authors Richts RICHTS)
           (input   "Optional a flag (default is T).")
           (effect  "None.")
           (value   "A substitution that contains all variables in the binding-lists as its domain"
                    "and the terms in the binding slots of these variables as codomain."
                    "If recursiv is T (the default), the codomain-terms are built with"
                    "top~~insert-bindings!, i.e. the binding slots are regarded;"
                    "else the binding-terms itself are taken.")
           (example "(top~~binding-list!) -->NIL"
                    "(subst~~create-binding-substitution) --> {}"))
  (let ((new-substitution (subst~create nil nil)))
    (mapc #'(lambda (binding-list)
              (mapc #'(lambda (var)
                        (if (and (data~variable-p var)
                                 (data~binding var)
                                 (not (subst~get-component var new-substitution)))
                            (subst~insert-component! var 
                                                     (if recursiv 
                                                         (top~insert-bindings! (data~binding var)) 
                                                       (data~binding var))
                                                     new-substitution)))
                    binding-list))
          (top~binding-lists))
    new-substitution))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Idempotent Substitut
;; By idempotent substitutions we mean substitutions which have variable disjoint domain and codomain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Remark by Ameier:
;; If found that subst~idem returns nil when called on {bb -> bb} where the bb are eq!
;; Unfortunately, i was not able to detect the failure. Hence, i added a small function removing pairs from a substitution that are
;; eq!

(defun subst~idem (substi)
  (declare (edited  "21-MAY-1996 15:10")
	   (authors GKLEIN)
	   (input   "A substitution.")
	   (effect  "operates on the labels of the domain variables of SUBST, leaving them NIL afterwards.")
	   (value   "A new substituton with variable disjoint domain and codomain which behaves like SUBST."
		    "If such a substitution does not exist, NIL will be returned."))
  (let* ((subst (subst=remove-equal-pairs substi))
	 (dom (subst~domain subst))
	 (codom (subst~codomain subst)))
    (mapc #'(lambda (var)
	      (setf (data~label var) nil))  ; zur Sicherheit, damit nix anbrennt; DEF
	  dom)
    (let ((new-codom (mapcar #'(lambda (var datum)
				 (subst=idem var datum subst))
			     dom codom)))
      (mapc #'(lambda (var)
		(setf (data~label var) nil))
	    dom)
      (unless (some #'null new-codom)  ; wann genau liefert subst=idem nil???
	(subst~create dom new-codom)))))

(defun subst=remove-equal-pairs (subst)
  (do* ((rest-domain (subst~domain subst) (rest rest-domain))
	(rest-codomain (subst~codomain subst) (rest rest-codomain))
	(back-domain nil)
	(back-codomain nil))
      ((null rest-domain)
       (subst~create back-domain back-codomain))
    (let* ((domv (first rest-domain))
	   (codt (first rest-codomain)))
      (when (null (eq domv codt))
	(setf back-domain (append back-domain (list domv)))
	(setf back-codomain (append back-codomain (list codt)))))))

(defun subst=idem (var datum subst)
  (declare (edited  "21-MAY-1996 15:19")
	   (authors GKLEIN)
	   (input   "A variable, the datum directly associated to it by a substitution, and the substitution itself.")
	   (effect  "label-slot manipulation.")
	   (value   "The fully instantiated DATUM for VAR by the substitution SUBST."))
  (let ((old-label (data~label var)))
    (if old-label
	old-label
      (progn
	(setf (data~label var) T)  ; wozu soll das denn gut sein, wird doch eh gleich wieder umgesetzt?
	                           ; hmm, aber vorher im rekursiven Teil steht es halt bereits auf T
	                           ; (hier waren wir schon, aber nicht fertig)
	(let ((inst-datum (subst~idem-rec datum subst)))
	  (setf (data~label var) inst-datum)
	  inst-datum)))))


(defgeneric subst~idem-rec ((datum) subst)
  (declare (edited  "21-MAY-1996 15:16")
	   (authors GKLEIN)
	   (input   "A datum and a substitution.")
	   (effect  "label-slot manipulation.")
	   (value   "The full instantiated DATUM by the substitution SUBST."))
  (:method ((const data+constant) subst) ; checked, DEF
	   (declare (ignore subst))
	   const)
  (:method ((var data+variable) subst)   ; checked, DEF
	   (cond ((eq (data~label var) t)
		  nil)                             ; d.h. occur check fail; DEF
		 ((data~label var)
		  (data~label var))                ; bereits einmal errechneter Wert kann genommen werden
		 (t
		  (let ((datum (subst~get-component var subst)))
		    (if datum
			(subst=idem var datum subst)
		      var)))))
  (:method ((appl data+appl) subst)
	   (let ((new-func (subst~idem-rec (data~appl-function appl) subst)))
	     (when new-func
	       (let ((new-args (mapcar #'(lambda (arg)
					   (subst~idem-rec arg subst))
				       (data~appl-arguments appl))))
		 (unless (some #'null new-args)
		   (data~appl-create new-func new-args))))))  
  (:method ((abstr data+abstr) subst)              ; geht jetzt auch fuer Binderlisten; DEF
	   (let* ((binder (data~abstr-binder abstr))
		  (old-labels (mapcar #'data~label binder))
		  (new-abstr))
	     (unwind-protect
		 (progn
		   (mapc #'(lambda (x) (setf (data~label x) x)) binder)
		   (let ((new-range (subst~idem-rec (data~abstr-range abstr) subst)))
		     (when new-range
		       (setf new-abstr (data~abstr-create binder new-range)))))
	       (mapc #'(lambda (x y) (setf (data~label x) y)) binder old-labels))
	     new-abstr)))

; subst=idem-rec --> subst~idem-rec. Es kommen in term.lisp Methoden fuer Terme dazu.
; Dummerweise muss subst~idem-rec exportiert werden.!!!!!   DEF

; Wie ist das mit den Typen? die brauchen doch wohl auch spezielle Methoden???
;                   Holger?????

; Geht im Moment wohl nur fuer Substitutionen, die KEINE TYPBINDUNGEN ENTHALTEN!

(defgeneric subst~reduce (subst &key restrict-vars)
  (declare (edited  "27-NOV-1996")
	   (authors kk chris)
	   (input   "a problem (e.g. a term) and a (list of) substitution")
	   (effect  "none")
	   (value   "the beta-reduced substitution restricted"
		    "to the free variables of problem"))
  (:method ((subst subst+substitution) &key restrict-vars)
	   (let ((new-subst (subst~idem subst)))
	     (when restrict-vars
	       (setf new-subst (subst~restrict-substitution new-subst restrict-vars)))
	     (subst~create (subst~domain new-subst)
			   (mapcar #'beta~normalize (subst~codomain new-subst)))))
  (:method ((subst list) &key restrict-vars)
	   (mapcar #'(lambda (x) (subst~reduce x :restrict-vars restrict-vars))
		   subst)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keim- + Data-Methods on Substitutions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod keim~copy ((substitution subst+substitution) &key (explode :all-classes) share preserve downto)
  (declare (edited  "27-JAN-1998")
	   (authors Ameier)
	   (input   "A substitution, and the keywords explode,share,preserve and downto.")
	   (effect  "None.")
	   (value   "A copy of the substitution."
		    "Warning: Which subparts of the substitution are shared depends on the"
		    "         keyword argument downto."))
  (if (or (eq downto :all-classes) (some #'(lambda (x) (typep substitution x)) downto))
      substitution
    (subst~create (keim~copy (subst~domain substitution)
			     :explode explode
			     :share share
			     :preserve preserve
			     :downto downto)
		  (keim~copy (subst~codomain substitution)
			     :explode explode
			     :share share
			     :preserve preserve
			     :downto downto))))


(defmethod data~free-variables ((substitution subst+substitution))
  (declare (edited  "27-JAN-1998")
	   (authors Ameier)
	   (input   "A substitution.")
	   (effect  "None.")
	   (value   "All free variables of terms in the codomain of the substitution."))
  (remove-duplicates (apply 'append
			    (mapcar #'data~free-variables (subst~codomain substitution)))))

  
(defmethod data=replace-structs ((substitution subst+substitution) struct-list1 struct-list2 destructive downto replacers-downto test)
  (declare (edited  "05-NOV-1991 10:32")
           (authors RICHTS)
           (input   "")
           (effect  "")
           (value   "The new substitution where all occurrences of structs of STRUCT-LIST-1"
		    "in the codomain of SUBSTITUTION (tested for equality with TEST) are"
		    "replaced with the corresponding structs of STRUCT-LIST-2."))
  (cond ((or (find 'subst+substitution downto) destructive)
	 ;; -> destructive replacement directly on substitution structure
	 (setf (subst~codomain substitution) (mapcar #'(lambda (term)
							 (data=replace-structs term
									       struct-list1 struct-list2
									       't
									       downto
									       replacers-downto
									       test))
						     (subst~codomain substitution)))
	 substitution)
	(t
	 ;; -> destructive replacement possibly later							      
	 (let ((new-codomain (mapcar #'(lambda (struct)
					 (data=replace-structs struct struct-list1 struct-list2
							       destructive
							       downto
							       replacers-downto
							       test))
				     (subst~codomain substitution))))
	   (subst~create (subst~domain substitution) new-codomain)))))

(defmethod data=replace-fv ((substitution subst+substitution) bound variables structs destructive downto test)
  (declare (edited  "05-NOV-1991 10:32")
           (authors RICHTS)
           (input   "")
           (effect  "")
           (value   "The new substitution where all free occurrences of variables of VARIABLES"
		    "in the codomain of SUBSTITUTION (tested for equality with TEST) are"
		    "replaced with the corresponding structs of STRUCTS."))
  (cond ((or (find 'subst+substitution downto) destructive)
	 ;; -> destructive replacement directly on substitution structure
	 (setf (subst~codomain substitution) (mapcar #'(lambda (term)
							 (data=replace-fv term
									  nil
									  variables structs
									  't
									  downto
									  test))
						     (subst~codomain substitution)))
	 substitution)
	(t
	 ;; -> destructive replacement possibly later							      
	 (let ((new-codomain (mapcar #'(lambda (struct)
					 (data=replace-fv struct
							  nil
							  variables structs
							  destructive
							  downto
							  test))
				     (subst~codomain substitution))))
	   (subst~create (subst~domain substitution) new-codomain)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mehtods for the hop stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keine Ahnung was das macht AMEIER


;(defmethod hop~beta-normform ((subst subst+substitution))
;  (let ((new-substitution (subst~create nil nil)))
;    (mapc #'(lambda (variable old-term)
;              (let ((new-term (hop~beta-normform old-term)))
;                (subst~insert-component! (term~copy variable) new-term new-substitution)))
;          (subst~domain subst) (subst~codomain subst))
;    new-substitution))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; POST interface 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod post~print ((substitution subst+substitution) stream)
  (if (subst~empty-p substitution)
      (format stream "(substitution () ())")
    (progn
      (format stream "(substitution (")
      (post~print (subst~domain substitution) stream)
      (format stream ") (")
      (post~print (subst~codomain substitution) stream)
      (format stream "))"))))

(defmethod post~read-object (sub (env env+environment) 
			    (indicator (eql :substitution)))
  (let* ((domain (mapcar #'(lambda (x)
			     (if (not (symbolp x))
				 (error "post~~read-object, substitution: ~A is not a variable in the environment" x)
			       (env~lookup-object x env)))
			 (first sub))))
    (when (not (every #'data~variable-p domain))
      (error "In post~~read-object substitution, domain has to consist of variables."))
    
    (subst~create domain
		  (mapcar #'(lambda (var x)
			      (if (typep var 'term+term) 
				  (post~read-object x env :existing-term)
				(post~read-object x env :existing-type)))
			  domain
			  (second sub)))))

(defmethod post~read-object (sub (env env+environment) 
				 (indicator (eql :renaming)))
  (let* ((domain (mapcar #'(lambda (x)
			     (if (not (symbolp x))
				 (error "post~~read-object, renaming: ~A is not a variable in the environment" x)
			       (env~lookup-object x env))) 
			 (first sub)))
	 (codomain (mapcar #'(lambda (var x)
			       (if (not (symbolp x))
				   (error "post~~read-object, renaming: ~A is not a variable in the environment" x)
				 (if (typep var 'term+term) 
				     (post~read-object x env :existing-term)
				   (post~read-object x env :existing-type))))
			   domain
			   (second sub))))

    (when (not (every #'data~variable-p domain))
      (error "In post~~read-object, renaming, domain has to consist of variables."))

    (when (not (every #'data~variable-p codomain))
      (error "In post~~read-object, renaming, codomain has to consist of variables."))

    (subst~create domain codomain)))


(defun subst~read (sub env)
  (declare (edited  "09-FEB-1993 12:46")
	   (authors nesmith)
	   (input   "A substitution SUB in POST format and an environment.")
	   (effect  "The substitution is constructed.")
	   (value   "The new substitution is returned."))
  (post~read-object (cdr sub) env :substitution))

(defun subst~read-renaming (sub env)
  (declare (edited  "09-FEB-1993 12:46")
	   (authors nesmith)
	   (input   "A substitution SUB in POST format and an environment.")
	   (effect  "The substitution is constructed.")
	   (value   "The new substitution is returned."))
  (post~read-object (cdr sub) env :renaming))



;; Hilfsfunktion fuer das Kooperation von bind~with-bindings und substitutionen,
;; Fuer den Fall, dass man im binding stack zurueck kommt und nicht alle bindings
;; vergessen will
(defun subst~bind-subst (subst to-bind)
  (declare (edited  "10-FEB-1998")
	   (authors Gebhard)
	   (input   "A substitution and a list of variables to bind")
	   (effect  "Binds all variables of to-bind in the actual binding context with the
value of the variable in the substitution.")
	   (value   "True"))
  (let ((aac (pairlis (subst~domain subst) (subst~codomain subst))))
    (map 'nil #'(lambda (x) (if (find (car x) to-bind)
				(bind~bind (car x) (cdr x))
			      nil))
	 aac)
    t))









;;
;;========================================================================================
;;                SYNTACTICALLY SORTED TERMS SPECIFIC FUNCTIONS
;;                   (mit aeusserster Vorsicht zu geniessen)
;; =======================================================================================


(defun subst~apply-nso (substitution object &key (destructive nil) (replacers-downto '(data+struct))
				 (downto '(data+primitive)) (test #'eq))
  (declare (edited  "04-NOV-1991 11:14")
           (authors RICHTS AMEIER)
           (input   "A substitution and a struct.")
           (effect  "Depends on the keyword arguments DESTRUCTIVE and DOWNTO."
		    "If DESTRUCTIVE is true the input struct is changed destructively."
		    "If DESTRUCTIVE is nil, but DOWNTO is a list of Clos-Classes,"
		    "all objects of this classes are shared. So if some substructs are"
		    "of one of this shared objects, the replacement is also done destructively"
		    "on them.")
           (value   "The new object with its terms instantiated by SUBSTITUTION."
		    "Remark: Which structs are shared depends also on the keyword argument REPLACERS-DOWNTO."
		    "        A copy of all structs of the codomain of the substitution is made one time, using"
		    "        data~copy with REPLACERS-DOWNTO as downto argument and :all-classes as preserve"
		    "        argument. This (possibly new created) structs are then used for the replacement.")
	   (example "{(X --> A) (Y --> B)} (Q X (G X Y)) --> (Q A (G A B))"))
  (data~replace-free-variables-ntc object (subst~domain substitution) (subst~codomain substitution)
				   :destructive destructive
				   :downto downto
				   :replacers-downto replacers-downto
				   :test test))

	   
(defun subst~compose-subst-nso (outer-substitution inner-substitution
						      &key (destructive nil) (replacers-downto '(data+struct))
						      (downto '(data+primitive)) (test #'eq))
  (declare (edited  "04-NOV-1991 11:19")
           (authors RICHTS AMEIER)
           (input   "Two substitutions.")
	   (effect  "Depends on the keyword arguments DESTRUCTIVE and DOWNTO."
		    "are of one of this shared objects, the replacement is also done destructively on them.")
           (value   "OUTER-SUBSTITUTION is applied to the codomain of INNER-SUBSTITUTION and all domain-variables of"
                    "OUTER-SUBSTITUTION not in the domain of INNER-SUBSTITUTION are added with their codomain-terms:"
                    "        are added (together with there according domain variables) as new pairs.")
	   (example "{(Z --> A)} {(X --> A) (Y --> (F Z))} --> {(Z --> A) (Y --> (F A)) (X --> A)}"
		    "{(X --> A) (Y --> (F Z))} {(Z --> A)} --> {(Y --> (F Z)) (X --> A) (Z --> A)}"))
  (let* ((shared-outer-substitution (subst~create-nso (subst~domain outer-substitution)
						  (mapcar #'(lambda (codo-term)
							      (data~copy codo-term
									 :downto
									 replacers-downto
									 :explode nil
									 :preserve :all-classes))
							  (subst~codomain outer-substitution))))
	 (new-substitution (subst~apply-nso shared-outer-substitution
					     inner-substitution
					     :destructive destructive
					     :replacers-downto '(data+struct)
					     :downto downto
					     :test test)))
    (mapc #'(lambda (var term)
              (unless (member var (subst~domain new-substitution) :test #'data~equal)
		(subst~insert-component! var term new-substitution)))
          (subst~domain shared-outer-substitution)
	  (subst~codomain shared-outer-substitution))
    (if destructive
	(progn
	  (setf (subst~domain outer-substitution) (subst~domain new-substitution))
	  (setf (subst~codomain outer-substitution) (subst~codomain new-substitution))
	  outer-substitution)
      new-substitution)))		 


(defun subst~add-component-nso (variable term substitution
				     &key (destructive nil) (replacers-downto '(data+struct)) (downto '(data+primitive)) (test #'eq))
  (declare (edited  "04-NOV-1991 11:10")
           (authors RICHTS AMEIER)
           (input   "A variable, a term and a substitution (which may be nil).")
           (effect  "Depends on the keyword arguments DESTRUCTIVE and DOWNTO."
		    "If DESTRUCTIVE is true the codomain is changed destructively."
		    "If DESTRUCTIVE is nil, but DOWNTO is a list of Clos-Classes,"
		    "all objects of this classes are shared. So if some substructs are"
		    "of one of this shared objects, the replacement is also done"
		    "destructively on them.")
           (value   "A new substitution where the pair [VARIABLE --> TERM] is added"
                    "at the front of the domain and codomain lists"
                    "of SUBSTITUTION and is applied as a substitution to the codomain list."
		    "Remark: Which structs are shared depends also on the keyword argument REPLACERS-DOWNTO."
		    "        A copy of TERM is made one time, using data~copy with"
		    "        REPLACERS-DOWNTO as downto argument and :all-classes as preserve argument."
		    "        This (possibly new created) struct is then used for the replacement.")
	   (example "X B {(X --> F) (Y --> (F X))}"
		    "{(X --> B) (Y --> (F B)) (X --> F)}"))
  (subst~compose-subst-nso (subst~create-nso (list variable) (list term))
			      substitution
			      :destructive destructive
			      :replacers-downto replacers-downto
			      :downto downto
			      :test test))


(defun subst~create-nso (domain codomain &optional copy)
  (declare (edited  "04-NOV-1991 11:00")
           (authors GKLEIN RICHTS)
           (input   "A list of variables and a list of terms. Optional a flag.")
           (effect  "None.")
           (value   "The new substitution with the domain DOMAIN and the codomain CODOMAIN."
                    "If COPY = T the input-lists are copied.")
           (example "(x y) (a (f a)) --> {(X --> A) (Y --> (F A))}"))
  (let ((mapping (mapp~create domain codomain copy)))
    (cond
     ((notevery #'(lambda (item) (typep item 'data+variable)) domain)
      (error "the domain of a substitution has to consist of variables!"))
     
     (t
      (change-class mapping 'subst+substitution)))))


(defmethod data=replace-fv-nso ((substitution subst+substitution) bound variables structs destructive downto test)
  (declare (edited  "05-NOV-1991 10:32")
           (authors RICHTS)
           (input   "")
           (effect  "")
           (value   "replaced with the corresponding structs of STRUCTS."))
  (cond ((or (find 'subst+substitution downto) destructive)
	 ;; -> destructive replacement directly on substitution structure
	 (setf (subst~codomain substitution) (mapcar #'(lambda (term)
							 (data=replace-fv term
									  nil
									  variables structs
									  't
									  downto
									  test))
						     (subst~codomain substitution)))
	 substitution)
	(t
	 ;; -> destructive replacement possibly later							      
	 (let ((new-codomain (mapcar #'(lambda (struct)
					 (data=replace-fv struct
							  nil
							  variables structs
							  destructive
							  downto
							  test))
				     (subst~codomain substitution))))
	   (subst~create-nso (subst~domain substitution) new-codomain)))))

;;;;  the worst:
(eval-when (load compile eval)
  (setf data*global-type-var-subst (subst~create nil nil)))
