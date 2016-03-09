(in-package :omega)

(defmethod meth~subst-apply ((subst subst+substitution) (term term+schema))
  (term~schema-create (meth~subst-apply subst (data~schema-range term))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;condfuncs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun homo=test-sort (sort)
  (when (data~appl-p sort)
    (let ((defi (th~find-assumption (keim~name (data~appl-function sort))
				    (prob~theory omega*current-proof-plan))))
      (when defi
	(let* ((definiens (data~copy (th~ass-node defi) :downto '(term+constant
								type+primitive)))
	       (schema (if (data~schema-p definiens)
			   (data~schema-range definiens)
			 definiens))
	       (scopus (if (data~abstr-p schema)
			   (data~abstr-range schema)
			 schema)))
	  (when
	      (and  (logic~existential-quantification-p scopus)
		    (logic~equality-p (logic~quantification-scope scopus))
		    (= (length (data~appl-arguments scopus)) 2))
	    (beta~normalize
	     (term~appl-create definiens
			       (data~appl-arguments sort)))))))))

(meth~defcond newsort (args cmapp)  ;;sort newsort term newconst/var what
	     (declare )
	     (let ((expandsort (homo=test-sort (car args))))
	       (if expandsort
		   (let* ((expandsort (if (data~abstr-p expandsort)
					  (data~abstr-range expandsort)
					expandsort))
			  (newsort (second (data~appl-arguments expandsort)))
			  (newconst (if (eq (fifth args) 'const)
					(term~generate-term-primitive-with-new-name
					 'c (car (data~abstr-domain (term~type newsort)))
					 'term+constant (pds~environment
							 omega*current-proof-plan))
				      (meth=defn-newmetavar 'x (car
							    (data~abstr-domain
							     (term~type newsort))))))
			  (equality
			   (beta~normalize
			    (term~appl-create
			     (car (data~appl-arguments expandsort))
			     (list newconst))))
			  (side (if (term~variable-p (car (data~appl-arguments equality)))
				    (second (data~appl-arguments equality))
				  (car (data~appl-arguments equality)))))
		     (meth~mapp-extend-mapp cmapp (second args) newsort)
		     (meth~mapp-extend-mapp cmapp (third args) side)
		     (meth~mapp-extend-mapp cmapp (fourth args) newconst)
		     (meth~mapp-new-constraint cmapp t))
		 (let ((newconst (if (eq (fifth args) 'const)
				     (term~generate-term-primitive-with-new-name
				      'c (car (data~abstr-domain (term~type (car args))))
				      'term+constant (pds~environment
						      omega*current-proof-plan))
				   (meth=defn-newmetavar 'x  (car (data~abstr-domain (term~type (car args))))))))
		   (meth~mapp-extend-mapp cmapp (second args)(car args))
		   (meth~mapp-extend-mapp cmapp (third args) newconst)
		   (meth~mapp-extend-mapp cmapp (fourth args) newconst)
		   (meth~mapp-new-constraint cmapp t)))))
		 
		 
(meth~new-relational-function 'newsort)


(meth~defcond mymatch (args cmapp)
	      (declare)
	      (let (a)
;	      (omega~trace "~%myequal ~A ~%" args)
		   (meth~mapp-new-constraint cmapp (apply #'term~alpha-match args))))

  
(meth~defcond expandalldefis (args cmapp)  ;; oldterm newterm expand not-expand
	     (declare )
	     (let* ((oldterm (car args))
		    (expanded
		     (gentac=substitute-defis
		      oldterm
		      (mapcar #'(lambda (x)
				  (th~find-assumption x (prob~theory omega*current-proof-plan)))
						      (fourth args)))))
	     (meth~mapp-extend-mapp cmapp (second args)
				    (if (logic~conjunction-p expanded)
					(batac=split-on-and expanded)
				      (list expanded)))
	     (meth~mapp-new-constraint cmapp t)))
		
		 
(meth~new-relational-function  'expandalldefis)

		 


(meth~defcond termvar? (var cmapp)
	      (declare (edited  "03-NOV-1999")
		       (authors Gebhard)
		       (input   "A type")
		       (effect  "nil")
		       (value   "A new constant with the given type."))
	      (meth~mapp-new-constraint cmapp
					(and (term~variable-p (first var))
					     (not (meta~p var)))))

;(let ((newconsts))
;
;  (defun ilo=delete-constantlist ()
;    (setf newconsts nil))                      
;  
;  (defun ilo=show-constantlist ()
;     newconsts)

  (meth~deffun newconst (name type)
	     (declare )
	     (let ((new (term~generate-term-primitive-with-new-name name type 'term+constant
								    (pds~environment
								     omega*current-proof-plan))))
	       new))

;  (meth~deffun addtohack (thing)
;               (declare)
;               (progn
;                 (omega~trace "~%;;;;;;;;;;;;;;;;;~%;addtohack ~A~%;;;;;;;;;;;;;;;;;~%"
;                              (push  thing newconsts))
;                 thing))
;  
;
;  (meth~defcond hack-support (arg cmapp)
;                (declare (edited  "")
;                         (authors )
;                         (input   "")
;                         (effect  "")
;                         (value   ""))
;                  (let ((pool (pds~constraint-pool omega*current-proof-plan)))
;                    (when pool
;                      (let* ((sub  (pds~cstrpool-bindings pool))
;                             (node (pds~label2node (keim~name (car arg))))
;                             (mvs  (remove-if-not #'meta~p (data~all-substructs (node~formula node))))
;                             (consts
;                              (append
;                               (mapcan #'(lambda (subterm)
;                                           (when (some #'(lambda (c) (data~equal subterm c))
;                                                       newconsts)
;                                             (list subterm)))
;                                       (data~all-substructs (node~formula node))) ;get the consts of form 
;                               (mapcan #'(lambda (mv)
;                                           (mapcan #'(lambda (dom co)
;                                                       (when (eq mv dom) 
;                                                         (intersection (data~all-substructs co) newconsts)))
;                                                   (subst~domain sub)(subst~codomain sub))) mvs)))
;                             (supps (mapcan #'(lambda (sup)
;                                                (when (some #'(lambda (c) (data~substruct-positions c (node~formula sup)))
;                                                            consts)
;                                                  (list sup)))
;                                            (prob~proof-steps omega*current-proof-plan))))
;                        (omega~trace "~%;;;;;;;;;;;;;;;;;~%;mvs ~A consts ~A supp ~A~%;;;;;;;;;;;;;;;;;~%" mvs consts supps)
;                        (pds~add-sponsors node supps)
;                        (meth~mapp-new-constraint cmapp nil)))))
;
;) ;;;end local-global let

;	     (progn
;               (setf argi args)
;               (setf cmappi cmapp)
;               (when (cstr~binding-p (meth~mapp-constraint cmapp))
;                 (let* ((binding (cstr~arguments (meth~mapp-constraint cmapp)))
;                        (const (first args))
;                        (supps (mapcar #'(lambda (sym)
;                                           (some #'(lambda (dom)
;                                                     (omega~trace "~%~%HALLO: sym ~A dom ~A name ~A~%~%"
;                                                                  sym dom (keim~name dom))
;                                                     (when (equal (keim~name dom) sym)
;                                                       (mapp~get-component dom cmapp :mapp)))
;                                                 (mapp~domain (meth~mapp-mapp cmapp)))) 
;                                       (second args))))
;                   (omega~trace "~%~%cons ~A supp ~A binding ~A~%~%" const supps binding)
;                   (when (eq const (second binding))
;                     (mapc #'(lambda (node)
;                               (when (data~substruct-p (first binding) (node~formula node))
;                                 (pds~add-sponsors node supps)))
;                           (pds~open-nodes omega*current-proof-plan)))))
;               (meth~mapp-new-constraint cmapp T)))
		      
			     
(meth~defcond equality? (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Lassaad Jzimmer)
		       (input   "A term TERM.")
		       (effect  )
		       (value   "<T, mmapp> when TERM is an existential quantification."
				"<NIL, mmapp>, otherwise."))
	      (meth~mapp-new-constraint cmapp
					(frame~equality? (first args))))
(meth~defcond termequal? (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Lassaad Jzimmer)
		       (input   "A term TERM.")
		       (effect  )
		       (value   "<T, mmapp> when TERM is an existential quantification."
				"<NIL, mmapp>, otherwise."))
	      (meth~mapp-new-constraint cmapp
					(data~schema-equal
					 (if (symbolp (first args))
					     (env~lookup-object (first args)
								(pds~environment omega*current-proof-plan))
					   (first args))
					 (if (symbolp (second args))
					     (env~lookup-object (second args)
								(pds~environment omega*current-proof-plan))
					   (second args)))))


(meth~defcond add-hyps-to (args cmapp)
              (declare (edited  "11-AUG-1999")
                       (authors)
                       (input   "Two objects (OBJ1 and OBJ2).")
                       (effect  )
                       (value   "<T, mmapp> when OBJ1 and OBJ2 are keim~equal"
                                "<NIL, mmapp>, otherwise."))
	      (let* ((hyps (if (consp (First args)) args (list args)))
		     (line (second args))
		     (line-hyps (pdsn~hyps line))
		     (line-supp (pds~node-supports line)))
		(dolist (hyp hyps)
		  (when (not (member hyp line-hyps))
		    (push hyp (pdsn~hyps line))))
              (meth~mapp-new-constraint cmapp t)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;; ANDI-M
(infer~defmethod "AndI-m"
		 (outline-mappings (((existent nonexistent nonexistent) andi-m-b)
				    ((existent existent existent) andi-m-a)
				    ((nonexistent closed closed) andi-m-f)))
		 (help "The method for AND introduction"))

;;;was and-intro
(meth~defmethod ;;method name inference name
                andi-m-b AndI-m
		(in frame)
		(rating 10)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  ;;(meta-var typ sort)
		  (phi o) (psi o)
		  )
		 )
		(premises (+ l1) (+ l2))
		(conclusions (- l3))
		(decl-content
		 (l1 () phi ("Open" () ()))
		 (l2 () psi ("Open" () ()))
		 (l3 () (and phi psi) ("AndI" () (l1 l2)))
		 )
		(proc-content schema-interpreter)
		(remark "Backward applying the AND introduction")
		)




;;;ANDE-M:
(infer~defmethod "AndE-m"
		 (outline-mappings (((existent existent existent) ande-m-a)
				    ((nonexistent nonexistent existent) ande-m-f)
				    ((nonexistent nonexistent closed) ande-m-f)))
		 (help "The method for AND elimination"))


(meth~defmethod ande-m-f AndE-m
		(in frame)
		(rating 0.4)
		(reasoning :normalizing)
		(declarations
		 (sorted-meta-variables
		  ;;(meta-var typ sort)
		  (phi o) (psi o)
		  )
		 )
		(premises (- l1))
		(conclusions (+ l2) (+ l3))
		(decl-content
		 (l1 () (and phi psi))
		 (l2 () phi ("AndEL" () (l1))) 
		 (l3 () psi ("AndER" () (l1))) 
		 )
		(proc-content schema-interpreter)
		(remark "Forward application of AND elimination")
		)



(infer~defmethod Reflexu-m
                (outline-mappings (((existent) reflexu-m-b)))
                (help "Closes a goal of the form 'a = a'"))

(meth~defmethod reflexu-m-b reflexu-m
               (in frame)
               (rating 50)
               (reasoning :planning :middle-out)
               (declarations
                (type-variables aa)
                (sorted-meta-variables
                 (phi aa term)
                 (psi aa term)))
               
               (premises )
               (conclusions (- L1))
               
               (application-condition
                (unify phi psi))
               
               (decl-content
                (l1 () (= phi psi) ("=ref" (phi) ())))
	       (manual (author "AMeier")
		       (examples "ZMZ, Homo")))




(infer~defmethod "element-of-domain-m"
                (outline-mappings (
                                   ((existent existent nonexistent) element-of-domain-m-f)
                                  ; ((existent existent nonexistent) element-of-domain-m-b2)
                                  ; ((existent existent nonexistent) element-of-domain-m-f)
				  ))
                (help ""))

;(meth~defmethod element-of-domain-m-f element-of-domain-m
;               (in frame)
;               (rating 10)
;               (reasoning :planning :middle-out)
;               (declarations
;                (type-variables aaa bbb)
;                (sorted-meta-variables
;                 (conc (o aaa) term)             (image-expand o term)  
;                 (dom (o aaa) term)              (ran (o aaa) term)
;                 (ranelem bbb const)             (domelem aaa metavar)
;                 (sub o sub)
;                 (lhs aaa metavar)               (rhs aaa metavar)
;                 (vars o termlist)               (fun (bbb aaa) term)))
;
;               (premises  (- l05))
;               (conclusions (+ l10)(+ l07))
;               
;               (application-condition
;                (mand
;                 (alpha-matcher (:term (= lhs rhs)) conc sub)
;                 (mnot (mand (var-in (subst-apply sub lhs)) (var-in (subst-apply sub rhs))))
;                 (mbind domelem (newconst (:symbol domel) aaa))
;                 (mor (unify  conc  (:term (= (fun domelem) ranelem)))
;                      (unify  conc  (:term (= ranelem  (fun domelem)))))
;                 (th-restricted-definition (:symbol :base) image image-def)))
;
;                (outline-computations
;                 (image-expand (beta-normalize (appl-create image-def (mlist fun dom ranelem)))))
;
;               (decl-content 
;                (l05 () (IMAGE FUN dom ranelem))
;                (l07 () (dom domelem)  ("solved" () (l05))) 
;                (l10 () conc           ("solved" () (l05))) 
;               ))

(meth~defmethod element-of-domain-m-f element-of-domain-m
               (in frame)
               (rating 10)
               (reasoning :planning :middle-out)
               (declarations
                (type-variables aaa bbb)
                (sorted-meta-variables
		 (conc o term)             (image-expand o term)  
		 (dom (o aaa) term)		 (ran (o aaa) term)
		 (ranelem bbb const)		 (domelem aaa metavar)
		 (sub o sub)
		 (lhs aaa metavar)		 (rhs aaa metavar)
		 (vars o termlist)		 (fun (bbb aaa) term)))

               (premises  (- l05)(+ l07))
               (conclusions (- l10))
	       
               (application-condition)
;		(th-restricted-definition (:symbol :base) image image-def))

	       (expansion-computations		)

	       (outline-computations
		(domelem (type-newconst aaa)))  ;;;(newconst (:symbol domel) aaa)))
		
               (decl-content 
		(l00 () (and (dom domelem) (= ranelem (fun domelem)))   ("Hyp" () ()))
		(l05 () (IMAGE FUN dom ranelem)              )
		(p10 () (exists-sort (lam (x aaa) (= ranelem (fun x))) dom ) ("defne" (image image-def)(l05)))
		(l07 (l00) conc                                         ("open" () ())) 
                (l10 () conc                                            ("existse-sort" (domelem) (p10 l07))) 
               )
	       (remark ;local short, long
		("<U>Element of domain:</U><BR>
<TERM>ranelem</TERM> is an element of <TERM>fun</TERM>(<TERM>dom</TERM>), therefore
there exists a <TERM>domelem</TERM> in <TERM>dom</TERM> with <TERM>(= ranelem (fun domelem))</TERM>."
	       "")
	      ;global text, constr
		("<TERM>ranelem</TERM> is an element of <TERM>fun</TERM>(<TERM>dom</TERM>), therefore
there exists a <TERM>domelem</TERM> in <TERM>dom</TERM> with <TERM>(= ranelem (fun domelem))</TERM>.<BR>
<LISP>(verbalize-text-next l07)</LISP>"
		 "<TERM>ranelem</TERM> is an element of <TERM>fun</TERM>(<TERM>dom</TERM>), therefore
there exists a <TERM>domelem</TERM> in <TERM>dom</TERM> with <TERM>(= ranelem (fun domelem))</TERM>.<BR>
<LISP>(verbalize-cons-next l07)</LISP>")))




;(meth~defmethod element-of-domain-m-f element-of-domain-m
;               (in frame)
;               (rating 10)
;               (reasoning :planning :middle-out)
;               (declarations
;                (type-variables aaa bbb)
;                (sorted-meta-variables
;                 (conc (o aaa))          
;                 (dom (o aaa) term)              (ran (o aaa) term)
;                 (ranelem bbb term)              (domelem aaa metavar)
;                 (sub o sub) 
;                 (lhs aaa metavar)               (rhs aaa metavar)
;                 (meta aaa metavar)              (vars o termlist)
;                 (fun (bbb aaa) term)))
;
;               (premises  l05 (+ l07))
;               (conclusions (- l10))
;               
;               (application-condition
;                (mand
;                 (alpha-matcher (:term (= lhs rhs)) conc sub)
;                 (mnot (mand (var-in (subst-apply sub lhs)) (var-in (subst-apply sub rhs))))
;                 (mbind domelem (newconst (:symbol domelem) aaa))
;                 (mor (unify  conc (:term (= (fun domelem) ranelem)))
;                      (unify conc  (:term (= ranelem  (fun domelem)))))))
;                       
;               (outline-computations)
;                
;               (decl-content 
;                (l05 () conc)
;                (l07 () (dom domelem)    ("open" ()())) 
;                (l10 () (IMAGE FUN dom ranelem)) ("solved" () (l05 l07))) 
;                )



(infer~defmethod "neutral-in-group-m"
                 (outline-mappings (((existent existent) "neutral-in-group-m-c"))))

(meth~defmethod neutral-in-group-m-c neutral-in-group-m
               (in frame)

               (rating 10)
               (reasoning :planning :middle-out)

	       (declarations
		 (type-variables aaa bbb)
		 (sorted-meta-variables
		  (G (o aaa) term)
		  (op (aaa aaa aaa) term)
                 ))

               (premises  l00)

               (conclusions (- l10))

               (decl-content
                (l00 () (group G op))
                (l10 () (G (group-unit G op)) ("solved" () (l00)))
                ))

(infer~defmethod "inverse-in-group-m"
                 (outline-mappings (((existent existent nonexistent) "inverse-in-group-m-b"))))

(meth~defmethod inverse-in-group-m-b inverse-in-group-m
               (in frame)

               (rating 10)
               (reasoning :planning :middle-out)

	       (declarations
		 (type-variables aaa bbb)
		 (sorted-meta-variables
		  (G (o aaa) term)
		  (elem aaa term)
		  (op (aaa aaa aaa) term)
                 ))

               (premises  l00  (+ l05))

               (conclusions (- l10))

               (decl-content
                (l00 () (group G op))
		(l05 () (G elem) ("open" ()()))
                (l10 () (G (group-inverse G op elem)) ("solved" () (l00 l05)))
                ))


(infer~defmethod "subset-image-range-m"
                 (outline-mappings (((existent existent nonexistent) subset-image-range-m-b))))

(meth~defmethod subset-image-range-m-b subset-image-range-m
               (in frame)

               (rating 10)
               (reasoning :planning :middle-out)

	       (declarations
		 (type-variables aaa bbb)
		 (sorted-meta-variables
		  (const bbb term)
		  (dom (o aaa) term)
		  (ran (o bbb) term)
		  (fun (bbb aaa) term)
		  (op (aaa aaa aaa) term)
		  (op1 (bbb bbb bbb) term)
;                  (subst o sub)
;                  (subst1 o sub)
;                  (dummy (o o o) metavar)
		  (any o term)
		  (phi bbb term)
                 ))

               (premises  l00 (+ l05))

;               (application-condition
;                (unify phi const))
	       
               (conclusions (- l10))

               (decl-content
                (l00 () (HOMOMORPHISM dom OP ran OP1 fun))
                (l05 () (IMAGE FUN dom const) ("open" ()()))
                (l10 () (ran const) ("solved" () (l00 l05)))
                ))


(infer~defmethod "group-closed-m"
		 (outline-mappings (((existent existent nonexistent nonexistent) group-closed-m-b)))
                 (help ""))

(meth~defmethod group-closed-m-b group-closed-m
               (in frame)
               (rating 100)
               (reasoning :planning :middle-out)
               (declarations
		 (type-variables aaa bbb)
		 (sorted-meta-variables
		  (grp (o aaa) term)		  (op (aaa aaa aaa) term)
		  (elem1 aaa term)		  (elem2 aaa term)
		  (conc o term)    		  (hyps o prlnlist)
		  (sub o subst)                   (tl list termlist)
		  (group-expand o term)		  (closed-expand o term)
                 ))

               (premises l00 (+ l01) (+ l02))
               (conclusions (- l10))

	       (application-condition)
		;(mand ;(mbind elem1 (newmetavar (:symbol celem) aaa))
		      ;(mbind elem2 (newmetavar (:symbol celem) aaa))
		      ;(unify (:term (grp (op elem1 elem2))) conc)
		 ;(th-restricted-definition (:symbol :base) group group-def)
		 ;(th-restricted-definition (:symbol :base) closed-under closed-def)))

	       (outline-computations)
                 ;(group-expand (beta-normalize (appl-create group-def (mlist grp op))))
                 ;(closed-expand (beta-normalize (appl-create closed-def (mlist grp op))))
		 ;(tl (mlist elem1 elem2)))

               (decl-content
                (l00 () (group grp op))
		(l01 () (grp elem1) ("open" () ()))
		(l02 () (grp elem2) ("open" () ()))
	        (l10 () (grp (op elem1 elem2)) ("foralle-sort*" (tl) (l00 l01 l02)))

;                (p10 () group-expand                                  ("defne" (group group-def)(l00)))
;                (p20 ()  (and (closed-under grp op)
;                              (associative grp op))                   ("andel" ()(p10)))
;                (p30 ()  (closed-under grp op)                        ("andel" ()(p20)))
;                (p40 () closed-expand                                 ("defne" (closed-under closed-def)(p30)))
                )
	       
	       (remark ;local short, long
		("<U>Group-Closed:</U><BR>
<TERM>(in (op elem1 elem2) grp)</TERM> follows from <TERM>(in elem1 grp)</TERM> and <TERM>(in elem1 grp)</TERM>
because the group operation <TERM>op</TERM> is closed on <TERM>grp</TERM>."
	       "")
	      ;global text, constr
		("Since <TERM>elem1</TERM> and <TERM>elem2</TERM> are elements of <TERM>grp</TERM>
<TERM>(op elem1 elem2)</TERM> is an element of <TERM>grp</TERM>."
		 "<TERM>(op elem1 elem2)</TERM> is an element of <TERM>grp</TERM>, if
<TERM>elem1</TERM> and <TERM>elem2</TERM> are elements of <TERM>grp</TERM>."
	       )))


(infer~defmethod "group-neut-m"
		 (outline-mappings (((existent existent nonexistent) group-neut-m-b)))
                 (help ""))

(meth~defmethod group-neut-m-b group-neut-m
               (in frame)
               (rating 100)
               (reasoning :planning :middle-out)
               (declarations
		 (type-variables aaa bbb)
		 (sorted-meta-variables
		  (grp (o aaa) term)		  (op (aaa aaa aaa) term)
		  (elem1 aaa term)		  (neut aaa term)
		  (conc o term)		          (sub o subst)
                 ))

               (premises l00 (+ l10))

	       (application-condition
		(mand (mbind neut (:Term (group-unit grp op))) 
		      (mbind elem1 (newmetavar (:symbol elem) aaa))
		      (mor (unify  (:term (= (op neut elem1) elem1)) conc)
			   (unify  (:term (= elem1 (op neut elem1))) conc)
			   (unify  (:term (= elem1 (op elem1 neut))) conc)
			   (unify  (:term (= (op elem1 neut) elem1)) conc))))

	       (outline-computations)
	       
               (conclusions (- l20))

               (decl-content
                (l00 () (group grp op) )
		(l10 () (grp elem1) ("open" ()()))
	        (l20 () conc  ("solved" () (l00 l10)))
                ))

(infer~defmethod "group-inv-m"
		 (outline-mappings (((existent existent nonexistent) group-inv-m-b)))
                 (help ""))

(meth~defmethod group-inv-m-b group-inv-m
		(in frame)
		(rating 100)
		(reasoning :planning :middle-out)
		(declarations
		 (type-variables aaa bbb)
		 (sorted-meta-variables
		  (grp (o aaa) term)		  (op (aaa aaa aaa) term)
		  (lhs aaa term)		  (rhs aaa term)
		  (elem aaa term) (el aaa metavar) (elem-elem aaa term)
		  (inv aaa term)
		  (neut aaa metavar)              (neut-elem o term)
		  (conc o term)                   (neut-pre o term)
		  (sub o subst) 	     
		  ))

		(premises l00 (+ l10))
	       
                (application-condition
		 (mand 
		  (mbind neut-elem (:term  (group-unit grp op)))  
		  (mbind elem (newmetavar (:symbol elem) aaa))
		  (mbind inv (:term (group-inverse grp op elem)))   
		  (mor (unify  (:term (= neut-elem (op elem inv))) conc)
		       (unify  (:term (= neut-elem (op inv elem))) conc)
		       (unify  (:term (= (op inv elem) neut-elem)) conc)
		       (unify  (:term (= (op elem inv) neut-elem)) conc))))
		
	       (conclusions (- l20))

	       (outline-computations)

		(decl-content
		 (l00 () (group grp op))
		 (l10 () (grp elem) ("open" ()()))
		 (l20 () conc    ("solved" () (l00 l01 )))
		 ))

(infer~defmethod "group-assoc-m"
		 (outline-mappings (((existent existent nonexistent nonexistent nonexistent) group-assoc-m-b)))
                 (help ""))

(meth~defmethod group-assoc-m-b group-assoc-m
               (in frame)
               (rating 100)
               (reasoning :planning :middle-out)
               (declarations
		 (type-variables aaa bbb)
		 (sorted-meta-variables
		  (grp (o aaa) term)
		  (op (aaa aaa aaa) term)
		  (elem1 aaa metavar)
		  (elem2 aaa metavar)
		  (elem3 aaa metavar)
		  (e1 aaa term)
		  (e2 aaa term)
		  (e3 aaa term)
		  (conc o term)
		  (sub o subst)
                 ))

               (premises l00 (+ l01)(+ l02)(+ l03))

	       (application-condition
		(mor (alpha-matcher (:term (= (op elem1 (op elem2 elem3))
					      (op (op elem1 elem2) elem3)))
					   conc sub)
		     (alpha-matcher (:term (= (op (op elem1 elem2) elem3)
					      (op elem1 (op elem2 elem3))))
					   conc sub)))

	       (outline-computations
		(e1 (subst-apply sub elem1))
		(e2 (subst-apply sub elem2))
		(e3 (subst-apply sub elem3)))
	       
               (conclusions (- l10))

               (decl-content
                (l00 () (group grp op))
		(l01 () (grp e1) ("open" ()()))
		(l02 () (grp e2) ("open" ()()))
		(l03 () (grp e3) ("open" ()()))
	        (l10 () conc ("solved" () (l00 l01 l02 l03)))
                ))


(infer~defmethod "homomorphism-m"
                 (outline-mappings (((existent existent nonexistent nonexistent nonexistent nonexistent nonexistent) homomorphism-m-b)))
                 (help ""))

(meth~defmethod homomorphism-m-b homomorphism-m
             (in frame)
             (rating 10)
             (reasoning :planning :middle-out)
             (declarations
               (type-variables aaa bbb)
               (sorted-meta-variables
		(dom (o aaa) term)                    (ran (o bbb) term)
                (fun (bbb aaa) term)
                (opdom (aaa aaa aaa) term)            (opran (bbb bbb bbb) term)
	        (pos o pos)                           (thepos o pos)
                (de1 aaa term)                        (de2 aaa term)
                (re1 bbb term)                        (re2 bbb term)
                (remv1 bbb metavar)                        (remv2 bbb metavar)
                (conc o term)
                (replaced-hom-conc o term)
                (replaced-re1-conc o term)
                (replaced-re2-conc o term)
               ))

             (premises l00 (+ l01) (+ l02) (+ l10) (+ l20) (+ l30))

             (application-condition
	      (mand
	       (equality? conc)
	       (match-and-bind-subterm (:term (opran remv1 remv2)) conc subst pos)))
;	       (termoccs opran conc pos)
;	       (mbind thepos (posbutlast (mfirst pos)))
;	       (appl-p (termatpos conc thepos))

;               (mbind re1 (subst-apply subst ranelem1))
;               (mbind re2 (subst-apply subst ranelem2))
;               (mif (mand (appl-p re1) (mequal (applfunc re1) fun))
;                    (mbind de1 (mfirst (applargs re1)))
;                    (mbind de1 (NewMetaVarForVar domelem1)))
;               (mif (mand (appl-p re2) (mequal (applfunc re2) fun))
;                    (mbind de2 (mfirst (applargs re2)))
;                    (mbind de2 (NewMetaVarForVar domelem2)))))

	     (outline-computations
              ;(de1 (NewMetaVarForVar domelem1))
              ;(de2 (NewMetaVarForVar domelem2))
               ( re1 (subst-apply subst remv1))
               ( re2 (subst-apply subst remv2))
              ;(re1 (mfirst (applargs (termatpos conc thepos))))
              ;(re2 (msecond (applargs (termatpos conc thepos))))
              (de1 (if (mand (appl-p re1) (mequal (applfunc re1) fun))
                       (mfirst (applargs re1))
                     (NewMetaVar (:symbol :dom)  aaa)))
              (de2 (if (mand (appl-p re2) (mequal (applfunc re2) fun))
                       (mfirst (applargs re2))
                     (NewMetaVar (:symbol :dom)  aaa)))
	      (replaced-re1-conc (termrploccs  conc re1 (:term (fun de1))))
	      (replaced-re2-conc (termrploccs  replaced-re1-conc re2 (:term (fun de2))))
	      (replaced-hom-conc (termrplatpos replaced-re2-conc pos (:term (fun (opdom de1 de2))))))


	     (conclusions (- l100))

	     (outline-orderings (before l20 l10)(before l30 l10))

             (decl-content
              (l00 ()  ( homomorphism  dom opdom ran opran fun))
              (l01 ()  (dom de1)              ("open" () ()))
              (l02 ()  (dom de2)             ("open" () ()))
              (l10 ()  replaced-hom-conc               ("open" () ()))
	      (l20 ()  (= (fun de2) re2)        ("open" () ()))						 
	      (l30 ()  (= (fun de1) re1)        ("open" () ())) 
              (l100 () conc ("solved" () (l00 l10 l20 l30 l01 l02)))
              )
	     (remark ;local short, long
	      ("<U>Homomorphism:</U><BR>
The homomorphism equation<BR>
<TAB><TERM>(= (fun (opdom de1 de2)) (opran (fun de1)(fun de2)))</TERM><BR>
is applied to<BR>
<TAB><TERM>conc</TERM><BR>
resulting in<BR>
<TAB><TERM>replaced-hom-conc</TERM>.<BR>
It has to be shown that <TERM>re1</TERM> and  <TERM>re2</TERM> are in 
<TERM>fun</TERM>(<TERM>dom</TERM>)."
	       "<U>Homomorphism (detailed):</U><BR>
The homomorphism equation<BR>
<TAB><TERM>(= (fun (opdom de1 de2))(opran (fun de1)(fun de2)))</TERM><BR>
is applied to the goal<BR>
<TAB><TERM>conc</TERM><BR>
resulting in the new open goal<BR>
<TAB><TERM>replaced-hom-conc</TERM>.<BR>
For the application of the homomorphism equation, it has to be shown that<BR>
<TAB><TERM>(= (fun de1) re1)</TERM> and <TERM>(= (fun de2) re2)></TERM><BR>
for elements <TERM>de1</TERM>, <TERM>de2</TERM> of <TERM>dom</TERM>, that is,
<TERM>re1</TERM> and <TERM>re1</TERM> have to be
elements of the image <TERM>fun</TERM>(<TERM>dom</TERM>).")
	      ;global text, constr
	      ("The homomorphism equation<BR>
<TAB><TERM>(= (fun (opdom de1 de2)) (opran (fun de1)(fun de2)))</TERM><BR>
is applied to<BR>
<TAB><TERM>replaced-hom-conc</TERM><BR>
results in<BR>
<TAB><TERM>conc</TERM>,<BR>
where <TERM>(= (fun de1) re1)</TERM> and <TERM>(= (fun de2) re2)</TERM>.<BR>
<LISP>(verbalize-text-next l10)</LISP>"
	       "The homomorphism equation<BR>
<TAB><TERM>(= (fun (opdom de1 de2)) (opran (fun de1)(fun de2)))</TERM><BR>
is applied to<BR>
<TAB><TERM>conc</TERM><BR>
resulting in<BR>
<TAB><TERM>replaced-hom-conc</TERM>.<BR>
It has to be shown that <TERM>re1</TERM> and  <TERM>re2</TERM> are in 
<TERM>fun</TERM>(<TERM>dom</TERM>).<BR>
<LISP>(verbalize-cons-next l10)</LISP>"
	       )))


(infer~defmethod "equal-subst-m" 
		 (outline-mappings (((existent existent) equal-subst-m-b)))
		 (parameter-types position)
		 (help "Controlled application the =subst tactic."))

(meth~defmethod equal-subst-m-b equal-subst-m
		(in frame)
		(rating 0)

		(declarations
		 (type-variables aa)
		 (sorted-meta-variables
		  (New-Lines o prlnlist)  (New-prem o prln)
		  (f o)		          (position o pos)
		  (phi aa)                (phi-prime aa)))

		(parameters position)
		
		(premises (+ new-prem) l2)
		(application-condition
		 (mand (bound-p position)
		       (test-tactic (:symbol :=subst) (mlist l1 (mnil) l2) (mlist position))
		       ))
		(conclusions (- l1))
		(outline-computations
		 (new-lines (apply-tactic (:symbol :=subst) (mlist l1 (mnil) l2) (mlist position)))
		 (new-prem (msecond new-lines)))

		(decl-content 
		 (l2 () (= phi phi-prime))
		 (l1 () F ("=subst" () ())))
		(proc-content apply-tactic))

(infer~defmethod equal-func-m    
                (outline-mappings
                                   (((existent nonexistent) equal-func-m-b)))
                (help "The application of an equation."))

(meth~defmethod  equal-func-m-b  equal-func-m
                (in frame)
               (rating 45)
               (reasoning :planning :middle-out)
               (declarations
                (type-variables aaa bbb)
                (sorted-meta-variables
                 (lhs aaa term)
                 (rhs  aaa term)
                 (f (bbb aaa) term)
                 )
                )
               
               (premises (+ L10))
               
               (outline-computations)

               (conclusions (- L30))
               
               (decl-content
                (L10 () (= rhs lhs) ("open" ()()))
                (L30 () (= (f rhs) (f lhs))           ("closed" () (L10)))
                ))


(infer~defmethod equal-ref-m    
		 (outline-mappings
				    (((existent) equal-ref-m-c)))
		 (help "The application of an equation."))

(meth~defmethod  equal-ref-m-c equal-ref-m
		 (in frame)
		(rating 45)
		(reasoning :planning :middle-out)
		(declarations
		 (type-variables aaa bbb)
		 (sorted-meta-variables
		  (lhs aaa term)
		  (rhs  aaa term)
		  )
		 )
		
		(application-condition
		 (unify rhs lhs))
		 
		
		(outline-computations)

		(conclusions (- L30))
		
		(decl-content
		 (L30 () (= rhs lhs)           ("=ref" (rhs) ()))
		 ))




(infer~defmethod xweaken-m    
		 (outline-mappings (((existent existent) weaken-m-c)))
		 (help "The application of an weaken."))

(meth~defmethod weaken-m-c xweaken-m
		 (in frame)
		(rating 45)
		(reasoning :planning :middle-out)
		(declarations
		 (type-variables aaa)
		 (sorted-meta-variables
		  (term1 o term)
		  (term2 o term)
		  (rhs1 aaa metavar)
		  (rhs2 aaa metavar)
		  (lhs1 aaa metavar)
		  (lhs2 aaa metavar)
		  (sub o subst)
		  )
		 )
		
		(premises L10)
		
		(application-condition
		 (mor (unify term1 term2)
		       (mand (equality? term1)
			     (equality? term2)
			     (unify (mfirst (applargs term1))(msecond (applargs term2)))
			     (unify (mfirst (applargs term2))(msecond (applargs term1))))))

		(conclusions (- L30))
		
		(decl-content
		 (L10 () term1 )
		 (L30 () term2  ("weaken" () (L10)))
		 ))



;;;;; kernel-methods


(infer~defmethod "homomorphism-on-domain-m"
                 (outline-mappings (((existent existent nonexistent) homomorphism-on-domain-m-b)))
                 (help ""))

(meth~defmethod homomorphism-on-domain-m-b homomorphism-on-domain-m
             (in frame)
             (rating 10)
             (reasoning :planning :middle-out)
             (declarations
               (type-variables aaa bbb)
               (sorted-meta-variables
		(dom (o aaa) term)                    (ran (o bbb) term)
                (fun (bbb aaa) term)
                (opdom (aaa aaa aaa) term)            (opran (bbb bbb bbb) term)
	        (pos o pos)                           (subst o sub)
                (ranelem1 bbb metavar)                (ranelem2 bbb metavar)
                (domelem1 aaa metavar)                (domelem2 aaa metavar)
                (de1 aaa term)                        (de2 aaa term)
                (re1 bbb term)                        (re2 bbb term)
                (conc o term)
                (replaced-hom-conc o term)
               ))

             (premises l00 (+ l10))

             (application-condition
	      (mand
	       (equality? conc)
	       (match-and-bind-subterm (:term (fun (opdom domelem1 domelem2))) conc subst pos)
	       (mbind de1 (subst-apply subst domelem1))
	       (mbind de2 (subst-apply subst domelem2))))
	     
             (outline-computations
	      (replaced-hom-conc (termrplatpos conc pos (:term (opran (fun de1)(fun  de2))))))

	     (conclusions (- l100))

             (decl-content
              (l00 ()  ( homomorphism  dom opdom ran opran fun))
              (l10 ()  replaced-hom-conc               ("open" () ()))
              (l100 () conc ("solved" () (l00 l10 l20 l30)))
              ))

(infer~defmethod "image-of-neut-m"
                 (outline-mappings (((existent existent nonexistent) image-of-neut-m-b)))
                 (help ""))

(meth~defmethod image-of-neut-m-b image-of-neut-m
             (in frame)
             (rating 10)
             (reasoning :planning :middle-out)
             (declarations
               (type-variables aaa bbb)
               (sorted-meta-variables
		(dom (o aaa) term)                    (ran (o bbb) term)
                (fun (bbb aaa) term)
                (opdom (aaa aaa aaa) term)            (opran (bbb bbb bbb) term)
	        (pos o pos)                           (subst o sub)
                (conc o term)
                (replaced-hom-conc o term)
               ))

             (premises l00 (+ l10))

             (application-condition
	      (mand
	       (equality? conc)
	       (match-and-bind-subterm (:term (fun (group-unit dom opdom))) conc subst pos)))
	     
             (outline-computations
	      (replaced-hom-conc (termrplatpos conc pos (:term (group-unit ran opran)))))

	     (conclusions (- l100))

             (decl-content
              (l00 ()  ( homomorphism  dom opdom ran opran fun))
              (l10 ()  replaced-hom-conc               ("open" () ()))
              (l100 () conc ("solved" () (l00 l10 l20 l30)))
              ))

(infer~defmethod "inv-of-neut-m"
                 (outline-mappings (((existent existent nonexistent) inv-of-neut-m-b)))
                 (help ""))

(meth~defmethod inv-of-neut-m-b inv-of-neut-m
             (in frame)
             (rating 10)
             (reasoning :planning :middle-out)
             (declarations
               (type-variables aaa bbb)
               (sorted-meta-variables
		(dom (o aaa) term)                    (ran (o bbb) term)
                (fun (bbb aaa) term)
                (opdom (aaa aaa aaa) term)            (opran (bbb bbb bbb) term)
	        (pos o pos)                           (subst o sub)
                (conc o term)
                (replaced-hom-conc o term)
               ))

             (premises l00 (+ l10))

             (application-condition
	      (mand
	       (equality? conc)
	       (match-and-bind-subterm (:term (group-inverse dom opdom (group-unit dom opdom))) conc subst pos)))
	     
             (outline-computations
	      (replaced-hom-conc (termrplatpos conc pos (:term (group-unit dom opdom)))))

	     (conclusions (- l100))

             (decl-content
              (l00 ()  (group dom opdom))
              (l10 ()  replaced-hom-conc               ("open" () ()))
              (l100 () conc ("solved" () (l00 l10 l20 l30)))
              ))

(infer~defmethod "image-of-inv-m"
                 (outline-mappings (((existent existent nonexistent) image-of-inv-m-b)))
                 (help ""))

(meth~defmethod image-of-inv-m-b image-of-inv-m
             (in frame)
             (rating 10)
             (reasoning :planning :middle-out)
             (declarations
               (type-variables aaa bbb)
               (sorted-meta-variables
		(dom (o aaa) term)                    (ran (o bbb) term)
                (fun (bbb aaa) term)                  
                (opdom (aaa aaa aaa) term)            (opran (bbb bbb bbb) term)
	        (pos o pos)                           (subst o sub)
		(elem aaa metavar)                    (real-elem aaa term)
                (conc o term)
                (replaced-hom-conc o term)
               ))

             (premises l00 (+ l10))

             (application-condition
	      (mand
	       (equality? conc)
	       (match-and-bind-subterm (:term (fun (group-inverse dom opdom elem))) conc subst pos)
	       (mbind real-elem (subst-apply subst elem)))
	      )
	     
             (outline-computations
	      (replaced-hom-conc (termrplatpos conc pos (:term (group-inverse ran opran (fun real-elem))))))

	     (conclusions (- l100))

             (decl-content
              (l00 ()  ( homomorphism  dom opdom ran opran fun))
              (l10 ()  replaced-hom-conc               ("open" () ()))
              (l100 () conc ("solved" () (l00 l10 l20 l30)))
              ))

(infer~defmethod "element-of-kernel-m"
                (outline-mappings (
                                   ((nonexistent existent) element-of-kernel-m-f)))
                (help ""))

(meth~defmethod element-of-kernel-m-f element-of-kernel-m
               (in frame)
               (rating 10)
               (reasoning :planning :middle-out)
               (declarations
                (type-variables aaa bbb)
                (sorted-meta-variables
		 (conc o term)		 (fun (bbb aaa) term)
		 (dom (o aaa) term)      (ran (o bbb) term)
		 (delem aaa term)        (relem bbb term)))

               (premises (- l00))
               (conclusions (+ l05))
	       
               (application-condition)

	       (outline-computations)

               (decl-content
		(l00 () (URBILD FUN dom relem delem))
		(l05 () (and (dom delem) (= (fun delem) relem)) ("closed" () (l00)))
               ))

(infer~defmethod "subset-kernel-domain-m"
                 (outline-mappings (((existent existent nonexistent) subset-kernel-domain-m-b))))

(meth~defmethod subset-kernel-domain-m-b subset-kernel-domain-m
               (in frame)

               (rating 10)
               (reasoning :planning :middle-out)

	       (declarations
		 (type-variables aaa bbb)
		 (sorted-meta-variables
		  (const bbb term)
		  (dom (o aaa) term)
		  (ran (o bbb) term)
		  (op (aaa aaa aaa) term)
		  (op1 (aaa aaa aaa) term)
		  (fun (bbb aaa) term)
                 ))

               (premises  l00 (+ l05))

               (application-condition
		(mfalse));                (unify phi const))
	       
               (conclusions (- l10))

               (decl-content
                (l00 () (HOMOMORPHISM  dom OP ran OP1 fun))
                (l05 () (URBILD FUN dom (GROUP-UNIT ran OP1) const) ("open" ()()))
                (l10 () (dom const) ("solved" () (l00 l05)))
                ))

(infer~defmethod "Apply-Assertion-m"
		 (outline-mappings (((EXISTENT EXISTENT NONEXISTENT) Apply-Assertion-m-b))
				   ))

(meth~defmethod Apply-Assertion-m-b Apply-Assertion-m
		(in frame)
		(rating 3)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  (psi o term)
		  (phi o term)
		  (psi-prime o term)
		  (pformulas list termlist)
		  (prems o prlnlist)) 
		 )
		
		(premises l10 (+ prems))
		(conclusions (- l30))

		(application-condition
		 (mor 
		      (myassert phi psi pformulas) ;assertion
		      (mand (equality? psi)          ;assertion with =sym
			    (myassert phi
				      (appl-create (:term =)
						   (mlist
						    (msecond (applargs psi))(mfirst (applargs psi))))
			    pformulas))))

		
		(outline-computations
		 (prems (if (mequal pformulas (mnil))
			    (mnil)
			  (premises-for l30 pformulas))))

		(decl-content
		 (l10 () phi)
		 (l30 () psi ("Assertion" () (l10 prems)))
		 )
		
		)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TWO ADDITIONAL METHODS FOR PRESENTATION THAT CONVERT (in ITEM SET) zu (beta-normalize (set item))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod "Expand-in-m"
		 (outline-mappings (((existent nonexistent) Expand-in-m-b)
				    ((nonexistent existent) Expand-in-m-f)))
		 (help "The method for expanding the in."))


(meth~defmethod Expand-in-m-b Expand-in-m
		(in frame)
		(rating 10)
		(reasoning :planning)
		(declarations
		 (type-variables aa)
		 (sorted-meta-variables
		  (phi o) 
		  (set (o aa))
		  (a aa)))
		
		(premises (+ l0))
		(conclusions (- l1))
		
		(outline-computations
		 (phi (beta-normalize (appl-create set (mlist a)))))
		
		(decl-content
		 (l0 () phi                             ("Open" () ()))
		 (l1 () (in a set)                      ("Defn-Expand" (l0) (in))))
		
		(proc-content schema-interpreter)
		(manual (documentation ""))
		)

(meth~defmethod Expand-in-m-f Expand-in-m
		(in frame)
		(rating 10)
		(reasoning :planning)
		(declarations
		 (type-variables aa)
		 (sorted-meta-variables
		  (phi o) 
		  (set (o aa))
		  (a aa)))
		
		(premises (- l0))
		(conclusions (+ l1))
		
		(outline-computations
		 (phi (beta-normalize (appl-create set (mlist a)))))
		
		(decl-content
		 (l0 () (in a set))
		 (l1 () phi                             ("Defn-Expand" (l0) (in))))
		
		(proc-content schema-interpreter)
		(manual (documentation ""))
		)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;XFORALLI-SORT-M
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;(infer~defmethod "Xforalli-Sort-m"
;                 (outline-mappings (((existent nonexistent) Xforalli-Sort-m-b)))
;                 (help "The method for FORALL-sort introduction"))
;
;
;(meth~defmethod Xforalli-Sort-m-b Xforalli-Sort-m
;                (in frame)
;                (rating 10)
;                (reasoning :planning :middle-out)
;
;                (declarations
;                 (type-variables aa bb)
;                 (sorted-meta-variables
;                  (phi o term)
;                  (phi1 o term)
;                  (sort1 (o aa) term)
;                  (newsort (o bb) term)
;                  (c bb term)
;                  (term aa term)
;                  )
;                 )
;                (premises (+ l2))
;                (conclusions (- l3))
;
;                (application-condition
;                 (newsort sort1 newsort term c (:symbol const)))
;                          
;                (outline-computations
;                 (phi1 (subst-apply (subst-create (mlist x) (mlist term)) phi))
;                 )
;                
;                (decl-content
;                 (l1 ()   (newsort c)                                        ("Hyp" () ()))
;                 (l2 (l1) phi1                                             ("Open" () ()))
;                 (l3 ()   (forall-sort (lam (x aa var) phi) sort1)         ("Xforalli-Sort" (c) (l2)))
;                 )
;                
;                (proc-content schema-interpreter)
;                (manual (documentation "This methods has the same effect as the backward application
;                                       of the tactic ForallI-sort."))
;                )
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;Xexistsi-sort-m
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;(infer~defmethod "Xexistsi-sort-m"
;                 (outline-mappings (((existent nonexistent nonexistent) Xexistsi-sort-m-b)))
;                 (help "The method for Exists-sort introduction"))
;
;(meth~defmethod Xexistsi-sort-m-b Xexistsi-sort-m
;                (in frame)
;                (rating 10)
;                (reasoning :planning :middle-out)
;
;                (declarations
;                 (type-variables aa bb)
;                 (sorted-meta-variables
;                  (phi o)
;                  (pos o poslist)
;                  (phi1 o)
;                  (sort1 (o aa))
;                  (c bb term)
;                  (term aa term)
;                  (newsort (o bb) term)
;                  )
;                 )
;                
;                (premises (+ l1) (+ l2))
;                (conclusions (- l3))
;                
;                (application-condition
;                 (newsort sort1 newsort term c (:symbol var)))
;                
;                 ;(termoccs x phi pos))
;                
;                (outline-computations
;                 (phi1 (subst-apply (subst-create (mlist x) (mlist term)) phi))
;                 )
;                
;                (decl-content
;                 (l1 ()   (newsort c)                                       ("Open" () ()))   
;                 (l2 ()   phi1                                              ("Open" () ()))
;                 (l3 ()   (exists-sort (lam (x aa var) phi) sort1)    ("Xexistsi-Sort" (mv pos) (l1 l2)))
;                 )
;
;                (outline-orderings (before l2 l1))
;                
;                (proc-content schema-interpreter)
;                (manual (documentation "This methods has the same effect as the backward application
;                                       of the tactic Xexistsi-sort"))
;                )
;
;;;;;;;;;;apply-assert
;
;(infer~defmethod "Xapply-assert-m"
;                 (outline-mappings (((existent closed) Xapply-assert-m-b)))
;                 (parameter-types term)
;                 (help "The method for Exists-sort introduction"))
;
;(meth~defmethod Xapply-assert-m-b Xapply-assert-m
;                (in frame)
;                (rating 3)
;                (reasoning :planning :middle-out)
;                (declarations
;                 (sorted-meta-variables
;                  (psi o term)
;                  (phi o term)
;                  (phiprime o term)
;                  (subphis list termlist)
;                  (psi-prime o term)
;                  (form o term)
;                  (pformulas list termlist)
;                  (prems o prlnlist)) 
;                 )
;
;                (parameters phiprime)
;                
;                (premises l10 (+ prems))
;                (conclusions (- l30))
;
;                (application-condition
;                 (mand
;                  (expandalldefis phi subphis mtrue (mlist (:symbol =)
;                                                           (:symbol forall-sort)
;                                                           (:symbol exists-sort)
;                                                           (:symbol struct-unit)
;                                                           (:symbol group-unit)))
;                  (mexists phiprime subphis
;                          (mor (myassert phiprime psi pformulas) ;assertion
;                               (mand (equality? psi)          ;assertion with =sym
;                                     (myassert phiprime
;                                               (appl-create (:term =)
;                                                            (mlist
;                                                             (msecond (applargs psi))(mfirst (applargs psi))))
;                                               pformulas))))))
;
;                (outline-computations
;                 (prems (if (mequal pformulas (mnil))
;                            (mnil)
;                          (premises-for l30 pformulas))))
;
;
;                (decl-content
;                 (l10 () phi)
;                 (l30 () psi          ("Assertion" () (l10 prems)))
;                 )
;                
;                )
;
;
;(infer~defmethod "xhomomorphism-m"
;                 (outline-mappings (((existent existent nonexistent nonexistent nonexistent) xhomomorphism-m-b)))
;                 (help ""))
;
;(meth~defmethod xhomomorphism-m-b xhomomorphism-m
;             (in frame)
;             (rating 10)
;             (reasoning :planning :middle-out)
;             (declarations
;               (type-variables aaa bbb)
;               (sorted-meta-variables
;                (dom (o aaa) term)                    (ran (o bbb) term)
;                (fun (bbb aaa) term)
;                (opdom (aaa aaa aaa) term)            (opran (bbb bbb bbb) term)
;                (pos o pos)                           (subst o sub)
;                (ranelem1 bbb metavar)                (ranelem2 bbb metavar)
;                (domelem1 aaa metavar)                (domelem2 aaa metavar)
;                (de1 aaa term)                        (de2 aaa term)
;                (re1 bbb term)                        (re2 bbb term)
;                (conc o term)
;                (replaced-hom-conc o term)
;               ))
;
;             (premises l00 (+ l01) (+ l02) (+ l10) )
;
;             (application-condition
;              (mand
;               (equality? conc)
;               (match-and-bind-subterm (:term (opran (fun domelem1)(fun domelem2))) conc subst pos)
;               (mbind de1 (subst-apply subst domelem1))
;               (mbind de2 (subst-apply subst domelem2))))
;
;             
;;             (outline-orderings
;;              (before l20 l10) (before l30 l10) 
;;              )
;             
;             (outline-computations
;              ;(de1 (NewMetaVarForVar domelem1))
;              ;(de2 (NewMetaVarForVar domelem2))
;              ;(re1 (subst-apply subst ranelem1))
;              ;(re2 (subst-apply subst ranelem2))
;              (replaced-hom-conc (termrplatpos conc pos (:term (fun (opdom de1 de2))))))
;
;             (conclusions (- l100))
;
;             (outline-orderings (before l10 l01)(before l10 l02))
;
;             (decl-content
;              (l00 ()  ( homomorphism  dom opdom ran opran fun))
;              (l01 ()  (dom de1)             ("open" () ()))
;              (l02 ()  (dom de2)             ("open" () ()))
;              (l10 ()  replaced-hom-conc     ("open" () ()))
;              (l100 () conc ("solved" () (l00 l10 l01 l02)))
;              ))
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;FORALLI-SORT-M
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod "ForallI-Sort-m"
		 (outline-mappings (((existent nonexistent) ForallI-Sort-m-b)))
		 (help "The method for FORALL-sort introduction"))


(meth~defmethod ForallI-Sort-m-b ForallI-Sort-m
		(in frame)
		(rating 10)
		(reasoning :planning :middle-out)

		(declarations
		 (type-variables aa)
		 (sorted-meta-variables
		  (phi o term)
		  (phi1 o term)
		  (sort1 (o aa) term)
		  (c aa const)
		  )
		 )
		(premises (+ l2))
		(conclusions (- l3))

		(outline-computations
		 (c (type-newconst aa))
		 (phi1 (subst-apply (subst-create (mlist x) (mlist c)) phi))
		 )
		
		(decl-content
		 (l1 ()   (sort1 c)                                        ("Hyp" () ()))
		 (l2 (l1) phi1                                             ("Open" () ()))
		 (l3 ()   (forall-sort (lam (x aa var) phi) sort1)         ("ForallI-Sort" (c) (l2)))
		 )
		
		(proc-content schema-interpreter)
		(manual (documentation "This method has the same effect as the backward application of the tactic ForallI-sort."))
		(remark ;local short, long
		("<U>ForallI-Sort:</U><BR>
We have to prove<BR>
<TAB><TERM>forall</TERM><TERM>(in x sort1)</TERM>><BR>
<TAB><TERM>phi</TERM>.<BR>
So we take an arbitrary <TERM>c</TERM> and have to show<BR>
<TAB><TERM>phi1</TERM><BR>
under the assumption <TERM>(in c sort1)</TERM>."
	       "")
	      ;global text, constr
		("We have to prove<BR>
<TAB><TERM>forall</TERM><TERM>(in x sort1)</TERM><BR>
<TAB><TERM>phi</TERM>.<BR>
So we take an arbitrary <TERM>c</TERM> and prove<BR>
<TAB><TERM>phi1</TERM><BR>
under the assumption <TERM>(in c sort1)</TERM>.<BR>
<LISP>(verbalize-text-next l2)</LISP>"
		 "We have to prove<BR>
<TAB><TERM>forall</TERM><TERM>(in x sort1)</TERM>><BR>
<TAB><TERM>phi</TERM>.<BR>
So we take an arbitrary <TERM>c</TERM> and prove<BR>
<TAB><TERM>phi1</TERM><BR>
under the assumption <TERM>(in c sort1)</TERM>.<BR>
<LISP>(verbalize-cons-next l2)</LISP>")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;ExistsI-SORT-M
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod "ExistsI-Sort-m"
		 (outline-mappings (((existent nonexistent nonexistent) ExistsI-Sort-m-b)))
		 (help "The method for Exists-sort introduction"))

(meth~defmethod ExistsI-Sort-m-b ExistsI-Sort-m
		(in frame)
		(rating 10)
		(reasoning :planning :middle-out)

		(declarations
		 (type-variables aa)
		 (sorted-meta-variables
		  (phi o)
		  (pos o poslist)
		  (phi1 o)
		  (sort1 (o aa))
		  (mv aa metavar)
		  )
		 )
		
		(premises (+ l1) (+ l2))
		(conclusions (- l3))
		
		(application-condition
		 (termoccs x phi pos))
		(outline-computations
		 (mv (newmetavar (:symbol :M) aa))
		 (phi1 (subst-apply (subst-create (mlist x) (mlist mv)) phi))
		 )
		
		(decl-content
		 (l1 ()   (sort1 mv)                                       ("Open" () ()))   
		 (l2 ()   phi1                                             ("Open" () ()))
		 (l3 ()   (exists-sort (lam (x aa var) phi) sort1)    ("ExistsI-Sort" (mv pos) (l1 l2)))
		 )

		;;(outline-orderings (before l2 l1))
		
		(proc-content schema-interpreter)
		(manual (documentation "This methods has the same effect as the backward application
                                       of the tactic ExistsI-sort"))
		(remark ;local short, long
		("<U>ExistsI-Sort:</U><BR>
We have to prove<BR>
<TAB><TERM>exists</TERM><TERM>(in x sort1)</TERM><BR>
<TAB><TERM>phi</TERM>.<BR>
So we have to find a term <TERM>mv</TERM> such that<BR>
<TAB><TERM>phi1</TERM><BR>
and <TERM>(in mv sort1)</TERM> holds."
	       "")
	      ;global text, constr
		("For <TERM>mv</TERM> we show that<BR>
<TAB><TERM>phi1</TERM><BR>
and <TERM>(in mv sort1)</TERM> holds.<BR>
<LISP>(verbalize-text-next l2)</LISP>"
		 "For <TERM>mv</TERM> we show that<BR>
<TAB><TERM>phi1</TERM><BR>
and <TERM>(in mv sort1)</TERM> holds.<BR>
<LISP>(verbalize-text-next l2)</LISP>")))


;; frame------------------------------------------------------

(infer~defmethod "defnexpfra-m" 
		 (outline-mappings (((existent list) defnexpfra-m-b)))
		 (outline-mappings (((existent nonexistent) defnexpfra-m-b)))
		 (parameter-types term-list)
		 (help "Controlled application the ore** tactic."))


(meth~defmethod defnexpfra-m-b defnexpfra-m
		(in frame)
		(rating 50)
		(parameters prems)
		(reasoning :planning)

		(declarations
		 (sorted-meta-variables
		  (f o term)
		  (New-Prems o prlnlist)
		  (prems o termlist)))
		 
		(premises (+ new-prems))

		(application-condition
		 (mand (mtrue)
		       (bound-p prems)))
		
		(conclusions (- l1))
		
		(outline-computations
		 (New-Prems (compute-open-nodes l1 prems)))

		(decl-content
		 (l1 () F ("defnexp" () New-Prems))
		 )

		(proc-content schema-interpreter)
		)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;frame methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(meth~new-relational-function 'tu-unify)
(meth~defcond tu-unify (args cmapp)
	      (declare (edited  "DEC-2006")
		       (authors Pollet)
		       (input   "Two terms (T1, T2),"
				"a list with equations ,"
				"a meta-variable for the subst,"
				"an MV for used existent supports, an MV for new goal-formulas.")
		       (effect  )
		       (value   "<T, mapp'>, if either SUB is bound and SUB(T1) == SUB(T2) modulo EQs,"
				"or T1 matches T2 and SUB is bound to the alpha-matcher."
				"<NIL, mapp>, otherwise." ))
	      (let ((mapp (meth~mapp-mapp cmapp))
		    (lhs (first args))
		    (rhs (second args))
		    (equs (when (listp (third args))
			    (mapcar #'(lambda (equ) (list (frame~mapsto equ)
							  (frame~mapsfrom equ)
							  (frame~conditions equ)
							  (list (keim~name equ)(frame~info equ))))
				    (mapcar #'frame~find-object (third args)))))
		    (sub (fourth args))
		    (prems (fifth args))
		    (newprems (sixth args))
		    (goal (seventh args)))
;		(omega~trace "tu-unify mit ~A ~A" args cmapp)
		(let ((unifyer (tu~unify lhs rhs equs)))
;		  (omega~trace "Found unifier ~A" (setf uni unifyer))
		  (if unifyer
		      (let ((domain (list sub prems newprems))
			    (codomain (list unifyer
					    (remove-if-not #'(lambda (elem)(and (node~p elem)
										(not (eq elem goal))))
							   (tu~context unifyer))
					    (tu~prems unifyer))))
			(setf lll lhs rrr rhs eee equs sss unifyer)
			(meth~mapp-new-constraint
			 (meth~mapp-new-extension cmapp
						  (if (meth~mapp-extension cmapp)
						      (mapp~create (append domain
									   (mapp~domain (meth~mapp-extension cmapp)))
								   (append codomain
									   (mapp~codomain (meth~mapp-extension cmapp))))
						    (mapp~create (list domain) (list codomain))))
			 (cstr~conjunction
			  (cons (meth~mapp-constraint cmapp)
				(mapcar #'(lambda (vv tt)
					    (cstr~binding-create (list vv tt)))
					(subst~domain unifyer) (subst~codomain unifyer))))))
			(meth~mapp-new-constraint cmapp nil)))))
		

(meth~deffun get-eq-for (node)
	     ()
	     (mapcar #'keim~name (frame~get-equations-for-objects node)))

;;;;;;;;;;;;;;

(infer~defmethod Freflexu-m
                (outline-mappings (((existent list) freflexu-m-b))
				  (((existent) freflexu-m-b)))
                (help "Closes a goal of the form 'a = a'"))

(meth~defmethod freflexu-m-b freflexu-m
               (in frame)
               (rating 50)
               (reasoning :planning :middle-out)
               (declarations
                (type-variables aa)
                (sorted-meta-variables
                 (phi aa term)
                 (psi aa term)
		 (equs o any) (sub o subst) (newpremsformulas o termlist) (new-prems o prlnlist) (prems o prlnlist)))
	       
               (premises (+ new-prems))
               (conclusions (- L1))
               
               (application-condition
		(mxor  (mand (unify phi psi)
			    (mbind new-prems (mnil)))
		      (mand
		       (mbind equs (mappend (get-eq-for phi)(get-eq-for psi)))
		       (tu-unify phi psi equs sub prems newpremsformulas l1)
		       (mif (bound-p newpremsformulas)
			    (mbind new-prems (compute-open-nodes l1 newpremsformulas))
			    (mbind new-prems (mnil))))))
		
               (decl-content
                (l1 () (= phi psi) ("=ref" (phi) ())))
	       (manual (author "AMeier")
		       (examples "ZMZ, Homo")))


(infer~defmethod fgroup-neut-m
		 (outline-mappings (((existent existent list) fgroup-neut-m-b)))
                 (help ""))

(meth~defmethod fgroup-neut-m-b fgroup-neut-m
		(in frame)
		(rating 100)
		(reasoning :planning :middle-out)
		(declarations
		 (type-variables aaa bbb)
		 (sorted-meta-variables
		  (phi aaa term)
		  (psi aaa term)
		  (grp (o aaa) term)		  (op (aaa aaa aaa) term)
		  (elem1 aaa term)		  (neut aaa term)
		  (conc o term)		          (sub o subst)
		  (equs o any) (sub o subst) (newpremsformulas o termlist) (new-prems o prlnlist) (prems o prlnlist)))

		(premises l00 (+ new-prems))

		(application-condition
		 (mand (mbind neut (:Term (group-unit grp op))) 
		       (mbind elem1 (newmetavar (:symbol elem) aaa))
		       (mbind equs (mappend (get-eq-for neut)(get-eq-for l20)))
		       (tu-unify  (:term (= (op neut elem1) elem1)) (:term (= phi psi)) equs sub prems newpremsformulas l20)
		       (mif (bound-p newpremsformulas)
			    (mbind new-prems (compute-open-nodes l20 newpremsformulas))
			    (mbind new-prems (mnil)))))
		      
		      
		(conclusions (- l20))

		(decl-content
		 (l00 () (group grp op) )
;		(l10 () (grp elem1) ("open" ()()))  durch conds bei tu-unify!
		 (l20 () (= phi psi)  ("solved" () (l00)))
		 ))


(infer~defmethod fgroup-inv-m
		 (outline-mappings (((existent existent list) fgroup-inv-m-b)))
                 (help ""))

(meth~defmethod fgroup-inv-m-b fgroup-inv-m
		(in frame)
		(rating 100)
		(reasoning :planning :middle-out)
		(declarations
		 (type-variables aaa bbb)
		 (sorted-meta-variables
		  (grp (o aaa) term)		  (op (aaa aaa aaa) term)
		  (lhs aaa term)		  (rhs aaa term)
		  (elem aaa term) (el aaa metavar) (elem-elem aaa term)
		  (inv aaa term)
		  (neut aaa metavar)              (neut-elem o term)
		  (conc o term)                   (neut-pre o term)
		  (sub o subst) 	     
		  (phi aaa term) (psi aaa term)(equs o any) (sub o subst) (newpremsformulas o termlist) (new-prems o prlnlist) (prems o prlnlist)))

		(premises l00 (+ new-prems))
	       
                (application-condition
		 (mand 
		  (mbind neut-elem (:term  (group-unit grp op)))  
		  (mbind elem (newmetavar (:symbol elem) aaa))
		  (mbind inv (:term (group-inverse grp op elem)))   
		  (mbind equs  (mappend (mappend (get-eq-for neut) (get-eq-for inv)) (get-eq-for l20)))
		  (tu-unify  (:term (= neut-elem (op elem inv))) (:term (= phi psi)) equs sub prems newpremsformulas l20)
		  (mif (bound-p newpremsformulas)
		       (mbind new-prems (compute-open-nodes l20 newpremsformulas))
		       (mbind new-prems (mnil)))))

	       (conclusions (- l20))

		(decl-content
		 (l00 () (group grp op))
;		 (l10 () (grp elem) ("open" ()()))
		 (l20 () (= phi psi)  ("solved" () (l00)))
		 ))



(infer~defmethod fxweaken-m    
		 (outline-mappings (((existent existent list) fweaken-m-c)))
		 (help "The application of a weaken."))

(meth~defmethod fweaken-m-c fxweaken-m
		 (in frame)
		(rating 45)
		(reasoning :planning :middle-out)
		(declarations
		 (type-variables aaa)
		 (sorted-meta-variables
		  (term1 o term)
		  (term2 o term)
		  (sub o subst)
		  (equs o any) (sub o subst) (newpremsformulas o termlist) (new-prems o prlnlist) (prems o prlnlist)))
		
		(premises L10 (+ new-prems))
		
                (application-condition
		   (mxor  (mand (unify term1 term2)
				(mbind new-prems (mnil)))
			  (mand 
			   (mbind equs  (mappend  (get-eq-for l10) (get-eq-for l30)))
			   (tu-unify  term1 term2 equs sub prems newpremsformulas l30)
			   (mif (bound-p newpremsformulas)
				(mbind new-prems (compute-open-nodes l30 newpremsformulas))
				(mbind new-prems (mnil))))))

		(conclusions (- L30))
		
		(decl-content
		 (L10 () term1 )
		 (L30 () term2  ("weaken" () (L10)))
		 ))



(infer~defmethod neutral-in-group-m
                 (outline-mappings (((existent existent) "neutral-in-group-m-c"))))

(meth~defmethod neutral-in-group-m-c neutral-in-group-m
               (in frame)

               (rating 10)
               (reasoning :planning :middle-out)

	       (declarations
		 (type-variables aaa bbb)
		 (sorted-meta-variables
		  (G (o aaa) term)
		  (op (aaa aaa aaa) term)
                 ))

               (premises  l00)

               (conclusions (- l10))

               (decl-content
                (l00 () (group G op))
                (l10 () (G (group-unit G op)) ("solved" () (l00)))
                ))
