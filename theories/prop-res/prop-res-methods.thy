;;;
;;; Methoden (Theorie: prop-res)
;;;



(in-package "OMEGA")

(infer~defmethod derive-meth
		 (outline-mappings (((existent existent existent existent existent 
					       nonexistent
					       nonexistent
					       nonexistent
					       nonexistent
					       nonexistent
					       ) derive-meth-m-b)))
		 (help "A method for prooving derivability goals with derivability assumtpions."))

;; Diese Methode kommt im Verlauf des Beweises des Disjunktionslemmas zum Zuge. Sie
;; konstruiert eine Ableitung in der Form, dass sie eine Ableitung einer Klausel cl1 und
;; eine Ableitung einer Klausel cl2 aneinanderhaengt, und um die Resolvente dieser beiden
;; Klauseln erweitert. Dabei hat man als Voraussetzung, dass die Ableitungseigenschaft der
;; beiden Teilableitungen als Voaussetzung zur Verfuegun steht.
;; Anschliessend fuehrt die Mehtode eine Fallunterscheidung durch:
;;  Beim Beweis, dass die konstruierte Liste eine Ableitung ist, wird bei der zufaelligen
;;  Wahl eine beliebigen Listenelementes unterschieden, ob das Element ein Element der
;;  ersten Liste  (Ableitung), der zweiten Liste (Ableitung) oder das Schlusselement ist.
;; Die einzelnen Faelle koennen dann unmittelbar auf die Voraussetzungen zurueck gefuehrt werden.

(meth~defmethod derive-meth-m-b derive-meth
		(in prop-res) 
		(rating 30)
		(reasoning :planning)
		(declarations
		 (constants               )
		 (sorted-meta-variables
		  (cl1 (o form) term) (cl2 (o form) term) ;; Die beiden Klauseln die aus K abgeleitet werden
							  ;; koennen. entweder Uespruengliche Klauseln oder
							  ;; die Klauseln plus LI

		  (cl3 (o form) term) (cl4 (o form) term)
		  ;;(helper (o form) term)
		  (aset(o (o form)) term)
		  (li1 form term )
		  (li2 form term)
		  (concat glist term)
		  (aseq1 glist term)
		  (aseq2 glist term)
		  (goal (o form) term)
		  (ait item term)
		  (apos8 o pos)
		  )
		 )
		(parameters ) 
		(premises (- l99) (- l89) (- l80) (- l78)
			  (+ l11) (+ l24) (+ l37) (+ l50) (+ l26)
		 )
		(application-condition )  
		(outline-actions
		 (l26 (sponsor l80))
		 (l26 (sponsor l78))
		 (l11 (sponsor l97)) (l11 (sponsor l87))
		 (l37 (sponsor l49)) (l37 (sponsor l70)) (l37 (sponsor l85)) (l37 (sponsor l69a))
		 (l50 (sponsor l65)) (l50 (sponsor l70)) (l50 (sponsor l85)) (l50 (sponsor l69a))

		 )
		(outline-orderings
		 (before l26 l50)
		 (before l50 l37)
		 (before l37 l24)
		 (before l24 l11)
		 )
		(outline-computations
		 (apos8 (:position (1 1 1 0 1 0 1)))
		 (aseq1 (type-newconst (:type glist)))
		 (aseq2 (type-newconst (:type glist)))
		 (ait   (type-newconst (:type item)))
;		 (cl1   (type-newconst (:type (o form))))
;		 (cl2   (type-newconst (:type (o form))))
;		 (li1   (type-newconst (:type form)))
;		 (li2   (type-newconst (:type form)))
		 (concat   (:term (append aseq1
					  (append (push-idx aseq2 (glist-length aseq1))
						  (cons (der-item-constr res-res
                                                                goal
                                                                (cons (form2item li1)
                                                                      (cons (form2item li2) NULL))
                                                                (cons (num2item (glist-length aseq1))
                                                                      (cons
                                                                       (num2item (plus (glist-length aseq1)
                                                                                       
                                                                                       (glist-length aseq2)))
                                                                       NULL)))
							NULL)))))
		 )
		(conclusions (- ltp))
		(decl-content

		 
		 
		 ;; Wichtige Annahme: Die Klausel cl1 ist in weniger als cx12 Schritten
		 ;; aus K ableitbar.  Wichtig ist
		 ;; die Tatsache, dass man die Existenz einer Ableitung von cl1
		 ;; voraussetzen kann. Zeilen l99 bis l95. Der Name fuer diese Ableitung
		 ;; ist aseq1
                 (l99 () (derivable cl1 aset crule-set)                                  )

                 (l98 () (exists (lam (seq glist)
                                       (derivation-of aset crule-set seq cl1))) ("DefnE" (derivable) (l99)))

		 (l97 () (derivation-of aset crule-set aseq1 cl1)     ("Hyp"()())          )

;                 (l96 () (and (= (der-item-info (last aseq1)) cl1)
;                              (derivation aset crule-set aseq1))     ("DefnE" (derivation-of) (l97)))
;
;                 (l95 () (derivation aset crule-set aseq1)           ("AndER" () (l96)))

		 ;; --------------------

		 ;; Ebenso wie Zeile l99. Die IH aus dem Induktionsbeweis des
		 ;; Disjunktionslemmas besagt, dass cl2 ableitbar ist. Der Name fuer diese
		 ;; Ableitung ist aseq2
                 (l89 () (derivable  cl2 aset crule-set)                                           )

                 (l88 () (exists (lam (seq glist)
                                       (derivation-of aset crule-set seq cl2))) ("defnE" (derivable)(l89)))

                 (l87 () (derivation-of aset crule-set aseq2  cl2)   ("Hyp"()())         );

                 (l86 () (and (= (der-item-info (last aseq2))
                                 cl2)
                              (derivation aset crule-set aseq2))    ("DefnE"  (derivation-of) (l87)))

                 (l85 () (derivation aset crule-set aseq2) ("AndER" ()(l86)))

		 ;; ---------------------------------------

		 ;; Wichtige Voraussetzung ist, dass cl3 und cl4 auch resolvierbar sind.
		 (l80 () (resolvable-b cl3  cl4 li1 li2 )                        )               
		 
                 (l79 () (= goal 
                            (resolvent-of cl3  cl4 li1 li2 ) )         ("abstract"()()))
                         ;; Zeile l79 direkt aus l80 nach definition
;
		 (l78 () (resolvable-s cl1 cl2)             )
		 
                 (l70 () (is-component ait concat) ("Hyp"()()))

;                 (l69 () (forall (lam (x item)
;                            (implies (and (is-component x concat)
;                                          (and (leq one (position x concat))
;                                               (leq (position x concat)
;                                                    (glist-length aseq1))))
;                                     (= x (glist-nth (position x concat)
;                                                     aseq1))))) ("abstract"()()))
                 (l69a () (forall (lam (x item)
                            (implies (and (is-component x concat)
                                          (and (leq (plus (glist-length aseq1)
                                                          one)
                                                    (position x concat))
                                               (leq (position x concat)
                                                    (plus (glist-length aseq1)
                                                          (glist-length aseq2)))))
                                     (= (idx-minus (glist-length aseq1) x)
		                         (glist-nth (minus (position x concat)
                                                            (glist-length aseq1))
                                                     aseq2))))) ("abstract"()()))

;                 (l68 () (forall (lam (x item)
;                            (implies (= (position x concat)
;                                        (plus one
;                                           (plus (glist-length aseq1)
;                                                 (glist-length aseq2))))
;                                  (= x
;                                     (der-item-constr res-res
;                                                      goal
;                                                      (cons (form2item li1)
;                                                            (cons (form2item li2)
;                                                                  NULL))
;                                                      (cons (num2item (glist-length aseq1))
;                                                            (cons (num2item
;                                                                   (plus (glist-length aseq1)
;                                                                         (glist-length
;                                                                          aseq2)))
;                                                                  NULL)))))))     ("abstract"()()))
;
;                 ;; -------------------------------------
;                 
;                 (l67 () (or (= (position ait concat)
;                                (plus one
;                                      (plus (glist-length aseq1)
;                                            (glist-length aseq2))))
;                             (or (and (leq one (position ait concat))
;                                               (leq (position ait concat)
;                                                    (glist-length aseq1)))
;                                 (and (leq (plus (glist-length aseq1)
;                                                          one)
;                                                    (position ait concat))
;                                               (leq (position ait concat)
;                                                    (plus (glist-length aseq1)
;                                                          (glist-length aseq2))))))
;                      ("Abstract"()()))

                 (l66 ()  (or (and (leq one (position ait concat))
                                   (leq (position ait concat)
                                        (glist-length aseq1)))
                              (and (leq (plus (glist-length aseq1)
                                              one)
                                        (position ait concat))
                                   (leq (position ait concat)
                                        (plus (glist-length aseq1)
                                              (glist-length aseq2)))))    ("Hyp"()()))

                 (l65 () (and (leq one (position ait concat))
			      (leq (position ait concat)
				   (glist-length aseq1)))                  ("Hyp"()()))

		 ;; Diesen Teilbeweis in eine andere Methode packen
;                 (l64 () (implies (and (is-component ait concat)
;                                          (and (leq one (position ait concat))
;                                               (leq (position ait concat)
;                                                    (glist-length aseq1))))
;                                     (= ait (glist-nth (position ait concat)
;                                                     aseq1))) ("ForallE" (ait) (l69)))
;
;                 (l63 (l65 l70) (and (is-component ait concat)
;                                     (and (leq one (position ait concat))
;                                          (leq (position ait concat)
;                                               (glist-length aseq1)))) ("AndI" () (l65 l70)))
;                 
;                 (l62 (l65 l70) (= ait (glist-nth (position ait concat)
;                                                  aseq1)) ("ImpE"() (l64 l63)))
;
;                 (l61 (l69 l65) (=  (poslist2infolist (der-item-prems ait) concat)
;                                    (poslist2infolist (der-item-prems (glist-nth (position ait  concat) aseq1)) aseq1))
;                      ("abstract"()()))
;
;                 (l60 (l69 l65) (= (position (glist-nth (position ait concat) aseq1) concat)
;                                   (position (glist-nth (position ait concat) aseq1)  aseq1)) ("Abstract"()()))

	
                      
                 (l50 (l97 l87 l70 l66 l65)
                      (and (and (crule-set (der-item-crule ait))
				(crule-applicable (der-item-crule ait)
						  (append (append
							   (der-item-inp ait)
							   (poslist2infolist (der-item-prems ait) concat))
							  (cons (cl2item (der-item-info ait))
								NULL))
						  aset))
			   (and (forall (lam (y num)
					     (implies (is-component (num2item y) (der-item-prems ait))
						      (less y
							    (position ait concat)))))
				(= (der-item-info ait)         
				   (crule-application (der-item-crule ait)
						      (append (append
							       (der-item-inp ait)
							       (poslist2infolist (der-item-prems ait) concat))
							      (cons (cl2item
								     (der-item-info ait))
								    NULL))
						      aset))))       ("Open"()()))

                 ;; ----------------------------------------------------------
                 
                 (l49 () (and (leq (plus (glist-length aseq1) one)
				   (position ait concat))
			      (leq (position ait concat)
				   (plus (glist-length aseq1)
					 (glist-length aseq2))))		                           ("Hyp"()()))
                 

                 (l37 (l97 l87 l85 l70 l66 l49)
                      (and (and (crule-set (der-item-crule ait))
				(crule-applicable (der-item-crule ait)
						  (append (append
							   (der-item-inp ait)
							   (poslist2infolist (der-item-prems ait) concat))
							  (cons (cl2item (der-item-info ait))
								NULL))
						  aset))
			   (and (forall (lam (y num)
					     (implies (is-component (num2item y) (der-item-prems ait))
						      (less y
							    (position ait concat)))))
				(= (der-item-info ait)         
				   (crule-application (der-item-crule ait)
						      (append (append
							       (der-item-inp ait)
							       (poslist2infolist (der-item-prems ait) concat))
							      (cons (cl2item
								     (der-item-info ait))
								    NULL))
						      aset)))) ("Open"()()))
                 
;                 (l36 (l97 l87 l70 l66)(and (and (crule-set (der-item-crule ait))
;                                                 (crule-applicable (der-item-crule ait)
;                                                   (append (append
;                                                            (der-item-inp ait)
;                                                            (poslist2infolist (der-item-prems ait) concat))
;                                                           (cons (cl2item (der-item-info ait))
;                                                                 NULL))
;                                                   aset))
;                                            (and (forall (lam (y num)
;                                                    (implies (is-component (num2item y) (der-item-prems ait))
;                                                             (less y
;                                                                   (position ait concat)))))
;                                                 (= (der-item-info ait)         
;                                                    (crule-application (der-item-crule ait)
;                                                         (append (append
;                                                                  (der-item-inp ait)
;                                                                  (poslist2infolist (der-item-prems ait) concat))
;                                                                 (cons (cl2item
;                                                                        (der-item-info ait))
;                                                                       NULL))
;                                                         aset))))           ("OrE"()(l66  l37 l50)))
                 
                 (l35 () (= (position ait concat)
                            (plus one
                                  (plus (glist-length aseq1)
                                        (glist-length aseq2)))) ("Hyp"()()))
		 
                 (l26 (l97 l87 l70 l35) (and (and (crule-set (der-item-crule ait))
                                                 (crule-applicable (der-item-crule ait)
                                                   (append (append
                                                            (der-item-inp ait)
                                                            (poslist2infolist (der-item-prems ait) concat))
                                                           (cons (cl2item (der-item-info ait))
                                                                 NULL))
                                                   aset))
                                            (and (forall (lam (y num)
                                                    (implies (is-component (num2item y) (der-item-prems ait))
                                                             (less y
                                                                   (position ait concat)))))
                                                 (= (der-item-info ait)         
                                                    (crule-application (der-item-crule ait)
                                                         (append (append
                                                                  (der-item-inp ait)
                                                                  (poslist2infolist (der-item-prems ait) concat))
                                                                 (cons (cl2item
                                                                        (der-item-info ait))
                                                                       NULL))
                                                         aset)))) ("Open"()()))
                 
;                 (l25 (l97 l87 l70) (and (and (crule-set (der-item-crule ait))
;                                                 (crule-applicable (der-item-crule ait)
;                                                   (append (append
;                                                            (der-item-inp ait)
;                                                            (poslist2infolist (der-item-prems ait) concat))
;                                                           (cons (cl2item (der-item-info ait))
;                                                                 NULL))
;                                                   aset))
;                                            (and (forall (lam (y num)
;                                                    (implies (is-component (num2item y) (der-item-prems ait))
;                                                             (less y
;                                                                   (position ait concat)))))
;                                                 (= (der-item-info ait)         
;                                                    (crule-application (der-item-crule ait)
;                                                         (append (append
;                                                                  (der-item-inp ait)
;                                                                  (poslist2infolist (der-item-prems ait) concat))
;                                                                 (cons (cl2item
;                                                                        (der-item-info ait))
;                                                                       NULL))
;                                                         aset)))) ("OrE"()(l67 l26 l36)))

		 ;; -------------------------------------------------------
		 
                 (l24 (l97 l87 l70) (free-derivation-cond concat) ("Open"()()))
;                 
;                 ;; ------------------------------------------------------
;
;                 (l23 (l97 l87 l70) (and (free-derivation-cond concat)          
;                                       (and (and (crule-set (der-item-crule ait))
;                                                 (crule-applicable (der-item-crule ait)
;                                                   (append (append
;                                                            (der-item-inp ait)
;                                                            (poslist2infolist (der-item-prems ait) concat))
;                                                           (cons (cl2item (der-item-info ait))
;                                                                 NULL))
;                                                   aset))
;                                            (and (forall (lam (y num)
;                                                    (implies (is-component (num2item y) (der-item-prems ait))
;                                                             (less y
;                                                                   (position ait concat)))))
;                                                 (= (der-item-info ait)         
;                                                    (crule-application (der-item-crule ait)
;                                                         (append (append
;                                                                  (der-item-inp ait)
;                                                                  (poslist2infolist (der-item-prems ait) concat))
;                                                                 (cons (cl2item
;                                                                       (der-item-info ait))
;                                                                       NULL))
;                                                         aset))))) ("Open"()()))
;                      ;;("AndI"()(l24 l25)))
;                 
;                 (l22 (l97 l87) (implies (is-component ait concat)
;                                  (and (free-derivation-cond concat)            
;                                       (and (and (crule-set (der-item-crule ait))
;                                                 (crule-applicable (der-item-crule ait)
;                                                   (append (append
;                                                            (der-item-inp ait)
;                                                            (poslist2infolist (der-item-prems ait) concat))
;                                                           (cons (cl2item (der-item-info ait))
;                                                                 NULL))
;                                                   aset))
;                                            (and (forall (lam (y num)
;                                                    (implies (is-component (num2item y) (der-item-prems ait))
;                                                             (less y
;                                                                   (position ait concat)))))
;                                                 (= (der-item-info ait)         
;                                                    (crule-application (der-item-crule ait)
;                                                         (append (append
;                                                                  (der-item-inp ait)
;                                                                  (poslist2infolist (der-item-prems ait) concat))
;                                                                 (cons (cl2item
;                                                                        (der-item-info ait))
;                                                                       NULL))
;                                                         aset)))))) ("ImpI"()(l23 l70)))
;                 
;                 (l21 (l97 l87) (forall (lam (x item) 
;                          (implies (is-component x concat)
;                                   (and (free-derivation-cond concat)           
;                                       (and (and (crule-set (der-item-crule x))
;                                                 (crule-applicable (der-item-crule x)
;                                                   (append (append
;                                                            (der-item-inp x)
;                                                            (poslist2infolist (der-item-prems x) concat))
;                                                           (cons (cl2item (der-item-info x))
;                                                                 NULL))
;                                                   aset))
;                                            (and (forall (lam (y num)
;                                                    (implies (is-component (num2item y) (der-item-prems x))
;                                                             (less y
;                                                                   (position x concat)))))
;                                                 (= (der-item-info x)           
;                                                    (crule-application (der-item-crule x)
;                                                         (append (append
;                                                                  (der-item-inp x)
;                                                                  (poslist2infolist (der-item-prems x) concat))
;                                                                 (cons (cl2item
;                                                                        (der-item-info x))
;                                                                       NULL))
;                                                         aset))))))))
;                      ("ForallI" (ait) (l22)))
;
		 ;; Nachweis der Ableitungseigenschaft von concat
		 ;; Unterscheide 3 Faelle
		 ;; l26 : expliziet fuer das letzte Element von concat
		 ;; l37 : ait liegt in der ersten Ableitung
		 ;; l50 : ait liegt in der zweiten Ableitung
		 ;; Zunaechst bis l23 standard Aufloesung
;                 (l20 (l97 l87) (derivation aset crule-set concat)                      ("DefnI"(derivation) (l21)))

                 ;; -------------------------------------------------------------

	         (l15 () (= (last concat)
                            (der-item-constr res-res
                                             goal
                                             (cons (form2item li1)
                                                   (cons (form2item li2) NULL))
                                             (cons (num2item (glist-length aseq1))
                                                   (cons
                                                    (num2item (plus (glist-length aseq1)
                                                                    
                                                                    (glist-length aseq2)))
                                                                       NULL)))) ("abstract"()()))
		 
		 (l11 (l97 l87) (=  (der-item-info
				     (der-item-constr res-res
						      goal 
						      (cons (form2item li1)
							    (cons (form2item li2) NULL))
						      (cons (num2item (glist-length aseq1))
							    (cons
                                                            (num2item (plus (glist-length aseq1)
                                                                            
                                                                            (glist-length aseq2)))
                                                            NULL))))
				    goal)  ("Open" () ()))

		 ;; Kann gemaess der Konstruktion von concat bewiesen werden: Beweis gekuerzt l11 l15
                 (l10 (l97 l87) (= (der-item-info (last concat))  goal)                   ("Subst="()(l15 l11)))

		 ;; ---------------------------------------------------------------------
		 
                 (l5  (l97 l87) (and (= (der-item-info (last concat))  goal)
                                     (derivation aset crule-set concat))                  ("AndI" () (l10 l20)))    
                 
                 (l4  (l97 l87) (derivation-of aset crule-set concat  goal)               ("DefnI" () (l5)))

                 (l3  (l97 l87) (exists (lam (seq glist)
                                  (derivation-of aset crule-set seq goal)))
                                                                                          ("ExistsI" (concat) (l4)))
                 
                 (l2  (l97 l87) (derivable goal aset crule-set)                           ("DefnI" () (l3 )))

                 (l1  (l97) (derivable goal aset crule-set) ("abstract"()()))
		      ;;("ExistsE"(aseq2)(l2 l88)))

		 ;; -------------------------------------------------------------------
		 
		 ;; ZIEL: goal ist aus aset ableitbar. goal ist belibige Klausel,
		 ;; entstanden beim Beweis des Disjunktionslemma. Ist eine Klauselmenge K
		 ;; goal ist in cx12 Schritten ableibar.
                 (ltp () (derivable goal aset crule-set)   ("Open"()()))
		      ;; ("ExistsE"(aseq1)(l1 l98)))
		 
		  )
		(proc-content schema-interpreter)
		(remark "A method for prooving derivability goals with derivability assumtpions.")
		)




(infer~defmethod derive-meth2
		 (outline-mappings (((existent
				      ;; existent  ;; existent
				      ;; existent ;;existent
				      )
				     derive-meth2-m-b)))
		 (help "A method for prooving derivability goals with derivability assumtpions."))

(meth~defmethod derive-meth2-m-b derive-meth2
		(in prop-res) 
		(rating 30)
		(reasoning :planning)
		(declarations
		 (constants               )
		 (sorted-meta-variables
		  (apos10 o pos)(apos11 o pos)(apos12 o pos)(apos13 o pos)(apos14 o pos)(apos15 o pos)(apos16 o pos)
		  (apos17 o pos)(apos18 o pos)(apos19 o pos)(apos20 o pos)(apos21 o pos)(apos31 o pos)(apos32 o pos)
		  (apos33 o pos)(apos40 o pos)(apos41 o pos)
		  (ait  item term)
		  (aco  num  term)
		  (concat glist term)
		  (aseq1  glist term)
		  (aseq2  glist term)
		  (aset   (o (o form)) term)
		  (lobo   num term)
		  (upbo   num term)
		  (a-num-const num term)
		  )
		 )
		(parameters ) 
		(premises
		 ;; (- lh0) ;; (- lh1) ;; (- lh3) ;;(- lh4)
		 )
		(application-condition )  
		(outline-actions
		 )
		(outline-orderings
		 )
		(outline-computations
		 (aterm (:term (glist-nth (minus (position ait concat)
						 (glist-length aseq1))
					  aseq2)))
;	         (apos10 (:position (1 2 3)))
;                 (apos11 (:position (1 2 3)))
;                 (apos12 (:position (1 2 3)))
;                 (apos13 (:position (1 2 3)))
;                 (apos14 (:position (1 2 3)))
;                 (apos15 (:position (1 2 3)))
;                 (apos16 (:position (1 2 3)))
;                 (apos17 (:position (1 2 3)))
;                 (apos18 (:position (1 2 3)))
;                 (apos19 (:position (1 2 3)))
;                 (apos20 (:position (1 2 3)))
;                 (apos21 (:position (1 2 3)))
;                 (apos31 (:position (1 2 3)))
;                 (apos32 (:position (1 2 3)))
;                 (apos33 (:position (1 2 3)))
;                 (apos40 (:position (1 2 3)))
;                 (apos41 (:position (1 2 3)))
		 )
		(conclusions (- ltp))
		(decl-content
		 (lh0 () (greater aco (s zero))    ) 
		 
		 (lh3 () (is-component ait concat)      ) ;; muss vorhanden sein

                 (lh4 () (and (leq lobo
                                   (position ait concat))
                              (leq (position ait concat)
                                   upbo))   ) ;; muss vorhanden sein
                 
                 (lh1 ()  (derivation aset crule-set aseq2)                  )
                
                 (lh2 () (forall (lam (x item)
                            (implies (and (is-component x concat)
                                          (and (leq lobo
                                                    (position x concat))
                                               (leq (position x concat)
                                                    upbo)))
                                     (= (idx-minus a-num-const x)
                                        (glist-nth (minus (position x concat)
                                                          (glist-length aseq1))
                                                   aseq2)))))            )

		 ;; --------------------------------------------------------------------
		 
                 (l1 () (implies (and (is-component ait concat)
                                          (and (leq lobo
                                                    (position ait concat))
                                               (leq (position ait concat)
                                                    upbo)))
                                     (= (idx-minus a-num-const ait)
                                        (glist-nth (minus (position ait concat)
                                                          (glist-length aseq1))
                                                   aseq2))) ("ForallE" (ait)(lh2)))

                 (l2 () (and (is-component ait concat)
                             (and (leq lobo
                                       (position ait concat))
                                  (leq (position ait concat)
                                       upbo)))
                     ("AndI" () (lh3 lh4)))

;                 (l3 () (= (idx-minus a-num-const ait)
;                           (glist-nth (minus (position ait concat)
;                                             (glist-length aseq1))
;                                      aseq2))    ("ImpE" ()(l1 l2)))
;                
;
;                 ;; Hinfuehrung zum Ziel
;
;                 ;; ---------------------------------------------------------------------------
;                 
;                 (l4  ()  (forall (lam (x item) 
;                          (implies (is-component x aseq2)
;                                   (and (free-derivation-cond aseq2)            
;                                       (and (and (crule-set (der-item-crule x))
;                                                 (crule-applicable (der-item-crule x)
;                                                       (append (append
;                                                                (der-item-inp x)
;                                                                (poslist2infolist (der-item-prems x) aseq2))
;                                                               (cons (cl2item (der-item-info x))
;                                                                     NULL))
;                                                       aset))
;                                            (and (forall (lam (y num)
;                                                    (implies (is-component (num2item y) (der-item-prems x))
;                                                             (less y
;                                                                   (position x aseq2)))))
;                                                 (= (der-item-info x)           
;                                                    (crule-application (der-item-crule x)
;                                                         (append (append
;                                                                  (der-item-inp x)
;                                                                  (poslist2infolist (der-item-prems x) aseq2))
;                                                                 (cons (cl2item
;                                                                        (der-item-info x))
;                                                                       NULL))
;                                                         aset)))))))) ("DefnE" (derivation)(lh1)))
;                 
;                 (l5 () (implies (is-component (glist-nth (minus (position ait concat)
;                                                 (glist-length aseq1))
;                                          aseq2)  aseq2)
;                                   (and (free-derivation-cond aseq2)            
;                                       (and (and (crule-set (der-item-crule (glist-nth (minus (position ait concat)
;                                                 (glist-length aseq1))
;                                          aseq2)))
;                                                 (crule-applicable (der-item-crule (glist-nth (minus (position ait concat)
;                                                 (glist-length aseq1))
;                                          aseq2))
;                                                       (append (append
;                                                                (der-item-inp (glist-nth (minus (position ait concat)
;                                                 (glist-length aseq1))
;                                          aseq2))
;                                                                (poslist2infolist (der-item-prems (glist-nth (minus (position ait concat)
;                                                 (glist-length aseq1))
;                                          aseq2)) aseq2))
;                                                               (cons (cl2item (der-item-info (glist-nth (minus (position ait concat)
;                                                 (glist-length aseq1))
;                                          aseq2)))
;                                                                     NULL))
;                                                       aset))
;                                            (and (forall (lam (y num)
;                                                    (implies (is-component (num2item y) (der-item-prems (glist-nth (minus (position ait concat)
;                                                 (glist-length aseq1))
;                                          aseq2)))
;                                                             (less y
;                                                                   (position (glist-nth (minus (position ait concat)
;                                                 (glist-length aseq1))
;                                          aseq2) aseq2)))))
;                                                 (= (der-item-info (glist-nth (minus (position ait concat)
;                                                 (glist-length aseq1))
;                                          aseq2))               
;                                                    (crule-application (der-item-crule (glist-nth (minus (position ait concat)
;                                                 (glist-length aseq1))
;                                          aseq2))
;                                                         (append (append
;                                                                  (der-item-inp (glist-nth (minus (position ait concat)
;                                                 (glist-length aseq1))
;                                          aseq2))
;                                                                  (poslist2infolist (der-item-prems (glist-nth (minus (position ait concat)
;                                                 (glist-length aseq1))
;                                          aseq2)) aseq2))
;                                                                 (cons (cl2item
;                                                                        (der-item-info (glist-nth (minus (position ait concat)
;                                                 (glist-length aseq1))
;                                          aseq2)))
;                                                                       NULL))
;                                                         aset))))))  ("ForallE" (aterm)
;                                                                      (l4)))
;
;                 (l6  () (is-component (glist-nth (minus (position ait concat)
;                                                 (glist-length aseq1))
;                                          aseq2)  aseq2)    ("abstract"()()))
;
;                 (l7  () (and (free-derivation-cond aseq2)              
;                              (and (and (crule-set (der-item-crule (glist-nth (minus (position ait concat)
;                                                                                     (glist-length aseq1))
;                                                                              aseq2)))
;                                        (crule-applicable (der-item-crule (glist-nth (minus (position ait concat)
;                                                                                            (glist-length aseq1))
;                                                                                     aseq2))
;                                                          (append (append
;                                                                   (der-item-inp (glist-nth (minus (position ait concat)
;                                                                                                   (glist-length aseq1))
;                                                                                            aseq2))
;                                                                   (poslist2infolist (der-item-prems (glist-nth (minus (position ait concat)
;                                                                                                                       (glist-length aseq1))
;                                                                                                                aseq2)) aseq2))
;                                                                  (cons (cl2item (der-item-info (glist-nth (minus (position ait concat)
;                                                                                                                  (glist-length aseq1))
;                                                                                                           aseq2)))
;                                                                        NULL))
;                                                          aset))
;                                   (and (forall (lam (y num)
;                                                     (implies (is-component (num2item y) (der-item-prems (glist-nth (minus (position ait concat)
;                                                                                                                           (glist-length aseq1))
;                                                                                                                    aseq2)))
;                                                              (less y
;                                                                    (position (glist-nth (minus (position ait concat)
;                                                                                                (glist-length aseq1))
;                                                                                         aseq2) aseq2)))))
;                                        (= (der-item-info (glist-nth (minus (position ait concat)
;                                                                            (glist-length aseq1))
;                                                                     aseq2))            
;                                           (crule-application (der-item-crule (glist-nth (minus (position ait concat)
;                                                                                                (glist-length aseq1))
;                                                                                         aseq2))
;                                                              (append (append
;                                                                       (der-item-inp (glist-nth (minus (position ait concat)
;                                                                                                       (glist-length aseq1))
;                                                                                                aseq2))
;                                                                       (poslist2infolist (der-item-prems (glist-nth (minus (position ait concat)
;                                                                                                                           (glist-length aseq1))
;                                                                                                                    aseq2)) aseq2))
;                                                                      (cons (cl2item
;                                                                             (der-item-info (glist-nth (minus (position ait concat)
;                                                                                                              (glist-length aseq1))
;                                                                                                       aseq2)))
;                                                                            NULL))
;                                                              aset))))) ("ImpE" ()(l6 l5)))
;                 
;                 (l8 () (and (and (crule-set (der-item-crule (glist-nth (minus (position ait concat)
;                                                                                     (glist-length aseq1))
;                                                                              aseq2)))
;                                        (crule-applicable (der-item-crule (glist-nth (minus (position ait concat)
;                                                                                            (glist-length aseq1))
;                                                                                     aseq2))
;                                                          (append (append
;                                                                   (der-item-inp (glist-nth (minus (position ait concat)
;                                                                                                   (glist-length aseq1))
;                                                                                            aseq2))
;                                                                   (poslist2infolist (der-item-prems (glist-nth (minus (position ait concat)
;                                                                                                                       (glist-length aseq1))
;                                                                                                                aseq2)) aseq2))
;                                                                  (cons (cl2item (der-item-info (glist-nth (minus (position ait concat)
;                                                                                                                  (glist-length aseq1))
;                                                                                                           aseq2)))
;                                                                        NULL))
;                                                          aset))
;                                   (and (forall (lam (y num)
;                                                     (implies (is-component (num2item y) (der-item-prems (glist-nth (minus (position ait concat)
;                                                                                                                           (glist-length aseq1))
;                                                                                                                    aseq2)))
;                                                              (less y
;                                                                    (position (glist-nth (minus (position ait concat)
;                                                                                                (glist-length aseq1))
;                                                                                         aseq2) aseq2)))))
;                                        (= (der-item-info (glist-nth (minus (position ait concat)
;                                                                            (glist-length aseq1))
;                                                                     aseq2))            
;                                           (crule-application (der-item-crule (glist-nth (minus (position ait concat)
;                                                                                                (glist-length aseq1))
;                                                                                         aseq2))
;                                                              (append (append
;                                                                       (der-item-inp (glist-nth (minus (position ait concat)
;                                                                                                       (glist-length aseq1))
;                                                                                                aseq2))
;                                                                       (poslist2infolist (der-item-prems (glist-nth (minus (position ait concat)
;                                                                                                                           (glist-length aseq1))
;                                                                                                                    aseq2)) aseq2))
;                                                                      (cons (cl2item
;                                                                             (der-item-info (glist-nth (minus (position ait concat)
;                                                                                                              (glist-length aseq1))
;                                                                                                       aseq2)))
;                                                                            NULL))
;                                                              aset))))  ("AndER"() (l7)))
;                 
;                 (l9 () (and (and (crule-set (der-item-crule (idx-minus a-num-const ait)))
;                                        (crule-applicable (der-item-crule (glist-nth (minus (position ait concat)
;                                                                                            (glist-length aseq1))
;                                                                                     aseq2))
;                                                          (append (append
;                                                                   (der-item-inp (glist-nth (minus (position ait concat)
;                                                                                                   (glist-length aseq1))
;                                                                                            aseq2))
;                                                                   (poslist2infolist (der-item-prems (glist-nth (minus (position ait concat)
;                                                                                                                       (glist-length aseq1))
;                                                                                                                aseq2)) aseq2))
;                                                                  (cons (cl2item (der-item-info (glist-nth (minus (position ait concat)
;                                                                                                                  (glist-length aseq1))
;                                                                                                           aseq2)))
;                                                                        NULL))
;                                                          aset))
;                                   (and (forall (lam (y num)
;                                                     (implies (is-component (num2item y) (der-item-prems (glist-nth (minus (position ait concat)
;                                                                                                                           (glist-length aseq1))
;                                                                                                                    aseq2)))
;                                                              (less y
;                                                                    (position (glist-nth (minus (position ait concat)
;                                                                                                (glist-length aseq1))
;                                                                                         aseq2) aseq2)))))
;                                        (= (der-item-info (glist-nth (minus (position ait concat)
;                                                                            (glist-length aseq1))
;                                                                     aseq2))            
;                                           (crule-application (der-item-crule (glist-nth (minus (position ait concat)
;                                                                                                (glist-length aseq1))
;                                                                                         aseq2))
;                                                              (append (append
;                                                                       (der-item-inp (glist-nth (minus (position ait concat)
;                                                                                                       (glist-length aseq1))
;                                                                                                aseq2))
;                                                                       (poslist2infolist (der-item-prems (glist-nth (minus (position ait concat)
;                                                                                                                           (glist-length aseq1))
;                                                                                                                    aseq2)) aseq2))
;                                                                      (cons (cl2item
;                                                                             (der-item-info (glist-nth (minus (position ait concat)
;                                                                                                              (glist-length aseq1))
;                                                                                                       aseq2)))
;                                                                            NULL))
;                                                              aset))))  ("Subst="(apos10)
;                                                                         ( l3 l8)))
;
;                 (l10 () (and (and (crule-set (der-item-crule (idx-minus a-num-const ait)))
;                                        (crule-applicable (der-item-crule (idx-minus a-num-const ait))
;                                                          (append (append
;                                                                   (der-item-inp (glist-nth (minus (position ait concat)
;                                                                                                   (glist-length aseq1))
;                                                                                            aseq2))
;                                                                   (poslist2infolist (der-item-prems (glist-nth (minus (position ait concat)
;                                                                                                                       (glist-length aseq1))
;                                                                                                                aseq2)) aseq2))
;                                                                  (cons (cl2item (der-item-info (glist-nth (minus (position ait concat)
;                                                                                                                  (glist-length aseq1))
;                                                                                                           aseq2)))
;                                                                        NULL))
;                                                          aset))
;                                   (and (forall (lam (y num)
;                                                     (implies (is-component (num2item y) (der-item-prems (glist-nth (minus (position ait concat)
;                                                                                                                           (glist-length aseq1))
;                                                                                                                    aseq2)))
;                                                              (less y
;                                                                    (position (glist-nth (minus (position ait concat)
;                                                                                                (glist-length aseq1))
;                                                                                         aseq2) aseq2)))))
;                                        (= (der-item-info (glist-nth (minus (position ait concat)
;                                                                            (glist-length aseq1))
;                                                                     aseq2))            
;                                           (crule-application (der-item-crule (glist-nth (minus (position ait concat)
;                                                                                                (glist-length aseq1))
;                                                                                         aseq2))
;                                                              (append (append
;                                                                       (der-item-inp (glist-nth (minus (position ait concat)
;                                                                                                       (glist-length aseq1))
;                                                                                                aseq2))
;                                                                       (poslist2infolist (der-item-prems (glist-nth (minus (position ait concat)
;                                                                                                                           (glist-length aseq1))
;                                                                                                                    aseq2)) aseq2))
;                                                                      (cons (cl2item
;                                                                             (der-item-info (glist-nth (minus (position ait concat)
;                                                                                                              (glist-length aseq1))
;                                                                                                       aseq2)))
;                                                                            NULL))
;                                                              aset)))) ("Subst=" (apos11)
;                                                                        ( l3 l9)))
;
;                 (l11 () (and (and (crule-set (der-item-crule (idx-minus a-num-const ait)))
;                                        (crule-applicable (der-item-crule (idx-minus a-num-const ait))
;                                                          (append (append
;                                                                   (der-item-inp (idx-minus a-num-const ait))
;                                                                   (poslist2infolist (der-item-prems (glist-nth (minus (position ait concat)
;                                                                                                                       (glist-length aseq1))
;                                                                                                                aseq2)) aseq2))
;                                                                  (cons (cl2item (der-item-info (glist-nth (minus (position ait concat)
;                                                                                                                  (glist-length aseq1))
;                                                                                                           aseq2)))
;                                                                        NULL))
;                                                          aset))
;                                   (and (forall (lam (y num)
;                                                     (implies (is-component (num2item y) (der-item-prems (glist-nth (minus (position ait concat)
;                                                                                                                           (glist-length aseq1))
;                                                                                                                    aseq2)))
;                                                              (less y
;                                                                    (position (glist-nth (minus (position ait concat)
;                                                                                                (glist-length aseq1))
;                                                                                         aseq2) aseq2)))))
;                                        (= (der-item-info (glist-nth (minus (position ait concat)
;                                                                            (glist-length aseq1))
;                                                                     aseq2))            
;                                           (crule-application (der-item-crule (glist-nth (minus (position ait concat)
;                                                                                                (glist-length aseq1))
;                                                                                         aseq2))
;                                                              (append (append
;                                                                       (der-item-inp (glist-nth (minus (position ait concat)
;                                                                                                       (glist-length aseq1))
;                                                                                                aseq2))
;                                                                       (poslist2infolist (der-item-prems (glist-nth (minus (position ait concat)
;                                                                                                                           (glist-length aseq1))
;                                                                                                                    aseq2)) aseq2))
;                                                                      (cons (cl2item
;                                                                             (der-item-info (glist-nth (minus (position ait concat)
;                                                                                                              (glist-length aseq1))
;                                                                                                       aseq2)))
;                                                                            NULL))
;                                                              aset)))) ("Subst="(apos12)
;                                                                        (l3 l10)))
;                 
;                 (l12 () (and (and (crule-set (der-item-crule (idx-minus a-num-const ait)))
;                                        (crule-applicable (der-item-crule (idx-minus a-num-const ait))
;                                                          (append (append
;                                                                   (der-item-inp (idx-minus a-num-const ait))
;                                                                   (poslist2infolist
;                                                                    (der-item-prems
;                                                                     (idx-minus a-num-const ait)) aseq2))
;                                                                  (cons (cl2item (der-item-info (glist-nth (minus (position ait concat)
;                                                                                                                  (glist-length aseq1))
;                                                                                                           aseq2)))
;                                                                        NULL))
;                                                          aset))
;                                   (and (forall (lam (y num)
;                                                     (implies (is-component (num2item y) (der-item-prems (glist-nth (minus (position ait concat)
;                                                                                                                           (glist-length aseq1))
;                                                                                                                    aseq2)))
;                                                              (less y
;                                                                    (position (glist-nth (minus (position ait concat)
;                                                                                                (glist-length aseq1))
;                                                                                         aseq2) aseq2)))))
;                                        (= (der-item-info (glist-nth (minus (position ait concat)
;                                                                            (glist-length aseq1))
;                                                                     aseq2))            
;                                           (crule-application (der-item-crule (glist-nth (minus (position ait concat)
;                                                                                                (glist-length aseq1))
;                                                                                         aseq2))
;                                                              (append (append
;                                                                       (der-item-inp (glist-nth (minus (position ait concat)
;                                                                                                       (glist-length aseq1))
;                                                                                                aseq2))
;                                                                       (poslist2infolist (der-item-prems (glist-nth (minus (position ait concat)
;                                                                                                                           (glist-length aseq1))
;                                                                                                                    aseq2)) aseq2))
;                                                                      (cons (cl2item
;                                                                             (der-item-info (glist-nth (minus (position ait concat)
;                                                                                                              (glist-length aseq1))
;                                                                                                       aseq2)))
;                                                                            NULL))
;                                                              aset)))) ("Subst=" (apos13)
;                                                                        (l3 l11)))
;
;                 (l13 () (and (and (crule-set (der-item-crule (idx-minus a-num-const ait)))
;                                        (crule-applicable (der-item-crule (idx-minus a-num-const ait))
;                                                          (append (append
;                                                                   (der-item-inp (idx-minus a-num-const ait))
;                                                                   (poslist2infolist
;                                                                    (der-item-prems
;                                                                     (idx-minus a-num-const ait)) aseq2))
;                                                                  (cons (cl2item
;                                                                         (der-item-info
;                                                                          (idx-minus a-num-const ait))) NULL))
;                                                          aset))
;                                   (and (forall (lam (y num)
;                                                     (implies (is-component (num2item y) (der-item-prems (glist-nth (minus (position ait concat)
;                                                                                                                           (glist-length aseq1))
;                                                                                                                    aseq2)))
;                                                              (less y
;                                                                    (position (glist-nth (minus (position ait concat)
;                                                                                                (glist-length aseq1))
;                                                                                         aseq2) aseq2)))))
;                                        (= (der-item-info (glist-nth (minus (position ait concat)
;                                                                            (glist-length aseq1))
;                                                                     aseq2))            
;                                           (crule-application (der-item-crule (glist-nth (minus (position ait concat)
;                                                                                                (glist-length aseq1))
;                                                                                         aseq2))
;                                                              (append (append
;                                                                       (der-item-inp (glist-nth (minus (position ait concat)
;                                                                                                       (glist-length aseq1))
;                                                                                                aseq2))
;                                                                       (poslist2infolist (der-item-prems (glist-nth (minus (position ait concat)
;                                                                                                                           (glist-length aseq1))
;                                                                                                                    aseq2)) aseq2))
;                                                                      (cons (cl2item
;                                                                             (der-item-info (glist-nth (minus (position ait concat)
;                                                                                                              (glist-length aseq1))
;                                                                                                       aseq2)))
;                                                                            NULL))
;                                                              aset)))) ("Subst=" (apos14)
;                                                                        (l3 l12)))
;
;
;                 (l14 () (and (and (crule-set (der-item-crule (idx-minus a-num-const ait)))
;                                        (crule-applicable (der-item-crule (idx-minus a-num-const ait))
;                                                          (append (append
;                                                                   (der-item-inp (idx-minus a-num-const ait))
;                                                                   (poslist2infolist
;                                                                    (der-item-prems
;                                                                     (idx-minus a-num-const ait)) aseq2))
;                                                                  (cons (cl2item
;                                                                         (der-item-info
;                                                                          (idx-minus a-num-const ait))) NULL))
;                                                          aset))
;                                   (and (forall (lam (y num)
;                                                     (implies (is-component (num2item y)
;                                                                            (der-item-prems
;                                                                             (idx-minus a-num-const ait)))
;                                                              (less y
;                                                                    (position (glist-nth (minus (position ait concat)
;                                                                                                (glist-length aseq1))
;                                                                                         aseq2) aseq2)))))
;                                        (= (der-item-info (glist-nth (minus (position ait concat)
;                                                                            (glist-length aseq1))
;                                                                     aseq2))            
;                                           (crule-application (der-item-crule (glist-nth (minus (position ait concat)
;                                                                                                (glist-length aseq1))
;                                                                                         aseq2))
;                                                              (append (append
;                                                                       (der-item-inp (glist-nth (minus (position ait concat)
;                                                                                                       (glist-length aseq1))
;                                                                                                aseq2))
;                                                                       (poslist2infolist (der-item-prems (glist-nth (minus (position ait concat)
;                                                                                                                           (glist-length aseq1))
;                                                                                                                    aseq2)) aseq2))
;                                                                      (cons (cl2item
;                                                                             (der-item-info (glist-nth (minus (position ait concat)
;                                                                                                              (glist-length aseq1))
;                                                                                                       aseq2)))
;                                                                            NULL))
;                                                              aset)))) ("Subst=" (apos15)
;                                                                        (l3 l13)))
;
;                 (l15 () (and (and (crule-set (der-item-crule (idx-minus a-num-const ait)))
;                                        (crule-applicable (der-item-crule (idx-minus a-num-const ait))
;                                                          (append (append
;                                                                   (der-item-inp (idx-minus a-num-const ait))
;                                                                   (poslist2infolist
;                                                                    (der-item-prems
;                                                                     (idx-minus a-num-const ait)) aseq2))
;                                                                  (cons (cl2item
;                                                                         (der-item-info
;                                                                          (idx-minus a-num-const ait))) NULL))
;                                                          aset))
;                                   (and (forall (lam (y num)
;                                                     (implies (is-component (num2item y)
;                                                                            (der-item-prems
;                                                                             (idx-minus a-num-const ait)))
;                                                              (less y
;                                                                    (position
;                                                                     (idx-minus a-num-const ait)
;                                                                     aseq2)))))
;                                        (= (der-item-info (glist-nth (minus (position ait concat)
;                                                                            (glist-length aseq1))
;                                                                     aseq2))            
;                                           (crule-application (der-item-crule (glist-nth (minus (position ait concat)
;                                                                                                (glist-length aseq1))
;                                                                                         aseq2))
;                                                              (append (append
;                                                                       (der-item-inp (glist-nth (minus (position ait concat)
;                                                                                                       (glist-length aseq1))
;                                                                                                aseq2))
;                                                                       (poslist2infolist (der-item-prems (glist-nth (minus (position ait concat)
;                                                                                                                           (glist-length aseq1))
;                                                                                                                    aseq2)) aseq2))
;                                                                      (cons (cl2item
;                                                                             (der-item-info (glist-nth (minus (position ait concat)
;                                                                                                              (glist-length aseq1))
;                                                                                                       aseq2)))
;                                                                            NULL))
;                                                              aset)))) ("Subst=" (apos16)
;                                                                        (l3 l14)))
;
;                 (l16 () (and (and (crule-set (der-item-crule (idx-minus a-num-const ait)))
;                                        (crule-applicable (der-item-crule (idx-minus a-num-const ait))
;                                                          (append (append
;                                                                   (der-item-inp (idx-minus a-num-const ait))
;                                                                   (poslist2infolist
;                                                                    (der-item-prems
;                                                                     (idx-minus a-num-const ait)) aseq2))
;                                                                  (cons (cl2item
;                                                                         (der-item-info
;                                                                          (idx-minus a-num-const ait))) NULL))
;                                                          aset))
;                                   (and (forall (lam (y num)
;                                                     (implies (is-component (num2item y)
;                                                                            (der-item-prems
;                                                                             (idx-minus a-num-const ait)))
;                                                              (less y
;                                                                    (position
;                                                                     (idx-minus a-num-const ait)
;                                                                     aseq2)))))
;                                        (= (der-item-info (idx-minus a-num-const ait))          
;                                           (crule-application (der-item-crule (glist-nth (minus (position ait concat)
;                                                                                                (glist-length aseq1))
;                                                                                         aseq2))
;                                                              (append (append
;                                                                       (der-item-inp (glist-nth (minus (position ait concat)
;                                                                                                       (glist-length aseq1))
;                                                                                                aseq2))
;                                                                       (poslist2infolist (der-item-prems (glist-nth (minus (position ait concat)
;                                                                                                                           (glist-length aseq1))
;                                                                                                                    aseq2)) aseq2))
;                                                                      (cons (cl2item
;                                                                             (der-item-info (glist-nth (minus (position ait concat)
;                                                                                                              (glist-length aseq1))
;                                                                                                       aseq2)))
;                                                                            NULL))
;                                                              aset)))) ("Subst=" (apos17)
;                                                                        (l3 l15)))
;
;                 (l17 () (and (and (crule-set (der-item-crule (idx-minus a-num-const ait)))
;                                        (crule-applicable (der-item-crule (idx-minus a-num-const ait))
;                                                          (append (append
;                                                                   (der-item-inp (idx-minus a-num-const ait))
;                                                                   (poslist2infolist
;                                                                    (der-item-prems
;                                                                     (idx-minus a-num-const ait)) aseq2))
;                                                                  (cons (cl2item
;                                                                         (der-item-info
;                                                                          (idx-minus a-num-const ait))) NULL))
;                                                          aset))
;                                   (and (forall (lam (y num)
;                                                     (implies (is-component (num2item y)
;                                                                            (der-item-prems
;                                                                             (idx-minus a-num-const ait)))
;                                                              (less y
;                                                                    (position
;                                                                     (idx-minus a-num-const ait)
;                                                                     aseq2)))))
;                                        (= (der-item-info (idx-minus a-num-const ait))          
;                                           (crule-application (der-item-crule (idx-minus a-num-const ait))
;                                                              (append (append
;                                                                       (der-item-inp (glist-nth (minus (position ait concat)
;                                                                                                       (glist-length aseq1))
;                                                                                                aseq2))
;                                                                       (poslist2infolist (der-item-prems (glist-nth (minus (position ait concat)
;                                                                                                                           (glist-length aseq1))
;                                                                                                                    aseq2)) aseq2))
;                                                                      (cons (cl2item
;                                                                             (der-item-info (glist-nth (minus (position ait concat)
;                                                                                                              (glist-length aseq1))
;                                                                                                       aseq2)))
;                                                                            NULL))
;                                                              aset)))) ("Subst=" (apos18)
;                                                                        (l3 l16)))
;
;                 (l18 () (and (and (crule-set (der-item-crule (idx-minus a-num-const ait)))
;                                        (crule-applicable (der-item-crule (idx-minus a-num-const ait))
;                                                          (append (append
;                                                                   (der-item-inp (idx-minus a-num-const ait))
;                                                                   (poslist2infolist
;                                                                    (der-item-prems
;                                                                     (idx-minus a-num-const ait)) aseq2))
;                                                                  (cons (cl2item
;                                                                         (der-item-info
;                                                                          (idx-minus a-num-const ait))) NULL))
;                                                          aset))
;                                   (and (forall (lam (y num)
;                                                     (implies (is-component (num2item y)
;                                                                            (der-item-prems
;                                                                             (idx-minus a-num-const ait)))
;                                                              (less y
;                                                                    (position
;                                                                     (idx-minus a-num-const ait)
;                                                                     aseq2)))))
;                                        (= (der-item-info (idx-minus a-num-const ait))          
;                                           (crule-application (der-item-crule (idx-minus a-num-const ait))
;                                                              (append (append
;                                                                       (der-item-inp (idx-minus a-num-const ait))
;                                                                       (poslist2infolist (der-item-prems (glist-nth (minus (position ait concat)
;                                                                                                                           (glist-length aseq1))
;                                                                                                                    aseq2)) aseq2))
;                                                                      (cons (cl2item
;                                                                             (der-item-info (glist-nth (minus (position ait concat)
;                                                                                                              (glist-length aseq1))
;                                                                                                       aseq2)))
;                                                                            NULL))
;                                                              aset)))) ("Subst=" (apos19) (l3 l17)))
;
;                 (l19 () (and (and (crule-set (der-item-crule (idx-minus a-num-const ait)))
;                                        (crule-applicable (der-item-crule (idx-minus a-num-const ait))
;                                                          (append (append
;                                                                   (der-item-inp (idx-minus a-num-const ait))
;                                                                   (poslist2infolist
;                                                                    (der-item-prems
;                                                                     (idx-minus a-num-const ait)) aseq2))
;                                                                  (cons (cl2item
;                                                                         (der-item-info
;                                                                          (idx-minus a-num-const ait))) NULL))
;                                                          aset))
;                                   (and (forall (lam (y num)
;                                                     (implies (is-component (num2item y)
;                                                                            (der-item-prems
;                                                                             (idx-minus a-num-const ait)))
;                                                              (less y
;                                                                    (position
;                                                                     (idx-minus a-num-const ait)
;                                                                     aseq2)))))
;                                        (= (der-item-info (idx-minus a-num-const ait))          
;                                           (crule-application (der-item-crule (idx-minus a-num-const ait))
;                                                              (append (append
;                                                                       (der-item-inp (idx-minus a-num-const ait))
;                                                                       (poslist2infolist
;                                                                        (der-item-prems
;                                                                         (idx-minus a-num-const ait)) aseq2))
;                                                                      (cons (cl2item
;                                                                             (der-item-info (glist-nth (minus (position ait concat)
;                                                                                                              (glist-length aseq1))
;                                                                                                       aseq2)))
;                                                                            NULL))
;                                                              aset)))) ("Subst=" (apos20)
;                                                                        (l2 l18)))
;
;                 (l20 ()(and (and (crule-set (der-item-crule (idx-minus a-num-const ait)))
;                                        (crule-applicable (der-item-crule (idx-minus a-num-const ait))
;                                                          (append (append
;                                                                   (der-item-inp (idx-minus a-num-const ait))
;                                                                   (poslist2infolist
;                                                                    (der-item-prems
;                                                                     (idx-minus a-num-const ait)) aseq2))
;                                                                  (cons (cl2item
;                                                                         (der-item-info
;                                                                          (idx-minus a-num-const ait))) NULL))
;                                                          aset))
;                                   (and (forall (lam (y num)
;                                                     (implies (is-component (num2item y)
;                                                                            (der-item-prems
;                                                                             (idx-minus a-num-const ait)))
;                                                              (less y
;                                                                    (position
;                                                                     (idx-minus a-num-const ait)
;                                                                     aseq2)))))
;                                        (= (der-item-info (idx-minus a-num-const ait))          
;                                           (crule-application (der-item-crule (idx-minus a-num-const ait))
;                                                              (append (append
;                                                                       (der-item-inp (idx-minus a-num-const ait))
;                                                                       (poslist2infolist
;                                                                        (der-item-prems
;                                                                         (idx-minus a-num-const ait)) aseq2))
;                                                                      (cons (cl2item
;                                                                             (der-item-info
;                                                                              (idx-minus a-num-const ait)))
;                                                                            NULL))
;                                                              aset)))) ("Subst=" (apos21)
;                                                                        (l3 l19)))
;                 
;                 (l21 () (= (der-item-crule (idx-minus a-num-const ait))
;                            (der-item-crule ait))                                        ("abstract"()()))
;                 
;                 (l22 () (and (and (crule-set (der-item-crule ait))
;                                        (crule-applicable (der-item-crule (idx-minus a-num-const ait))
;                                                          (append (append
;                                                                   (der-item-inp (idx-minus a-num-const ait))
;                                                                   (poslist2infolist
;                                                                    (der-item-prems
;                                                                     (idx-minus a-num-const ait)) aseq2))
;                                                                  (cons (cl2item
;                                                                         (der-item-info
;                                                                          (idx-minus a-num-const ait))) NULL))
;                                                          aset))
;                                   (and (forall (lam (y num)
;                                                     (implies (is-component (num2item y)
;                                                                            (der-item-prems
;                                                                             (idx-minus a-num-const ait)))
;                                                              (less y
;                                                                    (position
;                                                                     (idx-minus a-num-const ait)
;                                                                     aseq2)))))
;                                        (= (der-item-info (idx-minus a-num-const ait))          
;                                           (crule-application (der-item-crule (idx-minus a-num-const ait))
;                                                              (append (append
;                                                                       (der-item-inp (idx-minus a-num-const ait))
;                                                                       (poslist2infolist
;                                                                        (der-item-prems
;                                                                         (idx-minus a-num-const ait)) aseq2))
;                                                                      (cons (cl2item
;                                                                             (der-item-info
;                                                                              (idx-minus a-num-const ait)))
;                                                                            NULL))
;                                                              aset)))) ("subst=" (apos31)
;                                                                        (l21 l20)))
;
;                 (l23 () (and (and (crule-set (der-item-crule ait))
;                                        (crule-applicable (der-item-crule ait)
;                                                          (append (append
;                                                                   (der-item-inp (idx-minus a-num-const ait))
;                                                                   (poslist2infolist
;                                                                    (der-item-prems
;                                                                     (idx-minus a-num-const ait)) aseq2))
;                                                                  (cons (cl2item
;                                                                         (der-item-info
;                                                                          (idx-minus a-num-const ait))) NULL))
;                                                          aset))
;                                   (and (forall (lam (y num)
;                                                     (implies (is-component (num2item y)
;                                                                            (der-item-prems
;                                                                             (idx-minus a-num-const ait)))
;                                                              (less y
;                                                                    (position
;                                                                     (idx-minus a-num-const ait)
;                                                                     aseq2)))))
;                                        (= (der-item-info (idx-minus a-num-const ait))          
;                                           (crule-application (der-item-crule (idx-minus a-num-const ait))
;                                                              (append (append
;                                                                       (der-item-inp (idx-minus a-num-const ait))
;                                                                       (poslist2infolist
;                                                                        (der-item-prems
;                                                                         (idx-minus a-num-const ait)) aseq2))
;                                                                      (cons (cl2item
;                                                                             (der-item-info
;                                                                              (idx-minus a-num-const ait)))
;                                                                            NULL))
;                                                              aset)))) ("subst=" (apos32)
;                                                                        (l21 l22)))
;                 
;                 (l24 () (and (and (crule-set (der-item-crule ait))
;                                        (crule-applicable (der-item-crule ait)
;                                                          (append (append
;                                                                   (der-item-inp (idx-minus a-num-const ait))
;                                                                   (poslist2infolist
;                                                                    (der-item-prems
;                                                                     (idx-minus a-num-const ait)) aseq2))
;                                                                  (cons (cl2item
;                                                                         (der-item-info
;                                                                          (idx-minus a-num-const ait))) NULL))
;                                                          aset))
;                                   (and (forall (lam (y num)
;                                                     (implies (is-component (num2item y)
;                                                                            (der-item-prems
;                                                                             (idx-minus a-num-const ait)))
;                                                              (less y
;                                                                    (position
;                                                                     (idx-minus a-num-const ait)
;                                                                     aseq2)))))
;                                        (= (der-item-info (idx-minus a-num-const ait))          
;                                           (crule-application (der-item-crule ait)
;                                                              (append (append
;                                                                       (der-item-inp (idx-minus a-num-const ait))
;                                                                       (poslist2infolist
;                                                                        (der-item-prems
;                                                                         (idx-minus a-num-const ait)) aseq2))
;                                                                      (cons (cl2item
;                                                                             (der-item-info
;                                                                              (idx-minus a-num-const ait)))
;                                                                            NULL))
;                                                              aset)))) ("subst=" (apos33)
;                                                                        (l21 l23)))
;                 
;                 (l25 () (= (der-item-inp (idx-minus a-num-const ait))
;                            (der-item-inp ait)) ("abstract"()()))
;
;                 (l26 () (and (and (crule-set (der-item-crule ait))
;                                        (crule-applicable (der-item-crule ait)
;                                                          (append (append
;                                                                   (der-item-inp ait)
;                                                                   (poslist2infolist
;                                                                    (der-item-prems
;                                                                     (idx-minus a-num-const ait)) aseq2))
;                                                                  (cons (cl2item
;                                                                         (der-item-info
;                                                                          (idx-minus a-num-const ait))) NULL))
;                                                          aset))
;                                   (and (forall (lam (y num)
;                                                     (implies (is-component (num2item y)
;                                                                            (der-item-prems
;                                                                             (idx-minus a-num-const ait)))
;                                                              (less y
;                                                                    (position
;                                                                     (idx-minus a-num-const ait)
;                                                                     aseq2)))))
;                                        (= (der-item-info (idx-minus a-num-const ait))          
;                                           (crule-application (der-item-crule ait)
;                                                              (append (append
;                                                                       (der-item-inp (idx-minus a-num-const ait))
;                                                                       (poslist2infolist
;                                                                        (der-item-prems
;                                                                         (idx-minus a-num-const ait)) aseq2))
;                                                                      (cons (cl2item
;                                                                             (der-item-info
;                                                                              (idx-minus a-num-const ait)))
;                                                                            NULL))
;                                                              aset)))) ("Subst" (apos40) (l25 l24)))
;
;                 (l27 ()(and (and (crule-set (der-item-crule ait))
;                                        (crule-applicable (der-item-crule ait)
;                                                          (append (append
;                                                                   (der-item-inp ait)
;                                                                   (poslist2infolist
;                                                                    (der-item-prems (idx-minus a-num-const ait)) aseq2))
;                                                                  (cons (cl2item
;                                                                         (der-item-info
;                                                                          (idx-minus a-num-const ait))) NULL))
;                                                          aset))
;                                   (and (forall (lam (y num)
;                                                     (implies (is-component (num2item y)
;                                                                            (der-item-prems
;                                                                             (idx-minus a-num-const ait)))
;                                                              (less y
;                                                                    (position
;                                                                     (idx-minus a-num-const ait)
;                                                                     aseq2)))))
;                                        (= (der-item-info (idx-minus a-num-const ait))          
;                                           (crule-application (der-item-crule ait)
;                                                              (append (append
;                                                                       (der-item-inp ait)
;                                                                       (poslist2infolist
;                                                                        (der-item-prems
;                                                                         (idx-minus a-num-const ait)) aseq2))
;                                                                      (cons (cl2item
;                                                                             (der-item-info
;                                                                              (idx-minus a-num-const ait)))
;                                                                            NULL)) 
;                                                              aset))))  ("Subst" (apos41) (l25 l26)))

		 ;; Die folgenden 3 Gleicheiten muessen noch substituiert werden, dann
		 ;; ist die zu zeigende Zeile bewiesen
		 
;                 (l28 () (=  (poslist2infolist
;                              (der-item-prems (idx-minus a-num-const ait)) aseq2)
;                             (poslist2infolist (der-item-prems ait) concat) ("abstract"()()))
;
;                 (l29 () (= (der-item-info (idx-minus a-num-const ait))
;                            (der-item-info ait)) ("abstract"()()))
;
;                 (l30 () (= (forall (lam (y num)
;                                         (implies (is-component (num2item y)
;                                                                (der-item-prems
;                                                                 (idx-minus a-num-const ait)))
;                                                  (less y
;                                                        (position
;                                                         (idx-minus a-num-const ait)
;                                                         aseq2)))))
;                            (forall (lam (y num)
;                                         (implies (is-component (num2item y) (der-item-prems ait))
;                                                  (less y
;                                                        (position ait concat)))))) ("abstract"()()))
			   					      
		 ;; ----------------------------------------------------------------------------------
		 
		 (ltp () (and
			  (and
			   (crule-set (der-item-crule ait))
			   (crule-applicable
			    (der-item-crule ait)
			    (append
			     (append
			      (der-item-inp ait)
			      (poslist2infolist
			       (der-item-prems ait)
			       concat))
			     (cons
			      (cl2item (der-item-info ait))
			      NULL))
			    aset))
			  (and (forall (lam (y num)
					    (implies (is-component (num2item y) (der-item-prems ait))
						     (less y
							   (position ait concat)))))
			       (= (der-item-info ait)         
				  (crule-application (der-item-crule ait)
						     (append (append
							      (der-item-inp ait)
							      (poslist2infolist (der-item-prems ait) concat))
							     (cons (cl2item
								    (der-item-info ait))
								   NULL))
						     aset)))) ("Open"()()))
		 )
		(proc-content schema-interpreter)
		(remark "A method for prooving derivability goals with derivability assumtpions.")
		)



(infer~defmethod pureSetTh
		 (help "Justification for a - as atomic - abstracted subgoal"))
;; These subgoals are solvebale declaratif or by a goaldirected atp-call.

(infer~defmethod derive-meth3
		 (outline-mappings (((existent
				      existent  existent existent existent existent existent
				      nonexistent
				      )
				     derive-meth3-m-b)))
		 (help "A method for prooving derivability goals with derivability assumtpions."))

(meth~defmethod derive-meth3-m-b derive-meth3
		(in prop-res) 
		(rating 30)
		(reasoning :planning)
		(declarations
		 (constants               )
		 (sorted-meta-variables
		  (li1 form term)     (li2 form term)
		  (cl1 (o form) term) (cl2 (o form) term)
		  (cl3 (o form) term) (cl4 (o form) term)
		  (aseq1 glist term)  (aseq2 glist term)
		  (aset (o (o form)) term)
		  (ait  item term)
		  (concat glist term)
		  (goal   (o form) term)
		  (co8    (o form) term)
		  )
		 )
		(parameters ) 
		(premises (- lh1) (- lh2) (- lh3) (- lh6) (- lh4) (- lh5)
			  (+ l43) ;; (+ l99)
		 )
		(application-condition )  
		(outline-actions
		 ;; (l99 (sponsor lh41))
		 (l43 (sponsor lh5))
		 )
		(outline-orderings
		 ; (before l99 l43)
		 )
		(outline-computations
		 )
		(conclusions (- ltp))
		(decl-content

		 ;; Concat ist der Term:
;	         (append aseq1
;                         (append (push-idx aseq2 (glist-length aseq1))
;                                 (cons (der-item-constr res-res
;                                                        goal
;                                                        (cons (form2item li1)
;                                                              (cons (form2item li2) NULL))
;                                                        (cons (num2item (glist-length aseq1))
;                                                              (cons
;                                                               (num2item (plus (glist-length aseq1)
;                                                                               
;                                                                               (glist-length aseq2)))
;                                                               NULL)))
;                                       NULL)));
;                 

		 (lh1 () (= (position ait
				      (append aseq1
					      (append (push-idx aseq2 (glist-length aseq1))
						      (cons (der-item-constr res-res
									     goal
									     (cons (form2item li1)
										   (cons (form2item li2) NULL))
									     (cons (num2item (glist-length aseq1))
										   (cons
										    (num2item (plus (glist-length aseq1)
												    (glist-length aseq2)))
										    NULL)))
							    NULL))))
			    (plus one
				  (plus (glist-length aseq1)
					(glist-length aseq2))))   )
		 
		 (lh2 () (derivation-of aset crule-set aseq1 cl1)  )

		 (lh3 () (derivation-of aset crule-set aseq2 cl2)  )

		 (lh6 () (resolvable-s cl1 cl2)                    )

                 
                 (lh4 () (= co8
                            (resolvent-of cl3
                                          cl4
                                          li1 li2))               )

                 (lh41 () (= co8
                             (setminus (setminus (union cl3 cl4)
                                                 (singleton li1))       
                                       (singleton li2))) ("abstract"()()))
                 
		 
                 (lh5  () (resolvable-b cl3 cl4 li1 li2)          )
                 
;                 (l100  () (= ait
;                              (der-item-constr res-res
;                                               goal
;                                               (cons (form2item li1)
;                                                     (cons (form2item li2) NULL))
;                                               (cons (num2item (glist-length aseq1))
;                                                     (cons
;                                                      (num2item (plus (glist-length aseq1)
;                                                                      
;                                                                      (glist-length aseq2)))
;                                                      NULL))))                      ("abstract" ()()))
;                 (l70 () (= (der-item-info ait)
;                            goal)                                                    ("abstract"()()))
;                 (l69 () (=  
;                          (der-item-crule ait) res-res)                              ("abstract" ()()))
;
;                 (l68 () (= (append (append
;                                     (der-item-inp ait)
;                                     (poslist2infolist (der-item-prems ait) concat))
;                                    (cons (cl2item
;                                           (der-item-info ait))
;                                          NULL))
;                            (cons (cl2item cl1)
;                                  (cons (cl2item cl2)
;                                        (cons (form2item li1)
;                                              (cons (form2item li2)
;                                                    (cons (cl2item goal)
;                                                          NULL)))))) ("abstract"()()))
;
;                 (l67 () (= (crule-application res-res
;                               (cons (cl2item cl1)
;                                  (cons (cl2item cl2)
;                                        (cons (form2item li1)
;                                              (cons (form2item li2)
;                                                    (cons (cl2item goal)
;                                                          NULL)))))
;                               aset)
;                            (resolvent-of cl1 cl2 li1 li2)) ("abstract"()()))

;                 (l66 () (= (that (lam (rk (o form))
;                                       (set= rk
;                                             (setminus (setminus (union cl1 cl2)
;                                                                 (singleton li1))       
;                                                       (singleton li2)))))
;                            (setminus (setminus (union cl1 cl2)
;                                                                 (singleton li1))       
;                                                       (singleton li2)))                ("abstract"()()))
			    
		 (l99 () (= goal
			    (setminus (setminus (union cl1 cl2);
						(singleton li1))	
				      (singleton li2)))		                                   ("pureSetTh"()())) 

		
;	         (l65 () (= goal
;                            (that (lam (rk (o form))
;                             (set= rk
;                                   (setminus (setminus (union cl1 cl2)
;                                                       (singleton li1)) 
;                                             (singleton li2))))))           ("Subst="(apos10)(l66 l99)))
;        	 
;                 (l64 () (= goal
;                            (resolvent-of cl1 cl2 li1 li2))  ("defnI" (resolvent-of) (l65)))
;
;                 (l63 () (= goal  
;                            (crule-application res-res
;                                               (cons (cl2item cl1)
;                                                     (cons (cl2item cl2)
;                                                           (cons (form2item li1)
;                                                                 (cons (form2item li2)
;                                                                       (cons (cl2item goal)
;                                                                             NULL)))))
;                                               aset)) ("subst=" (apos6) (l64  l67)))
;
;                 (l62 () (= goal         
;                            (crule-application res-res
;                                               (append (append
;                                                        (der-item-inp ait)
;                                                        (poslist2infolist (der-item-prems ait) concat))
;                                                       (cons (cl2item
;                                                              (der-item-info ait))
;                                             NULL))
;                                               aset)) ("Subst=" (apos5) (l63  l68)))
;                 
;                 (l61 () (= goal         
;                            (crule-application (der-item-crule ait)
;                               (append (append
;                                        (der-item-inp ait)
;                                        (poslist2infolist (der-item-prems ait) concat))
;                                       (cons (cl2item
;                                              (der-item-info ait))
;                                             NULL))
;                               aset)) ("Subst=" (apos4) (l62 l69))) 
;                 
;                 (l6 () (= (der-item-info ait)         
;                           (crule-application (der-item-crule ait)
;                              (append (append
;                                       (der-item-inp ait)
;                                       (poslist2infolist (der-item-prems ait) concat))
;                                      (cons (cl2item
;                                             (der-item-info ait))
;                                            NULL))
;                              aset))  ("Subst=" (apos3) (l61 l70)))
;
;                 ;; ---------------------------------------------------------------------------
;                 
;                 (l5 () (forall (lam (y num)
;                                      (implies (is-component (num2item y) (der-item-prems ait))
;                                               (less y
;                                                     (position ait concat))))) ("abstract"()()))
;
;                 ;; -------------------------------------------------------------------
;                 
		
		 

		 (l43 () (resolvable-b cl2 cl1 li1 li2)    ("Open"()()))
		 
;                 (l42 () (crule-applicable res-res
;                                          (cons (cl2item cl1)
;                                                (cons (cl2item cl2)
;                                                      (cons (form2item li1)
;                                                            (cons (form2item li2)
;                                                                  (cons (cl2item goal)
;                                                                        NULL)))))
;                                          aset) ("Subst=" (apos9) (l43 l49)))
;                 
;                 (l41 () (crule-applicable res-res
;                                          (append (append
;                                                   (der-item-inp ait)
;                                                   (poslist2infolist (der-item-prems ait) concat))
;                                                  (cons (cl2item (der-item-info ait))
;                                                        NULL))
;                                          aset) ("subst=" (apos8) (l42 l68)))
;                 
;                 (l4 () (crule-applicable (der-item-crule ait)
;                                          (append (append
;                                                   (der-item-inp ait)
;                                                   (poslist2infolist (der-item-prems ait) concat))
;                                                  (cons (cl2item (der-item-info ait))
;                                                        NULL))
;                                          aset) ("subst=" (apos7) (l41 l69)))
;
;                     ;; ----------------------------------------------------------------
;                     
;                 (l33 () (= (der-item-crule (der-item-constr res-res
;                                                             goal
;                                                             (cons (form2item li1)
;                                                                   (cons (form2item li2) NULL))
;                                                             (cons (num2item (glist-length aseq1))
;                                                                   (cons
;                                                                    (num2item (plus (glist-length aseq1)
;                                                                                    
;                                                                                    (glist-length aseq2)))
;                                                                    NULL))))
;                            res-res)  ("abstract"()()))
;
;                 (l32 () (crule-set res-res) ("abstract"()()))
;                 
;                 (l31 () (crule-set (der-item-crule (der-item-constr res-res
;                                                                     goal
;                                                                     (cons (form2item li1)
;                                                                           (cons (form2item li2) NULL))
;                                                                     (cons (num2item (glist-length aseq1))
;                                                                           (cons
;                                                                            (num2item (plus (glist-length aseq1)
;                                                                                            (glist-length aseq2)))
;                                                                            NULL)))))
;                      ("Subst=" (apos2) (l32 l33)))    
;                     
;                 (l3 () (crule-set (der-item-crule ait))   ("Subst=" (apos1) (l100 l31)))
;                 
;                 (l2 () (and (forall (lam (y num)
;                                      (implies (is-component (num2item y) (der-item-prems ait))
;                                               (less y
;                                                     (position ait concat)))))
;                                   (= (der-item-info ait)         
;                                      (crule-application (der-item-crule ait)
;                                        (append (append
;                                                 (der-item-inp ait)
;                                                 (poslist2infolist (der-item-prems ait) concat))
;                                                (cons (cl2item
;                                                       (der-item-info ait))
;                                                      NULL))
;                                        aset)))    ("AndI" ()(l5 l6)))
;                 
;                 (l1 () (and (crule-set (der-item-crule ait))
;                             (crule-applicable (der-item-crule ait)
;                                               (append (append
;                                                        (der-item-inp ait)
;                                                        (poslist2infolist (der-item-prems ait) concat))
;                                                       (cons (cl2item (der-item-info ait))
;                                                             NULL))
;                                               aset))                 ("AndI" ()(l3 l4)))
		 
		 ;; ------------------------------------------------------

		 ;; zunaechst standard aufloesung der AND
		 (ltp () (and (and (crule-set (der-item-crule ait))
				   (crule-applicable (der-item-crule ait)
				     (append (append
					      (der-item-inp ait)
					      (poslist2infolist (der-item-prems ait) concat))
					     (cons (cl2item (der-item-info ait))
						   NULL))
				     aset))
			      (and (forall (lam (y num)
				      (implies (is-component (num2item y) (der-item-prems ait))
					       (less y
						     (position ait concat)))))
				   (= (der-item-info ait)         
				      (crule-application (der-item-crule ait)
                                        (append (append
						 (der-item-inp ait)
						 (poslist2infolist (der-item-prems ait) concat))
						(cons (cl2item
						       (der-item-info ait))
						      NULL))
					aset)))) ("abstract"()()))
		      ;; ("AndI"()(l1 l2)))
		 )
		(proc-content schema-interpreter)
		(remark "A method for prooving derivability goals with derivability assumtpions.")
		)



;;; =========================================================
;;;
;;;  ELN METHODEN
;;;
;;; =========================================================

(infer~defmethod eln-ind-bc1-m
		 (outline-mappings (((existent nonexistent nonexistent) eln-ind-bc1-m-b)))  
		 (help "."))



(meth~defmethod eln-ind-bc1-m-b eln-ind-bc1-m
		(in prop-res) 
		(rating 30)
		(declarations
		 (type-variables rr)
		 (type-constants o i num form)
		 (constants (dum-cl-set (o (o form))))
		 (sorted-meta-variables ;;(meta-var type sort)
		  (x1 (o form) term)  (y1 (o (o form)) term)
		  (listbox glist term))
		 )
		(parameters             ) 
		(premises (+ l7) (+ l8) )
		(application-condition  )    
		(outline-actions        )
		(outline-orderings      )
		(outline-computations
		 (y1 (type-newconst (termtype dum-cl-set)))
		 (x1 (type-newconst (termtype empty-cl)))
		 (listbox (:term (cons (der-item-constr
					res-start x1 NULL NULL) NULL)))
		 )
		(conclusions (- ltp))
		(decl-content

		 (l9 ()  (in x1 y1) ("Hyp"()()))


		 (l8 (l9) (derivation y1 crule-set        
				      (cons (der-item-constr
					     res-start x1 NULL NULL) NULL))          ("Open" ()()))
		     
		 (l7 (l9) (= (der-item-info
			      (last (cons (der-item-constr
					   res-start x1 NULL NULL) NULL)))
			     x1)                                                        ("Open" ()()))
		 
                 (l6 (l9) (AND (= (DER-ITEM-INFO
                                   (LAST (cons (der-item-constr
                                                res-start x1 NULL NULL) NULL)))
                                  X1)                                                                    
                               (DERIVATION Y1 CRULE-SET (cons (der-item-constr
                                                               res-start x1 NULL NULL) NULL)))
                     ("AndI" () (l7 l8)))
               
                 (l5 (l9) (derivation-of y1 crule-set (cons (der-item-constr
                                                             res-start x1 NULL NULL) NULL) x1)
                     ("DefnI" (derivation-of)(l6)))
                 
                 (l4 (l9) (exists (lam (L glist)                                
                                       (derivation-of y1 crule-set L  x1)))   ("ExistsI" (listbox) (l5)))
		 
		 (l3 (l9) (derivable x1 y1 crule-set)                       ("DefnI" (derivable) (l4)))
		 
		 (l2  () (implies (in x1 y1)
				  (derivable x1 y1 crule-set))                        ("ImpI" () (l3)))
		 
		 (l1  () (forall (lam (y (o (o form)))
			     (implies (in x1 y)
				      (derivable x1 y crule-set))))              ("ForallI" (y1) (l2)))
		 
		 ;; =======================================================
		 
		 (ltp () (forall (lam (x (o form)) (forall (lam (y (o (o form)))
			     (implies (in x y)
				      (derivable x y crule-set))))))        ("ForallI" (x1)(l1)))
		 )
		(proc-content schema-interpreter)
		(remark "")
		)



(infer~defmethod eln-ind-bc1-1-m
		 (outline-mappings (((existent nonexistent nonexistent
					       nonexistent nonexistent) eln-ind-bc1-1-m-b)))  
		 (help "."))



(meth~defmethod eln-ind-bc1-1-m-b eln-ind-bc1-1-m
		(in prop-res) 
		(rating 30)
		(declarations
		 (type-variables rr)
		 (type-constants o i num form)
		 (constants (dum-cl-set (o (o form))))
		 (sorted-meta-variables ;;(meta-var type sort)
		  (y1 (o (o form)) term)
		  (const6 (o (o form)) term)
		  (const7 (o form) term)
		  (ru crule term) (el (o form) term) (aanc form term)(pr glist term) (inp glist term)
		  (pos211 o pos) (pos212 o pos) (pos21 o pos) (pos2 o pos)
		  (alist o list) 
		 )
		 )
		(parameters             ) 
		(premises (+ l9) (+ la3) (+ la4) (+ la5) )
		(application-condition  )    
		(outline-actions        )
		(outline-orderings      )
		(outline-computations
		 (pos2211 (:position (2 2 1 1)))
		 (pos2212 (:position (2 2 1 2)))
		 (pos22  (:position (2 2)))
		 (pos2   (:position (2)))
		 )
		(conclusions (- ltp))
		(decl-content
;		 (ax1 () (forall (lam (ru crule) (forall (lam (el (o form))
;                         (forall (lam (pr glist) (forall (lam (inp glist) 
;                            (= (der-item-inp (der-item-constr ru el pr inp))
;                               inp))))))))) ("Axiom"()()))
;
;                 (la1 () (= (der-item-inp
;                             (der-item-constr res-start
;                                              const7
;                                              NULL
;                                              NULL))
;                               NULL) ("ForallE*+"()(ax1)))
;
;                 (ax2 () (forall (lam (ru crule) (forall (lam (el (o form))
;                          (forall (lam (pr glist) (forall (lam (inp glist) 
;                             (= (der-item-prems (der-item-constr ru el pr inp))
;                                pr))))))))) ("Axiom"()()))
;
;                 (la2 () (= (der-item-prems
;                             (der-item-constr
;                              res-start
;                              const7
;                              NULL
;                              NULL))
;                            NULL)                       ("ForallE*+"()(ax2)))

		 (la3 () (= (POSLIST2INFOLIST                                    
                            NULL                                            
                            (CONS                                               
                             (DER-ITEM-CONSTR                                   
                              RES-START                                         
                              CONST7                                            
                              NULL                                              
                              NULL)                                             
                             NULL))
			    NULL)                                ("Open"()()))

		 (la4  () (= (append NULL NULL)
			    NULL)                                  ("Open"()()))

		 (la5 () (= (append NULL
				    (CONS                                                 
				     (CL2ITEM                                             
				      (DER-ITEM-INFO                                      
				       (DER-ITEM-CONSTR                                   
					RES-START                                         
					CONST7                                            
					NULL                                              
					NULL)))                                           
				     NULL))
			    (CONS                                                 
			     (CL2ITEM                                             
			      (DER-ITEM-INFO                                      
			       (DER-ITEM-CONSTR                                   
				RES-START                                         
				CONST7                                            
				NULL                                              
				NULL)))                                           
			     NULL))  ("Open"()()))

;                 (ax6  () (forall (lam (seq glist) (forall (lam (S (o (o form)))
;                          (equiv (crule-applicable res-start seq S)
;                                 (in (item2cl (first seq)) S)))))) ("Axiom"()()))
;
;                 (la61 () (equiv (crule-applicable
;                                  res-start
;                                  (CONS                                                 
;                                   (CL2ITEM                                             
;                                    (DER-ITEM-INFO                                      
;                                     (DER-ITEM-CONSTR                                   
;                                      RES-START                                         
;                                      CONST7                                            
;                                      NULL                                              
;                                      NULL)))                                           
;                                   NULL)
;                                  const6)
;                                 (in (item2cl (first (CONS                                                 
;                                   (CL2ITEM                                             
;                                    (DER-ITEM-INFO                                      
;                                     (DER-ITEM-CONSTR                                   
;                                      RES-START                                         
;                                      CONST7                                            
;                                      NULL                                              
;                                      NULL)))                                           
;                                   NULL))) const6)) ("ForallE*+"()(ax6)))
;
;                 (la62 () (implies (in (item2cl (first (CONS                                                 
;                                                        (CL2ITEM                                             
;                                                         (DER-ITEM-INFO                                      
;                                                          (DER-ITEM-CONSTR                                   
;                                                           RES-START                                         
;                                                           CONST7                                            
;                                                           NULL                                              
;                                                           NULL)))                                           
;                                                        NULL))) const6)
;                                   (crule-applicable
;                                    res-start
;                                    (CONS                                                 
;                                     (CL2ITEM                                             
;                                      (DER-ITEM-INFO                                      
;                                       (DER-ITEM-CONSTR                                   
;                                        RES-START                                         
;                                        CONST7                                            
;                                        NULL                                              
;                                        NULL)))                                           
;                                     NULL)
;                                    const6))   ("EquivER"()(la61)))
;
;                 (ax7 () (forall (lam (x item) (forall (lam (L glist)
;                            (= (first (cons x L)) x))))) ("Axiom"()()))
;
;                 (la7 () (= (first (cons (CL2ITEM                                             
;                                               (DER-ITEM-INFO                                      
;                                                (DER-ITEM-CONSTR                                   
;                                                 RES-START                                         
;                                                 CONST7                                            
;                                                 NULL                                              
;                                                 NULL))) NULL))
;                            (CL2ITEM                                             
;                             (DER-ITEM-INFO                                      
;                              (DER-ITEM-CONSTR                                   
;                               RES-START                                         
;                               CONST7                                            
;                               NULL                                              
;                               NULL)))) ("ForallE*+"()(ax7)))
;
;                 (ax8 () (forall (lam (akl (o form))
;                            (= (item2cl (cl2item akl))
;                               akl))) ("Axiom"()()))
;
;                 (la8 () (= (item2cl (cl2item (DER-ITEM-INFO                                      
;                                        (DER-ITEM-CONSTR                                   
;                                         RES-START                                         
;                                         CONST7                                            
;                                         NULL                                              
;                                         NULL))))
;                               (DER-ITEM-INFO                                      
;                                        (DER-ITEM-CONSTR                                   
;                                         RES-START                                         
;                                         CONST7                                            
;                                         NULL                                              
;                                         NULL))) ("ForallE*+" () (ax8)))
;
;                 (ax9 () (forall (lam (ru crule) (forall (lam (el (o form))
;                           (forall (lam (pr glist) (forall (lam (inp glist) 
;                                (= (der-item-info (der-item-constr ru el pr inp))
;                                   el))))))))) ("Axiom"()()))
;
;                 (la9 () (= (DER-ITEM-INFO                                      
;                             (DER-ITEM-CONSTR                                   
;                              RES-START                                         
;                              CONST7                                            
;                              NULL                                              
;                              NULL))
;                            const7) ("forallE*+" () (ax9)))

		 (l9 () (in  const7 const6) ("Open"()()))
		 
;                 (l8 () (in (DER-ITEM-INFO                                      
;                             (DER-ITEM-CONSTR                                   
;                              RES-START                                         
;                              CONST7                                            
;                              NULL                                              
;                              NULL)) 
;                             const6) ("Subst=" (pos1) (l9 la9)))
;                      
;                 (l7  () (in (item2cl (CL2ITEM                                             
;                                       (DER-ITEM-INFO                                      
;                                        (DER-ITEM-CONSTR                                   
;                                         RES-START                                         
;                                         CONST7                                            
;                                         NULL                                              
;                                         NULL)))) const6)  ("Subst=" (pos1) (l8 la8)))
;                      
;                 (l6  () (in (item2cl (first (CONS                                                 
;                                              (CL2ITEM                                             
;                                               (DER-ITEM-INFO                                      
;                                                (DER-ITEM-CONSTR                                   
;                                                 RES-START                                         
;                                                 CONST7                                            
;                                                 NULL                                              
;                                                 NULL)))                                           
;                                              NULL))) const6) ("Subst=" (pos11) (l7 la7)))
;
;                 (l5  () (CRULE-APPLICABLE       
;                          RES-START                                              
;                          (CONS                                                 
;                           (CL2ITEM                                             
;                            (DER-ITEM-INFO                                      
;                             (DER-ITEM-CONSTR                                   
;                              RES-START                                         
;                              CONST7                                            
;                              NULL                                              
;                              NULL)))                                           
;                           NULL)                                             
;                          CONST6)    ("ImpE" ()(la62 l6)))
;                 
;                 (l4  () (CRULE-APPLICABLE       
;                          RES-START                                              
;                          (APPEND                                                
;                           NULL                                            
;                          (CONS                                                 
;                           (CL2ITEM                                             
;                            (DER-ITEM-INFO                                      
;                             (DER-ITEM-CONSTR                                   
;                              RES-START                                         
;                              CONST7                                            
;                              NULL                                              
;                              NULL)))                                           
;                           NULL))                                               
;                         CONST6) ("subst=" (pos2) (l5 la5)))
;                 
;                 (l3  () (CRULE-APPLICABLE       
;                          RES-START                                              
;                          (APPEND                                                
;                           (APPEND                                               
;                            NULL                                             
;                           NULL)                                            
;                          (CONS                                                 
;                           (CL2ITEM                                             
;                            (DER-ITEM-INFO                                      
;                             (DER-ITEM-CONSTR                                   
;                              RES-START                                         
;                              CONST7                                            
;                              NULL                                              
;                              NULL)))                                           
;                           NULL))                                               
;                         CONST6) ("subst" (pos21) (l4 la4)))
;                 
;                 (l2  () (CRULE-APPLICABLE       
;                          RES-START                                              
;                          (APPEND                                                
;                           (APPEND                                               
;                            NULL                                             
;                           (POSLIST2INFOLIST                                    
;                            NULL                                            
;                            (CONS                                               
;                             (DER-ITEM-CONSTR                                   
;                              RES-START                                         
;                              CONST7                                            
;                              NULL                                              
;                              NULL)                                             
;                             NULL)))                                            
;                          (CONS                                                 
;                           (CL2ITEM                                             
;                            (DER-ITEM-INFO                                      
;                             (DER-ITEM-CONSTR                                   
;                              RES-START                                         
;                              CONST7                                            
;                              NULL                                              
;                              NULL)))                                           
;                           NULL))                                               
;                         CONST6) ("subst=" (pos212) (l3 la3)))
;                 
;                 (l1  () (CRULE-APPLICABLE       
;                          RES-START                                              
;                          (APPEND                                                
;                           (APPEND                                               
;                            NULL                                             
;                           (POSLIST2INFOLIST                                    
;                            (DER-ITEM-PREMS                                     
;                             (DER-ITEM-CONSTR                                   
;                              RES-START                                         
;                              CONST7                                            
;                              NULL                                              
;                              NULL))                                            
;                            (CONS                                               
;                             (DER-ITEM-CONSTR                                   
;                              RES-START                                         
;                              CONST7                                            
;                              NULL                                              
;                              NULL)                                             
;                             NULL)))                                            
;                          (CONS                                                 
;                           (CL2ITEM                                             
;                            (DER-ITEM-INFO                                      
;                             (DER-ITEM-CONSTR                                   
;                              RES-START                                         
;                              CONST7                                            
;                              NULL                                              
;                              NULL)))                                           
;                           NULL))                                               
;                         CONST6) ("Subst="(pos2121) (l2 la2)))

		 (ltp () (CRULE-APPLICABLE       
			  ;;RES-START
			  (DER-ITEM-CRULE                                        
                           (DER-ITEM-CONSTR                                      
                            RES-START                                            
                            const7                                   
                            NULL                                                 
                            NULL))   
			  (APPEND                                                
			   (APPEND                                               
			    (DER-ITEM-INP                                        
                            (DER-ITEM-CONSTR                                    
                             RES-START                                          
                             CONST7                                             
                             NULL                                               
                             NULL))                                             
                           (POSLIST2INFOLIST                                    
                            (DER-ITEM-PREMS                                     
                             (DER-ITEM-CONSTR                                   
                              RES-START                                         
                              CONST7                                            
                              NULL                                              
                              NULL))                                            
                            (CONS                                               
                             (DER-ITEM-CONSTR                                   
                              RES-START                                         
                              const7                                            
                              NULL                                              
                              NULL)                                             
                             NULL)))                                            
                          (CONS                                                 
                           (CL2ITEM                                             
                            (DER-ITEM-INFO                                      
                             (DER-ITEM-CONSTR                                   
                              RES-START                                         
                              CONST7                                            
                              NULL                                              
                              NULL)))                                           
                           NULL))                                               
                         CONST6) ("Open"()()))
		      ;;("Subst=" (pos211) (l1 la1)))
		  )
		(proc-content schema-interpreter)
		(remark "")
		)


(infer~defmethod eln-ind-bc1-2-m
		 (outline-mappings (((existent nonexistent nonexistent
					       nonexistent nonexistent) eln-ind-bc1-2-m-b)))  
		 (help "."))



(meth~defmethod eln-ind-bc1-2-m-b eln-ind-bc1-2-m
		(in prop-res) 
		(rating 30)
		(declarations
		 (type-variables rr)
		 (type-constants o i num form)
		 (constants (dum-cl-set (o (o form))))
		 (sorted-meta-variables ;;(meta-var type sort)
		  (y1 (o (o form)) term)
		  (const6 (o (o form)) term)
		  (const7 (o form) term)
		  (ru crule term) (el (o form) term) (pr glist term) (inp glist term)
		  (pos211 o pos) (pos212 o pos) (pos21 o pos) (pos2 o pos)
		  (alist o list) 
		 )
		 )
		(parameters             ) 
		(premises (+ l8) (+ la3) (+ la4) (+ la5) )
		(application-condition  )    
		(outline-actions        )
		(outline-orderings      )
		(outline-computations
		 (pos2211 (:position (2 2 1 1)))
		 (pos2212 (:position (2 2 1 2)))
		 (pos22  (:position (2 2)))
		 (pos2   (:position (2)))
		 )
		(conclusions (- ltp))
		(decl-content
;		 (ax1 () (forall (lam (ru crule) (forall (lam (el (o form))
;                         (forall (lam (pr glist) (forall (lam (inp glist) 
;                            (= (der-item-inp (der-item-constr ru el pr inp))
;                               inp))))))))) ("Axiom"()()))
;
;                 (la1 () (= (der-item-inp
;                             (der-item-constr res-start
;                                              const7
;                                              NULL
;                                              NULL))
;                               NULL) ("ForallE*+"()(ax1)))

;                 (ax2 () (forall (lam (ru crule) (forall (lam (el (o form))
;                          (forall (lam (pr glist) (forall (lam (inp glist) 
;                             (= (der-item-prems (der-item-constr ru el pr inp))
;                                pr))))))))) ("Axiom"()()))
;
;                 (la2 () (= (der-item-prems
;                             (der-item-constr
;                              res-start
;                              const7
;                              NULL
;                              NULL))
;                            NULL)                       ("ForallE*+"()(ax2)))

		 (la3 () (= (POSLIST2INFOLIST                                    
                            NULL                                            
                            (CONS                                               
                             (DER-ITEM-CONSTR                                   
                              RES-START                                         
                              CONST7                                            
                              NULL                                              
                              NULL)                                             
                             NULL))
			    NULL)                                ("Open"()()))

		 (la4  () (= (append NULL NULL)
			    NULL)                                  ("Open"()()))

		 (la5 () (= (APPEND                                               
			     NULL                                           
			     (CONS (CL2ITEM CONST7) NULL))
			    (CONS (CL2ITEM CONST7) NULL))("Open"()()))

		 (ax6 () (forall (lam (seq glist) (forall (lam (S (o (o form)))
			   (= (crule-application res-start seq S)
			      (item2cl (first seq)))))))("Axiom"()()))

		 (la6 () (= (crule-application
			     res-start
			     (CONS (CL2ITEM CONST7) NULL)
			     CONST6)
			    (item2cl (first (CONS (CL2ITEM CONST7) NULL)))) ("ForallE*+"()(ax6)))

		 (ax7 () (forall (lam (x item) (forall (lam (L glist)
                      (= (first (cons x L)) x))))) ("Axiom"()()))

		 (la7 () (= (first (cons (CL2ITEM CONST7) NULL)) (CL2ITEM CONST7)) ("ForallE*+"()(ax8)))

		 (ax8 () (forall (lam (akl (o form))
			    (= (item2cl (cl2item akl))
			       akl))) ("Axiom"()()))
		 
		 (la8 () (= (item2cl (cl2item CONST7))
			    CONST7) ("ForallE*+"()(ax8)))
		 
		 (l8 () (=    CONST7 CONST7) ("Open"()()))
		 
		 (l7 () (=                            
			 CONST7                                                 
			 (item2cl(CL2ITEM CONST7)))  ("subst=" (pos2) (l8 la8)))
		 
		 (l6 () (=                            
			 CONST7                                                 
			 (item2cl (first (CONS (CL2ITEM CONST7) NULL)))) ("subst="
									  (pos21)(l7 la7)))
		 
		 (l5 () (=                            
			 CONST7                                                 
			 (CRULE-APPLICATION                                     
			  RES-START                                             
			  (CONS (CL2ITEM CONST7) NULL)                     
			  CONST6)) ("Subst=" (pos22)(l6 la6)))
		      
		 (l4 () (=                            
			 CONST7                                                 
			 (CRULE-APPLICATION                                     
			  RES-START                                             
			  (APPEND                                               
			   NULL                                           
			   (CONS (CL2ITEM CONST7) NULL))                        
			  CONST6)) ("subst=" (pos222)(l5 la5)))
		 
		 (l3 () (=                            
			  CONST7                                                 
			  (CRULE-APPLICATION                                     
			   RES-START                                             
			   (APPEND                                               
			    (APPEND                                              
			     NULL                                            
			     NULL)                                           
			    (CONS (CL2ITEM CONST7) NULL))                        
			   CONST6)) ("subst=" (pos221) (l4 la4)))
		 
		 (l2 ()  (=                            
			  CONST7                                                 
			  (CRULE-APPLICATION                                     
			   RES-START                                             
			   (APPEND                                               
			    (APPEND                                              
			     NULL                                            
			     (POSLIST2INFOLIST                                   
			      NULL                                           
			      (CONS                                              
			       (DER-ITEM-CONSTR                                  
				RES-START                                        
				CONST7                                           
				NULL                                             
				NULL)                                            
			       NULL)))                                           
			    (CONS (CL2ITEM CONST7) NULL))                        
			   CONST6)) ("subst="(pos2212)(l3 la3)))
		 
		 (l1 ()  (=                            
			  CONST7                                                 
			  (CRULE-APPLICATION                                     
			   RES-START                                             
			   (APPEND                                               
			    (APPEND                                              
			     NULL                                            
			     (POSLIST2INFOLIST                                   
			      (DER-ITEM-PREMS                                    
			       (DER-ITEM-CONSTR                                  
				RES-START                                        
				CONST7                                           
				NULL                                             
				NULL))                                           
			      (CONS                                              
			       (DER-ITEM-CONSTR                                  
				RES-START                                        
				CONST7                                           
				NULL                                             
				NULL)                                            
			       NULL)))                                           
			    (CONS (CL2ITEM CONST7) NULL))                        
			   CONST6))  ("Subst=" (pos22121) (l2 la2)))
		 
		 (ltp () (=                            
			  CONST7                                                 
			  (CRULE-APPLICATION                                     
			   (DER-ITEM-CRULE                                        
                           (DER-ITEM-CONSTR                                      
                            RES-START                                            
                            const7                                   
                            NULL                                                 
                            NULL))
			   (APPEND                                               
			    (APPEND                                              
			     (DER-ITEM-INP                                       
			      (DER-ITEM-CONSTR                                   
			       RES-START                                         
			       CONST7                                            
			       NULL                                              
			       NULL))                                            
			     (POSLIST2INFOLIST                                   
			      (DER-ITEM-PREMS                                    
			       (DER-ITEM-CONSTR                                  
				RES-START                                        
				CONST7                                           
				NULL                                             
				NULL))                                           
			      (CONS                                              
			       (DER-ITEM-CONSTR                                  
				RES-START                                        
				CONST7                                           
				NULL                                             
				NULL)                                            
			       NULL)))                                           
			    (CONS (CL2ITEM CONST7) NULL))                        
			   CONST6)) ("Subst="(pos2211)(l1 la1)))
		 )
		(proc-content schema-interpreter)
		(remark "")
		)


(infer~defmethod eln-ind-bc2-1-m
		 (outline-mappings (((existent nonexistent nonexistent nonexistent nonexistent) eln-ind-bc2-1-m-b)))  
		 (help "Methode zur Vereinfachung des ELN-Ind-Schrittes."))


(meth~defmethod eln-ind-bc2-1-m-b eln-ind-bc2-1-m
		(in prop-res) 
		(rating 30)
		(declarations
		 (type-variables rr)
		 (type-constants o i num form)
		 (sorted-meta-variables ;;(meta-var type sort)
		  (y1 (o (o form)) term)
		  (const6 form term)
		  (const7 form term)
		  (ru crule term) (el (o form) term) (pr glist term) (inp glist term)
		  (pos211 o pos) (pos212 o pos) (pos21 o pos) (pos2 o pos)
		  (alist o list)
		 ))
		(parameters             ) 
		(premises (+ ng1) (+ ax3) (+ ax5) (+ ax6))
		(application-condition  )    
		(outline-actions        )
		(outline-orderings      )
		(outline-computations
		 ;; (alist  (mlist ru el pr inp))
		 (pos211 (:position (2 1 1)))
		 (pos212 (:position (2 1 2)))
		 (pos21  (:position (2 1)))
		 (pos2   (:position (2)))
		 )
		(conclusions (- ltp))
		(decl-content
	         (ax1 () (forall (lam (ru crule) (forall (lam (el (o form))
                         (forall (lam (pr glist) (forall (lam (inp glist) 
                            (= (der-item-inp (der-item-constr ru el pr inp))
                               inp))))))))) ("Axiom"()()))

                 (lax1-1 () (= (der-item-inp (der-item-constr
                                              res-res empty-cl
                                              (CONS (NUM2ITEM ZERO) (CONS (NUM2ITEM (S ZERO)) NULL))
                                              (CONS (FORM2ITEM CONST6) (CONS (FORM2ITEM CONST7) NULL))))
                                (CONS (FORM2ITEM CONST6) (CONS (FORM2ITEM CONST7) NULL)))
                         ("ForallE*+" ()(ax1)))
;;                                      (res-res empty-cl
;;                                                (CONS (NUM2ITEM ZERO) (CONS (NUM2ITEM (S ZERO)) NULL))
;;                                                (CONS (FORM2ITEM CONST6) (CONS (FORM2ITEM CONST7) NULL))))))

                 (ax2 () (forall (lam (ru crule) (forall (lam (el (o form))
                          (forall (lam (pr glist) (forall (lam (inp glist) 
                             (= (der-item-prems (der-item-constr ru el pr inp))
                                pr)))))))))   ("Axiom"()()))

                 (lax2-1 () (= (der-item-prems (der-item-constr res-res empty-cl
                                                             (CONS (NUM2ITEM ZERO) (CONS (NUM2ITEM (S ZERO)) NULL))
                                                             (CONS (FORM2ITEM CONST6)  (CONS  (FORM2ITEM CONST7)  NULL))))
                               (CONS (NUM2ITEM ZERO) (CONS (NUM2ITEM (S ZERO)) NULL)))
                         ("forallE*+" ()(ax2)))
;;                                      (res-res empty-cl
;;                                       (CONS (NUM2ITEM ZERO) (CONS (NUM2ITEM (S ZERO)) NULL))
;;                                       (CONS (FORM2ITEM CONST6)  (CONS  (FORM2ITEM CONST7)  NULL))))
;;                          (ax2)))

					
                 (ax3 () (= (POSLIST2INFOLIST                                    
                           (CONS (NUM2ITEM ZERO) (CONS (NUM2ITEM (S ZERO)) NULL))                    
                           (CONS                                               
                            (DER-ITEM-CONSTR                                   
                             RES-START (SINGLETON CONST6) NULL NULL)
                            (CONS  (DER-ITEM-CONSTR                                  
                                    RES-START (SINGLETON CONST7) NULL NULL)    
                                   (CONS                                             
                                    (DER-ITEM-CONSTR                                 
                                     RES-RES EMPTY-CL
                                     (CONS (NUM2ITEM ZERO) (CONS  (NUM2ITEM (S ZERO)) NULL))                                        
                                     (CONS                                           
                                      (FORM2ITEM CONST6)                             
                                      (CONS                                          
                                      (FORM2ITEM CONST7)                            
                                      NULL)))                                       
                                    NULL))))
                          (cons (cl2item (SINGLETON CONST6))
                                (cons (cl2item (SINGLETON CONST7)) NULL)))   ("Open"()()))

                 (ax4 () (forall (lam (ru crule) (forall (lam (el (o form))
                           (forall (lam (pr glist) (forall (lam (inp glist) 
                                (= (der-item-info (der-item-constr ru el pr inp))
                                   el)))))))))  ("Axiom" ()()))

;                 (lax4-1 () (= (der-item-info (der-item-constr res-res
;                                                               empty-cl
;                                                               (CONS (NUM2ITEM ZERO) (CONS (NUM2ITEM (S ZERO)) NULL))
;                                                               (CONS (FORM2ITEM CONST6) (CONS (FORM2ITEM CONST7) NULL))))
;                               empty-cl) ("ForallE*+" ()(ax4)))
;;                                               (res-res empty-cl
;;                                                 (CONS (NUM2ITEM ZERO) (CONS (NUM2ITEM (S ZERO)) NULL))
;;                                                 (CONS (FORM2ITEM CONST6) (CONS (FORM2ITEM CONST7) NULL))))
;;                                    (ax4)))

                 (ax5 () (= (APPEND                                               
                             (CONS (FORM2ITEM CONST6) (CONS (FORM2ITEM CONST7) NULL))                
                             (cons (cl2item (SINGLETON CONST6))
                                   (cons (cl2item (SINGLETON CONST7)) NULL)))
                            (CONS (FORM2ITEM CONST6)
                                  (CONS (FORM2ITEM CONST7)
                                        (cons (cl2item (SINGLETON CONST6))
                                              (cons (cl2item (SINGLETON CONST7)) NULL))))) ("Open"()()))

                 (ax6 () (= (APPEND                                                
                           (CONS (FORM2ITEM CONST6)
                                  (CONS (FORM2ITEM CONST7)
                                        (cons (cl2item (SINGLETON CONST6))
                                              (cons (cl2item (SINGLETON CONST7)) NULL))))                                       
                            (CONS (CL2ITEM  empty-cl) NULL))
                           (CONS (FORM2ITEM CONST6)
                                 (CONS (FORM2ITEM CONST7)
                                       (cons (cl2item (SINGLETON CONST6))
                                             (cons (cl2item (SINGLETON CONST7))
                                                   (CONS (CL2ITEM  empty-cl) NULL) ))))) ("Open"()()))

                 ;; ---------------------------------

                 (ng1 () (CRULE-APPLICABLE                          
                          RES-RES                                                
                          (CONS (FORM2ITEM CONST6)
                                (CONS (FORM2ITEM CONST7)
                                      (cons (cl2item (SINGLETON CONST6))
                                            (cons (cl2item (SINGLETON CONST7))
                                                  (CONS (CL2ITEM  empty-cl) NULL) ))))                                               
                          Y1)     ("Open"()()))
                 
                 ;; ----------------------------------
                 
                 (l5 () (CRULE-APPLICABLE                          
                         RES-RES                                                
                         (APPEND                                                
                           (CONS (FORM2ITEM CONST6)
                                  (CONS (FORM2ITEM CONST7)
                                        (cons (cl2item (SINGLETON CONST6))
                                              (cons (cl2item (SINGLETON CONST7)) NULL))))                                       
                           (CONS (CL2ITEM  empty-cl) NULL))                                               
                         Y1) ("Subst=" (pos2) (ax6 ng1)))
                 
                 (l4 () (CRULE-APPLICABLE                          
                         RES-RES                                                
                         (APPEND                                                
                          (APPEND                                               
                           (CONS (FORM2ITEM CONST6) (CONS (FORM2ITEM CONST7) NULL))                
                           (cons (cl2item (SINGLETON CONST6))
                                 (cons (cl2item (SINGLETON CONST7)) NULL)))                                        
                          (CONS (CL2ITEM  empty-cl) NULL))                                               
                         Y1) ("Subst=" (pos21) (ax5 l5)))
                 
                 (l3 () (CRULE-APPLICABLE                          
                         RES-RES                                                
                         (APPEND                                                
                          (APPEND                                               
                           (CONS (FORM2ITEM CONST6) (CONS (FORM2ITEM CONST7) NULL))                
                           (cons (cl2item (SINGLETON CONST6))
                                 (cons (cl2item (SINGLETON CONST7)) NULL)))                                        
                          (CONS                                                 
                           (CL2ITEM                                             
                            (DER-ITEM-INFO                                      
                             (DER-ITEM-CONSTR                                   
                              RES-RES                                           
                              EMPTY-CL                                          
                              (CONS                                             
                               (NUM2ITEM ZERO)                                  
                               (CONS (NUM2ITEM (S ZERO)) NULL))                 
                              (CONS                                             
                               (FORM2ITEM CONST6)                               
                               (CONS                                            
                                (FORM2ITEM CONST7)                              
                                NULL)))))                                       
                           NULL))                                               
                         Y1) ("Subst=" ()(lax4-1 l4)))
                     
                 (l2  () (CRULE-APPLICABLE                          
                       RES-RES                                                
                       (APPEND                                                
                        (APPEND                                               
                         (CONS (FORM2ITEM CONST6) (CONS (FORM2ITEM CONST7) NULL))                
                         (POSLIST2INFOLIST                                    
                          (CONS (NUM2ITEM ZERO) (CONS (NUM2ITEM (S ZERO)) NULL))                    
                          (CONS                                               
                           (DER-ITEM-CONSTR                                   
                            RES-START (SINGLETON CONST6) NULL NULL)
                           (CONS  (DER-ITEM-CONSTR                                  
                                   RES-START (SINGLETON CONST7) NULL NULL)    
                                  (CONS                                             
                                   (DER-ITEM-CONSTR                                 
                                    RES-RES EMPTY-CL
                                    (CONS (NUM2ITEM ZERO) (CONS  (NUM2ITEM (S ZERO)) NULL))                                        
                                    (CONS                                           
                                     (FORM2ITEM CONST6)                             
                                     (CONS                                          
                                      (FORM2ITEM CONST7)                            
                                      NULL)))                                       
                                   NULL)))))                                        
                        (CONS                                                 
                           (CL2ITEM                                             
                            (DER-ITEM-INFO                                      
                             (DER-ITEM-CONSTR                                   
                              RES-RES                                           
                              EMPTY-CL                                          
                              (CONS                                             
                               (NUM2ITEM ZERO)                                  
                               (CONS (NUM2ITEM (S ZERO)) NULL))                 
                              (CONS                                             
                               (FORM2ITEM CONST6)                               
                               (CONS                                            
                                (FORM2ITEM CONST7)                              
                                NULL)))))                                       
                           NULL))                                               
                         Y1) ("Subst="(pos212) (lax3 l3)))

                 (l1  () (CRULE-APPLICABLE                          
                       RES-RES                                                
                       (APPEND                                                
                        (APPEND                                               
                         (CONS (FORM2ITEM CONST6) (CONS (FORM2ITEM CONST7) NULL))                
                         (POSLIST2INFOLIST                                    
                          (DER-ITEM-PREMS                                     
                           (DER-ITEM-CONSTR                                   
                            RES-RES EMPTY-CL (CONS (NUM2ITEM ZERO) (CONS (NUM2ITEM (S ZERO)) NULL))                 
                                             (CONS (FORM2ITEM CONST6)  (CONS  (FORM2ITEM CONST7)  NULL))))                     
                          (CONS                                               
                           (DER-ITEM-CONSTR                                   
                            RES-START (SINGLETON CONST6) NULL NULL)  (CONS  (DER-ITEM-CONSTR                                  
                                                                             RES-START                                        
                                                                             (SINGLETON CONST7)                               
                                                                             NULL      NULL)    
                            (CONS                                             
                             (DER-ITEM-CONSTR                                 
                              RES-RES EMPTY-CL (CONS (NUM2ITEM ZERO) (CONS                                          
                                (NUM2ITEM (S ZERO))                           
                                NULL))                                        
                                (CONS                                           
                                 (FORM2ITEM CONST6)                             
                                 (CONS                                          
                                  (FORM2ITEM CONST7)                            
                                  NULL)))                                       
                             NULL)))))                                        
                        (CONS                                                 
                           (CL2ITEM                                             
                            (DER-ITEM-INFO                                      
                             (DER-ITEM-CONSTR                                   
                              RES-RES                                           
                              EMPTY-CL                                          
                              (CONS                                             
                               (NUM2ITEM ZERO)                                  
                               (CONS (NUM2ITEM (S ZERO)) NULL))                 
                              (CONS                                             
                               (FORM2ITEM CONST6)                               
                               (CONS                                            
                                (FORM2ITEM CONST7)                              
                                NULL)))))                                       
                           NULL))                                               
                         Y1)   ("Subst=" (pos) (lax2-1 l2)))
		 
		 (ltp  () (CRULE-APPLICABLE                          
		       ;;RES-RES
			(DER-ITEM-CRULE                                        
			 (DER-ITEM-CONSTR                                      
			  RES-RES                                              
			  EMPTY-CL                                             
			  (CONS                                                
			   (NUM2ITEM ZERO)                                     
			   (CONS (NUM2ITEM (S ZERO)) NULL))                    
			  (CONS                                                
			   (FORM2ITEM CONST6)                                  
			   (CONS (FORM2ITEM CONST7) NULL))))     
		       (APPEND                                                
			(APPEND                                               
			 (DER-ITEM-INP                                        
			  (DER-ITEM-CONSTR                                    
			   RES-RES  EMPTY-CL (CONS (NUM2ITEM ZERO) (CONS (NUM2ITEM (S ZERO)) NULL))                  
			                     (CONS (FORM2ITEM CONST6) (CONS (FORM2ITEM CONST7) NULL))))                 
			 (POSLIST2INFOLIST                                    
			  (DER-ITEM-PREMS                                     
			   (DER-ITEM-CONSTR                                   
			    RES-RES EMPTY-CL (CONS (NUM2ITEM ZERO) (CONS (NUM2ITEM (S ZERO)) NULL))                 
			                     (CONS (FORM2ITEM CONST6)  (CONS  (FORM2ITEM CONST7)  NULL))))                     
			  (CONS                                               
			   (DER-ITEM-CONSTR                                   
			    RES-START
			    (SINGLETON CONST6)
			    NULL NULL)
			   (CONS
			    (DER-ITEM-CONSTR
			     RES-START (SINGLETON CONST7)
			     NULL
			     NULL)    
			    (CONS                                             
			     (DER-ITEM-CONSTR                                 
			      RES-RES EMPTY-CL (CONS (NUM2ITEM ZERO) (CONS                                          
				(NUM2ITEM (S ZERO))                           
				NULL))                                        
                                (CONS                                           
                                 (FORM2ITEM CONST6)                             
                                 (CONS                                          
                                  (FORM2ITEM CONST7)                            
                                  NULL)))                                       
			     NULL)))))                                        
			(CONS                                                 
                           (CL2ITEM                                             
                            (DER-ITEM-INFO                                      
                             (DER-ITEM-CONSTR                                   
                              RES-RES                                           
                              EMPTY-CL                                          
                              (CONS                                             
                               (NUM2ITEM ZERO)                                  
                               (CONS (NUM2ITEM (S ZERO)) NULL))                 
                              (CONS                                             
                               (FORM2ITEM CONST6)                               
                               (CONS                                            
                                (FORM2ITEM CONST7)                              
                                NULL)))))                                       
                           NULL))                                               
                         Y1)  ("Subst=" (pos211) (l1 lax1-1)))  
		 )
		(proc-content schema-interpreter)
		(remark "Method to simplify the eln-ind base-case.")
		)


(infer~defmethod eln-ind-bc2-2-m
		 (outline-mappings (((existent nonexistent nonexistent nonexistent nonexistent) eln-ind-bc2-2-m-b)))  
		 (help "Methode zur Vereinfachung des ELN-Ind-Schrittes."))


(meth~defmethod eln-ind-bc2-2-m-b eln-ind-bc2-2-m
		(in prop-res) 
		(rating 30)
		(declarations
		 (type-variables rr)
		 (type-constants o i num form)
		 (sorted-meta-variables ;;(meta-var type sort)
		  (y1 (o (o form)) term)
		  (const6 form term)
		  (const7 form term)
		  (cost7  form term)
		  (ru crule term) (el (o form) term) (pr glist term) (inp glist term)
		  (pos211 o pos) (pos212 o pos) (pos21 o pos) (pos2 o pos)
		  (alist o list)
		  (acl-const (o form) term)
		 ))
		(parameters             ) 
		(premises (+ l9) (+ ax3) (+ ax4) (+ ax5))
		(application-condition  )    
		(outline-actions        )
		(outline-orderings      )
		(outline-computations
		 ;; (alist  (mlist ru el pr inp))
		 (pos2211 (:position (2 2 1 1)))
		 (pos2212 (:position (2 2 1 2)))
		 (pos22  (:position (2 2)))
		 (pos2   (:position (2)))
		 (pos11111 (:position (1 1 1 1 1)))
		 (pos111   (:position (1 1 1)))
		 (pos1     (:position (1)))
		 )
		(conclusions (- ltp))
		(decl-content

;                 (ax1 () (forall (lam (ru crule) (forall (lam (el (o form))
;                         (forall (lam (pr glist) (forall (lam (inp glist) 
;                            (= (der-item-inp (der-item-constr ru el pr inp))
;                               inp))))))))) ("Axiom"()()))
;                 
;                 (la1 () (= (der-item-inp
;                             (der-item-constr res-start
;                                              (singleton const7)
;                                              NULL NULL))
;                            NULL) ("Foralle*+"()()))
;
;                 (ax2 () (forall (lam (ru crule) (forall (lam (el (o form))
;                          (forall (lam (pr glist) (forall (lam (inp glist) 
;                             (= (der-item-prems (der-item-constr ru el pr inp))
;                                pr))))))))) ("Axiom" ()()))
;
;                 (la2 () (= (der-item-prems
;                             (der-item-constr res-start
;                                              (SINGLETON CONST7)
;                                              NULL
;                                              NULL))
;                            NULL) ("ForallE*+"()()))

                 (ax3 () (= (POSLIST2INFOLIST                       
                             NULL                                         
                             (CONS                                               
                              (DER-ITEM-CONSTR                                   
                               RES-START                                         
                               (SINGLETON CONST6)                                
                               NULL                                              
                               NULL)                                             
                               (CONS                                              
                                (DER-ITEM-CONSTR                                  
                                 RES-START                                        
                                 (SINGLETON CONST7)                               
                                 NULL                                             
                                 NULL)                                            
                                (CONS                                             
                                 (DER-ITEM-CONSTR                                 
                                  RES-RES                                         
                                  EMPTY-CL                                        
                                  (CONS                                           
                                   (NUM2ITEM ZERO)                                
                                   (CONS                                          
                                    (NUM2ITEM (S ZERO))                           
                                    NULL))                                        
                                  (CONS                                           
                                   (FORM2ITEM CONST6)                             
                                   (CONS                                          
                                    (FORM2ITEM CONST7)                            
                                    NULL)))                                       
                                 NULL))))
                            NULL)   ("Open"()()))

                 (ax4 () (= (APPEND NULL NULL)
                            NULL) ("Open"()()))

                 (ax5 () (= (APPEND                                                
                             NULL                                        
                             (CONS                                                 
                              (CL2ITEM                                             
                               (DER-ITEM-INFO                                      
                                (DER-ITEM-CONSTR                                   
                                 RES-START                                         
                                 (SINGLETON CONST7)                                
                                 NULL                                              
                                 NULL)))                                           
                              NULL))
                            (CONS                                                 
                              (CL2ITEM                                             
                               (DER-ITEM-INFO                                      
                                (DER-ITEM-CONSTR                                   
                                 RES-START                                         
                                 (SINGLETON CONST7)                                
                                 NULL                                              
                                 NULL)))                                           
                              NULL)) ("Open"()()))

;
;                 (ax6 () (forall (lam (seq glist) (forall (lam (S (o (o form)))
;                          (equiv (crule-applicable res-start seq S)
;                                 (in (item2cl (first seq)) S)))))) ("axiom"()()))
;
;                 (la6 () (equiv (crule-applicable res-start
;                                                  (CONS                                                 
;                                                   (CL2ITEM                                             
;                                                    (DER-ITEM-INFO                                      
;                                                     (DER-ITEM-CONSTR                                   
;                                                      RES-START                                         
;                                                      (SINGLETON CONST7)                                
;                                                      NULL                                              
;                                                      NULL)))                                           
;                                                   NULL)
;                                                  Y1)
;                                (in (item2cl (first (CONS                                                 
;                                                     (CL2ITEM                                             
;                                                      (DER-ITEM-INFO                                      
;                                                       (DER-ITEM-CONSTR                                   
;                                                        RES-START                                         
;                                                        (SINGLETON CONST7)                                
;                                                        NULL                                              
;                                                        NULL)))                                           
;                                                     NULL) )) Y1))
;                      ("forallE*+"()(ax6)))
;
;                 (la61 () (implies (in (item2cl (first (CONS                                                 
;                                                     (CL2ITEM                                             
;                                                      (DER-ITEM-INFO                                      
;                                                       (DER-ITEM-CONSTR                                   
;                                                        RES-START                                         
;                                                        (SINGLETON CONST7)                                
;                                                        NULL                                              
;                                                        NULL)))                                           
;                                                     NULL) )) Y1)
;                                   (crule-applicable res-start
;                                                     (CONS                                                 
;                                                      (CL2ITEM                                             
;                                                       (DER-ITEM-INFO                                      
;                                                        (DER-ITEM-CONSTR                                   
;                                                         RES-START                                         
;                                                         (SINGLETON CONST7)                                
;                                                         NULL                                              
;                                                         NULL)))                                           
;                                                      NULL)
;                                                     Y1))
;                       ("EquiveR" () (la6)))
;
;
;                 (ax7 () (forall (lam (ru crule) (forall (lam (el (o form))
;                           (forall (lam (pr glist) (forall (lam (inp glist) 
;                                (= (der-item-info (der-item-constr ru el pr inp))
;                                   el))))))))) ("Axiom"()()))
;
;                 (la7 () (= (der-item-info
;                             (der-item-constr
;                              res-start
;                              (SINGLETON CONST7)
;                              NULL NULL))
;                            (singleton const7)) ("ForallE*+"()(ax7)))
;
;                 (ax8 () (forall (lam (x item) (forall (lam (L glist)
;                                 (= (first (cons x L)) x))))) ("Axiom"()()))
;
;                 (la8 () (= (first (cons (CL2ITEM (singleton const7))
;                                         NULL ))
;                            (CL2ITEM (singleton const7))) ("ForallE*+"()(ax8)))
;
;                 (ax9 () (forall (lam (akl (o form))
;                            (= (item2cl (cl2item akl))
;                               akl)))                              ("Axiom"()()))
;                 
;                 (la9 () (= (item2cl (CL2ITEM                                             
;                                      (singleton const7)))
;                            (singleton const7))            ("ForallE*+"()(ax9)))
		 
		 (l9 () (in (singleton cost7)
			    Y1)                                    ("Open"()()))
		 
;	         (l8 () (in (item2cl (CL2ITEM                                             
;                                      (singleton const7)))
;                            Y1)                      ("Subst=" (pos1) (l9 la9)))
;                 
;                 (l7 () (in (item2cl (first (CONS                                                 
;                                             (CL2ITEM                                             
;                                              (singleton const7))                        
;                                             NULL) ))
;                            Y1)                      ("Subst="(pos111) (l8 la8)))
;                     
;                 (l6 () (in (item2cl (first (CONS
;                                             (CL2ITEM                                             
;                                              (DER-ITEM-INFO                                      
;                                               (DER-ITEM-CONSTR                                   
;                                                RES-START                                         
;                                                (SINGLETON CONST7)                                
;                                                NULL                                              
;                                                NULL)))                                           
;                                             NULL) )) Y1) ("subst=" (pos11111) (l7 la7)))   
;                      
;                 (l5 (l6) (CRULE-APPLICABLE   
;                           RES-START                                              
;                           (CONS                                                 
;                            (CL2ITEM                                             
;                             (DER-ITEM-INFO                                      
;                              (DER-ITEM-CONSTR                                   
;                               RES-START                                         
;                               (SINGLETON CONST7)                                
;                               NULL                                              
;                               NULL)))                                           
;                            NULL)                                              
;                           Y1)   ("ImpE"()(l6 la61)))
;                     
;                 (l4 () (CRULE-APPLICABLE   
;                           RES-START                                              
;                           (APPEND                                                
;                            NULL                                        
;                            (CONS                                                 
;                             (CL2ITEM                                             
;                              (DER-ITEM-INFO                                      
;                               (DER-ITEM-CONSTR                                   
;                                RES-START                                         
;                                (SINGLETON CONST7)                                
;                                NULL                                              
;                                NULL)))                                           
;                             NULL))                                               
;                           Y1)   ("subst=" (pos2) (l5 ax5)))
;                 
;                 (l3 () (CRULE-APPLICABLE   
;                           RES-START                                              
;                           (APPEND                                                
;                            (APPEND NULL NULL)                                        
;                            (CONS                                                 
;                             (CL2ITEM                                             
;                              (DER-ITEM-INFO                                      
;                               (DER-ITEM-CONSTR                                   
;                                RES-START                                         
;                                (SINGLETON CONST7)                                
;                                NULL                                              
;                                NULL)))                                           
;                             NULL))                                               
;                           Y1) ("Subst=" (pos21) (l4 ax4)))
;                            
;                 (l2 () (CRULE-APPLICABLE   
;                           RES-START                                              
;                           (APPEND                                                
;                            (APPEND                               
;                             NULL                                           
;                             (POSLIST2INFOLIST                       
;                              NULL                                         
;                              (CONS                                               
;                               (DER-ITEM-CONSTR                                   
;                                RES-START                                         
;                                (SINGLETON CONST6)                                
;                                NULL                                              
;                                NULL)                                             
;                               (CONS                                              
;                                (DER-ITEM-CONSTR                                  
;                                 RES-START                                        
;                                 (SINGLETON CONST7)                               
;                                 NULL                                             
;                                 NULL)                                            
;                                (CONS                                             
;                                 (DER-ITEM-CONSTR                                 
;                                  RES-RES                                         
;                                  EMPTY-CL                                        
;                                  (CONS                                           
;                                   (NUM2ITEM ZERO)                                
;                                   (CONS                                          
;                                    (NUM2ITEM (S ZERO))                           
;                                    NULL))                                        
;                                  (CONS                                           
;                                   (FORM2ITEM CONST6)                             
;                                   (CONS                                          
;                                    (FORM2ITEM CONST7)                            
;                                    NULL)))                                       
;                                 NULL)))))                                        
;                            (CONS                                                 
;                             (CL2ITEM                                             
;                              (DER-ITEM-INFO                                      
;                               (DER-ITEM-CONSTR                                   
;                                RES-START                                         
;                                (SINGLETON CONST7)                                
;                                NULL                                              
;                                NULL)))                                           
;                             NULL))                                               
;                           Y1) ("subst=" (pos212) (l3 ax3)))
;                 
;                 (l1  () (CRULE-APPLICABLE   
;                           RES-START                                              
;                           (APPEND                                                
;                            (APPEND                               
;                             NULL                                           
;                             (POSLIST2INFOLIST                                    
;                              (DER-ITEM-PREMS                                     
;                               (DER-ITEM-CONSTR                                   
;                                RES-START                                         
;                                (SINGLETON CONST7)                                
;                                NULL                                              
;                                NULL))                                            
;                              (CONS                                               
;                               (DER-ITEM-CONSTR                                   
;                                RES-START                                         
;                                (SINGLETON CONST6)                                
;                                NULL                                              
;                                NULL)                                             
;                               (CONS                                              
;                                (DER-ITEM-CONSTR                                  
;                                 RES-START                                        
;                                 (SINGLETON CONST7)                               
;                                 NULL                                             
;                                 NULL)                                            
;                                (CONS                                             
;                                 (DER-ITEM-CONSTR                                 
;                                  RES-RES                                         
;                                  EMPTY-CL                                        
;                                  (CONS                                           
;                                   (NUM2ITEM ZERO)                                
;                                   (CONS                                          
;                                    (NUM2ITEM (S ZERO))                           
;                                    NULL))                                        
;                                  (CONS                                           
;                                   (FORM2ITEM CONST6)                             
;                                   (CONS                                          
;                                    (FORM2ITEM CONST7)                            
;                                    NULL)))                                       
;                                 NULL)))))                                        
;                            (CONS                                                 
;                             (CL2ITEM                                             
;                              (DER-ITEM-INFO                                      
;                               (DER-ITEM-CONSTR                                   
;                                RES-START                                         
;                                (SINGLETON CONST7)                                
;                                NULL                                              
;                                NULL)))                                           
;                             NULL))                                               
;                           Y1)  ("Subst=" (pos2121) (l2 la2)))

		 (ltp ()  (CRULE-APPLICABLE   
			   (DER-ITEM-CRULE                                        
                           (DER-ITEM-CONSTR                                      
                            RES-START                                            
                            (singleton cost7)                                   
                            NULL                                                 
                            NULL))                                         
			   (APPEND                                                
			    (APPEND                                               
			     (DER-ITEM-INP                                        
			      (DER-ITEM-CONSTR                                    
			       RES-START                                          
			       (SINGLETON COST7)   ;; Cost7 noch noch oben expabdieren in
						   ;; die restlichen Zeilen       
			       NULL                                               
			       NULL))                                             
			     (POSLIST2INFOLIST                                    
			      (DER-ITEM-PREMS                                     
			       (DER-ITEM-CONSTR                                   
				RES-START                                         
				(SINGLETON COST7)                                
				NULL                                              
				NULL))                                            
			      (CONS                                               
			       (DER-ITEM-CONSTR                                   
				RES-START                                         
				(SINGLETON CONST6)                                
				NULL                                              
				NULL)                                             
			       (CONS                                              
				(DER-ITEM-CONSTR                                  
				 RES-START                                        
				 (SINGLETON CONST7)                               
				 NULL                                             
				 NULL)                                            
				(CONS                                             
				 (DER-ITEM-CONSTR                                 
				  RES-RES                                         
				  EMPTY-CL                                        
				  (CONS                                           
				   (NUM2ITEM ZERO)                                
				   (CONS                                          
				    (NUM2ITEM (S ZERO))                           
				    NULL))                                        
				  (CONS                                           
				   (FORM2ITEM CONST6)                             
				   (CONS                                          
				    (FORM2ITEM CONST7)                            
				    NULL)))                                       
				 NULL)))))                                        
			    (CONS                                                 
			     (CL2ITEM                                             
			      (DER-ITEM-INFO                                      
			       (DER-ITEM-CONSTR                                   
				RES-START                                         
				(SINGLETON COST7)                                
				NULL                                              
				NULL)))                                           
			     NULL))                                               
			   Y1) ("Open"()()))
		      ;; ("Subst="(pos22)(l1 la1)))
		 )
		(proc-content schema-interpreter)
		(remark "Method to simplify the eln-ind base-case.")
		)


(infer~defmethod eln-ind-bc2-3-m
		 (outline-mappings (((existent nonexistent) eln-ind-bc2-3-m-b)))  
		 (help "Methode zur Vereinfachung des ELN-Ind-Schrittes."))


(meth~defmethod eln-ind-bc2-3-m-b eln-ind-bc2-3-m
		(in prop-res) 
		(rating 30)
		(declarations
		 (type-variables rr)
		 (type-constants o i num form)
		 (sorted-meta-variables ;;(meta-var type sort)
		  (y1 (o (o form)) term)
		  (const6 form term)
		  (const7 form term)
		   ))
		(parameters             ) 
		(premises (+ l1))
		(application-condition  )    
		(outline-actions        )
		(outline-orderings      )
		(outline-computations
		 ;; (alist  (mlist ru el pr inp))
		 )
		(conclusions (- ltp))
		(decl-content


		 (l1  () (RESOLVABLE-B                                    
			  (SINGLETON CONST6)
			  (SINGLETON CONST7)
			  const6
			  const7
			  ) ("Open"()()))
		 
		 (ltp () (RESOLVABLE-B         
			  (INPUT-FIRST                                           
			   (CONS                                                 
			    (FORM2ITEM CONST6)                                   
			    (CONS                                                
			     (FORM2ITEM CONST7)                                  
			     (CONS                                               
			      (CL2ITEM (SINGLETON CONST6))                       
			      (CONS                                              
			       (CL2ITEM (SINGLETON CONST7))                      
			       (CONS                                             
				(CL2ITEM EMPTY-CL)                               
				NULL))))))                                       
			  (INPUT-SECOND                                          
			   (CONS                                                 
			    (FORM2ITEM CONST6)                                   
                           (CONS                                                
                            (FORM2ITEM CONST7)                                  
                            (CONS                                               
                             (CL2ITEM (SINGLETON CONST6))                       
                             (CONS                                              
                              (CL2ITEM (SINGLETON CONST7))                      
                              (CONS                                             
                               (CL2ITEM EMPTY-CL)                               
                               NULL))))))                                       
                         (INPUT-THIRD                                           
                          (CONS                                                 
                           (FORM2ITEM CONST6)                                   
                           (CONS                                                
                            (FORM2ITEM CONST7)                                  
                            (CONS                                               
                             (CL2ITEM (SINGLETON CONST6))                       
                             (CONS                                              
                              (CL2ITEM (SINGLETON CONST7))                      
                              (CONS                                             
                               (CL2ITEM EMPTY-CL)                               
                               NULL))))))                                       
                         (INPUT-FOURTH                                          
                          (CONS                                                 
                           (FORM2ITEM CONST6)                                   
                           (CONS                                                
                            (FORM2ITEM CONST7)                                  
                            (CONS                                               
                             (CL2ITEM (SINGLETON CONST6))                       
                             (CONS                                              
                              (CL2ITEM (SINGLETON CONST7))                      
                              (CONS                                             
                               (CL2ITEM EMPTY-CL)                               
                               NULL)))))))
		 ("Defni"()(l1)))
		 )
		(proc-content schema-interpreter)
		(remark "Method to simplify the eln-ind base-case.")
		)


(infer~defmethod eln-ind-bc2-4-m
		 (outline-mappings (((existent nonexistent nonexistent
					       nonexistent nonexistent) eln-ind-bc2-4-m-b)))  
		 (help "Methode zur Vereinfachung des ELN-Ind-Schrittes."))

(meth~defmethod eln-ind-bc2-4-m-b eln-ind-bc2-4-m
		(in prop-res) 
		(rating 30)
		(declarations
		 (type-variables rr)
		 (type-constants o i num form)
		 (sorted-meta-variables ;;(meta-var type sort)
		  (y1 (o (o form)) term)
		  (const6 form term)
		  (const7 form term)
		  (ru crule term) (el (o form) term) (pr glist term) (inp glist term)
		  (pos211 o pos) (pos212 o pos) (pos21 o pos) (pos2 o pos)
		  (alist o list)
		 )
		 )
		(parameters             ) 
		(premises (+ l8) (+ ax4) (+ ax5) (+ ax6))
		(application-condition  )    
		(outline-actions        )
		(outline-orderings      )
		(outline-computations
		 ;; (alist  (mlist ru el pr inp))
		 (pos2211 (:position (2 2 1 1)))
		 (pos2212 (:position (2 2 1 2)))
		 (pos22  (:position (2 2)))
		 (pos2   (:position (2)))
		 )
		(conclusions (- ltp))
		(decl-content


		 (ax1 () (FORALL (lam (SEQ GLIST) (forall (lam (S (O (O FORM))) 
                              (=                                                  
                               (CRULE-APPLICATION RES-RES SEQ S)   
                               (RESOLVENT-OF                      
                                (INPUT-FIRST SEQ)            
                                (INPUT-SECOND SEQ)       
                                (INPUT-THIRD SEQ)       
                                (INPUT-FOURTH SEQ))))))) ("Axiom"()())) 
		 
		 (ax2 () (forall (lam (ru crule) (forall (lam (el (o form))
			 (forall (lam (pr glist) (forall (lam (inp glist) 
	                    (= (der-item-inp (der-item-constr ru el pr inp))
			       inp)))))))))                         ("Axiom"()()))

		 (l3ax1 () (= (der-item-inp
			       (der-item-constr
				RES-RES
				EMPTY-CL
				(CONS                                             
				 (NUM2ITEM ZERO)                                  
				 (CONS (NUM2ITEM (S ZERO)) NULL))
				(CONS                                             
				 (FORM2ITEM CONST6)                               
				 (CONS                                            
				  (FORM2ITEM CONST7)                              
				  NULL))))
			      (CONS                                             
			       (FORM2ITEM CONST6)                               
			       (CONS                                            
				(FORM2ITEM CONST7)                              
				NULL)) )                        ("ForallE*+"()(ax2)))

		 (ax3 () (forall (lam (ru crule) (forall (lam (el (o form))
			  (forall (lam (pr glist) (forall (lam (inp glist) 
	                     (= (der-item-prems (der-item-constr ru el pr inp))
				pr)))))))))    ("axion"()()))

		 (l3ax3 () (= (der-item-prems
			       (der-item-constr
				RES-RES                                          
				EMPTY-CL
				(CONS                                            
                                (NUM2ITEM ZERO)                                 
                                (CONS                                           
                                 (NUM2ITEM (S ZERO))                            
                                 NULL))
				(CONS                                            
                                (FORM2ITEM CONST6)                              
                                (CONS                                           
                                 (FORM2ITEM CONST7)                             
                                 NULL))))
				(CONS                                            
                                (NUM2ITEM ZERO)                                 
                                (CONS                                           
                                 (NUM2ITEM (S ZERO))                            
                                 NULL)))                    ("ForallE*+"()(ax3)))

		 (ax4 () (= (POSLIST2INFOLIST                                    
			     (CONS (NUM2ITEM ZERO) (CONS (NUM2ITEM (S ZERO)) NULL))                    
			     (CONS                                               
			      (DER-ITEM-CONSTR                                   
			       RES-START (SINGLETON CONST6) NULL NULL)
			      (CONS  (DER-ITEM-CONSTR                                  
				      RES-START (SINGLETON CONST7) NULL NULL)    
				     (CONS                                             
				      (DER-ITEM-CONSTR                                 
				       RES-RES EMPTY-CL
				       (CONS (NUM2ITEM ZERO) (CONS  (NUM2ITEM (S ZERO)) NULL))   
				       (CONS                                           
					(FORM2ITEM CONST6)                             
					(CONS                                          
					 (FORM2ITEM CONST7)                            
					 NULL)))                                       
				      NULL))))
			    (cons (cl2item (SINGLETON CONST6))
				  (cons (cl2item (SINGLETON CONST7)) NULL))) ("Open"()()))
		      

		 (ax5 () (= (APPEND                                              
			     (CONS                                             
			      (FORM2ITEM CONST6)                               
			      (CONS                                            
			       (FORM2ITEM CONST7)                              
			       NULL))                                     
			     (cons (cl2item (SINGLETON CONST6))
				   (cons (cl2item (SINGLETON CONST7)) NULL)))
			    (CONS (FORM2ITEM CONST6)                               
				  (CONS (FORM2ITEM CONST7)                              
					(cons (cl2item (SINGLETON CONST6))
					      (cons (cl2item (SINGLETON CONST7)) NULL))
					)))                                   ("Open"()()))

		 (ax6 () (= (APPEND                                               
			     (CONS (FORM2ITEM CONST6)                               
				   (CONS (FORM2ITEM CONST7)                              
					 (cons (cl2item (SINGLETON CONST6))
					       (cons (cl2item (SINGLETON CONST7)) NULL)))) 
			     (CONS (CL2ITEM EMPTY-CL) NULL))
			    (CONS (FORM2ITEM CONST6)                               
				   (CONS (FORM2ITEM CONST7)                              
					 (cons (cl2item (SINGLETON CONST6))
					       (cons (cl2item (SINGLETON CONST7))
						     (CONS (CL2ITEM EMPTY-CL) NULL)))))) ("Open"()()))

		 (ax7 () (forall (lam (seq glist) (forall (lam (S (o (o form)))
			 (= (crule-application res-res seq S)
			    (resolvent-of (input-first seq) (input-second seq)
					  (input-third seq) (input-fourth seq)))))))
		      ("Axiom"()()))

		 (l37 () (= (crule-application res-res
						 (CONS (FORM2ITEM CONST6)                               
						       (CONS (FORM2ITEM CONST7)                              
							     (cons (cl2item (SINGLETON CONST6))
								   (cons (cl2item (SINGLETON CONST7))
									 (CONS (CL2ITEM EMPTY-CL)  NULL)))))
						 Y1)
			      (resolvent-of
			       (input-first (CONS (FORM2ITEM CONST6)                               
						       (CONS (FORM2ITEM CONST7)                              
							     (cons (cl2item (SINGLETON CONST6))
								   (cons (cl2item (SINGLETON CONST7))
									 (CONS (CL2ITEM EMPTY-CL)  NULL))))))
			       (input-second (CONS (FORM2ITEM CONST6)                               
						       (CONS (FORM2ITEM CONST7)                              
							     (cons (cl2item (SINGLETON CONST6))
								   (cons (cl2item (SINGLETON CONST7))
									 (CONS (CL2ITEM EMPTY-CL)  NULL))))))
			       (input-third (CONS (FORM2ITEM CONST6)                               
						       (CONS (FORM2ITEM CONST7)                              
							     (cons (cl2item (SINGLETON CONST6))
								   (cons (cl2item (SINGLETON CONST7))
									 (CONS (CL2ITEM EMPTY-CL)  NULL))))))
			       (input-fourth (CONS (FORM2ITEM CONST6)                               
						       (CONS (FORM2ITEM CONST7)                              
							     (cons (cl2item (SINGLETON CONST6))
								   (cons (cl2item (SINGLETON CONST7))
									 (CONS (CL2ITEM EMPTY-CL)  NULL))))))))
			("ForallE*+"()(ax7)))

		 (l8 () (=                                               
			 EMPTY-CL
			 (that (lam (rk (o form))
			     (set= rk
				   (setminus (setminus (union (singleton const6)(singleton const7))
						       (singleton const6))	
					     (singleton const7))))))                    ("Open"()()))
			
		 
		 ;; Dieser Zeilensprung ist abgekuerztz und falsch (von l6 nach l7)
		 (l7 () (=                                               
			 EMPTY-CL                                               
			 (resolvent-of
			  (singleton const6) (singleton const7)
			  const6 const7  ))                                       ("DefnI"(resolvemt-of)(l8)))
			
		 (l6 () (=                                               
			 EMPTY-CL                                               
			 (resolvent-of
			       (input-first (CONS (FORM2ITEM CONST6)                               
						       (CONS (FORM2ITEM CONST7)                              
							     (cons (cl2item (SINGLETON CONST6))
								   (cons (cl2item (SINGLETON CONST7))
									 (CONS (CL2ITEM EMPTY-CL)  NULL))))))
			       (input-second (CONS (FORM2ITEM CONST6)                               
						       (CONS (FORM2ITEM CONST7)                              
							     (cons (cl2item (SINGLETON CONST6))
								   (cons (cl2item (SINGLETON CONST7))
									 (CONS (CL2ITEM EMPTY-CL)  NULL))))))
			       (input-third (CONS (FORM2ITEM CONST6)                               
						       (CONS (FORM2ITEM CONST7)                              
							     (cons (cl2item (SINGLETON CONST6))
								   (cons (cl2item (SINGLETON CONST7))
									 (CONS (CL2ITEM EMPTY-CL)  NULL))))))
			       (input-fourth (CONS (FORM2ITEM CONST6)                               
						       (CONS (FORM2ITEM CONST7)                              
							     (cons (cl2item (SINGLETON CONST6))
								   (cons (cl2item (SINGLETON CONST7))
									 (CONS (CL2ITEM EMPTY-CL)  NULL))))))))
		     ("Foralle*+"()(l7)))
		     
		 
		 (l5 () (=                                               
			 EMPTY-CL                                               
			 (CRULE-APPLICATION                                     
			  RES-RES                                               
			  (CONS (FORM2ITEM CONST6)                               
				(CONS (FORM2ITEM CONST7)                              
				      (cons (cl2item (SINGLETON CONST6))
					    (cons (cl2item (SINGLETON CONST7))
						  (CONS (CL2ITEM EMPTY-CL) NULL)))))                     
                          Y1)) ("subst="(pos2)(l37 l6)))
		     
		 (l4 () (=                                               
			 EMPTY-CL                                               
			 (CRULE-APPLICATION                                     
			  RES-RES                                               
			  (APPEND                                               
			   (CONS (FORM2ITEM CONST6)                               
				  (CONS (FORM2ITEM CONST7)                              
					(cons (cl2item (SINGLETON CONST6))
					      (cons (cl2item (SINGLETON CONST7)) NULL)))) 
                           (CONS (CL2ITEM EMPTY-CL) NULL))                      
                          Y1)) ("subst=" (pos22) (l5 ax6)))
		 

		 (l3 () (=                                               
			 EMPTY-CL                                               
			 (CRULE-APPLICATION                                     
			  RES-RES                                               
			  (APPEND                                               
			   (APPEND                                              
			    (CONS                                             
			     (FORM2ITEM CONST6)                               
			     (CONS                                            
			      (FORM2ITEM CONST7)                              
			      NULL))                                     
			    (cons (cl2item (SINGLETON CONST6))
				  (cons (cl2item (SINGLETON CONST7)) NULL))
			    )                                       
                           (CONS (CL2ITEM EMPTY-CL) NULL))                      
                          Y1))   ("Subst="(pos221)(ax5 l4)))
		 

		 (l2 () (=                                               
			 EMPTY-CL                                               
			 (CRULE-APPLICATION                                     
			  RES-RES                                               
			  (APPEND                                               
			   (APPEND                                              
			    (CONS                                             
			     (FORM2ITEM CONST6)                               
			     (CONS                                            
			      (FORM2ITEM CONST7)                              
			      NULL))                                     
                            (POSLIST2INFOLIST                                   
			     (CONS                                            
			      (NUM2ITEM ZERO)                                 
			      (CONS                                           
			       (NUM2ITEM (S ZERO))                            
			       NULL))
                             (CONS                                              
                              (DER-ITEM-CONSTR                                  
                               RES-START                                        
                               (SINGLETON CONST6)                               
                               NULL                                             
                               NULL)                                            
                              (CONS                                             
                               (DER-ITEM-CONSTR                                 
                                RES-START                                       
                                (SINGLETON CONST7)                              
                                NULL                                            
                                NULL)                                           
                               (CONS                                            
                                (DER-ITEM-CONSTR                                
                                 RES-RES                                        
                                 EMPTY-CL                                       
                                 (CONS                                          
                                  (NUM2ITEM ZERO)                               
                                  (CONS                                         
                                   (NUM2ITEM (S ZERO))                          
                                   NULL))                                       
                                 (CONS                                          
                                  (FORM2ITEM CONST6)                            
                                  (CONS                                         
                                   (FORM2ITEM CONST7)                           
                                   NULL)))                                      
                                NULL)))))                                       
                           (CONS (CL2ITEM EMPTY-CL) NULL))                      
                          Y1))   ("Subst=" (pos222) (ax4 l3)))
				      
		 (l1 () (=                                               
			 EMPTY-CL                                               
			 (CRULE-APPLICATION                                     
			  RES-RES                                               
			  (APPEND                                               
			   (APPEND                                              
			    (CONS                                             
			     (FORM2ITEM CONST6)                               
			     (CONS                                            
			      (FORM2ITEM CONST7)                              
			      NULL))                                     
                            (POSLIST2INFOLIST                                   
                             (DER-ITEM-PREMS                                    
                              (DER-ITEM-CONSTR                                  
                               RES-RES                                          
                               EMPTY-CL                                         
                               (CONS                                            
                                (NUM2ITEM ZERO)                                 
                                (CONS                                           
                                 (NUM2ITEM (S ZERO))                            
                                 NULL))                                         
                               (CONS                                            
                                (FORM2ITEM CONST6)                              
                                (CONS                                           
                                 (FORM2ITEM CONST7)                             
                                 NULL))))                                       
                             (CONS                                              
                              (DER-ITEM-CONSTR                                  
                               RES-START                                        
                               (SINGLETON CONST6)                               
                               NULL                                             
                               NULL)                                            
                              (CONS                                             
                               (DER-ITEM-CONSTR                                 
                                RES-START                                       
                                (SINGLETON CONST7)                              
                                NULL                                            
                                NULL)                                           
                               (CONS                                            
                                (DER-ITEM-CONSTR                                
                                 RES-RES                                        
                                 EMPTY-CL                                       
                                 (CONS                                          
                                  (NUM2ITEM ZERO)                               
                                  (CONS                                         
                                   (NUM2ITEM (S ZERO))                          
                                   NULL))                                       
                                 (CONS                                          
                                  (FORM2ITEM CONST6)                            
                                  (CONS                                         
                                   (FORM2ITEM CONST7)                           
                                   NULL)))                                      
                                NULL)))))                                       
                           (CONS (CL2ITEM EMPTY-CL) NULL))                      
                          Y1)) ("subst="(pos22121) (l3ax3 l2)))
		 
		 (ltp ()
		      (=                                               
		       EMPTY-CL                                               
		       (CRULE-APPLICATION                                     
			;;RES-RES
			(DER-ITEM-CRULE                                       
			 (DER-ITEM-CONSTR                                     
			  RES-RES                                             
			  EMPTY-CL                                            
			  (CONS                                               
			   (NUM2ITEM ZERO)                                    
			   (CONS (NUM2ITEM (S ZERO)) NULL))                   
			  (CONS                                               
			   (FORM2ITEM CONST6)                                 
			   (CONS (FORM2ITEM CONST7) NULL)))) 
			(APPEND                                               
			 (APPEND                                              
			  (DER-ITEM-INP                                       
			   (DER-ITEM-CONSTR                                   
                              RES-RES                                           
                              EMPTY-CL                                          
                              (CONS                                             
                               (NUM2ITEM ZERO)                                  
                               (CONS (NUM2ITEM (S ZERO)) NULL))                 
                              (CONS                                             
                               (FORM2ITEM CONST6)                               
                               (CONS                                            
                                (FORM2ITEM CONST7)                              
                                NULL))))                                        
                            (POSLIST2INFOLIST                                   
                             (DER-ITEM-PREMS                                    
                              (DER-ITEM-CONSTR                                  
                               RES-RES                                          
                               EMPTY-CL                                         
                               (CONS                                            
                                (NUM2ITEM ZERO)                                 
                                (CONS                                           
                                 (NUM2ITEM (S ZERO))                            
                                 NULL))                                         
                               (CONS                                            
                                (FORM2ITEM CONST6)                              
                                (CONS                                           
                                 (FORM2ITEM CONST7)                             
                                 NULL))))                                       
                             (CONS                                              
                              (DER-ITEM-CONSTR                                  
                               RES-START                                        
                               (SINGLETON CONST6)                               
                               NULL                                             
                               NULL)                                            
                              (CONS                                             
                               (DER-ITEM-CONSTR                                 
                                RES-START                                       
                                (SINGLETON CONST7)                              
                                NULL                                            
                                NULL)                                           
                               (CONS                                            
                                (DER-ITEM-CONSTR                                
                                 RES-RES                                        
                                 EMPTY-CL                                       
                                 (CONS                                          
                                  (NUM2ITEM ZERO)                               
                                  (CONS                                         
                                   (NUM2ITEM (S ZERO))                          
                                   NULL))                                       
                                 (CONS                                          
                                  (FORM2ITEM CONST6)                            
                                  (CONS                                         
                                   (FORM2ITEM CONST7)                           
                                   NULL)))                                      
                                NULL)))))                                       
                           (CONS (CL2ITEM EMPTY-CL) NULL))                      
                          Y1))                        ("Subst=" (pos2211)(l3ax1 l1)))
		 )
		(proc-content schema-interpreter)
		(remark "Method to simplify the eln-ind base-case.")
		)


(infer~defmethod eln-ind-bc2-5-m
		 (outline-mappings (((existent nonexistent nonexistent
					       nonexistent nonexistent) eln-ind-bc2-5-m-b)))  
		 (help "Methode zur Vereinfachung des ELN-Ind-Schrittes."))


(meth~defmethod eln-ind-bc2-5-m-b eln-ind-bc2-5-m
		(in prop-res) 
		(rating 30)
		(declarations
		 (type-variables rr)
		 (type-constants o i num form)
		 (sorted-meta-variables ;;(meta-var type sort)
		  (y1 (o (o form)) term)
		  (const6 form term)
		  (const7 form term) (cost7 form term)
		  (ru crule term) (el (o form) term) (pr glist term) (inp glist term)
		  (pos211 o pos) (pos212 o pos) (pos21 o pos) (pos2 o pos)
		  (alist o list)
		 ))
		(parameters             ) 
		(premises (+ l8) (+ la3) (+ la4) (+ la5))
		(application-condition  )    
		(outline-actions        )
		(outline-orderings      )
		(outline-computations
		 ;; (alist  (mlist ru el pr inp))
		 (pos2211 (:position (2 2 1 1)))
		 (pos2212 (:position (2 2 1 2)))
		 (pos22  (:position (2 2)))
		 (pos2   (:position (2)))
		 )
		(conclusions (- ltp))
		(decl-content

		 (ax1 () (forall (lam (ru crule) (forall (lam (el (o form))
			 (forall (lam (pr glist) (forall (lam (inp glist) 
	                    (= (der-item-inp (der-item-constr ru el pr inp))
			       inp)))))))))                        ("Axiom" ()()))
		 
		 (la1 () (= (der-item-inp
			     (der-item-constr
			      res-start
			      (singleton const7)
			      NULL
			      NULL))
			    NULL) ("ForallE*+"()(ax1)))

		 (ax2 () (forall (lam (ru crule) (forall (lam (el (o form))
			  (forall (lam (pr glist) (forall (lam (inp glist) 
	                     (= (der-item-prems (der-item-constr ru el pr inp))
				pr)))))))))                           ("Axiom"()()))

		 (la2 () (= (der-item-prems
			     (der-item-constr
			      res-start
			      (singleton const7)
			      NULL
			      NULL))
			    NULL) ("ForallE*+"()(ax2)))
		      
		 (la3 () (= (POSLIST2INFOLIST                                   
			      NULL                                            
			      (CONS                                              
			       (DER-ITEM-CONSTR                                  
				RES-START                                        
				(SINGLETON CONST6)                               
				NULL                                             
				NULL)                                            
			       (CONS                                             
				(DER-ITEM-CONSTR                                 
				 RES-START                                       
				 (SINGLETON CONST7)                              
				 NULL                                            
				 NULL)                                           
				(CONS                                            
				 (DER-ITEM-CONSTR                                
				  RES-RES                                        
				  EMPTY-CL                                       
				  (CONS                                          
				   (NUM2ITEM ZERO)                               
				   (CONS                                         
				    (NUM2ITEM (S ZERO))                          
				    NULL))                                       
				  (CONS                                          
				   (FORM2ITEM CONST6)                            
				   (CONS                                         
				    (FORM2ITEM CONST7)                           
				    NULL)))                                      
				 NULL))))
			    NULL) ("Open"()()))

		 (la4 () (= (append NULL NULL)
			    NULL) ("Open"()()))

		 (la5 () (= (APPEND                                               
			     NULL                                       
			     (CONS                                                
			      (CL2ITEM (SINGLETON CONST7))                        
			      NULL))
			    (CONS                                                
			      (CL2ITEM (SINGLETON CONST7))                        
			      NULL)) ("Open"()()))


		 (ax6 () (forall (lam (seq glist) (forall (lam (S (o (o form)))
			   (= (crule-application res-start seq S)
			      (item2cl (first seq))))))) ("Axiom"()()))

		 (la6 () (= (crule-application res-start
					       (CONS                                                
						(CL2ITEM (SINGLETON CONST7))                        
						NULL)
					       y1)
			      (item2cl
			       (first (CONS                                                
				       (CL2ITEM (SINGLETON CONST7))                        
				       NULL)))) ("ForallE*+"()(ax6)))
		 
		 (ax7 () (forall (lam (x item) (forall (lam (L glist)
			         (= (first (cons x L)) x))))) ("Axiom"()()))
		 
		 (la7 () (= (first (cons (CL2ITEM (SINGLETON CONST7)) NULL))
			    (CL2ITEM (SINGLETON CONST7)))  ("ForallE*+" () (ax7)))

		 (ax8 () (forall (lam (akl (o form))
			    (= (item2cl (cl2item akl))
			       akl))) ("Axiom"()()))

		 (la8 () (= (item2cl (cl2item (SINGLETON CONST7)))
			    (SINGLETON CONST7)) ("Foralle*+" ()(ax8)))

		 (l8 () (=                                              
			 (SINGLETON COST7)                                     
			 (SINGLETON COST7)) ("Open"()()))
		 
		 (l7 () (=                                              
			 (SINGLETON CONST7)                                     
			 (item2cl                                             
			  (CL2ITEM (SINGLETON CONST7)))) ("Subst=" (pos2)(l8 la8)))         
		 
		 (l6 () (=                                              
			 (SINGLETON CONST7)                                     
			 (item2cl
			  (first (CONS                                                
				  (CL2ITEM (SINGLETON CONST7))                        
				  NULL)))) ("Subst=" (pos21) (l7 la7)))
		 
		 (l5 () (=                                              
			 (SINGLETON CONST7)                                     
			 (CRULE-APPLICATION                                     
			  RES-START                                             
			  (CONS                                                
			   (CL2ITEM (SINGLETON CONST7))                        
			   NULL)                                             
			  Y1))   ("Subst=" (pos2) (l6 la6)))
		 
		 (l4 () (=                                              
			 (SINGLETON CONST7)                                     
			 (CRULE-APPLICATION                                     
			  RES-START                                             
			  (APPEND                                               
			   NULL                                       
			   (CONS                                                
			    (CL2ITEM (SINGLETON CONST7))                        
			    NULL))                                              
			  Y1)) ("Subst=" (pos22) (l5 la5)))
		 
		 (l3 () (=                                              
			  (SINGLETON CONST7)                                     
			  (CRULE-APPLICATION                                     
			   RES-START                                             
			   (APPEND                                               
			    (APPEND                                              
			     NULL                                            
			     NULL)                                       
			    (CONS                                                
			     (CL2ITEM (SINGLETON CONST7))                        
			     NULL))                                              
			   Y1)) ("Subst=" (pos221) (l4 la4)))
		      
		 (l2 ()  (=                                              
			  (SINGLETON CONST7)                                     
			  (CRULE-APPLICATION                                     
			   RES-START                                             
			   (APPEND                                               
			    (APPEND                                              
			     NULL                                            
			     (POSLIST2INFOLIST                                   
			      NULL                                            
			      (CONS                                              
			       (DER-ITEM-CONSTR                                  
				RES-START                                        
				(SINGLETON CONST6)                               
				NULL                                             
				NULL)                                            
			       (CONS                                             
				(DER-ITEM-CONSTR                                 
				 RES-START                                       
				 (SINGLETON CONST7)                              
				 NULL                                            
				 NULL)                                           
				(CONS                                            
				 (DER-ITEM-CONSTR                                
				  RES-RES                                        
				  EMPTY-CL                                       
				  (CONS                                          
				   (NUM2ITEM ZERO)                               
				   (CONS                                         
				    (NUM2ITEM (S ZERO))                          
				    NULL))                                       
				  (CONS                                          
				   (FORM2ITEM CONST6)                            
				   (CONS                                         
				    (FORM2ITEM CONST7)                           
				    NULL)))                                      
				 NULL)))))                                       
			    (CONS                                                
			     (CL2ITEM (SINGLETON CONST7))                        
			     NULL))                                              
			   Y1))  ("Subst=" (pos2212) (l3 la3)))
		  
		 (l1  () (=                                              
			  (SINGLETON CONST7)                                     
			  (CRULE-APPLICATION                                     
			   RES-START                                             
			   (APPEND                                               
			    (APPEND                                              
			     NULL                                            
			     (POSLIST2INFOLIST                                   
			      (DER-ITEM-PREMS                                    
			       (DER-ITEM-CONSTR                                  
				RES-START                                        
				(SINGLETON CONST7)                               
				NULL                                             
				NULL))                                           
			      (CONS                                              
			       (DER-ITEM-CONSTR                                  
				RES-START                                        
				(SINGLETON CONST6)                               
				NULL                                             
				NULL)                                            
			       (CONS                                             
				(DER-ITEM-CONSTR                                 
				 RES-START                                       
				 (SINGLETON CONST7)                              
				 NULL                                            
				 NULL)                                           
				(CONS                                            
				 (DER-ITEM-CONSTR                                
				  RES-RES                                        
				  EMPTY-CL                                       
				  (CONS                                          
				   (NUM2ITEM ZERO)                               
				   (CONS                                         
				    (NUM2ITEM (S ZERO))                          
				    NULL))                                       
				  (CONS                                          
				   (FORM2ITEM CONST6)                            
				   (CONS                                         
				    (FORM2ITEM CONST7)                           
				    NULL)))                                      
				 NULL)))))                                       
			    (CONS                                                
			     (CL2ITEM (SINGLETON CONST7))                        
			     NULL))                                              
			   Y1)) ("Subst="(pos22121)(l2 la2)))

		 (ltp () (=                                              
			  (SINGLETON COST7)                                     
			  (CRULE-APPLICATION                                     
			   (DER-ITEM-CRULE                                        
                           (DER-ITEM-CONSTR                                      
                            RES-START                                            
                            (singleton cost7)                                   
                            NULL                                                 
                            NULL))                                           
			   (APPEND                                               
			    (APPEND                                              
			     (DER-ITEM-INP                                       
			      (DER-ITEM-CONSTR                                   
			       RES-START                                         
			       (SINGLETON COST7)                                
			       NULL                                              
			       NULL))                                            
			     (POSLIST2INFOLIST                                   
			      (DER-ITEM-PREMS                                    
			       (DER-ITEM-CONSTR                                  
				RES-START                                        
				(SINGLETON COST7)                               
				NULL                                             
				NULL))                                           
			      (CONS                                              
			       (DER-ITEM-CONSTR                                  
				RES-START                                        
				(SINGLETON CONST6)                               
				NULL                                             
				NULL)                                            
			       (CONS                                             
				(DER-ITEM-CONSTR                                 
				 RES-START                                       
				 (SINGLETON CONST7)                              
				 NULL                                            
				 NULL)                                           
				(CONS                                            
				 (DER-ITEM-CONSTR                                
				  RES-RES                                        
				  EMPTY-CL                                       
				  (CONS                                          
				   (NUM2ITEM ZERO)                               
				   (CONS                                         
				    (NUM2ITEM (S ZERO))                          
				    NULL))                                       
				  (CONS                                          
				   (FORM2ITEM CONST6)                            
				   (CONS                                         
				    (FORM2ITEM CONST7)                           
				    NULL)))                                      
				 NULL)))))                                       
			    (CONS                                                
			     (CL2ITEM (SINGLETON COST7))                        
			     NULL))                                              
			   Y1))("Subst=" (pos2211)(l1 la1)))
		 )
		(proc-content schema-interpreter)
		(remark "Method to simplify the eln-ind base-case.")
		)


(infer~defmethod eln-ind-bc2-m
		 (outline-mappings (((existent nonexistent nonexistent) eln-ind-bc2-m-b)))  
		 (help "Methode zur Vereinfachung des ELN-Ind-Schrittes."))

(meth~defmethod eln-ind-bc2-m-b eln-ind-bc2-m
		(in prop-res) 
		(rating 30)
		(declarations
		 (type-variables rr)
		 (type-constants o i num form)
		 (constants (dum-cl-set (o (o form))))
		 (sorted-meta-variables ;;(meta-var type sort)
		  (y1 (o (o form)) term) (cl1 form term) (cl2 form term)
		 ))
		(parameters             ) 
		(premises (+ ng1) (+ l3))
		(application-condition  )    
		(outline-actions
		 (l3 (sponsor l14))
		 )
		(outline-orderings      )
		(outline-computations
		 (cl1 (type-newconst (:type form)))
		 (cl2 (type-newconst (:type form)))
		 (term (:term
			(cons (der-item-constr
			       res-start (singleton cl1) NULL NULL)
			      (cons (der-item-constr
				     res-start (singleton cl2) NULL NULL)
				    (cons (der-item-constr
					   res-res empty-cl
					   (cons (num2item zero) (cons (num2item (s zero)) NULL))
					   (cons (form2item cl1) (cons (form2item cl2) NULL)))
					  NULL)))))
		 )
		(conclusions (- ltp))
		(decl-content

		 (h1  () (AND  (FORALL (lam (X (O FORM))                                   
                                            (IMPLIES (IN X Y1) (SETUNIT X))))                         
                               (UNSAT-CL-SET Y1))                  )

                 (ng1 () (exists (lam (l1 form) (exists (lam (l2 form)
                             (and (and (y1 (singleton l1))
                                       (y1 (singleton l2))) 
                                  (com-pair l1 l2))))))    ("Open"()()))

                 (l15 () (exists (lam (l2 form)
                             (and (and (y1 (singleton cl1))
                                       (y1 (singleton l2))) 
                                  (com-pair cl1 l2))))     ("Hyp"()()))

                 (l14 () (and (and (y1 (singleton cl1))
				   (y1 (singleton cl2))) 
                                  (com-pair cl1 cl2))      ("Hyp"()()))
 
                 (l3  (l14) (DERIVATION-OF
                          Y1 CRULE-SET
                          (cons (der-item-constr
                                 res-start (singleton cl1) NULL NULL)
                                (cons (der-item-constr
                                       res-start (singleton cl2) NULL NULL)
                                      (cons (der-item-constr
                                             res-res empty-cl
                                             (cons (num2item zero) (cons (num2item (s zero)) NULL))
                                             (cons (form2item cl1) (cons (form2item cl2) NULL)))
                                            NULL)))
                          EMPTY-CL)                                             ("Open"()()))
                 
                 (l2  (l15 l14) (EXISTS (lam (dc-308 glist)      
                                (DERIVATION-OF Y1 CRULE-SET DC-308 EMPTY-CL))) ("ExistsI"
                                                                                (term) (l3)))

                 (l1  (l15)  (EXISTS (lam (dc-308 glist)      
                                (DERIVATION-OF Y1 CRULE-SET DC-308 EMPTY-CL)))  ("ExistsE"(cl2)(l2 l15))) 
                 
                 ;; --------------------------------------------------------------------
                 
                 (ltp () (EXISTS (lam (dc-308 glist)      
                            (DERIVATION-OF                                         
                             Y1                                                    
                             CRULE-SET                                             
                             DC-308                                                
                             EMPTY-CL)     ))           ("ExistsE" (cl1) (l1 ng1)))
		 )
		(proc-content schema-interpreter)
		(remark "Proof, that y ist last Element of an list.")
		)



;; =======================================================================
;;
;;  DISJUNCTION LEMMA   METODEN 
;;
;; =======================================================================

(infer~defmethod disj-ind
		 (outline-mappings (((existent existent nonexistent
					       nonexistent nonexistent nonexistent
					       nonexistent nonexistent nonexistent
					       )
				     disj-ind-m-b)))
		 (help "Methode zur Nutzung der Menge K1"))

(meth~defmethod disjl-ind-m-b disj-ind  
		(in prop-res) 
		(rating 30)
		(reasoning :planning)
		(declarations
		 (constants               )
		 (sorted-meta-variables
		  (co8 (o form) term)
		  (co9 (o (o form)) term)
		  (dc43 glist term)
		  (cx12 num term)
		  (lastdc43 item term)
		  (sL glist term)
		  (li1 form term)
		  (li2 form term)
		  (ih1-seq glist term)
		  (ih2-seq glist term)
		  (somenum num term)
		  (numc1 num term)
		  (numc2 num term)
		  (aseq1 glist term)
		  (aseq2 glist term)
		  )
		 )
		(parameters ) 
		(premises
		 (- h0)
		 (+ l122)                         ;; Trivialfall: co8 ist in co9
		 (+ l53) (+ l61) (+ l82) (+ l96)
		 (+ as7) (+ as8)                  ;; Anschluss an die Induktionsvoraussetzung eigentlich
		                                  ;; auch as5 und as6
		 )
		(application-condition )  
		(outline-actions
		  (l122 (sponsor l30))
;                  
                  (l61 (sponsor n20)) (l61 (sponsor n3)) (l61 (sponsor l67)) (l61 (sponsor l68)) (l61 (sponsor l69))
;
		  (l53 (sponsor n21)) (l53 (sponsor n3)) (l53 (sponsor l57)) (l53 (sponsor l59)) (l53 (sponsor l69))
;
		  (l82 (sponsor n22)) (l82 (sponsor n3)) (l82 (sponsor l93)) (l82 (sponsor l94)) (l82 (sponsor l110))
;
		  (l96 (sponsor n23)) (l96 (sponsor n3)) (l96 (sponsor l108)) (l96 (sponsor l109)) (l96 (sponsor l110))
;
		  (as7 (sponsor as3)) (as7 (sponsor as5))
;
		  (as8 (sponsor as4)) (as8 (sponsor as6))
		 )
		(outline-orderings
		   (before l53 l61) (before l61 l82) (before l82 l96) (before l96 l122) (before l122 as7) (before as7 as8)
		 )
		(outline-computations
		 (lastdc43 (:term (last dc43)))
		 (sL (type-newconst (:type glist)))
		 (numc1 (type-newconst (:type num)))
		 (numc2 (type-newconst (:type num)))
		 (li1   (type-newconst (:type form)))
		 (li2   (type-newconst (:type form)))
		 (co8   (type-newconst (:type (o form))))
		 (aseq1 (type-newconst (:type glist)))
		 (aseq2 (type-newconst (:type glist)))
		 (ih1-seq (type-newconst (:type glist)))
		 (ih2-seq (type-newconst (:type glist)))
		 )
		(conclusions (- ltp))
		(decl-content

		 (h0  ()     (greater cx12 somenum) )
		 
                 (h1  ()     (= (minus (glist-length dc43) one) cx12) ("Hyp"()()))

                 (h2  ()     (derivation-of (union (setminus co9 (singleton (good-clause co9)))
                                                   (singleton
                                                    (setminus (good-clause co9)
                                                              (singleton (good-literal
                                                                          (good-clause co9))))))
                                            crule-set dc43 co8)       ("Hyp"()()))

                 (n1 ()   (or (and (and (less numc1 cx12) (less numc2 cx12))
                                   (= dc43 (append sL
                                               (cons (der-item-constr
                                                      res-res
                                                      co8
                                                      (cons (form2item li1) (cons (form2item li2) NULL))
                                                      (cons (num2item numc1) (cons (num2item numc2) NULL)))
                                                     NULL))))
                              (= dc43 (append sL
                                               (cons (der-item-constr
                                                      res-start
                                                      co8
                                                      NULL NULL)
                                                     NULL)))) ("abstract"()()))

		 (n20 ()  (resolvable-b (der-item-info (glist-nth numc1 dc43))
				       (der-item-info (glist-nth numc2 dc43))
				       li1 li2)  	                                     ("abstract" ()()))

		 (n21 ()  (resolvable-b (der-item-info (glist-nth numc1 dc43))
				       (der-item-info (glist-nth numc2 dc43))
				       li1 li2)
		      ("abstract" ()()))

		 (n22 ()  (resolvable-b (der-item-info (glist-nth numc1 dc43))
				       (der-item-info (glist-nth numc2 dc43))
				       li1 li2)  	                                     ("abstract" ()()))

		 (n23 ()  (resolvable-b (der-item-info (glist-nth numc1 dc43))
				       (der-item-info (glist-nth numc2 dc43))
				       li1 li2)  	                                     ("abstract" ()()))

		 (n3 () (= co8
			   (resolvent-of (der-item-info (glist-nth numc1 dc43))
					 (der-item-info (glist-nth numc2 dc43))
					 li1 li2))                      		     ("abstract" ()()))

		 ;; ----------------------------------------------------------------------------

		 ;; K1 leitet co8 mit Hilfe der Startregel ab und ist deshalb kein
		 ;; Trivialfall: h4 zeigt die struktur von dc43 diese wird aufgeloest in
		 ;; der as-Zeilen	 
		 (h4  () (and (and (less numc1 cx12) (less numc2 cx12))
                                   (= dc43
                                      (append sL
                                              (cons (der-item-constr
                                                     res-res
                                                     co8
                                                     (cons (form2item li1) (cons (form2item li2) NULL))
                                                     (cons (num2item numc1) (cons (num2item numc2) NULL)))
                                                    NULL))))  ("Hyp"()()))

                 (as1 () (and (less numc1 cx12) (less numc2 cx12)) ("AndEL" ()(h4)))

                 (as2 () (= dc43
                            (append sL (cons (der-item-constr
                                              res-res
                                              co8
                                              (cons (form2item li1) (cons (form2item li2) NULL))
                                              (cons (num2item numc1) (cons (num2item numc2) NULL)))
                                             NULL)))       ("AndER" ()(h4)))

                 (as3 () (less numc1 cx12) ("AndEL"()(as1)))

                 (as4 () (less numc2 cx12) ("AndER"()(as1)))

		 ;; ih1/2-seq wird durch (prefix numc1/2 dc43) ersetzt (in as5/6)
		 ;; dann ist auch das abstract in ordnung
                 (as5 () (and (less (minus (glist-length ih1-seq) one)
                                    cx12)
                              (derivation-of (union (setminus co9 (singleton (good-clause co9)))
                                                    (singleton
                                                     (setminus (good-clause co9)
                                                               (singleton (good-literal
                                                                           (good-clause co9))))))
                                             crule-set ih1-seq (der-item-info (glist-nth numc1 dc43))))       ("abstract"()()))
                                                   
                 (as6 () (and (less (minus (glist-length ih2-seq) one)
                                    cx12)
                              (derivation-of (union (setminus co9 (singleton (good-clause co9)))
                                                    (singleton
                                                     (setminus (good-clause co9)
                                                               (singleton (good-literal
                                                                           (good-clause co9))))))
                                             crule-set ih2-seq (der-item-info (glist-nth numc2 dc43))))       ("abstract"()()))

                 (as7 (as5 as3) (or (derivable (der-item-info (glist-nth numc1 dc43)) co9 crule-set)                     
                             (derivable (union (der-item-info (glist-nth numc1 dc43))
                                               (singleton (good-literal (good-clause co9))))
                                        co9 crule-set))                                                         ("Open"()()))

                 (as8 (as6 as4) (or (derivable (der-item-info (glist-nth numc2 dc43)) co9 crule-set)                     
                             (derivable (union (der-item-info (glist-nth numc2 dc43))
                                               (singleton (good-literal (good-clause co9))))
                                        co9 crule-set)) 	                                                 ("Open"()()))

                 ;; =====================================================================================
                 
                 (l110 () (derivable (union (der-item-info (glist-nth numc2 dc43))
                                               (singleton (good-literal (good-clause co9))))
                                        co9 crule-set)                            ("Hyp"()()))

                 (l109 () (derivable (union (der-item-info (glist-nth numc1 dc43))
                                               (singleton (good-literal (good-clause co9))))
                                        co9 crule-set)                           ("Hyp"()()))

		 (l108 () (resolvable-s (union (der-item-info (glist-nth numc2 dc43))
                                               (singleton (good-literal (good-clause co9))))
					(union (der-item-info (glist-nth numc1 dc43))
                                               (singleton (good-literal (good-clause co9))))
					)                                         ("Hyp"()()))
		 
                 (l96 (h1 h2 h4 n23 n3 l110 l109 l108) (derivable (union co8
                                                             (singleton (good-literal (good-clause co9))))
                                                      co9 crule-set)              ("Open"()()))
                 
;                 (l95 (h1 h2 h4 l110 l109) (or (derivable co8 co9 crule-set)                     
;                                           (derivable (union co8
;                                                             (singleton (good-literal (good-clause co9))))
;                                                      co9 crule-set))                          ("OrIR" ()(l96)))
;
;                 ;; -------------------------------------
                 
                 (l94 () (derivable (der-item-info (glist-nth numc1 dc43)) co9 crule-set) ("Hyp" ()()))

		 (l93 () (resolvable-s (der-item-info (glist-nth numc1 dc43))
					       (union (der-item-info (glist-nth numc2 dc43))
                                               (singleton (good-literal (good-clause
									 co9))))) ("hyp"()()))
		 
                 (l82 (h1 h2 h4 n22 n3 l110 l94 l93) (derivable (union co8
                                                             (singleton (good-literal (good-clause co9))))
                                                      co9 crule-set)                                 ("Open"()()))
                 
;                 (l81 (h1 h2 h4 l110 l94)  (or (derivable co8 co9 crule-set)                     
;                                           (derivable (union co8
;                                                             (singleton (good-literal (good-clause co9))))
;                                                      co9 crule-set))                             ("OrIR"()(l82)))
;                 
;                 (l80 (h1 h2 h4 l110)  (or (derivable co8 co9 crule-set)                     
;                                           (derivable (union co8
;                                                             (singleton (good-literal (good-clause co9))))
;                                                      co9 crule-set))                     ("OrE"()(as7 l81 l95)))
;
;                 ;; ===================
                 
                 (l69 () (derivable (der-item-info (glist-nth numc2 dc43)) co9 crule-set)         ("Hyp"()()))
		 
                 (l68 () (derivable (union
                                     (der-item-info (glist-nth numc1 dc43))
                                     (singleton (good-literal (good-clause co9))))
                                    co9 crule-set)                                          ("Hyp"()()))

		 (l67 () (resolvable-s (der-item-info (glist-nth numc2 dc43))
				       (union
					(der-item-info (glist-nth numc1 dc43))
					(singleton (good-literal (good-clause co9)))))		  ("Hyp"()()))  
		 
                 (l61 (h1 h2 h4 n20 n3 l69 l68 l67) (derivable (union co8
                                                       (singleton (good-literal (good-clause co9))))
                                             co9 crule-set)		                    ("Open"()()))
                 
;                 (l60 (h1 h2 h4 l69 l68) (or (derivable co8 co9 crule-set)                     
;                                                (derivable (union co8
;                                                                  (singleton (good-literal (good-clause co9))))
;                                                           co9 crule-set))                    ("OrIR"()(l61)))
;
		 ;; --------------------------
                      
                 (l59 () (derivable (der-item-info (glist-nth numc1 dc43)) co9 crule-set)          ("Hyp"()()))

		 (l57 () (resolvable-s (der-item-info (glist-nth numc2 dc43))
				       (der-item-info (glist-nth numc1 dc43))
				        )                                             		  ("Hyp"()())) 

		 (l53 (h1 h2 h4 n21 n3 l69 l59 l57) (derivable co8 co9 crule-set)		         ("Open"()()))
                 
;                 (l52 (h1 h2 h4 l69 l59)   (or (derivable co8 co9 crule-set)                     
;                                               (derivable (union co8
;                                                                 (singleton (good-literal (good-clause co9))))
;                                                          co9 crule-set)) ("OrIL"()(l53))) 
;                      
;                 (l51 (h1 h2 h4 l69) (or (derivable co8 co9 crule-set)                     
;                                     (derivable (union co8
;                                                       (singleton (good-literal (good-clause co9))))
;                                             co9 crule-set)) ("OrE"()(as7 l52 l60)))
;
;               
		 ;; co8 wurde aus K1 mit Hilfe der resolutionsregel abgeleitet.
                 
                 (l50 (h1 h2 h4) (or (derivable co8 co9 crule-set)                     
                                     (derivable (union co8
                                                       (singleton (good-literal (good-clause co9))))
                                             co9 crule-set))   ("OrE"()(as8 l51 l80)))
                 

                 ;; T R I V I A L   F A L L   T E I L B E W E I S
                 ;; =============================================
                 
                 (h3  ()  (= dc43
                             (append sL (cons (der-item-constr
                                               res-start
                                               co8
                                               NULL NULL)
                                              NULL)))                        ("Hyp"()())) 

                 (l5  ()  (and (= (der-item-info (last dc43)) co8)
                               (derivation (union (setminus co9 (singleton (good-clause co9)))
                                                   (singleton
                                                    (setminus (good-clause co9)
                                                              (singleton (good-literal
                                                                          (good-clause co9))))))
                                           crule-set
                                           dc43)) ("DefnE" (derivation-of) (h2)))

                 (l6 () (derivation (union (setminus co9 (singleton (good-clause co9)))
                                                   (singleton
                                                    (setminus (good-clause co9)
                                                              (singleton (good-literal
                                                                          (good-clause co9))))))
                                           crule-set
                                           dc43)  ("AndER"()(l5)))

		 (l7 () (forall (lam (x item) 
			  (implies (is-component x dc43)
				   (and (free-derivation-cond dc43)		
					(and (and (crule-set (der-item-crule x))
						  (crule-applicable (der-item-crule x)
								    (append (append
									     (der-item-inp x)
									     (poslist2infolist (der-item-prems x) dc43))
									    (cons (cl2item (der-item-info x))
										  NULL))
								    (union (setminus co9 (singleton (good-clause co9)))
									   (singleton
									    (setminus (good-clause co9)
										      (singleton (good-literal
												  (good-clause co9))))))))
					     (and (forall (lam (y num)
							       (implies (is-component (num2item y) (der-item-prems x))
									(less y
									      (position x dc43)))))
						  (= (der-item-info x)		
						     (crule-application (der-item-crule x)
									(append (append
										 (der-item-inp x)
										 (poslist2infolist (der-item-prems x) dc43))
										(cons (cl2item
										       (der-item-info x))
										      NULL))
									(union (setminus co9 (singleton (good-clause co9)))
									       (singleton
										(setminus (good-clause co9)
											  (singleton (good-literal
												      (good-clause co9))))))))))))))
		     ("DefnE" (derivation) (l6)))
		 
		 (l8 () (implies (is-component lastdc43 dc43)
				 (and (free-derivation-cond dc43)		
				      (and (and (crule-set (der-item-crule lastdc43))
						(crule-applicable (der-item-crule lastdc43)
								  (append (append
									   (der-item-inp lastdc43)
									   (poslist2infolist (der-item-prems lastdc43) dc43))
									  (cons (cl2item (der-item-info lastdc43))
										NULL))
								  (union (setminus co9 (singleton (good-clause co9)))
									 (singleton
									  (setminus (good-clause co9)
										    (singleton (good-literal
												(good-clause co9))))))))
					   (and (forall (lam (y num)
							     (implies (is-component (num2item y) (der-item-prems lastdc43))
								      (less y
									    (position lastdc43 dc43)))))
						(= (der-item-info lastdc43)		
						   (crule-application (der-item-crule lastdc43)
								      (append (append
									       (der-item-inp lastdc43)
									       (poslist2infolist (der-item-prems lastdc43) dc43))
									      (cons (cl2item
										     (der-item-info lastdc43))
										    NULL))
								      (union (setminus co9 (singleton (good-clause co9)))
									     (singleton
									      (setminus (good-clause co9)
											(singleton (good-literal
												    (good-clause co9))))))))))))
		     ("ForallE" (lastdc43) (l7)))

                 (l9  () (is-component (last dc43) dc43) ("abstract"()()))

                 (l10 () (and (free-derivation-cond dc43)		
			      (and (and (crule-set (der-item-crule lastdc43))
					(crule-applicable (der-item-crule lastdc43)
							  (append (append
								   (der-item-inp lastdc43)
								   (poslist2infolist (der-item-prems lastdc43) dc43))
								  (cons (cl2item (der-item-info lastdc43))
									NULL))
							  (union (setminus co9 (singleton (good-clause co9)))
								 (singleton
								  (setminus (good-clause co9)
									    (singleton (good-literal
											(good-clause co9))))))))
				   (and (forall (lam (y num)
						     (implies (is-component (num2item y) (der-item-prems lastdc43))
							      (less y
								    (position lastdc43 dc43)))))
					(= (der-item-info lastdc43)		
					   (crule-application (der-item-crule lastdc43)
							      (append (append
								       (der-item-inp lastdc43)
								       (poslist2infolist (der-item-prems lastdc43) dc43))
								      (cons (cl2item
									     (der-item-info lastdc43))
									    NULL))
							      (union (setminus co9 (singleton (good-clause co9)))
								     (singleton
								      (setminus (good-clause co9)
										(singleton (good-literal
											    (good-clause co9)))))))))))        
                      ("ImpE" ()(l8 l9)))

              

                  (l11 () (and (and (crule-set (der-item-crule lastdc43))
                                         (crule-applicable (der-item-crule lastdc43)
                                                           (append (append
                                                                    (der-item-inp lastdc43)
                                                                    (poslist2infolist (der-item-prems lastdc43) dc43))
                                                                   (cons (cl2item (der-item-info lastdc43))
                                                                         NULL))
                                                           (union (setminus co9 (singleton (good-clause co9)))
                                                                  (singleton
                                                                   (setminus (good-clause co9)
                                                                             (singleton (good-literal
                                                                                         (good-clause co9))))))))
                                    (and (forall (lam (y num)
                                                      (implies (is-component (num2item y) (der-item-prems lastdc43))
                                                               (less y
                                                                     (position lastdc43 dc43)))))
                                         (= (der-item-info lastdc43)             
                                            (crule-application (der-item-crule lastdc43)
                                                               (append (append
                                                                        (der-item-inp lastdc43)
                                                                        (poslist2infolist (der-item-prems lastdc43) dc43))
                                                                       (cons (cl2item
                                                                              (der-item-info lastdc43))
                                                                             NULL))
                                                               (union (setminus co9 (singleton (good-clause co9)))
                                                                      (singleton
                                                                       (setminus (good-clause co9)
                                                                                 (singleton (good-literal
                                                                                             (good-clause co9))))))))))
                       ("AndER"() (l10)))

                  (l12 ()  (and (crule-set (der-item-crule lastdc43))
                                (crule-applicable (der-item-crule lastdc43)
                                                  (append (append
                                                           (der-item-inp lastdc43)
                                                           (poslist2infolist (der-item-prems lastdc43) dc43))
                                                          (cons (cl2item (der-item-info lastdc43))
                                                                NULL))
                                                  (union (setminus co9 (singleton (good-clause co9)))
                                                         (singleton
                                                          (setminus (good-clause co9)
                                                                    (singleton (good-literal
                                                                                (good-clause co9))))))))
                       ("AndEL" ()(l11)))

                  (l13 () (crule-applicable (der-item-crule lastdc43)
                                            (append (append
                                                     (der-item-inp lastdc43)
                                                     (poslist2infolist (der-item-prems lastdc43) dc43))
                                                    (cons (cl2item (der-item-info lastdc43))
                                                          NULL))
                                            (union (setminus co9 (singleton (good-clause co9)))
                                                   (singleton
                                                    (setminus (good-clause co9)
                                                              (singleton (good-literal
                                                                          (good-clause co9))))))) ("AndER" ()(l12)))
					
;                 (l16 () (= (der-item-inp (last dc43))
;                             NULL) ("abstract"()()))
;                 ;; Diesen Teilbeweis noch innerhalb dieser Methode expandieren
;
;                 (l17 () (crule-applicable res-start
;                                 (append (append
;                                          NULL
;                                          (poslist2infolist (der-item-prems (last dc43)) dc43))
;                                         (cons (cl2item (der-item-info (last dc43)))
;                                               NULL))
;                                 (union (setminus co9 (singleton (good-clause co9)))
;                                        (singleton
;                                         (setminus (good-clause co9)
;                                                   (singleton (good-literal
;                                                               (good-clause co9)))))))
;                      ("Subst=" (apos2) (l16 l15)))
;
;                 (l18 () (= (poslist2infolist (der-item-prems (last dc43)) dc43)
;                            NULL) ("abstract"()()))
;
;                 (l19 () (crule-applicable res-start
;                                 (append (append  NULL  NULL)
;                                         (cons (cl2item (der-item-info (last dc43)))
;                                               NULL))
;                                 (union (setminus co9 (singleton (good-clause co9)))
;                                        (singleton
;                                         (setminus (good-clause co9)
;                                                   (singleton (good-literal
;                                                               (good-clause co9)))))))
;                      ("Subst=" (apos3) (l18 l17)))
;
;                 (l20 () (= (append  NULL  NULL) NULL)              ("abstract"()()))
;
;                 (l21 () (crule-applicable res-start
;                                 (append NULL
;                                         (cons (cl2item (der-item-info (last dc43)))
;                                               NULL))
;                                 (union (setminus co9 (singleton (good-clause co9)))
;                                        (singleton
;                                         (setminus (good-clause co9)
;                                                   (singleton (good-literal
;                                                               (good-clause co9)))))))
;                      ("Subst=" (apos4)(l20 l19)))
;                 
;                 (l22 () (= (der-item-info (last dc43)) co8) ("abstract"()()))
;
;                 (l23 () (crule-applicable res-start
;                                 (append NULL (cons (cl2item co8)  NULL))
;                                 (union (setminus co9 (singleton (good-clause co9)))
;                                        (singleton
;                                         (setminus (good-clause co9)
;                                                   (singleton (good-literal
;                                                               (good-clause co9)))))))
;                      ("Subst=" (apos5) (l22 l21)))
;
;                 (l24 () (= (append NULL (cons (cl2item co8)  NULL))  (cons (cl2item co8)  NULL)) ("abstract"()()))
;
;                 (l25 () (crule-applicable res-start
;                                 (cons (cl2item co8)  NULL)
;                                 (union (setminus co9 (singleton (good-clause co9)))
;                                        (singleton
;                                         (setminus (good-clause co9)
;                                                   (singleton (good-literal
;                                                               (good-clause co9)))))))
;                      ("Subst=" (apos6) (l24 l23)))
;		 
;                 (lb1 () (forall (lam (seq glist) (forall (lam (S (o (o form)))
;                          (equiv (crule-applicable res-start seq S)
;                                 (in (item2cl (first seq)) S))))))  ("Axiom"()()))
;
;                 (lb2 () (forall (lam (S (o (o form)))
;                          (equiv (crule-applicable res-start
;                                                   (cons (cl2item co8)  NULL)S)
;                                 (in (item2cl (first (cons (cl2item co8)  NULL))) S))))   ("ForallE"(bterm1)(lb1)))
;                 
;                 (lb3 () (equiv (crule-applicable res-start
;                                                  (cons (cl2item co8)  NULL)
;                                                  (union (setminus co9 (singleton (good-clause co9)))
;                                                         (singleton
;                                                          (setminus (good-clause co9)
;                                                                    (singleton (good-literal
;                                                                                (good-clause co9)))))))
;                                 (in (item2cl (first (cons (cl2item co8)  NULL)))
;                                     (union (setminus co9 (singleton (good-clause co9)))
;                                         (singleton
;                                          (setminus (good-clause co9)
;                                                    (singleton (good-literal
;                                                                (good-clause co9))))))))
;                      ("ForallE" (bterm2) (lb2)))
;                 
;                 (l26 () (implies (crule-applicable res-start
;                                  (cons (cl2item co8)  NULL)
;                                  (union (setminus co9 (singleton (good-clause co9)))
;                                         (singleton
;                                          (setminus (good-clause co9)
;                                                    (singleton (good-literal
;                                                                (good-clause co9)))))))
;                                 (in (item2cl (first (cons (cl2item co8)  NULL)))
;                                     (union (setminus co9 (singleton (good-clause co9)))
;                                         (singleton
;                                          (setminus (good-clause co9)
;                                                    (singleton (good-literal
;                                                                (good-clause co9))))))))
;                      ("EquivEL"()(lb3)))
;
;                 (l27 () (in (item2cl (first (cons (cl2item co8)  NULL)))
;                             (union (setminus co9 (singleton (good-clause co9)))
;                                    (singleton
;                                     (setminus (good-clause co9)
;                                               (singleton (good-literal
;                                                           (good-clause co9)))))))
;                      ("ImpE"()(l26 l25)))
;
;                 (l28 () (= (item2cl (first (cons (cl2item co8)  NULL)))
;                            co8) ("abstract"()()))                
;                 
                 (l29 () (in  co8 
                             (union (setminus co9 (singleton (good-clause co9)))
                                    (singleton
                                     (setminus (good-clause co9)
                                               (singleton (good-literal
                                                           (good-clause co9)))))))
		      ("Open"()()))
                                   ;;                                   ("subst=" (apos7)(l28 l27)))

		 ;; dieses Ziel ist expandierbar
                 (l30 () (co9  co8)                                        ("abstract"()()))
		 
		 ;;  Dieses Ziel ist loesbar mit Zeile l30 analog dem Induktionsanfang.
		 (l122 (h1 h2 h3 l11 l30)  (derivable co8 co9 crule-set) ("Open"()()))
;                      
;                 (l4  (h1 h2 h3) (derivable co8 co9 crule-set)  ("ExistsE" (res-start) (l122 l10)))
;
;                 (l3  (h1 h2 h3) (or (derivable co8 co9 crule-set)                     
;                                  (derivable (union co8
;                                                    (singleton (good-literal (good-clause co9))))
;                                             co9 crule-set)) ("OrIL"()(l4)))

		 ;; ===============================================================================
		 
		 ;; Aufspaltung in Trivialfall und Resolutionsfall mit Hilfe der Zeile n1
		 ;; ganz oben. Man unterscheidet ob co8 aus K1 mit der Startregel oder der
		 ;; Resolutionsregel erzeugt wurde.    		 
		 (l2  (h1 h2) (or (derivable co8 co9 crule-set)                     
				  (derivable (union co8
						    (singleton (good-literal (good-clause co9))))
					     co9 crule-set)) ("Abstract"()())
		      ;; ("OrE" ()(n1 l3 l50))
		      )

		 (l1z (h1) (implies
			      (derivation-of (union (setminus co9 (singleton (good-clause co9)))
						    (singleton
						     (setminus (good-clause co9)
							       (singleton (good-literal
									   (good-clause co9))))))
					   crule-set dc43 co8)
			    (or (derivable co8 co9 crule-set)                     
			        (derivable (union co8
						  (singleton (good-literal (good-clause co9))))
					   co9 crule-set)))                          ("ImpI"()(l2)))
		 
		 (l1  (h1)  (forall (lam (co8x (o form))
			     (implies
			      (derivation-of (union (setminus co9 (singleton (good-clause co9)))
						    (singleton
						     (setminus (good-clause co9)
							       (singleton (good-literal
									   (good-clause co9))))))
					   crule-set dc43 co8x)
			    (or (derivable co8x co9 crule-set)                     
			        (derivable (union co8x
						  (singleton (good-literal (good-clause co9))))
					   co9 crule-set)))))              ("ForallI" (co8x) (l1z)))        


		 ;; Diese Zeile behauptet:
		 ;; WENN aus K1 in n Schritten mittels der Ableitung dc43 die Klausel c8
		 ;; ableitbar ist,
		 ;; DANN ist co8x oder co8x+li aus K ableitbar, wobei K = co9 ist.

		 ;; Bis l2 werden die implicationen standardmaessig aufgeloest.
		 (ltp ()  (implies
			   (= (minus (glist-length dc43) one) cx12)
			   (forall (lam (co8x (o form))
			   (implies
			    (derivation-of (union (setminus co9 (singleton (good-clause co9)))
						  (singleton
						   (setminus (good-clause co9)
							     (singleton (good-literal
									 (good-clause co9))))))
					   crule-set dc43 co8x)
			    (or (derivable co8x co9 crule-set)                     
			        (derivable (union co8x
						  (singleton (good-literal (good-clause co9))))
					   co9 crule-set))))))       ("ImpI"()(l1)))            
		 )
		(proc-content schema-interpreter)
		(remark "Proof the step case of an eln-induction-step with help of k1 and k2.")
		)


(infer~defmethod dsjk-base
		 (outline-mappings (((existent existent existent existent
				     nonexistent nonexistent)
				     dsjk-basem-b)))  
		 (help "Spezialmethode zum Beweis des DISJL"))

(meth~defmethod dsjk-basem-b dsjk-base  
		(in prop-res) 
		(rating 1)
		(declarations
		 (type-variables rr)
		 (type-constants o i)
		 (sorted-meta-variables 
		  (c31 num         term)
		  (dc glist        term)
		  (c2 (o (o form)) term)
		  (c1 (o form)     term)
		  (ct glist        term)
		  (ct1 o           term)
		  (ct2 item        term)
		  (c3 (o form)     term)
		  (pos1110101 o     pos)
		  (pos11110   o     pos)
		  (pos1110    o     pos)
		  (pos111     o     pos)
		  (pos11      o     pos)
		  (pos1110110 o     pos)
		  (pos11101   o     pos)
		  (pos101     o     pos)
		  (pos10      o     pos)
		  )
		 )
		(parameters) 
		(premises (- lh1) (- lh2) (- lh3) (+ ls10) (+ ls20))
		(application-condition )  
		(outline-actions
		 (ls20 (sponsor ls21))
		 (ls10 (sponsor ls11))
		 )
		(outline-orderings  )
		(outline-computations
		 (ct  (:term (cons (der-item-constr res-start c1 NULL NULL) NULL)))
		 (ct1 (:term (= c1 (SETMINUS (GOOD-CLAUSE C2)                                
					   (SINGLETON                                          
					    (GOOD-LITERAL (good-clause c2)))))))
		 (ct2        (:term (last dc)))
		 (pos1110101 (:position (1 1 1 0 1 0 1)))
		 (pos11110   (:position (1 1 1 1 0)))
		 (pos1110    (:position (1 1 1 0)))
		 (pos111     (:position (1 1 1)))
		 (pos11      (:position (1 1)))
		 (pos1110110 (:position (1 1 1 0 1 1 0)))
		 (pos11101   (:position (1 1 1 0 1)))
		 (pos101     (:position (1 0 1)))
		 (pos10      (:position (1 0)))
		 )
		(conclusions (- ltp) )
		(decl-content
		 (lh1 () (= c31 zero) )

		 (lh2 () (= (minus (glist-length dc) one)
			    zero) )

		 (lh3 () (DERIVATION-OF                                      
                         (UNION  (SETMINUS                                             
				  C2                                               
				  (SINGLETON (GOOD-CLAUSE C2)))                    
				 (SINGLETON                                            
				  (SETMINUS (GOOD-CLAUSE C2)                                
					    (SINGLETON                                          
					     (GOOD-LITERAL (good-clause c2))))))                          
                         CRULE-SET dc c1) )

		 (h1 () (and (= (der-item-info (last dc)) c1)
			     (derivation
			      (UNION  (SETMINUS                                             
				       C2                                               
				       (SINGLETON (GOOD-CLAUSE C2)))                    
				      (SINGLETON                                            
				       (SETMINUS (GOOD-CLAUSE C2)                                
						 (SINGLETON                                          
						  (GOOD-LITERAL (good-clause c2))))))
			      crule-set dc))                    ("DefnE" (derivation-of)(lh3)))

		 (h2 () (derivation
			      (UNION  (SETMINUS                                             
				       C2                                               
				       (SINGLETON (GOOD-CLAUSE C2)))                    
				      (SINGLETON                                            
				       (SETMINUS (GOOD-CLAUSE C2)                                
						 (SINGLETON                                          
						  (GOOD-LITERAL (good-clause c2))))))
			      crule-set dc)                                   ("AndER" ()(h1)))

		 (h3 () (forall (lam (x item) 
			  (implies
			   (is-component x dc)
			   (exists (lam (r crule)
			      (and (free-derivation-cond dc)		
				   (and (and (crule-set r)
					     (crule-applicable
					      r
					      (append (append
						       (der-item-inp x)
						       (poslist2infolist (der-item-prems x) dc))
						      (cons (cl2item (der-item-info x))
							    NULL))
					      (union (setminus c2 (singleton
								   (good-clause
								    c2)))
						     (singleton (setminus
								 (good-clause c2)
								 (singleton
								  (good-literal (good-clause c2))))))))
					(and (forall (lam (y num)
						(implies (is-component (num2item y) (der-item-prems x))
							 (less y
							       (position x dc)))))
					     (= (der-item-info x)		
						(crule-application
						 r
						 (append (append
							  (der-item-inp x)
							  (poslist2infolist (der-item-prems x) dc))
							 (cons (cl2item
								(der-item-info x))
							       NULL))
						 (union (setminus c2 (singleton
								      (good-clause
								       c2)))
							(singleton (setminus
								    (good-clause
								     c2)
								    (singleton
								     (good-literal
								      (good-clause c2))))))))))))))))
		     ("DefnE" (derivation) (h2)))

		 (h4   ()  (implies
			   (is-component (last dc) dc)
			   (exists (lam (r crule)
			      (and (free-derivation-cond dc)		
				   (and (and (crule-set r)
					     (crule-applicable
					      r
					      (append (append
						       (der-item-inp (last dc))
						       (poslist2infolist (der-item-prems (last dc)) dc))
						      (cons (cl2item (der-item-info (last dc)))
							    NULL))
					      (union (setminus c2 (singleton
								   (good-clause
								    c2)))
						     (singleton (setminus
								 (good-clause c2)
								 (singleton
								  (good-literal (good-clause c2))))))))
					(and (forall (lam (y num)
						(implies (is-component (num2item y) (der-item-prems (last dc)))
							 (less y
							       (position (last dc) dc)))))
					     (= (der-item-info (last dc))		
						(crule-application
						 r
						 (append (append
							  (der-item-inp (last dc))
							  (poslist2infolist (der-item-prems (last dc)) dc))
							 (cons (cl2item
								(der-item-info (last dc)))
							       NULL))
						 (union (setminus c2 (singleton
								      (good-clause
								       c2)))
							(singleton (setminus
								    (good-clause
								     c2)
								    (singleton
								     (good-literal
								      (good-clause
								       c2))))))))))))))
		                                                                 ("ForallE" (ct2) (h3)))

		 (h5   ()  (is-component (last dc) dc) ("Optional"()()))

		 (h6   ()  (exists (lam (r crule)
			      (and (free-derivation-cond dc)		
				   (and (and (crule-set r)
					     (crule-applicable
					      r
					      (append (append
						       (der-item-inp (last dc))
						       (poslist2infolist (der-item-prems (last dc)) dc))
						      (cons (cl2item (der-item-info (last dc)))
							    NULL))
					      (union (setminus c2 (singleton
								   (good-clause
								    c2)))
						     (singleton (setminus
								 (good-clause c2)
								 (singleton
								  (good-literal (good-clause c2))))))))
					(and (forall (lam (y num)
						(implies (is-component (num2item y) (der-item-prems (last dc)))
							 (less y
							       (position (last dc) dc)))))
					     (= (der-item-info (last dc))		
						(crule-application
						 r
						 (append (append
							  (der-item-inp (last dc))
							  (poslist2infolist (der-item-prems (last dc)) dc))
							 (cons (cl2item
								(der-item-info (last dc)))
							       NULL))
						 (union (setminus c2 (singleton
								      (good-clause
								       c2)))
							(singleton (setminus
								    (good-clause
								     c2)
								    (singleton
								     (good-literal
								      (good-clause
								       c2)))))))))))))
		                                                                        ("ImpE"()(h4 h5)))

		  (h7 () (and (free-derivation-cond dc)		
				   (and (and (crule-set res-start)
					     (crule-applicable
					      res-start
					      (append (append
						       (der-item-inp (last dc))
						       (poslist2infolist (der-item-prems (last dc)) dc))
						      (cons (cl2item (der-item-info (last dc)))
							    NULL))
					      (union (setminus c2 (singleton
								   (good-clause
								    c2)))
						     (singleton (setminus
								 (good-clause c2)
								 (singleton
								  (good-literal (good-clause c2))))))))
					(and (forall (lam (y num)
						(implies (is-component (num2item y) (der-item-prems (last dc)))
							 (less y
							       (position (last dc) dc)))))
					     (= (der-item-info (last dc))		
						(crule-application
						 res-start
						 (append (append
							  (der-item-inp (last dc))
							  (poslist2infolist (der-item-prems (last dc)) dc))
							 (cons (cl2item
								(der-item-info (last dc)))
							       NULL))
						 (union (setminus c2 (singleton
								      (good-clause
								       c2)))
							(singleton (setminus
								    (good-clause
								     c2)
								    (singleton
								     (good-literal
								      (good-clause
								       c2)))))))))))
		     ("Hyp"()()))
		 (h8 () (and (and (crule-set res-start)
					     (crule-applicable
					      res-start
					      (append (append
						       (der-item-inp (last dc))
						       (poslist2infolist (der-item-prems (last dc)) dc))
						      (cons (cl2item (der-item-info (last dc)))
							    NULL))
					      (union (setminus c2 (singleton
								   (good-clause
								    c2)))
						     (singleton (setminus
								 (good-clause c2)
								 (singleton
								  (good-literal (good-clause c2))))))))
					(and (forall (lam (y num)
						(implies (is-component (num2item y) (der-item-prems (last dc)))
							 (less y
							       (position (last dc) dc)))))
					     (= (der-item-info (last dc))		
						(crule-application
						 res-start
						 (append (append
							  (der-item-inp (last dc))
							  (poslist2infolist (der-item-prems (last dc)) dc))
							 (cons (cl2item
								(der-item-info (last dc)))
							       NULL))
						 (union (setminus c2 (singleton
								      (good-clause
								       c2)))
							(singleton (setminus
								    (good-clause
								     c2)
								    (singleton
								     (good-literal
								      (good-clause
								       c2))))))))))
		     ("AndER"()(h7)))
		 (h9 () (and (crule-set res-start)
			     (crule-applicable
			      res-start
			      (append (append
				       (der-item-inp (last dc))
				       (poslist2infolist (der-item-prems (last dc)) dc))
				      (cons (cl2item (der-item-info (last dc)))
					    NULL))
			      (union (setminus c2 (singleton
						   (good-clause
						    c2)))
				     (singleton (setminus
						 (good-clause c2)
						 (singleton
						  (good-literal
						   (good-clause c2))))))))
		     ("AndEL"()(h8)))

		 (h10 ()  (crule-applicable
			      res-start
			      (append (append
				       (der-item-inp (last dc))
				       (poslist2infolist (der-item-prems (last dc)) dc))
				      (cons (cl2item (der-item-info (last dc)))
					    NULL))
			      (union (setminus c2 (singleton
						   (good-clause
						    c2)))
				     (singleton (setminus
						 (good-clause c2)
						 (singleton
						  (good-literal
						   (good-clause c2)))))))     ("AndER" ()(h9)))

		 (h11 (l1a) (crule-applicable
			      res-start
			      (append (append
				       (der-item-inp (der-item-constr res-start c1 NULL NULL))
				       (poslist2infolist (der-item-prems (last dc)) dc))
				      (cons (cl2item (der-item-info (last dc)))
					    NULL))
			      (union (setminus c2 (singleton
						   (good-clause
						    c2)))
				     (singleton (setminus
						 (good-clause c2)
						 (singleton
						  (good-literal
						   (good-clause c2))))))) ("=Subst"
									   (pos1110101) (l1a)))  

		 (ax1a () (= NULL
			     (der-item-inp (der-item-constr res-start c1 NULL NULL))) ("Optional"()()))
		 
		 (h12 (l1a) (crule-applicable
			      res-start
			      (append (append
				       NULL
				       (poslist2infolist (der-item-prems (last dc)) dc))
				      (cons (cl2item (der-item-info (last dc)))
					    NULL))
			      (union (setminus c2 (singleton
						   (good-clause
						    c2)))
				     (singleton (setminus
						 (good-clause c2)
						 (singleton
						  (good-literal
						   (good-clause c2))))))) ("=Subst"  (pos11110) (ax1a h11)))

		 (ax1b () (= (poslist2infolist (der-item-prems (last dc)) dc)
			     (append NULL (poslist2infolist (der-item-prems (last dc)) dc))) ("Optional"()()))
		 
		 (h13 (l1a) (crule-applicable
			      res-start
			      (append  (poslist2infolist (der-item-prems (last dc)) dc)
			               (cons (cl2item (der-item-info (last dc)))
					     NULL))
			      (union (setminus c2 (singleton
						   (good-clause
						    c2)))
				     (singleton (setminus
						 (good-clause c2)
						 (singleton
						  (good-literal
						   (good-clause c2))))))) ("=Subst" (pos1110) (ax1b h12)))

		 (ax1c () (= (der-item-prems (last dc))
			     NULL))
		 
		 (h14 (l1a) (crule-applicable
			      res-start
			      (append  (poslist2infolist NULL dc)
			               (cons (cl2item (der-item-info (last dc)))
					     NULL))
			      (union (setminus c2 (singleton
						   (good-clause
						    c2)))
				     (singleton (setminus
						 (good-clause c2)
						 (singleton
						  (good-literal
						   (good-clause c2)))))))     ("=Subst" (pos1110)(ax1c h13)))

		 (ax1d () (= (poslist2infolist NULL dc)
			     NULL) ("Optional"()()))
		 
		 (h15 (l1a) (crule-applicable
			      res-start
			      (append  NULL
			               (cons (cl2item (der-item-info (last dc)))
					     NULL))
			      (union (setminus c2 (singleton
						   (good-clause
						    c2)))
				     (singleton (setminus
						 (good-clause c2)
						 (singleton
						  (good-literal
						   (good-clause c2)))))))     ("=Subst"(pos111)(ax1d h14)))

		 (ax1e () (= (append NULL (cons (cl2item (der-item-info (last dc)))  NULL))
			     (cons (cl2item (der-item-info (last dc))) NULL))  ("Optional"()()))
		 
		 (h16 (l1a) (crule-applicable
			      res-start
			      (cons (cl2item (der-item-info (last dc))) NULL)
			      (union (setminus c2 (singleton (good-clause c2)))
				     (singleton (setminus
						 (good-clause c2)
						 (singleton
						  (good-literal
						   (good-clause c2)))))))  ("=Subst" (pos11) (ax1e h15))) 

		 (h17 (l1a) (crule-applicable
			      res-start
			      (cons (cl2item (der-item-info
					      (der-item-constr res-start c1 NULL NULL))) NULL)
			      (union (setminus c2 (singleton (good-clause c2)))
				     (singleton (setminus
						 (good-clause c2)
						 (singleton
						  (good-literal
						   (good-clause c2)))))))   ("=Subst" (pos1110110) (l1a h16)))

		 (ax1f () (= (der-item-info (der-item-constr res-start c1 NULL NULL))
			     c1)                                                       ("Optional"()()))
			     
		 (h18 (l1a) (crule-applicable
			      res-start
			      (cons (cl2item c1) NULL)
			      (union (setminus c2 (singleton (good-clause c2)))
				     (singleton (setminus
						 (good-clause c2)
						 (singleton
						  (good-literal
						   (good-clause c2))))))) ("=Subst"(pos11101)(ax1f h17)))       

		 (ax1g () (equiv (crule-applicable res-start
						   (cons (cl2item c1) NULL)
						   (union (setminus c2 (singleton (good-clause c2)))
							  (singleton (setminus
								      (good-clause c2)
								      (singleton
								       (good-literal
									(good-clause c2)))))))
				 (in (item2cl (first (cons (cl2item c1) NULL)))
				     (union (setminus c2 (singleton (good-clause c2)))
				     (singleton (setminus
						 (good-clause c2)
						 (singleton
						  (good-literal
						   (good-clause c2))))))))  ("Optional"()())) 

		 (h19 () (in (item2cl (first (cons (cl2item c1) NULL)))
				     (union (setminus c2 (singleton (good-clause c2)))
				     (singleton (setminus
						 (good-clause c2)
						 (singleton
						  (good-literal
						   (good-clause c2)))))))
		                                                ("EquivER" ()(ax1g h18)))

		 (ax1h () (= (first (cons (cl2item c1) NULL))
			     (cl2item c1)) ("Optional"()()))
		 
		 (h20 () (in (item2cl (cl2item c1))
			     (union (setminus c2 (singleton (good-clause c2)))
				    (singleton (setminus
						(good-clause c2)
						(singleton
						 (good-literal
						  (good-clause c2)))))))
		                                             ("=Subst" (pos101) (ax1h h19)))

		 (ax1i () (= (item2cl (cl2item c1))
			     c1)                                               ("Optional"()()))
		 
		 (h21 () (in c1 (union (setminus c2 (singleton (good-clause c2)))
					(singleton (setminus
						    (good-clause c2)
						    (singleton
						     (good-literal
						      (good-clause c2)))))))
		                                             ("=Subst" (pos10) (ax1i)(h20)))   

		 (ax1j () (in c1 c2)                                          ("Optional" ()())) 
		 
		 ;; ==================================================================
		 
		 (ls11 () (c2 (UNION                                                
                               C1                                               
                               (SINGLETON (GOOD-LITERAL (good-clause c2))))
			      )                                             ("Optional"()()))
		 
		 (ls10 (l1 ls11 h7)
		            (DERIVABLE                                             
                              (UNION                                                
                               C1                                               
                               (SINGLETON (GOOD-LITERAL (good-clause c2))))                   
                              C2                                                
                              CRULE-SET)                                      ("Open"()()))
                 
		 (lsg1 (l1) (DERIVABLE                                             
                              (UNION                                                
                               C1                                               
                               (SINGLETON (GOOD-LITERAL (good-clause c2))))                  
                              C2                                                
                              CRULE-SET)                              ("ExistsE"  (res-start)(h6 ls10)))

		 ;; ----------------------------------------------------------------

		 (ls21 () (c2 c1) ("Optional"()()))
		 
                 (ls20 (l1 ls21 h7)
		            (DERIVABLE C1 C2 CRULE-SET)                       ("Open" ()()))
                 
                 (lsg2 (l1) (DERIVABLE C1 C2 CRULE-SET)                ("ExistsE" (res-start)(h6 ls20)))

		 ;; ----------------------------------------------
		 
                 (l1 ()  (= dc
                            (cons (der-item-constr res-start c1 NULL NULL)
                                  NULL))                                        ("Optional"()()))

		 (l1a () (= (last dc)
			    (der-item-constr res-start c1 NULL NULL))          ("Optional"()()))

		 ;; ====================================================================
		 
                 (l2 () (forall (lam (x o) (or x (not x))))                   ("Axiom"()()))

		 (l3 () (or (=  (SETMINUS (GOOD-CLAUSE C2)                                
					     (SINGLETON                                          
					      (GOOD-LITERAL (good-clause c2))))
				c1)
			    (not (=  (SETMINUS (GOOD-CLAUSE C2)                                
					     (SINGLETON                                          
					      (GOOD-LITERAL (good-clause c2))))
				     c1)))                               ("ForallE"(ct1)(l2)))

                 (l4 () (=  (SETMINUS (GOOD-CLAUSE C2)                                
					     (SINGLETON                                          
					      (GOOD-LITERAL (good-clause c2))))
			    c1)                                                ("Hyp" ()()))

		 (l5 (l4) (OR (DERIVABLE C1 C2 CRULE-SET)
			      (DERIVABLE                                             
                              (UNION                                                
                               C1                                               
                               (SINGLETON (GOOD-LITERAL (good-clause c2))))                   
                              C2                                                
                              CRULE-SET))                                  ("OrIR" ()(lsg1)))

                 (l6 () (not (= (SETMINUS (GOOD-CLAUSE C2)                                
					     (SINGLETON                                          
					      (GOOD-LITERAL (good-clause c2))))
				c1))                                             ("Hyp"()()))

                 (l7 (l6) (OR (DERIVABLE C1 C2 CRULE-SET)                    
                             (DERIVABLE                                             
                              (UNION                                                
                               C1                                               
                               (SINGLETON (GOOD-LITERAL (good-clause c2))))                   
                              C2                                                
                              CRULE-SET))                                    ("OriL" ()(lsg2))) 
                     
                 (l30 () (OR (DERIVABLE C1 C2 CRULE-SET)                    
                             (DERIVABLE                                             
                              (UNION                                                
                               C1                                               
                               (SINGLETON (GOOD-LITERAL (good-clause c2))))                   
                              C2                                                
                              CRULE-SET))                                  ("OrE" ()(l3 l5 l7)))
                      
		 ;; ===========================================================
		
		 (ltp ()   (OR                                              
			    (DERIVABLE C1 C2 CRULE-SET)                    
			    (DERIVABLE                                             
			     (UNION                                                
			      C1                                               
			      (SINGLETON (GOOD-LITERAL (good-clause c2))))                   
			     C2                                                
			     CRULE-SET))                  ("Weaken" ()(l30)))
		 )
		(proc-content schema-interpreter)
		(remark "Special method for proving disjunctionlemma.")
		)

(infer~defmethod dsjk-base1
		 (outline-mappings (((existent  existent existent existent
				     nonexistent nonexistent);;4 x nonexistent
				     dsjk-base1-b)))  
		 (help "Spezialmethode zum Beweis des DISJL"))


(meth~defmethod dsjk-base1-b dsjk-base1  
		(in prop-res) 
		(rating 1)
		(declarations
		 (type-variables rr)
		 (type-constants o i)
		 (sorted-meta-variables 
		 (c31 num       term)
		 (dc glist      term)
		 (c2 (o (o form)) term)
		 (c1 (o form)   term)
		 (ct  glist     term)
		 (ct1 o         term)
		 (ct2 item      term)
		 (c3  (o form)  term)
		 (pos1110101 o pos)
		 (pos11110   o pos)
		 (pos1110    o pos)
		 (pos111     o pos)
		 (pos11      o pos)
		 (pos1110110 o pos)
		 (pos11101   o pos)
		 (pos101     o pos)
		 (pos10      o pos)
		 )
		 )
		(parameters) 
		(premises (- lh1) (- lh2) (- lh3)
			  (+ ls11) (+ ls20))
		(application-condition )  
		(outline-actions
		 (ls20 (sponsor ax1j))
		 (ls11 (sponsor ls12))
		 )
		(outline-orderings  )
		(outline-computations
		 (c3  (type-newconst (:type (o form))))
		 (ct  (:term (cons (der-item-constr res-start c3 NULL NULL)
				   (cons (der-item-constr res-start (good-clause c2) NULL NULL)
					 NULL))))
		 (ct1 (:term (= c1 (SETMINUS (GOOD-CLAUSE C2)                                
					     (SINGLETON                                          
					      (GOOD-LITERAL (good-clause c2)))))))
		 (ct2 (:term (last dc)))
		 (pos1110101 (:position (1 1 1 0 1 0 1)))
		 (pos11110   (:position (1 1 1 1 0)))
		 (pos1110    (:position (1 1 1 0)))
		 (pos111     (:position (1 1 1)))
		 (pos11      (:position (1 1)))
		 (pos1110110 (:position (1 1 1 0 1 1 0)))
		 (pos11101   (:position (1 1 1 0 1)))
		 (pos101     (:position (1 0 1)))
		 (pos10      (:position (1 0)))
		 )
		(conclusions (- ltp) )
		(decl-content
	       
		 
		 (lh1 () (= c31 (s zero))                            )

		 (lh2 () (= (minus (glist-length dc) one)
			    (s zero))                                     )

		 (lh3 () (DERIVATION-OF                                      
                         (UNION  (SETMINUS                                             
				  C2                                               
				  (SINGLETON (GOOD-CLAUSE C2)))                    
				 (SINGLETON                                            
				  (SETMINUS (GOOD-CLAUSE C2)                                
					    (SINGLETON                                          
					     (GOOD-LITERAL (good-clause c2))))))                          
                         CRULE-SET dc c1)          )

		 (h1 () (and (= (der-item-info (last dc)) c1)
			     (derivation
			      (UNION  (SETMINUS                                             
				       C2                                               
				       (SINGLETON (GOOD-CLAUSE C2)))                    
				      (SINGLETON                                            
				       (SETMINUS (GOOD-CLAUSE C2)                                
						 (SINGLETON                                          
						  (GOOD-LITERAL (good-clause c2))))))
			      crule-set dc))                    ("DefnE" (derivation-of)(lh3)))

		 (h2 () (derivation
			      (UNION  (SETMINUS                                             
				       C2                                               
				       (SINGLETON (GOOD-CLAUSE C2)))                    
				      (SINGLETON                                            
				       (SETMINUS (GOOD-CLAUSE C2)                                
						 (SINGLETON                                          
						  (GOOD-LITERAL (good-clause c2))))))
			      crule-set dc)  ("AndER" ()(h1)))

		 (h3 () (forall (lam (x item) 
			  (implies
			   (is-component x dc)
			   (exists (lam (r crule)
			      (and (free-derivation-cond dc)		
				   (and (and (crule-set r)
					     (crule-applicable
					      r
					      (append (append
						       (der-item-inp x)
						       (poslist2infolist (der-item-prems x) dc))
						      (cons (cl2item (der-item-info x))
							    NULL))
					      (union (setminus c2 (singleton
								   (good-clause
								    c2)))
						     (singleton (setminus
								 (good-clause c2)
								 (singleton
								  (good-literal (good-clause c2))))))))
					(and (forall (lam (y num)
						(implies (is-component (num2item y) (der-item-prems x))
							 (less y
							       (position x dc)))))
					     (= (der-item-info x)		
						(crule-application
						 r
						 (append (append
							  (der-item-inp x)
							  (poslist2infolist (der-item-prems x) dc))
							 (cons (cl2item
								(der-item-info x))
							       NULL))
						 (union (setminus c2 (singleton
								      (good-clause
								       c2)))
							(singleton (setminus
								    (good-clause
								     c2)
								    (singleton
								     (good-literal
								      (good-clause c2))))))))))))))))
		     ("DefnE" (derivation) (h2)))

		 (h4   ()  (implies
			   (is-component (last dc) dc)
			   (exists (lam (r crule)
			      (and (free-derivation-cond dc)		
				   (and (and (crule-set r)
					     (crule-applicable
					      r
					      (append (append
						       (der-item-inp (last dc))
						       (poslist2infolist (der-item-prems (last dc)) dc))
						      (cons (cl2item (der-item-info (last dc)))
							    NULL))
					      (union (setminus c2 (singleton
								   (good-clause
								    c2)))
						     (singleton (setminus
								 (good-clause c2)
								 (singleton
								  (good-literal (good-clause c2))))))))
					(and (forall (lam (y num)
						(implies (is-component (num2item y) (der-item-prems (last dc)))
							 (less y
							       (position (last dc) dc)))))
					     (= (der-item-info (last dc))		
						(crule-application
						 r
						 (append (append
							  (der-item-inp (last dc))
							  (poslist2infolist (der-item-prems (last dc)) dc))
							 (cons (cl2item
								(der-item-info (last dc)))
							       NULL))
						 (union (setminus c2 (singleton
								      (good-clause
								       c2)))
							(singleton (setminus
								    (good-clause
								     c2)
								    (singleton
								     (good-literal
								      (good-clause
								       c2))))))))))))))
		       ("ForallE" (ct2) (h3)))

		 (h5   ()  (is-component (last dc) dc) ("Optional"()()))

		 (h6   ()  (exists (lam (r crule)
			      (and (free-derivation-cond dc)		
				   (and (and (crule-set r)
					     (crule-applicable
					      r
					      (append (append
						       (der-item-inp (last dc))
						       (poslist2infolist (der-item-prems (last dc)) dc))
						      (cons (cl2item (der-item-info (last dc)))
							    NULL))
					      (union (setminus c2 (singleton
								   (good-clause
								    c2)))
						     (singleton (setminus
								 (good-clause c2)
								 (singleton
								  (good-literal (good-clause c2))))))))
					(and (forall (lam (y num)
						(implies (is-component (num2item y) (der-item-prems (last dc)))
							 (less y
							       (position (last dc) dc)))))
					     (= (der-item-info (last dc))		
						(crule-application
						 r
						 (append (append
							  (der-item-inp (last dc))
							  (poslist2infolist (der-item-prems (last dc)) dc))
							 (cons (cl2item
								(der-item-info (last dc)))
							       NULL))
						 (union (setminus c2 (singleton
								      (good-clause
								       c2)))
							(singleton (setminus
								    (good-clause
								     c2)
								    (singleton
								     (good-literal
								      (good-clause
								       c2)))))))))))))
		       ("ImpE"()(h4 h5)))

		 (h7 () (and (free-derivation-cond dc)		
				   (and (and (crule-set res-start)
					     (crule-applicable
					      res-start
					      (append (append
						       (der-item-inp (last dc))
						       (poslist2infolist (der-item-prems (last dc)) dc))
						      (cons (cl2item (der-item-info (last dc)))
							    NULL))
					      (union (setminus c2 (singleton
								   (good-clause
								    c2)))
						     (singleton (setminus
								 (good-clause c2)
								 (singleton
								  (good-literal (good-clause c2))))))))
					(and (forall (lam (y num)
						(implies (is-component (num2item y) (der-item-prems (last dc)))
							 (less y
							       (position (last dc) dc)))))
					     (= (der-item-info (last dc))		
						(crule-application
						 res-start
						 (append (append
							  (der-item-inp (last dc))
							  (poslist2infolist (der-item-prems (last dc)) dc))
							 (cons (cl2item
								(der-item-info (last dc)))
							       NULL))
						 (union (setminus c2 (singleton
								      (good-clause
								       c2)))
							(singleton (setminus
								    (good-clause
								     c2)
								    (singleton
								     (good-literal
								      (good-clause
								       c2)))))))))))
		     ("Hyp"()()))
		 
		 (h8 () (and (and (crule-set res-start)
					     (crule-applicable
					      res-start
					      (append (append
						       (der-item-inp (last dc))
						       (poslist2infolist (der-item-prems (last dc)) dc))
						      (cons (cl2item (der-item-info (last dc)))
							    NULL))
					      (union (setminus c2 (singleton
								   (good-clause
								    c2)))
						     (singleton (setminus
								 (good-clause c2)
								 (singleton
								  (good-literal (good-clause c2))))))))
					(and (forall (lam (y num)
						(implies (is-component (num2item y) (der-item-prems (last dc)))
							 (less y
							       (position (last dc) dc)))))
					     (= (der-item-info (last dc))		
						(crule-application
						 res-start
						 (append (append
							  (der-item-inp (last dc))
							  (poslist2infolist (der-item-prems (last dc)) dc))
							 (cons (cl2item
								(der-item-info (last dc)))
							       NULL))
						 (union (setminus c2 (singleton
								      (good-clause
								       c2)))
							(singleton (setminus
								    (good-clause
								     c2)
								    (singleton
								     (good-literal
								      (good-clause
								       c2))))))))))
		     ("AndER"()(h7)))
		 (h9 () (and (crule-set res-start)
			     (crule-applicable
			      res-start
			      (append (append
				       (der-item-inp (last dc))
				       (poslist2infolist (der-item-prems (last dc)) dc))
				      (cons (cl2item (der-item-info (last dc)))
					    NULL))
			      (union (setminus c2 (singleton
						   (good-clause
						    c2)))
				     (singleton (setminus
						 (good-clause c2)
						 (singleton
						  (good-literal
						   (good-clause c2))))))))
		     ("AndEL"()(h8)))

		 (h10 ()  (crule-applicable
			      res-start
			      (append (append
				       (der-item-inp (last dc))
				       (poslist2infolist (der-item-prems (last dc)) dc))
				      (cons (cl2item (der-item-info (last dc)))
					    NULL))
			      (union (setminus c2 (singleton
						   (good-clause
						    c2)))
				     (singleton (setminus
						 (good-clause c2)
						 (singleton
						  (good-literal
						   (good-clause c2)))))))     ("AndER" ()(h9)))

		 (h11 (l1a) (crule-applicable
			      res-start
			      (append (append
				       (der-item-inp (der-item-constr res-start c1 NULL NULL))
				       (poslist2infolist (der-item-prems (last dc)) dc))
				      (cons (cl2item (der-item-info (last dc)))
					    NULL))
			      (union (setminus c2 (singleton
						   (good-clause
						    c2)))
				     (singleton (setminus
						 (good-clause c2)
						 (singleton
						  (good-literal
						   (good-clause c2))))))) ("=Subst"
									   (pos1110101)  (l1a h10)))  

		 (ax1a () (= NULL
			     (der-item-inp (der-item-constr res-start c1 NULL NULL))) ("Optional"()()))
		 
		 (h12 (l1a) (crule-applicable
			      res-start
			      (append (append
				       NULL
				       (poslist2infolist (der-item-prems (last dc)) dc))
				      (cons (cl2item (der-item-info (last dc)))
					    NULL))
			      (union (setminus c2 (singleton
						   (good-clause
						    c2)))
				     (singleton (setminus
						 (good-clause c2)
						 (singleton
						  (good-literal
						   (good-clause c2))))))) ("=Subst"  (pos11110) (ax1a h11)))

		 (ax1b () (= (poslist2infolist (der-item-prems (last dc)) dc)
			     (append NULL (poslist2infolist (der-item-prems (last dc)) dc))) ("Optional"()()))
		 
		 (h13 (l1a) (crule-applicable
			      res-start
			      (append  (poslist2infolist (der-item-prems (last dc)) dc)
			               (cons (cl2item (der-item-info (last dc)))
					     NULL))
			      (union (setminus c2 (singleton
						   (good-clause
						    c2)))
				     (singleton (setminus
						 (good-clause c2)
						 (singleton
						  (good-literal
						   (good-clause c2))))))) ("=Subst" (pos1110) (ax1b h12)))

		 (ax1c () (= (der-item-prems (last dc))
			     NULL)                            		       ("Optional"()()))
		 
		 (h14 (l1a) (crule-applicable
			      res-start
			      (append  (poslist2infolist NULL dc)
			               (cons (cl2item (der-item-info (last dc)))
					     NULL))
			      (union (setminus c2 (singleton
						   (good-clause
						    c2)))
				     (singleton (setminus
						 (good-clause c2)
						 (singleton
						  (good-literal
						   (good-clause c2)))))))     ("=Subst" (pos1110)(ax1c h13)))

		 (ax1d () (= (poslist2infolist NULL dc)
			     NULL)                                         ("Optional"()()))
		 
		 (h15 (l1a) (crule-applicable
			      res-start
			      (append  NULL
			               (cons (cl2item (der-item-info (last dc)))
					     NULL))
			      (union (setminus c2 (singleton
						   (good-clause
						    c2)))
				     (singleton (setminus
						 (good-clause c2)
						 (singleton
						  (good-literal
						   (good-clause c2)))))))     ("=Subst"(pos111)(ax1d h14)))

		 (ax1e () (= (append NULL (cons (cl2item (der-item-info (last dc)))  NULL))
			     (cons (cl2item (der-item-info (last dc))) NULL))  ("Optional"()()))
		 
		 (h16 (l1a) (crule-applicable
			      res-start
			      (cons (cl2item (der-item-info (last dc))) NULL)
			      (union (setminus c2 (singleton (good-clause c2)))
				     (singleton (setminus
						 (good-clause c2)
						 (singleton
						  (good-literal
						   (good-clause c2)))))))  ("=Subst" (pos11) (ax1e h15))) 

		 (h17 (l1a) (crule-applicable
			      res-start
			      (cons (cl2item (der-item-info
					      (der-item-constr res-start c1 NULL NULL))) NULL)
			      (union (setminus c2 (singleton (good-clause c2)))
				     (singleton (setminus
						 (good-clause c2)
						 (singleton
						  (good-literal
						   (good-clause c2)))))))   ("=Subst" (pos1110110) (l1a h16)))

		 (ax1f () (= (der-item-info (der-item-constr res-start c1 NULL NULL))
			     c1)                                                       ("Optional"()()))
			     
		 (h18 (l1a) (crule-applicable
			      res-start
			      (cons (cl2item c1) NULL)
			      (union (setminus c2 (singleton (good-clause c2)))
				     (singleton (setminus
						 (good-clause c2)
						 (singleton
						  (good-literal
						   (good-clause c2))))))) ("=Subst"(pos11101)(ax1f h17)))       

		 (ax1g () (equiv (crule-applicable res-start
						   (cons (cl2item c1) NULL)
						   (union (setminus c2 (singleton (good-clause c2)))
							  (singleton (setminus
								      (good-clause c2)
								      (singleton
								       (good-literal
									(good-clause c2)))))))
				 (in (item2cl (first (cons (cl2item c1) NULL)))
				     (union (setminus c2 (singleton (good-clause c2)))
				     (singleton (setminus
						 (good-clause c2)
						 (singleton
						  (good-literal
						   (good-clause c2))))))))            ("Optional"()())) 

		 (h19 () (in (item2cl (first (cons (cl2item c1) NULL)))
				     (union (setminus c2 (singleton (good-clause c2)))
				     (singleton (setminus
						 (good-clause c2)
						 (singleton
						  (good-literal
						   (good-clause c2))))))) ("EquivER" ()(ax1g h18)))

		 (ax1h () (= (first (cons (cl2item c1) NULL))
			     (cl2item c1)) ("Optional"()()))
		 
		 (h20 () (in (item2cl (cl2item c1))
			     (union (setminus c2 (singleton (good-clause c2)))
				    (singleton (setminus
						(good-clause c2)
						(singleton
						 (good-literal
						  (good-clause c2))))))) ("=Subst" (pos101) (ax1h h19)))

		 (ax1i () (=  (item2cl (cl2item c1))
			      c1)                                                        ("Optional"()()))
		 
		 (h21 () (in c1 (union (setminus c2 (singleton (good-clause c2)))
					(singleton (setminus
						    (good-clause c2)
						    (singleton
						     (good-literal
						      (good-clause c2)))))))  ("=Subst" (pos10) (ax1i)(h20)))   

		 (ax1j () (c2 c1)                            		          ("abstract" ()())) 
		 
		 ;; ------------------------------------------------------
		 
;		 (= (SETMINUS (GOOD-CLAUSE C2)                                
;                                     (SINGLETON                                          
;                                      (GOOD-LITERAL (good-clause c2))))
;                    c1)

		 (ls12  ()   (c2 (UNION                                                
                               C1                                               
                               (SINGLETON (GOOD-LITERAL (good-clause c2))))
				)	                                           ("abstract" ()()))

		 (ls11 (l1 ls12 h7) (DERIVABLE                                             
				 (UNION                                                
				  C1                                               
				  (SINGLETON (GOOD-LITERAL (good-clause c2))))                   
				 C2                                                
				 CRULE-SET)                                        ("Open" ()()))
		 
                 (lsg1 (l1) (DERIVABLE                                             
				 (UNION                                                
				  C1                                               
				  (SINGLETON (GOOD-LITERAL (good-clause c2))))                   
				 C2                                                
				 CRULE-SET)                    ("ExistsE" (res-start) (ls11 h6)))         

		 ;; ==================================================================
		 
                 (ls20 (l1 h7 ax1j)
		               (DERIVABLE C1 C2 CRULE-SET)       ("Open"()()))
                 
                 (lsg2 (l1)    (DERIVABLE C1 C2 CRULE-SET)       ("ExistsE"(res-start)(ls20 h6)))

		 ;; ===================================================================
		 
                 (l1 ()  (= dc
			    (cons (der-item-constr res-start c3 NULL NULL)
				  (cons (der-item-constr res-start c1 NULL NULL)
					NULL)))                                   ("Optional"()()))
		 (l1a () (= (last dc)
			    (der-item-constr res-start c1 NULL NULL))                 ("Optional"()()))

                 (l2 () (forall (lam (x o) (or x (not x))))                          ("Axiom"()()))

                 (l3 () (or (= (SETMINUS (GOOD-CLAUSE C2)                                
					   (SINGLETON                                          
					    (GOOD-LITERAL (good-clause c2))))
				c1)
			    (not (= (SETMINUS (GOOD-CLAUSE C2)                                
						(SINGLETON                                          
						 (GOOD-LITERAL (good-clause c2))))
				     c1)))                                 ("ForallE"(ct1)(l2)))

                 (l4 () (= (SETMINUS (GOOD-CLAUSE C2)                                
				     (SINGLETON                                          
				      (GOOD-LITERAL (good-clause c2))))
			   c1)                                                      ("Hyp" ()()))

                 (l5 (l4) (OR (DERIVABLE C1 C2 CRULE-SET)
                              (DERIVABLE                                             
                              (UNION                                                
                               C1                                               
                               (SINGLETON (GOOD-LITERAL (good-clause c2))))                   
                              C2                                                
                              CRULE-SET))                                       ("OrIR" ()(lsg1)))
		 
                 (l6 () (not (= (SETMINUS (GOOD-CLAUSE C2)                                
					   (SINGLETON                                          
					    (GOOD-LITERAL (good-clause c2))))
				c1))                                                  ("Hyp"()()))
		 
                 (l7 (l6) (OR (DERIVABLE C1 C2 CRULE-SET)
                              (DERIVABLE                                             
                              (UNION                                                
                               C1                                               
                               (SINGLETON (GOOD-LITERAL (good-clause c2))))                   
                              C2                                                
                              CRULE-SET))                                        ("OriL" ()(lsg2))) 
                     
                 (l30 () (OR (DERIVABLE C1 C2 CRULE-SET)                    
                             (DERIVABLE                                             
                              (UNION                                                
                               C1                                               
                               (SINGLETON (GOOD-LITERAL (good-clause c2) )))
                              C2                                                
                              CRULE-SET))                                      ("OrE" ()(l3 l7 l5)))
                      
		 ;; ===========================================================
		
		 (ltp ()   (OR                                              
			    (DERIVABLE C1 C2 CRULE-SET)                    
			    (DERIVABLE                                             
			     (UNION                                                
			      C1                                               
			      (SINGLETON (GOOD-LITERAL (good-clause c2))))                   
			     C2                                                
			     CRULE-SET))                                      ("weaken" ()(l30)))
		 )
		(proc-content schema-interpreter)
		(remark "Special method for proving disjunctionlemma.")
		)


(infer~defmethod disj-ih
		 (outline-mappings (((existent existent existent existent ;; existent
					       )
				     disj-ih-m-b)))
		 (help "Application of the ind-hyp for the "))


(meth~defmethod disj-ih-m-b disj-ih  
		(in prop-res) 
		(rating 30)
		(reasoning :planning)
		(declarations
		 (constants               )
		 (sorted-meta-variables		
		  (co9 (o (o form)) term)
		  (numc   num       term)
		  (ih-seq glist     term)
		  (dc55   glist     term)
		  (cx12   num       term)
		  )
		 )
		(parameters ) 
		(premises
		  (- l49) (- l52) ;;(- l51) ;; (- l50)
		 )
		(application-condition )  
		(outline-actions 
		 )
		(outline-orderings
		 )
		(outline-computations
		 )
		(conclusions (- ltp))
		(decl-content

;		 (l54 () (derivation-of (union (setminus co9 (singleton (good-clause co9)))
;                                                    (singleton
;                                                     (setminus (good-clause co9)
;                                                               (singleton (good-literal
;                                                                           (good-clause co9))))))
;                                             crule-set ih-seq (der-item-info (glist-nth
;                                                                               numc
;                                                                               dc55))) ("AndER"()(l52)))
;                 
;                 (l53 () (less (minus (glist-length ih-seq) one)
;                               cx12) ("andEL" ()(l52)))
;                 
                 (l52 ()  (and (less (minus (glist-length ih-seq) one)
                                    cx12)
                              (derivation-of (union (setminus co9 (singleton (good-clause co9)))
                                                    (singleton
                                                     (setminus (good-clause co9)
                                                               (singleton (good-literal
                                                                           (good-clause co9))))))
                                             crule-set ih-seq (der-item-info (glist-nth
                                                                               numc
                                                                               dc55))))
                      )
                 
                 
                 (l51 ()  (greater cx12 (s zero))        )
                                                                
                 (l50 ()  (less numc cx12)               )
		 
		 (l49 ()  (forall (lam (cx2 num)           
	                     (implies (and (greater cx12 (s zero))
					   (less cx2 cx12))
				      (forall (lam (dc54 glist)
					 (implies
					  (= (minus (glist-length dc54) one)
					     cx2)
					  (forall (lam (co8 (o form))
					  (implies                                            
					   (derivation-of
					    (union (setminus co9 (singleton (good-clause co9)))
						   (singleton (setminus (good-clause co9)
									(singleton
									 (good-literal
									  (good-clause co9))))))
					    crule-set
					    dc54
					    co8)
					   (or (derivable co8 co9 crule-set)                  
					       (derivable                                        
						(union co8 (singleton (good-literal
								       (good-clause
									co9))))
						co9
						crule-set)))))))))))                      )

		 (l48 () (implies (and (greater cx12 (s zero))
					   (less numc cx12))
				      (forall (lam (dc54 glist)
					 (implies
					  (= (minus (glist-length dc54) one)
					     numc)
					  (forall (lam (co8 (o form))
					  (implies                                            
					   (derivation-of
					    (union (setminus co9 (singleton (good-clause co9)))
						   (singleton (setminus (good-clause co9)
									(singleton
									 (good-literal
									  (good-clause co9))))))
					    crule-set
					    dc54
					    co8)
					   (or (derivable co8 co9 crule-set)                  
					       (derivable                                        
						(union co8 (singleton (good-literal
								       (good-clause
									co9))))
						co9
						crule-set)))))))))  ("forallE" (numc)
								     (l49)))

		 (l47 () (and (greater cx12 (s zero))
			      (less numc cx12))     ("AndI" () (l50 l51)))

		 (l46 () (forall (lam (dc54 glist)
				      (implies
				       (= (minus (glist-length dc54) one)
					  numc)
				       (forall (lam (co8 (o form))
						    (implies                                            
						     (derivation-of
						      (union (setminus co9 (singleton (good-clause co9)))
							     (singleton (setminus (good-clause co9)
										  (singleton
										   (good-literal
										    (good-clause co9))))))
						      crule-set
						      dc54
						      co8)
						     (or (derivable co8 co9 crule-set)                  
							 (derivable                                        
							  (union co8 (singleton (good-literal
								       (good-clause
									co9))))
							  co9
						crule-set))))))))         ("ImpE" ()(l47 l48)))
		 
		 (l45 () (implies
			  (= (minus (glist-length ih-seq) one)
			     numc)
			  (forall (lam (co8 (o form))
				       (implies                                            
					(derivation-of
					 (union (setminus co9 (singleton (good-clause co9)))
							     (singleton (setminus (good-clause co9)
										  (singleton
										   (good-literal
										    (good-clause co9))))))
					 crule-set
					 ih-seq
					 co8)
					(or (derivable co8 co9 crule-set)                  
					    (derivable                                        
					     (union co8 (singleton (good-literal
								    (good-clause
								     co9))))
					     co9
					     crule-set))))))          ("forallE" (ih-seq)
								       (l46)))

		 (l44 () (= (minus (glist-length ih-seq) one)
			    numc)   ("abstract"()()))

		 (l43 () (forall (lam (co8 (o form))
			   (implies                                            
			    (derivation-of
			     (union (setminus co9 (singleton (good-clause co9)))
				    (singleton (setminus (good-clause co9)
							 (singleton
							  (good-literal
							   (good-clause co9))))))
			     crule-set
			     ih-seq
			     co8)
			    (or (derivable co8 co9 crule-set)                  
				(derivable                                        
				 (union co8 (singleton (good-literal
							(good-clause
							 co9))))
				 co9
				 crule-set))))) ("impE"()(l44 l45)))
		 
		 (l42 () (implies                                            
			  (derivation-of
			   (union (setminus co9 (singleton (good-clause co9)))
				  (singleton (setminus (good-clause co9)
						       (singleton
							(good-literal
							 (good-clause co9))))))
			   crule-set ih-seq
			   (DER-ITEM-INFO                                        
			    (GLIST-NTH NUMC DC55)))
			  (or (derivable
			       (DER-ITEM-INFO                                        
				(GLIST-NTH NUMC DC55))
			       co9 crule-set)                  
			      (derivable                                        
			       (union
				(DER-ITEM-INFO                                        
				 (GLIST-NTH NUMC DC55))
				(singleton (good-literal
					    (good-clause
					     co9))))
			       co9 crule-set)))   ("forallE"(substterm)(l43)))
		 
		 ;; ==========================================================================
		 
		 (ltp () (or                                        
			  (DERIVABLE                                             
			   (DER-ITEM-INFO                                        
			    (GLIST-NTH NUMC DC55))                         
			   CO9                                                
			   CRULE-SET)                                            
			  (DERIVABLE                                             
			   (UNION                                                
			    (DER-ITEM-INFO                                       
			     (GLIST-NTH NUMC DC55))                        
			    (SINGLETON                                           
			     (GOOD-LITERAL                                       
			      (GOOD-CLAUSE CO9))))                            
			   CO9                                                
                          CRULE-SET))  ("impe" () (l42 l54)))
		 
		 )
		(proc-content schema-interpreter)
		(remark "")
	       )


(infer~defmethod simp-djl
		 (outline-mappings (((existent nonexistent) simp-djl-m-b)))  
		 (help "Lemma to simplify the Disjunctionlemma."))

(meth~defmethod simp-djl-m-b simp-djl  
		(in prop-res)
		(rating 30)
		(declarations
		 (type-constants o i num crule glist)
		 (type-variables bb rr) 
		 (sorted-meta-variables  ;;(meta-var type sort)
		  (nk  (o form) term) (nkm (o (o form)) term)
		  (fterm (o form) term) (sterm form term)
		  (tterm glist term)
		  )
		 )
		(premises  (+ l14)  )
		(application-condition  )
		(outline-orderings
		 )
		(outline-computations
                 (nk    (type-newconst  (:type (o form))))
                 (nkm   (type-newconst  (:type (o (o form)))))
                 (fterm (:term (good-clause nkm)))
                 (sterm (:term (good-literal (good-clause nkm))))
                 (tterm (type-newconst (:type glist)))
		 )
		(conclusions      (- ltp))
		(decl-content

                 (h () (and (in nkm all-clause-sets)
                               (exists (lam  (kl1 (o form))
                                             (and (nkm kl1) (card>2 kl1))))) ("Hyp"()()))

                 (k () (derivable nk
                           (union (setminus nkm (singleton fterm))
                                  (singleton (setminus fterm (singleton sterm))))
                           crule-set)                                        ("Hyp"()()))

                 (k1 (k) (exists (lam (seq glist)
                            (derivation-of                                      
                             (union (setminus nkm (singleton fterm))                      
                                    (singleton                                            
                                     (setminus fterm (singleton sterm))))                  
                             crule-set                                             
                             seq
                             nk                                                     
                             )))                 ("DefnE" (derivable) (k)))     


                 (k2 (k) (derivation-of                                      
                             (union (setminus nkm (singleton fterm))                      
                                    (singleton                                            
                                     (setminus fterm (singleton sterm))))                  
                             crule-set                                             
                             tterm
                             nk                                                     
                             )                                                ("Hyp"()()))

                 (l14 (h k k2) (forall (lam (av glist) (forall (lam (x (o form))
                                  (implies
                                   (derivation-of                                      
                                    (union (setminus nkm (singleton fterm))                      
                                           (singleton                                            
                                            (setminus fterm (singleton sterm))))                  
                                    crule-set
                                    av
                                    x                                                     
                                   )
                                   (or (derivable x nkm crule-set)
                                       (derivable (union x (singleton sterm))
                                                  nkm crule-set))))))) ("Open" ()()))
		 
		 (l13 (h k k2) (forall (lam (x (o form))
                                  (implies
                                   (derivation-of                                      
                                    (union (setminus nkm (singleton fterm))                      
                                           (singleton                                            
                                            (setminus fterm (singleton sterm))))                  
                                    crule-set
                                    tterm
                                    x                                                     
                                   )
                                   (or (derivable x nkm crule-set)
                                       (derivable (union x (singleton sterm))
                                                  nkm crule-set))))) ("ForallE" (tterm) (l14)))
				       
                 (l12 (h k k2) (implies
                                (derivation-of                                      
                                 (union (setminus nkm (singleton fterm))                      
                                        (singleton                                            
                                         (setminus fterm (singleton sterm))))                  
                                 crule-set                                             
                                 tterm
                                 nk  )
                                (or (derivable nk nkm crule-set)
                                    (derivable (union nk (singleton sterm))
                                               nkm crule-set))) ("ForallE" (nk) (l13)))
                 
                 (l11 (h k k2) (or (derivable nk nkm crule-set)
                                   (derivable (union nk (singleton sterm))
                                              nkm crule-set))   ("ImpE" ()(l12 k2)))    
                      
                 (l10 (h k) (or (derivable nk nkm crule-set)
                                (derivable (union nk (singleton sterm))
                                           nkm crule-set))   ("ExistsE" (tterm) (l11 k1)))   
                  
                 (l9 (h) (implies
                             (derivable
                              nk
                              (union (setminus nkm (singleton fterm))
                                     (singleton (setminus fterm (singleton sterm))))
                              crule-set)
                             (or (derivable nk nkm crule-set)
                                 (derivable (union nk (singleton sterm))
                                            nkm crule-set)))      ("ImpI" ()(l10)))
                 
                 (l8 (h) (fterm sterm) ("Open"()()))
                 
                 (l7 (h) (and (implies
			       (derivable
				nk
				(union (setminus nkm (singleton fterm))
				       (singleton (setminus fterm (singleton sterm))))
				crule-set)
			       (or (derivable nk nkm crule-set)
				   (derivable (union nk (singleton sterm))
					      nkm crule-set)))
			      (fterm sterm))                        ("AndI" ()(l9 l8)))
                 
                 (l6 (h) (and (nkm fterm) (card>2 fterm))                ("Open" ()()))
                 
                 (l5 (h) (and (and (implies
				    (derivable
				     nk
				     (union (setminus nkm (singleton fterm))
                                                  (singleton (setminus fterm (singleton sterm))))
				     crule-set)
				    (or (derivable nk nkm crule-set)
					(derivable (union nk (singleton sterm))
						   nkm crule-set)))
				   (fterm sterm))
			      (and (nkm fterm) (card>2 fterm)))          ("AndI" ()(l7 l6))) 
                 
                 (l4 (h) (exists (lam (l form)
                               (and (and (implies
                                          (derivable
                                           nk
                                           (union (setminus nkm (singleton fterm))
                                                  (singleton (setminus fterm (singleton l))))
                                           crule-set)
                                          (or (derivable nk nkm crule-set)
                                              (derivable (union nk (singleton l))
                                                         nkm crule-set)))
                                         (fterm l))
                                    (and (nkm fterm) (card>2 fterm))))) ("ExistsI" (sterm)(l5)))
                 
                 (l3 (h) (exists (lam (kl (o form)) (exists (lam (l form)
                                  (and (and (implies
                                             (derivable
                                              nk
                                              (union (setminus nkm (singleton kl))
                                                     (singleton (setminus kl (singleton l))))
                                              crule-set)
                                             (or (derivable nk nkm crule-set)
                                                 (derivable (union nk (singleton l))
                                                            nkm crule-set)))
                                            (kl l))
                                       (and (nkm kl) (card>2 kl))))))) ("ExistsI" (fterm)(l4)))
                 
                 (l2  () (implies
                               (and (in nkm all-clause-sets)
                                    (exists (lam  (kl1 (o form)) (and (nkm kl1) (card>2 kl1)))))
                               (exists (lam (kl (o form)) (exists (lam (l form)
                                  (and (and (implies
                                             (derivable
                                              nk
                                              (union (setminus nkm (singleton kl))
                                                     (singleton (setminus kl (singleton l))))
                                              crule-set)
                                             (or (derivable nk nkm crule-set)
                                                 (derivable (union nk (singleton l))
                                                            nkm crule-set)))
                                            (kl l))
                                       (and (nkm kl) (card>2 kl))))))))  ("ImpI" () (l3)))
                 
                 (l1  () (forall (lam (gkl (o form))
			      (implies
			       (and (in nkm all-clause-sets)
				    (exists (lam  (kl1 (o form)) (and (nkm kl1) (card>2 kl1)))))
			       (exists (lam (kl (o form)) (exists (lam (l form)
				  (and (and (implies
					     (derivable
					      gkl
					      (union (setminus nkm (singleton kl))
						     (singleton (setminus kl (singleton l))))
					      crule-set)
					     (or (derivable gkl nkm crule-set)
						 (derivable (union gkl (singleton l))
							    nkm crule-set)))
					    (kl l))
				       (and (nkm kl) (card>2 kl)))))))))) ("ForallI"(nk)(l2))) 
		 
		 (ltp () (forall (lam (klm (o (o form)))(forall (lam (gkl (o form))
			      (implies
			       (and (in klm all-clause-sets)
				    (exists (lam  (kl1 (o form)) (and (klm kl1) (card>2 kl1)))))
			       (exists (lam (kl (o form)) (exists (lam (l form)
				  (and (and (implies
					     (derivable
					      gkl
					      (union (setminus klm (singleton kl))
						     (singleton (setminus kl (singleton l))))
					      crule-set)
					     (or (derivable gkl klm crule-set)
						 (derivable (union gkl (singleton l))
							    klm crule-set)))
					    (kl l))
				       (and (klm kl) (card>2 kl)))))))))))) ("ForallI"(nkm)(l1)))
		 )
		(proc-content schema-interpreter)
		(remark "Method to prove a proposition by strong induction on natural numbers.")
		)


(infer~defmethod simp-fact4
		 (outline-mappings (((existent nonexistent) simp-fact4-m-b)))  
		 (help "Lemma to simplify the Disjunctionlemma."))



(meth~defmethod simp-fact4-m-b simp-fact4  
		(in prop-res)
		(rating 30)
		(declarations
		 (type-constants o i num crule glist)
		 (type-variables bb rr) 
		 (sorted-meta-variables  ;;(meta-var type sort)
		  (nk  (o form) term) (nkm (o (o form)) term)
		  (nl  form term)
		  (fterm (o form) term) (sterm form term)
		  (av glist term)
		  )
		 )
		(premises  (+ l7)  )
		(application-condition  )
		(outline-orderings
		 )
		(outline-computations
                 (nk   (type-newconst (:type (o form))))
                 (nkm  (type-newconst (:type (o (o form)))))
		 (nl   (type-newconst (:type form)))
		 (av   (type-newconst (:type glist)))
                 (fterm (:term (good-clause nkm)))
                 (sterm (:term (good-literal (good-clause nkm))))
                 (tterm (type-newconst (:type glist)))
		 )
		(conclusions      (- ltp))
		(decl-content

		 (la1  () (AND                                                  
			   (AND (nKM nK) (nK nL))                                
			   (AND                                                 
			    (DERIVABLE                                          
			     (UNION EMPTY-CL (SINGLETON nL))                     
			     nKM                                                
			     CRULE-SET)                                         
			    (DERIVABLE                                          
			     EMPTY-CL                                           
			     (UNION                                             
			      (SETMINUS nKM (SINGLETON nK))                     
			      (SINGLETON (SINGLETON nL)))                        
			     CRULE-SET)))                                        ("Hyp"()()))

		 (la2 ()  (AND                                                 
			    (DERIVABLE                                          
			     (UNION EMPTY-CL (SINGLETON nL))                     
			     nKM                                                
			     CRULE-SET)                                         
			    (DERIVABLE                                          
			     EMPTY-CL                                           
			     (UNION                                             
			      (SETMINUS nKM (SINGLETON nK))                     
			      (SINGLETON (SINGLETON nL)))                        
			     CRULE-SET))                                    ("AndER"()(la1)))

		 (la3 ()  (DERIVABLE                                          
			     (UNION EMPTY-CL (SINGLETON nL))                     
			     nKM                                                
			     CRULE-SET)                                     ("AndEL"()(la2)))

		 (la4 () (exists (lam (seq glist)
			     (derivation-of nKM CRULE-SET seq
					    (UNION EMPTY-CL (SINGLETON nL)))))
		                                                  ("DefnE" (derivable)(la3)))

		 (lb1 () (derivation-of nKM CRULE-SET av
					    (UNION EMPTY-CL (SINGLETON nL)))     ("Hyp"()()))

		 (l7  () (forall (lam (ad glist)
			    (implies
			     (derivation-of nKM CRULE-SET av
					    (UNION EMPTY-CL (SINGLETON nL)))
			     (DERIVABLE EMPTY-CL nKM CRULE-SET))))              ("Open"()()))
			    
		 (l6  () (implies
			  (derivation-of nKM CRULE-SET av
					    (UNION EMPTY-CL (SINGLETON nL)))
			  (DERIVABLE EMPTY-CL nKM CRULE-SET))          ("forallE" (av) (l7)))
		      
		 (l5  (la1 lb1)(DERIVABLE EMPTY-CL nKM CRULE-SET)        ("ImpE" ()(l6 lb1)))
		 
		 (l4  (la1) (DERIVABLE EMPTY-CL nKM CRULE-SET)       ("ExistsE"(av)(la4 l5)))     
		 
		 (l3  () (IMPLIES                                               
			  (AND                                                  
			   (AND (nKM nK) (nK nL))                                
			   (AND                                                 
			    (DERIVABLE                                          
			     (UNION EMPTY-CL (SINGLETON nL))                     
			     nKM                                                
			     CRULE-SET)                                         
			    (DERIVABLE                                          
			     EMPTY-CL                                           
			     (UNION                                             
			      (SETMINUS nKM (SINGLETON nK))                     
			      (SINGLETON (SINGLETON nL)))                        
			     CRULE-SET)))                                       
			  (DERIVABLE EMPTY-CL nKM CRULE-SET))            ("ImpI"()(l4 la1)))
		 
		 (l2  () (forall (lam (L FORM)     
			    (IMPLIES                                               
			     (AND                                                  
			      (AND (nKM nK) (nK L))                                
			      (AND                                                 
			       (DERIVABLE                                          
				(UNION EMPTY-CL (SINGLETON L))                     
				nKM                                                
				CRULE-SET)                                         
			       (DERIVABLE                                          
				EMPTY-CL                                           
				(UNION                                             
				 (SETMINUS nKM (SINGLETON nK))                     
				 (SINGLETON (SINGLETON L)))                        
				CRULE-SET)))                                       
			     (DERIVABLE EMPTY-CL nKM CRULE-SET))))   ("ForallI"(nl)(l3)))
		 
		 (l1  () (forall (lam (KL (O FORM))
			      (forall (lam (L FORM)     
				(IMPLIES                                               
				 (AND                                                  
				  (AND (nKM KL) (KL L))                                
				  (AND                                                 
				   (DERIVABLE                                          
				    (UNION EMPTY-CL (SINGLETON L))                     
				    nKM                                                
				    CRULE-SET)                                         
				   (DERIVABLE                                          
				    EMPTY-CL                                           
				    (UNION                                             
				     (SETMINUS nKM (SINGLETON KL))                     
				     (SINGLETON (SINGLETON L)))                        
				    CRULE-SET)))                                       
				 (DERIVABLE EMPTY-CL nKM CRULE-SET)))))) ("ForallI" (nk) (l2)))
		 
		 (ltp ()  (FORALL (lam (KLM (O (O FORM)))
			    (forall (lam (KL (O FORM))
			      (forall (lam (L FORM)     
				(IMPLIES                                               
				 (AND                                                  
				  (AND (KLM KL) (KL L))                                
				  (AND                                                 
				   (DERIVABLE                                          
				    (UNION EMPTY-CL (SINGLETON L))                     
				    KLM                                                
				    CRULE-SET)                                         
				   (DERIVABLE                                          
				    EMPTY-CL                                           
				    (UNION                                             
				     (SETMINUS KLM (SINGLETON KL))                     
				     (SINGLETON (SINGLETON L)))                        
				    CRULE-SET)))                                       
				 (DERIVABLE EMPTY-CL KLM CRULE-SET))))))))  ("ForallI"(nkm)(l1)))
		 )
		(proc-content schema-interpreter)
		(remark "Method to prepare the fact4-lemma for a natural induction application.")
		)


(infer~defmethod abstract
		 (outline-mappings (((existent) abstract-m-b)))  
		 (help "Close abstractable goals ind eln-technique."))

(meth~defmethod abstract-m-b abstract
		(in prop-res)
		(rating 10)
		(reasoning :planning)
		(declarations
		 (type-constants o i)
		 (sorted-meta-variables
		  (phi o term) )
		 )
		(parameters )
		(premises   )
		(application-condition
		 (eln-abstractable phi))  
		(outline-actions    )
		(outline-orderings  )
		(outline-computations
		 )
		(conclusions (- ltp) )
		(decl-content
		 
		 (ltp () phi ("Open"()()))

		 )
		(proc-content schema-interpreter)
		(remark "Close abstractable goal while applying eln-technique.")
		)



(infer~defmethod eln-ind-sc-k1
		 (outline-mappings (((existent existent nonexistent nonexistent
					       nonexistent ;; nonexistent
					       )
				     eln-ind-sc-k1-m-b)))
		 (help "Methode zur Nutzung der Menge K1"))


(meth~defmethod eln-ind-sc-k1-m-b eln-ind-sc-k1  
		(in prop-res) 
		(rating 30)
		(reasoning :planning)
		(declarations
		 (type-variables rr)
		 (type-constants o i)
		 (constants (skl (o form))
			    (sli form))
		 (sorted-meta-variables 
		  (vli form         const)
		  (vkl (o form)     term)
		  (vkm (o (o form)) term)
		  (md   o            term)
		  )
		 )
		(parameters
		 ) 
		(premises (- l100) (+ l3) (+ l0) (+ l11) ;; (+ f4h) (+ f41)
			  )
		(application-condition
		 )  
		(outline-actions
		 (l16 (sponsor l6))
		 (l16 (sponsor l15))
		 )
		(outline-orderings
		 (before  l3 l0)
		 (before l0 l11)
		 (before l11 f4h)
		 (before f4h f41)
		 )
		(outline-computations
		 (vli (type-newconst (termtype sli)))
		 (vkl (type-newconst (termtype skl)))
		 ;(klmx (type-newvar (:type (o (o form)))))
		 ;(gklx (type-newvar (:type (o form))))
		 ;; (gklx1 (type-newvar (:type (o form))))
		 ;; (md  (eln=set-globals vkl vli))
		 )
		(conclusions (- ltp))
		(decl-content

		 (l100 () (not (vkm empty-cl))             )
		 
		 
                 (l0  () (forall (lam (klmx (o (o form))) (forall (lam (gklx (o form))
                           (implies
                            (and (in klmx all-clause-sets)
                                 (exists (lam  (kl1 (o form)) (and (klmx kl1) (card>2 kl1)))))
                            (exists (lam (kl (o form)) (exists (lam (l form)
                                (and (and (implies
                                           (derivable
                                            gklx
                                            (union (setminus klmx (singleton kl))
                                                   (singleton (setminus kl (singleton l))))
                                            crule-set)
                                           (or (derivable gklx klmx crule-set)
                                               (derivable (union gklx (singleton l))
                                                          klmx crule-set)))
                                          (kl l))
                                     (and (klmx kl)
                                          (card>2 kl))))))))))))             ("Open"()()))

                                 
                 (l1  () (forall (lam (gklx1 (o form))
                           (implies
                            (and (in vkm all-clause-sets)
                                 (exists (lam  (kl1 (o form)) (card>2 kl1))))
                            (exists (lam (kl (o form)) (exists (lam (l form)
                               (and (and (implies
                                           (derivable
                                            gklx1
                                            (union (setminus vkm (singleton kl))
                                                   (singleton (setminus kl (singleton l))))
                                            crule-set)
                                           (or (derivable gklx1 vkm crule-set)
                                               (derivable (union gklx1 (singleton l))
                                                          vkm crule-set)))
                                          (kl l))
                                     (and (vkm kl)
                                         (card>2 kl))))))))))    ("ForallE"(vkm)(l0)))
                 
                 (l2 () (implies
                         (and (in vkm all-clause-sets)
                              (exists (lam  (kl1 (o form)) (card>2 kl1))))
                         (exists (lam (kl (o form)) (exists (lam (l form)
                            (and (and (implies
                                       (derivable
                                        empty-cl
                                        (union (setminus vkm (singleton kl))
                                               (singleton (setminus kl (singleton l))))
                                        crule-set)
                                       (or (derivable empty-cl vkm crule-set)
                                           (derivable (union empty-cl (singleton l))
                                                      vkm crule-set)))
                                      (kl l))
                                 (and (vkm kl)
                                      (card>2 kl))))))))    ("ForallE"(empty-cl)(l1)))

                 (l3 () (and (in vkm all-clause-sets)
                             (exists (lam  (kl1 (o form)) (card>2 kl1))))  ("Open"()()))

                 (l4 () (exists (lam (kl (o form)) (exists (lam (l form)
                            (and (and (implies
                                       (derivable
                                        empty-cl
                                        (union (setminus vkm (singleton kl))
                                               (singleton (setminus kl (singleton l))))
                                        crule-set)
                                       (or (derivable empty-cl vkm crule-set)
                                           (derivable (union empty-cl (singleton l))
                                                      vkm crule-set)))
                                      (kl l))
                                 (and (vkm kl)
                                      (card>2 kl)))))))               ("ImpE"()(l3 l2)))
                
                 (l5 () (exists (lam (l form)
                             (and (and (implies
                                        (derivable
                                         empty-cl
                                         (union (setminus vkm (singleton vkl))
                                                (singleton (setminus vkl (singleton l))))
                                         crule-set)
                                        (or (derivable empty-cl vkm crule-set)
                                            (derivable (union empty-cl (singleton l))
                                                       vkm crule-set)))
                                       (vkl l))
                                  (and (vkm vkl)
                                       (card>2 vkl)))))                  ("Hyp"()()))            
                
                 (l6 () (and (and (implies
                                   (derivable
                                    empty-cl
                                    (union (setminus vkm (singleton vkl))
                                           (singleton (setminus vkl (singleton vli))))
                                    crule-set)
                                   (or (derivable empty-cl vkm crule-set)
                                       (derivable (union empty-cl (singleton vli))
                                                  vkm crule-set)))
                                  (vkl vli))
                             (and (vkm vkl)
                                  (card>2 vkl)))                         ("Hyp" () ()))

                 (l7 () (and (implies
                              (derivable
                               empty-cl
                               (union (setminus vkm (singleton vkl))
                                      (singleton (setminus vkl (singleton vli))))
                               crule-set)
                              (or (derivable empty-cl vkm crule-set)
                                  (derivable (union empty-cl (singleton vli))
                                             vkm crule-set)))
                             (vkl vli))                                         ("AndEL" ()(l6)))

                 (l8 ()  (and (vkm vkl)
                              (card>2 vkl))                                     ("AndER" ()(l6)))

                 (l9 () (implies
                         (derivable
                          empty-cl
                          (union (setminus vkm (singleton vkl))
                                 (singleton (setminus vkl (singleton vli))))
                          crule-set)
                         (or (derivable empty-cl vkm crule-set)
                             (derivable (union empty-cl (singleton vli))
                                        vkm crule-set)))                         ("AndEL" () (l7)))

                 (l10 () (vkl vli)                                               ("AndER" () (l7)))

                 (l11 () (derivable empty-cl
                                    (union (setminus vkm (singleton vkl))
                                           (singleton (setminus vkl (singleton vli))))
                                    crule-set)                                 ("Open"()()))


                 
                 (l12 () (or  (derivable empty-cl vkm crule-set)
                              (derivable (union empty-cl (singleton vli))
                                         vkm crule-set))                 ("ImpE"()(l9 l11)))

                 
                 (l13 () (derivable empty-cl vkm crule-set)                   ("Hyp"()()))

                 (l14 (l13 l5 l6) (derivable empty-cl vkm crule-set)         ("Weaken"()(l13)))
                 
                 (l15 () (derivable (union empty-cl (singleton vli))
                                     vkm crule-set)                             ("Hyp" () ()))

		 (f41 () (forall (lam (klm (o (o form))) (forall (lam (kl (o form))
			     (forall (lam (l form)
			       (implies
				(and
				 (and (klm kl) (kl l))
				 (and (derivable
				       (union empty-cl (singleton l))     
				       klm  
				       crule-set)
				      (derivable
				       empty-cl
				       (union (setminus klm (singleton kl))
					      (singleton (singleton l)))
				       crule-set)))
				 (derivable empty-cl klm crule-set))))))))        ("Open"()()))

		 (f42 () (forall (lam (kl (o form))
			     (forall (lam (l form)
			       (implies
				(and
				 (and (vkm kl) (kl l))
				 (and (derivable
				       (union empty-cl (singleton l))     
				       vkm  
				       crule-set)
				      (derivable
				       empty-cl
				       (union (setminus vkm (singleton kl))
					      (singleton (singleton l)))
				       crule-set)))
				 (derivable empty-cl vkm crule-set))))))    ("ForallE" (vkm) (f41)))

		 (f43 () (forall (lam (l form)
			    (implies
				(and
				 (and (vkm vkl) (vkl l))
				 (and (derivable
				       (union empty-cl (singleton l))     
				       vkm  
				       crule-set)
				      (derivable
				       empty-cl
				       (union (setminus vkm (singleton vkl))
					      (singleton (singleton l)))
				       crule-set)))
				 (derivable empty-cl vkm crule-set))))    ("ForallE" (vkl)(f42)))

		 (f44 () (implies
			  (and
			   (and (vkm vkl) (vkl vli))
			   (and (derivable
				 (union empty-cl (singleton vli))     
				 vkm  
				 crule-set)
				(derivable
				 empty-cl
				 (union (setminus vkm (singleton vkl))
					(singleton (singleton vli)))
				 crule-set)))
			  (derivable empty-cl vkm crule-set))          ("ForallE"(vli) (f43))) 


		 (f4h () (and (and (vkm vkl) (vkl vli))
			      (and (derivable (union empty-cl (singleton vli))     
					      vkm  
					      crule-set)
				   (derivable empty-cl
					      (union (setminus vkm (singleton vkl))
						     (singleton (singleton vli)))
					      crule-set)))                        ("Open"()()))
			
		 ;; ---------------------------------------------------------
		 
		 (l16 (l6 l5 l15 f4h) (derivable empty-cl vkm crule-set)    ("ImpE"()(f4h f44)))

                 (l17 (l5 l6)  (derivable empty-cl vkm crule-set)      ("OrE" ()(l14 l16 l12)))

                 (l18  (l5)    (derivable empty-cl vkm crule-set)   ("ExistsE" (vli) (l5 l17)))
		 ;; --------------------------------------------------------------------

		 (ltp () (derivable empty-cl vkm crule-set)          ("ExistsE" (vkl)(l4 l18)))
		 )
		(proc-content schema-interpreter)
		(remark "Proof the step case of an eln-induction-step with help of k1 and k2.")
		)
