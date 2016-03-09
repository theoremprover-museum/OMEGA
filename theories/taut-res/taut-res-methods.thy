(in-package "OMEGA")


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
		(in taut-res) 
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
		 (l49 () (forall (lam (seq glist) (forall (lam (S (o (o form)))
			 (= (crule-applicable res-res seq S)
			    (and (and (taut-res-cond (input-first seq))
				      (taut-res-cond (input-second seq)))
			         (resolvable-b (input-first seq) (input-second seq)
					       (input-third seq) (input-fourth seq))))))))  ("Axiom"()()))

		 (l45 () (= (crule-applicable res-res
					      (cons (cl2item cl1)
                                                (cons (cl2item cl2)
                                                      (cons (form2item li1)
                                                            (cons (form2item li2)
                                                                  (cons (cl2item goal)
                                                                        NULL)))))
					      aset)
			      (and (and (taut-res-cond cl1)
					(taut-res-cond cl2))
				   (resolvable-b cl2 cl1 li1 li2)))  ("abstract"()()))
		 ;; Nur ueber Definitionen aus Zeile l49
		 

		 (l44 () (and (and (taut-res-cond cl1)
				   (taut-res-cond cl2))
				   (resolvable-b cl2 cl1 li1 li2))    ("ANDI"()(l43l l43)))

		 (l43l () (and (taut-res-cond cl1)
			       (taut-res-cond cl2))               ("abstract"()()))
		 
		 (l43 () (resolvable-b cl2 cl1 li1 li2)("Open"()()))
		 
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



