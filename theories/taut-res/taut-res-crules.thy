;; The control rules for planning eln-proofs

(in-package "OMEGA")


(cri~def-control-rule order-methods
		      (kind methods)
		      (if (always-true))
		      (then
		       (select (false-m-a
				Assertion2A
				resolvable-meth-m-b
				derive-meth-m-b
				derive-meth3-m-b
				derive-meth2-m-b
				SUBST=-M-A
				WEAKENM-M-A
				BACKCHAIN-M-B
				PREP-IND-M-B
				INDUCTION-M-B
				INTERVALL-PROPERTY-M-B
				APPLY-ASSUMPTION-M-B
				APPLY-ASS-M-B
				ELN-IND-BC1-M-B
				ELN-IND-BC2-M-B
				ELN-IND-BC2-1-M-B
				ELN-IND-BC2-2-M-B
				ELN-IND-BC2-3-M-B
				ELN-IND-BC2-4-M-B
				ABSTRACT-M-B
				LIST-LAST-M-B
				OREM-M-B
				dsjk-basem-b
				dsjk-base1-b
				case-split-M-B
				eln-ind-sc-K1-M-B
				DEFNI-M
				))))

(cri~def-control-rule apply-eln-ind-cond
                      (kind methods)
                      (if (and  (and (and (not-base-case)
				          (not-step-case));; (apply-ch2ind-once)
				     (clauseset-is-quantified))
                                (and (open-lines "opens")
                                     (get_exl-nr "para"))))
                      (then
                       (select ((prep-ind-m-b ("para") () ("opens") )))))

(cri~def-control-rule apply-eln-induction
                      (kind methods)
                      (if (and (and (applied-method-or (prep-ind-m-b prep-ind))
				    (open-lines "opens"))
			       (get_one "para")))
                      (then
                       (select ((induction-m-b ("para") () ("opens") )))))

(cri~def-control-rule reject-after-apply-ass
                      (kind methods)
                      (if (applied-method-or (apply-assumption apply-ass)))
                      (then
                       (reject (apply-ass-m-b
				apply-assumption-m-b
			       ))))

(cri~def-control-rule der-item-crule-appl-cond
                      (kind methods)
                      (if (and (der-item-crule-ac)
                               (and (next-agenda-task "opens")
                                    (get_der-item-crule "para"))))
                      (then
                       (select ((apply-ass-m-b ("para") () ("opens"))))))

(cri~def-control-rule der-item-info-appl-cond
                      (kind methods)
                      (if (and (der-item-info-ac)
                               (and (next-agenda-task "opens")
                                    (get_der-item-info "para"))))
                      (then
                       (select ((apply-ass-m-b ("para") () ("opens"))))))

(cri~def-control-rule is-component-step-appl-cond
                      (kind methods)
                      (if (and (is-comp-step-ac)
                               (and (next-agenda-task "opens")
                                    (get_is-component-step "para"))))
                      (then
                       (select ((apply-ass-m-b ("para") () ("opens"))))))


(cri~def-control-rule is-component-base-appl-cond
                      (kind methods)
                      (if (and (is-comp-base-ac)
                               (and (next-agenda-task "opens")
                                    (get_is-component-base "para"))))
                      (then
                       (select ((apply-ass-m-b ("para") () ("opens"))))))

(cri~def-control-rule free-derivation-appl-cond
                      (kind methods)
                      (if (and (free-derivation-ac)
                               (and (next-agenda-task "opens")
                                    (get_free-derivation-condition "para"))))
                      (then
                       (select ((apply-ass-m-b ("para") () ("opens"))))))

(cri~def-control-rule crule-appl-cond
                      (kind methods)
                      (if (and (crule-aplcble-ac)
                               (and (next-agenda-task "opens")
                                    (get_res-applicable "para"))))
                      (then
                       (select ((apply-ass-m-b ("para") () ("opens"))))))

(cri~def-control-rule fact1-appl-cond
                      (kind methods)
                      (if (and (fact1-ac)
                               (and (open-lines "opens")
                                    (get_fact1 "para"))))
                      (then
                       (select ((apply-assumption-m-b ("para") () ("opens"))))))

(cri~def-control-rule fact2-appl-cond
                      (kind methods)
                      (if (and (fact2-ac)
                               (and (open-lines "opens")
                                    (get_fact2 "para"))))
                      (then
                       (select ((apply-assumption-m-b ("para") () ("opens"))))))

(cri~def-control-rule case-split-appl-cond
                      (kind methods)
                      (if (and (and (applied-method  "ForallI*-m")
                                    (and (case-split-ac)
					 (step-case)))
                               (and (get_box-in-S "para")
                                    (open-lines "opens"))))
                      (then
                       (select ((case-split-m-b ("para") () ("opens"))))))

(cri~def-control-rule eln-k1args-ac
                      (kind methods)
                      (if (and (and (dsjl-ac)
				    (not (applied-method-or (eln-ind-sc-k1 eln-ind-sc-k1-m-b))))
			       (first-agenda-task "opens")))
		      (then
                       (select ((eln-ind-sc-k1-m-b ()()("opens"))))))

(cri~def-control-rule disjl-proof
                      (kind methods)
                      (if (and (and (dsjl-2proove&simplified)
				    (clauseset-is-quantified))
			       (open-lines "opens")))
				  ;;  (get_dip "para1"))))
                      (then
                       (select ((simp-djl-m-b () () ("opens"))))))

(cri~def-control-rule apply-dli-induction
                      (kind methods)
                      (if (and (and (applied-method-or (simp-djl simp-fact4))
				    (glist-is-quantified))
			       (and (open-lines "opens")
				    (get_dip "para1"))))
                      (then
                       (select ((prep-ind-m-b ("para1") () ("opens"))))))


;; --------------------------------------------------------------------------------

(cri~set-used-control-rules! '(order-methods
			       apply-dli-induction
			       der-item-info-appl-cond
			       der-item-crule-appl-cond
			       apply-eln-ind-cond
			       is-component-step-appl-cond
			       is-component-base-appl-cond
			       free-derivation-appl-cond
			       crule-appl-cond
			       apply-eln-induction
			       fact1-appl-cond
			       fact2-appl-cond
			       case-split-appl-cond
			       reject-after-apply-ass
			       eln-k1args-ac
			       ))









