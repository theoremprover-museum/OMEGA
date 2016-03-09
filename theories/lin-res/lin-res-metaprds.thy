;;
;; Metapraedikate (Theorie prop-res)
;;



(in-package "OMEGA")

(defun open-lines (arg)
  (format t "OPEN LINES RUNNING ~%")
   (list (list (cons arg 
		     (first (pds~open-nodes omega*current-proof-plan))))))


(defun applied-method (method)
  (let* ((next-node  (when cri*current-task (agenda~task-node cri*current-task)))
	 (reason     (when next-node (car (pdsj~reasons (node~justification next-node)))))
	 (applied-method (when reason (just~method (pdsc~an-just reason))))
	 )
    (format t "Angewandte Methode: ~A~%" (keim~name applied-method))
    (when next-node
      (when (string-equal (keim~name applied-method)
		 method)
	(list (list (cons T T)))))))

(defun applied-method-or (method)
  (let* ((next-node  (when cri*current-task (agenda~task-node cri*current-task)))
	 (reason     (when next-node (car (pdsj~reasons (node~justification next-node)))))
	 (applied-method (when reason (just~method (pdsc~an-just reason)))) )
    (format t "Angewandte Methode: ~A~%" (keim~name applied-method))
    (format t "first-task: ~A~%" next-node)
    (when next-node
      (when (some #'(lambda (x) (eql (keim~name applied-method)
                                           x))
                        method)
              (list (list (cons T T)))))))



(defun der-item-info-ac ()
  ;; Diese Bedingung besagt, dass zur Anwendung des Axiom der-item-info
  ;; der Ausdruck (der-item-info (der-item-constr x y z)) enthalten sein muss
  (let* ((openform  (when cri*current-task (node~formula (agenda~task-node cri*current-task))))
	 (act-goal  (when cri*current-task (agenda~task-node cri*current-task)))
	 (fun       (post~read-object '((dii1 crule) (dii2 (o form)) (dii3 glist) (dii4 glist)
					(dii5 (o form))(dii6 (o form))  ) 
				      (pds~environment omega*current-proof-plan)
				      :variables))
	 (pattern   (post~read-object  '(= (der-item-info (der-item-constr dii1 dii2 dii3 dii4))
					   dii5)
				      (pds~environment omega*current-proof-plan)
				      :existing-term))
	 (goal-subst (when openform (multiple-value-bind (success subst)
					(term~alpha-match pattern openform
                                                         :additional-bind
                                                         (data~free-variables pattern))
				      (when success subst))))
	 (hyp-pat  (post~read-object  '(= (der-item-info (der-item-constr dii1 dii2 dii3 dii4))
					   dii6)
				      (pds~environment omega*current-proof-plan)
				      :existing-term)) 
	 (not-yet-applied (not (formula-is-assumption-of-goal hyp-pat act-goal)))
	 )
    (when (and goal-subst not-yet-applied) t)
    ))	


(defun get_der-item-info (arg)
  (list (list (cons arg 'der-item-info))))

(defun der-item-crule-ac ()
  ;; Diese Bedingung besagt, dass zur Anwendung des Axiom der-item-info
  ;; der Ausdruck (der-item-info (der-item-constr x y z)) enthalten sein muss
  (let* ((openform  (when cri*current-task (node~formula (agenda~task-node cri*current-task))))
	 (act-goal  (when cri*current-task (agenda~task-node cri*current-task)))
	 (fun       (post~read-object '((diij1 crule) ) 
				      (pds~environment omega*current-proof-plan)
				      :variables))
	 (pattern   (post~read-object  '(crule-set (der-item-crule (der-item-constr dii1 dii2 dii3 dii4)))
				       (pds~environment omega*current-proof-plan)
				      :existing-term))
	 (goal-subst (when openform (multiple-value-bind (success subst)
					(term~alpha-match pattern openform
                                                          :additional-bind
                                                         (data~free-variables pattern))
				      (when success subst))))
	 (hyp-pat  (post~read-object  '(= (der-item-crule (der-item-constr dii1 dii2 dii3 dii4))
					   dii1)
				      (pds~environment omega*current-proof-plan)
				      :existing-term)) 
	 (not-yet-applied (not (formula-is-assumption-of-goal hyp-pat act-goal)))
	 )
    (when (and goal-subst not-yet-applied) t)
    ))


(defun get_der-item-crule (arg)
  (list (list (cons arg 'der-item-crule))))


(defun formula-is-assumption-of-goal (form goal)
  ;; Bestimmt ob eine Formel Annahme fuer ein Ziel ist und gibt, falls ja die entstehende
  ;; Substituion zurueck
  (let* (;; (goal      (agenda~task-node cri*current-task))
	 (ass-list0  (pds~node-supports goal))
	 (ass-list1  (pdsn~hyps goal))
	 (ass-list   (append ass-list0 ass-list1))
	 (ass-forms (mapcar #'(lambda (x) (node~formula x))
			    ass-list))
	 (res (mapcan #'(lambda (x) (multiple-value-bind (success subst)
			      (term~alpha-match form x)
			    (when success (list subst))))
	    ass-forms)))
    (when res (car res))))


(defun not-base-case ()
  (let* ((openform (when cri*current-task (node~formula (agenda~task-node cri*current-task))))
	 (openline (when cri*current-task (agenda~task-node cri*current-task)))
	 (fun       (post~read-object '((sc2-numvar num) )
				      (pds~environment omega*current-proof-plan)
				      :variables))
	 (pattern   (post~read-object '(or (= sc2-numvar zero) (= sc2-numvar (s zero)))
				      (pds~environment omega*current-proof-plan)
				      :existing-term)))
    (if (formula-is-assumption-of-goal pattern openline)
	nil
      t)
    ;;(when (formula-is-assumption-of-goal pattern openline)
    ;;  (format t "Test gelungen~%"))
    ;;(error "Testen")
    ))


(defun not-step-case ()
  (let* ((openform (when cri*current-task (node~formula (agenda~task-node cri*current-task))))
	 (openline (when cri*current-task (agenda~task-node cri*current-task)))
	 (fun       (post~read-object '((sc1-numvar num) (some-form o) )
				      (pds~environment omega*current-proof-plan)
				      :variables))
	 (pattern   (post~read-object '(and (greater sc1-numvar (s zero))
					    some-form)
				      (pds~environment omega*current-proof-plan)
				      :existing-term)))
    (if (formula-is-assumption-of-goal pattern openline)
	nil
      t)
    ;;(when (formula-is-assumption-of-goal pattern openline)
    ;;  (format t "Test gelungen~%"))
    ;;(error "Testen")
    ))


(defun get_exl-nr (arg1)
  (list (list (cons arg1 (post~read-object 'exl-nr
					   (pds~environment omega*current-proof-plan)
					   :existing-term)))))

(defun get_two (arg1)
  (list (list (cons arg1 (post~read-object '(s (s zero))
					   (pds~environment omega*current-proof-plan)
					   :existing-term)))))

(defun get_one (arg1)
  (list (list (cons arg1 (post~read-object '(s zero)
					   (pds~environment omega*current-proof-plan)
					   :existing-term)))))

(defun clauseset-is-quantified ()
  (let* ((quant-node (first (pds~open-nodes omega*current-proof-plan)))
	 (formula    (node~formula quant-node))
	 (quantor    (when (data~appl-p formula) (data~appl-function formula)))
	 (args       (when (data~appl-p formula) (data~appl-arguments formula)))
	 (quant-var  (when (data~abstr-p (first args)) (first (data~abstr-domain (first args)))))
	 (forallq    (when quantor
		       (term~alpha-equal (data~constant-origin quantor)
					 (post~read-object 'forall
							   (pds~environment omega*current-proof-plan)
							   :existing-term))))
	 (nyform     (env~lookup-object 'form (th~env omega*current-theory)))
	 (ooformt    (type~func-create  (list (type~func-create (list nyform) type*o))
					type*o))
	 (res        (and forallq
			  (type~alpha-equal ooformt (data~annotation quant-var)))))
    res))

(defun glist-is-quantified ()
  (let* ((quant-node (agenda~task-node cri*current-task))
	 ;; (quant-node (first (pds~open-nodes omega*current-proof-plan)))
	 (formula    (node~formula quant-node))
	 (quantor    (when (data~appl-p formula) (data~appl-function formula)))
	 (args       (when (data~appl-p formula) (data~appl-arguments formula)))
	 (quant-var  (when (data~abstr-p (first args)) (first (data~abstr-domain (first args)))))
	 (forallq    (when quantor
		       (term~alpha-equal (data~constant-origin quantor)
					 (post~read-object 'forall
							   (pds~environment omega*current-proof-plan)
							   :existing-term))))
	 (nyglist    (env~lookup-object 'glist (th~env omega*current-theory)))
	 (res        (and forallq
			  (type~alpha-equal nyglist (data~annotation quant-var)))))
    res))



(defun case-split-ac()
  (let* ((last-step  (pds~last-plan-step cri*current-pds))
	 (quant-node (pdsc~an-node last-step))
	 (formula    (node~formula quant-node))
	 (form       (node~formula (agenda~task-node cri*current-task)))
	 (oimp       (env~lookup-object 'implies (th~env omega*current-theory)))
	 (quant-var  (when (and (data~appl-p formula)
				(data~abstr-p (first (data~appl-arguments formula))))
		       (first (data~abstr-domain (first (data~appl-arguments formula))))))
	 (nyform     (env~lookup-object 'form (th~env omega*current-theory)))
	 (to-compare (type~func-create  (list (type~func-create (list nyform) type*o))
					type*o)))
    (when (and quant-var to-compare)
      (and (type~alpha-equal to-compare (data~annotation quant-var))
	   (data~equal oimp (data~appl-function form))))))

(defun get_box-in-S (arg)
  (let* ((last-step (pds~last-plan-step cri*current-pds))
	 (quant-node (pdsc~an-node last-step))
	 (formula   (node~formula quant-node))
	 (quant-var (first (data~abstr-domain (first (data~appl-arguments formula)))))
	 (firstpos  (car (data~substruct-positions quant-var
					   (data~abstr-range (first (data~appl-arguments formula))))))
	 (openform  (node~formula (agenda~task-node cri*current-task)))
	 ;;(openform  (node~formula (car (last (pds~open-nodes omega*current-proof-plan)))))
	 (searched  (data~struct-at-position openform firstpos))
	 (envecl    (env~lookup-object 'empty-cl (th~env omega*current-theory)))
	 (paravalue (term~appl-create searched (list envecl))))
    (list (list (cons arg paravalue)))))



(defun is-comp-step-ac ()
  ;; Diese Bedingung besagt, dass zur Anwendung des Axiom is-component-step
  ;; der entsprechende Ausdruck enthalten sein muss
  (let* ((openform  (when cri*current-task (node~formula (agenda~task-node cri*current-task))))
	 (act-goal  (when cri*current-task (agenda~task-node cri*current-task)))
	 (fun       (post~read-object '((diii1 item) (diii2 item) (diii4 glist)) 
				      (pds~environment omega*current-proof-plan)
				      :variables))
	 (pattern   (post~read-object  '(is-component diii1 (cons diii2 diii4))
				      (pds~environment omega*current-proof-plan)
				      :existing-term))
	 (goal-subst (when openform
		       (hth~subterm-matcher pattern openform
						 (data~free-variables pattern))))
	 )
    (when goal-subst t)
    ;; (error "NUR Zum TRACEN")
    ))



(defun get_is-component-step (arg)
  (list (list (cons arg 'is-component-step))))

(defun is-comp-base-ac ()
  ;; Diese Bedingung besagt, dass zur Anwendung des Axiom is-component-step
  ;; der entsprechende Ausdruck enthalten sein muss
  (let* ((openform  (when cri*current-task (node~formula (agenda~task-node cri*current-task))))
	 (act-goal  (when cri*current-task (agenda~task-node cri*current-task)))
	 (fun       (post~read-object '((div1 item)) 
				      (pds~environment omega*current-proof-plan)
				      :variables))
	 (pattern   (post~read-object  '(is-component div1 NULL)
				      (pds~environment omega*current-proof-plan)
				      :existing-term))
	 (goal-subst (when openform
		       (hth~subterm-matcher pattern openform
						 (data~free-variables pattern))))
	 )
    (when goal-subst t)
    ;; (error "NUR Zum TRACEN")
    ))


(defun get_is-component-base (arg)
  (list (list (cons arg 'is-component-base))))


(defun free-derivation-ac ()
  ;; Diese Bedingung besagt, dass zur Anwendung des Axiom is-component-step
  ;; der entsprechende Ausdruck enthalten sein muss
  (let* ((openform  (when cri*current-task (node~formula (agenda~task-node cri*current-task))))
	 (act-goal  (when cri*current-task (agenda~task-node cri*current-task)))
	 (fun       (post~read-object '((dv1 glist) (dv2 glist) (dv3 o)) 
				      (pds~environment omega*current-proof-plan)
				      :variables))
	 (pattern   (post~read-object  '(free-derivation-cond dv1)
				      (pds~environment omega*current-proof-plan)
				      :existing-term))
	 (goal-subst (when openform
		       (multiple-value-bind (success subst)
			   (term~alpha-match pattern openform
                                             :additional-bind
                                             (data~free-variables pattern))
			 (when success subst))))
	 (hyp-pat  (post~read-object  '(= (free-derivation-cond dv2)
					  dv3)
				      (pds~environment omega*current-proof-plan)
				      :existing-term)) 
	 (not-yet-applied (not (formula-is-assumption-of-goal hyp-pat act-goal)))
	 )
    (if (and goal-subst not-yet-applied)
	(format t "free-derivation-condition valid")
      (format t "free-derivation-condition NOT valid"))
   ;; (error "Testzwecke") 
    (when (and goal-subst not-yet-applied) t)
    ))


(defun get_free-derivation-condition (arg)
  (list (list (cons arg 'free-derivation-condition))))

(defun crule-aplcble-ac ()
  ;; Diese Bedingung besagt, dass zur Anwendung des Axiom is-component-step
  ;; der entsprechende Ausdruck enthalten sein muss
  (let* ((openform  (when cri*current-task (node~formula (agenda~task-node cri*current-task))))
	 (act-goal  (when cri*current-task (agenda~task-node cri*current-task)))
	 (fun       (post~read-object '((dvii1 glist) (dvii2 (o (o form))) (dvii3 o)(dvii4
										     item)
					(dvii5 glist)) 
				      (pds~environment omega*current-proof-plan)
				      :variables))
	 (pattern   (post~read-object  '(crule-applicable res-res (cons dvii4 dvii5) dvii2)
				      (pds~environment omega*current-proof-plan)
				      :existing-term))
	 (goal-subst (when openform
		       (multiple-value-bind (success subst)
			   (term~alpha-match pattern openform
                                             :additional-bind
                                             (data~free-variables pattern))
			 (when success subst))))
	 (hyp-pat  (post~read-object  '(= (crule-applicable res-res (cons dvii4 dvii5) dvii2)
					  dvii3)
				      (pds~environment omega*current-proof-plan)
				      :existing-term)) 
	 (not-yet-applied (not (formula-is-assumption-of-goal hyp-pat act-goal)))
	 )
    (if not-yet-applied
        (format t "not-yet-appl Bedingung ist erfuellt.")
      (format t "Fehler ist aufgetreten"))
    (if goal-subst
        (format t "goal-subst Bedingung ist erfuellt.")
      (format t "Fehler ist aufgetreten"))
    ;;(error "Zum Testen")
    (when (and goal-subst not-yet-applied) t)
    ))


(defun get_res-applicable (arg)
  (list (list (cons arg 'res-applicable))))


(defun get_fact4 (arg)
  (list (list (cons arg 'fact4))))

(defun get_fact1 (arg)
  (list (list (cons arg 'fact1))))

(defun fact1-ac ()
  ;; Diese Bedingung besagt, dass zur Anwendung des Fact1 entweder die exl-nr=0 sein muss
  ;; oder die leere klausel in klm enthalten sein muss
  (let* ((openform  (when cri*current-task (node~formula (agenda~task-node cri*current-task))))
	 (act-goal  (when cri*current-task (agenda~task-node cri*current-task)))
	 (fun       (post~read-object '((klmvar (o (o form))) (klvar (o form)) (hvar1 num)) 
				      (pds~environment omega*current-proof-plan)
				      :variables))
	 (klmvar    (env~lookup-object 'klmvar (th~env omega*current-theory)))
	 (klvar    (env~lookup-object 'klvar (th~env omega*current-theory)))
	 (hvar1    (env~lookup-object 'hvar1 (th~env omega*current-theory)))
	 (pattern   (post~read-object '(derivable klvar klmvar crule-set)
				      (pds~environment omega*current-proof-plan)
				      :existing-term))
	 (goal-subst (when openform (multiple-value-bind (success subst)
					(term~alpha-match pattern openform :additional-bind (list klmvar klvar))
				      (when success subst))))
	 (ass-pat1  (when goal-subst  (subst~apply goal-subst
						   (post~read-object '(= (exl-nr klmvar) zero)
								     (pds~environment omega*current-proof-plan)
								     :existing-term))))
;         (ass-pat3  (when goal-subst  (subst~apply goal-subst
;                                                   (post~read-object '(in klvar klmvar)
;                                                                     (pds~environment omega*current-proof-plan)
;                                                                     :existing-term))))
	 (ass-pat4  (when goal-subst (subst~apply goal-subst
						  (post~read-object '(klmvar klvar)
								    (pds~environment omega*current-proof-plan)
								    :existing-term))))
	 )
    (when goal-subst
      (or (formula-is-assumption-of-goal ass-pat4 act-goal)
	  (formula-is-assumption-of-goal ass-pat1 act-goal)
	   )
    ;;(error "NUR Zum TRACEN")
    )))

(defun get_fact2 (arg)
  (list (list (cons arg 'fact2))))

(defun fact2-ac ()
  ;; Diese Bedingung besagt, dass zur Anwendung des Fact2 entweder die exl-nr=1 sein muss
  ;; oder all klauslen unit sind und  klm widerspruechlich sein muss
  ;; Zunaechst wird nur der erste Teil der Bedingung implemmentiert
  (let* ((openform  (when cri*current-task (node~formula (agenda~task-node cri*current-task))))
	 (act-goal  (when cri*current-task (agenda~task-node cri*current-task)))
	 (fun       (post~read-object '((klmvar (o (o form))) (klvar (o form)) (hvar1 num)) 
				      (pds~environment omega*current-proof-plan)
				      :variables))
	 (klmvar   (env~lookup-object 'klmvar (th~env omega*current-theory)))
	 (klvar    (env~lookup-object 'klvar (th~env omega*current-theory)))
	 (hvar1    (env~lookup-object 'hvar1 (th~env omega*current-theory)))
	 (pattern   (post~read-object '(derivable klvar klmvar crule-set)
				      (pds~environment omega*current-proof-plan)
				      :existing-term))
	 (goal-subst (when openform (multiple-value-bind (success subst)
					(term~alpha-match pattern openform :additional-bind (list klmvar klvar))
				      (when success subst))))
	 (ass-pat1  (when goal-subst  (subst~apply goal-subst
						   (post~read-object '(= (exl-nr klmvar) hvar1)
								     (pds~environment omega*current-proof-plan)
								     :existing-term))))
	 (ass-pat2  (when goal-subst   (subst~apply goal-subst
						    (post~read-object '(= hvar1 (s zero))
								      (pds~environment omega*current-proof-plan)
								      :existing-term))))
	 )
    (when goal-subst
      (and (formula-is-assumption-of-goal ass-pat1 act-goal)
	   (formula-is-assumption-of-goal ass-pat2 act-goal)))
    ;;(error "NUR Zum TRACEN")
    ))

(defun dsjl-ac ()
  (let* ((openform (when cri*current-task (node~formula (agenda~task-node cri*current-task))))
	 (openline (when cri*current-task (agenda~task-node cri*current-task)))
	 (hyps     (when openline (pdsn~hyps openline)))
	 (hypforms (when hyps (mapcar #'(lambda (x) (node~formula x)) hyps)))
	 (fun       (post~read-object '((dac-klmvar (o (o form))) (dac-numvar num)) 
				      (pds~environment omega*current-proof-plan)
				      :variables))
	 ;;(dac-klmvar (env~lookup-object 'dac-klmvar (th~env omega*current-theory)))   
	 ;;(dac-numvar (env~lookup-object 'dac-numvar (th~env omega*current-theory)))
	  ;; goalpat ist: (derivable empty-cl klmvar crule-set)
	 (goalpat   (post~read-object '(derivable empty-cl dac-klmvar crule-set)
				      (pds~environment omega*current-proof-plan)
				      :existing-term))
	 (cond1     (when openform (multiple-value-bind (success subst)
				       (term~alpha-match goalpat openform
							 :additional-bind (list dac-klmvar))
				     (when success subst))))
	 ;; notinpat1 ist: (not (klmvar empty-cl))
	 (notinpat1 (when cond1
			  (subst~apply cond1
				       (post~read-object '(not (dac-klmvar empty-cl))
							 (pds~environment omega*current-proof-plan)
							 :existing-term) )))
	 (cond2a    (when notinpat1 (formula-is-assumption-of-goal notinpat1 openline))) 
	 (notinpat2 (when cond1
		      (subst~apply cond1
				   (post~read-object '(not (in empty-cl dac-klmvar))
						     (pds~environment omega*current-proof-plan)
						     :existing-term) )))
	 (cond2b    (when notinpat2 (formula-is-assumption-of-goal notinpat2 openline)))
	 (greater1  (when cond1
		      (subst~apply cond1
				   (post~read-object '(greater dac-numvar (s zero))
						     (pds~environment omega*current-proof-plan)
						     :existing-term) )))
	 (cond3a    (when greater1  (formula-is-assumption-of-goal greater1 openline)))
	 (greater2  (when cond1
		      (subst~apply cond1
				   (post~read-object '(not (leq dac-numvar (s zero)))
						     (pds~environment omega*current-proof-plan)
						     :existing-term) )))
	 (cond3b    (when greater2 (formula-is-assumption-of-goal greater2 openline)))
	 (substa    (when cond3a (subst~compose-substitution cond1 cond3a)))
	 (substb    (when cond3b (subst~compose-substitution cond1 cond3b)))
	 (test      (format t "AUSDRUCK: ~%" ))
	 (exl=pat1  (when substa (subst~apply
                                  substa
				  (post~read-object '(= (exl-nr dac-klmvar)  dac-numvar)
                                                    (pds~environment omega*current-proof-plan)
                                                    :existing-term) )))
         (exl=pat2  (when substb (subst~apply
                                  substb
                                  (post~read-object '(= (exl-nr dac-klmvar) dac-numvar)
                                                    (pds~environment omega*current-proof-plan)
                                                    :existing-term))))
         (cond4a    (when exl=pat1 (formula-is-assumption-of-goal exl=pat1 openline)))
         (cond4b    (when exl=pat2 (formula-is-assumption-of-goal exl=pat2 openline)))
	 )
    ;; (format t "COND1 ist: ~A~% COND2a ist: ~A~%  COND2b ist: ~A~%  COND3a ist: ~A~%  COND3b ist: ~A~%  COND4a ist: ~A~%  COND4b ist: ~A~%  " cond1 cond2a cond2b cond3a cond3b cond4a cond4b)
    ;; (when (and cond1 (or cond2a cond2b) (or cond3a cond3b) (or cond4a cond4b)) (format t "ergebnis erreicht~%"))
     (and cond1 (or cond2a cond2b) (or cond3a cond3b) (or cond4a cond4b))
    ;; (error "Zur Sicherheit")
    ))

(defun get_fact3 (arg)
  (list (list (cons arg 'fact3)))) 

(defun step-case ()
  (let* ((openform (when cri*current-task (node~formula (agenda~task-node cri*current-task))))
	 (openline (when cri*current-task (agenda~task-node cri*current-task)))
	 (fun       (post~read-object '((sc-numvar num) )
				      (pds~environment omega*current-proof-plan)
				      :variables))
	 (pattern   (post~read-object '(greater sc-numvar (s zero))
				      (pds~environment omega*current-proof-plan)
				      :existing-term)))
    (formula-is-assumption-of-goal pattern openline)
    ;;(when (formula-is-assumption-of-goal pattern openline)
    ;;  (format t "Test gelungen~%"))
    ;;(error "Testen")
    ))


(defun get_dip (arg)
  (let ((paravalue (post~read-object '((lam (seq glist) (minus
							 (glist-length seq) one)))
					   (pds~environment omega*current-proof-plan)
					   :existing-term)))
    (list (list  (cons arg paravalue)))))


(defun next-agenda-task (arg)
  (list (list (cons arg
		    (agenda~task-node (first (agenda~next-tasks (pds~agenda omega*current-proof-plan))))))))




;;(trace BOX-IN-S-CASE every-x-unit METH=DEFCOND-ELN-ABSTRACTABLE formula-is-assumption-of-goal)
;
;(meth~defcond phi-is-ok (args tmapp)
;   (let* ((p    (first args))
;          (l    (second args))
;          (u    (third args))
;          (x    (fourth args))
;          (disj (compose-disjunction x l u))
;          (il   (int2num l))
;          (iu   (int2num u))
;          (res  (and (term~alpha-equal phi disj)
;                     (< il ui)))
;          )
;     (if res tmapp
;       (meth~tm-new-value tmapp nil))))
;
;
;
;
;(meth~deffun position-of-in (a phi)
;             (car (data~substruct-positions a phi)))

;; =======================================================================================
;;  C O N D F U N C S  FUER DIE  METHODE    A P P L Y   T H M    B Y    N A M E 
;; =======================================================================================

;
;(meth~deffun utbn-thm-sisucc ()
;             geb*utbn-succ)
;
;(meth~deffun utbn-thm-siante ()
;              geb*utbn-ante)
;



;; =====================================================================
;;
;;                 D I S J U N K T I O N S L E M M A 
;;
;; =====================================================================
;
;(meth~deffun list-of-all-subquatifications (formula para)
;             (let* ((paratyp (data~n-normalize (data~annotation (data~appl-function  para))))
;                    (arity   (- (length (data~abstr-domain paratyp)) 1)))
;               (format t "paratyp ist ~A~%" paratyp)
;               (format t "Para is~A~%" para)
;               (if (subquants arity formula)
;                   (subquants arity formula)
;                 (env~lookup-object 'zero (th~env omega*current-theory)))))


(defun subquants (arity form)
  (if (= arity 0)
      nil
    (cons (data~appl-function form)
	  (subquants (- arity 1) (data~abstr-range (first (data~appl-arguments form)))))))

(meth~deffun induced-formula (formula para)
  (declare (edited  "03-NOV-1999")
	   (authors Gebhard)
	   (input   )
	   (effect  )
	   (value   ))
	     (let* ((paratyp (data~n-normalize (data~annotation para)))
		    (arity   (- (length (data~abstr-domain paratyp)) 1)))
	       (ind-form arity formula)))


(meth~deffun forallclosure (para vars)
  (declare (edited  "03-NOV-1999")
	   (authors Gebhard)
	   (input   )
	   (effect  )
	   (value   ))
  (let* ((eex  (env~lookup-object 'exists (th~env omega*current-theory)))
		    (efor (env~lookup-object 'forall (th~env omega*current-theory)))
		    (eand (env~lookup-object 'and (th~env omega*current-theory)))
		    (e=   (env~lookup-object '= (th~env omega*current-theory)))
		    (enat (env~lookup-object 'nat (th~env omega*current-theory)))
		    (enum (env~lookup-object 'num (th~env omega*current-theory)))
		    (envar (term~variable-create 'n enum))
		    (inner0 (term~appl-create para vars))
		    (inner1a (term~appl-create e=
					      (list inner0 envar)))
		    (inner1b (term~appl-create enat (list envar)))
		    (inner1 (term~appl-create eand (list inner1b inner1a)))
		    (inner2 (term~abstr-create (list envar)
					       inner1))
		    (inner3 (term~appl-create eex (list inner2))))
	       (do-closure-of inner3 vars efor)))


(defun do-closure-of (form vars quant)
  (if vars
      (do-closure-of
       (term~appl-create quant
			 (list (term~abstr-create (last vars) form)))
       (butlast vars)
       quant)
    form))
	       
	     

(meth~deffun phi+ass (formula para var-consts anum)
  (declare (edited  "03-NOV-1999")
	   (authors Gebhard)
	   (input   )
	   (effect  )
	   (value   ))
	     (let* ((eimp (env~lookup-object 'implies (th~env omega*current-theory)))
		    (e=   (env~lookup-object '= (th~env omega*current-theory))))
	       (term~appl-create
		eimp
		(list
		 (term~appl-create e=
				   (list
				    (term~appl-create para var-consts)
				    anum))
		 formula))))







;; ======================================
;; Universelle Metapreadikate
;; ======================================

(defun open-lines (arg)
  (format t "OPEN LINES RUNNING ~%")
   (list (list (cons arg 
		     (first (pds~open-nodes omega*current-proof-plan))))))







(defun last-method-or (method)  
  (let* ((last-step (pds~last-plan-step cri*current-pds)))
    (when last-step
      (let* ((just (pdsc~an-just last-step))
             (infer (just~method just))
             )
        (when (or (infer~method-p infer)
                  (infer~supermethod-p infer))
          (let* ((node (pdsc~an-node last-step))
                 (outln-pat (cri=actual-outline-pattern node
                                                        cri*current-pds
                                                        just))
                 (last-method (pds~inference-application infer outln-pat))
                 )
            (when (some #'(lambda (x) (eql (keim~name last-method)
                                           x))
                        method)
              (list (list (cons T T))))))))))


;; ===================================
;; Change2Induktion Metapreadikate
;; ===================================
;;(defvar cr*geb-ch2ind-once -1)
;;
;;(defun apply-ch2ind-once ()
;;  (setf cr*geb-ch2ind-once (+ 1 cr*geb-ch2ind-once))
;;  (= cr*geb-ch2ind-once 0))



(defun good-equality (qe-sign form)
  (and (data~appl-p form)
       (data~equal (data~appl-function form)
		   qe-sign)
       (not (data~equal (first (data~appl-arguments form))
			(second (data~appl-arguments form))))
       (not (data~equal (first (data~appl-arguments form))
			(post~read-object  'empty-cl
					   (pds~environment omega*current-proof-plan)
					   :existing-term)))))
  

(defun equality-is-hyp (arg)
  (let* ((open-node (when cri*current-task (agenda~task-node cri*current-task)))
	 (act-goal  (when cri*current-task (node~formula (agenda~task-node cri*current-task))))
	 (qe-sign   (post~read-object  '= 
				       (pds~environment omega*current-proof-plan)
				       :existing-term))
	 (ass-list0  (pds~node-supports open-node))
	 (ass-list1  (pdsn~hyps open-node))
	 (ass-list   (remove-duplicates (append ass-list0 ass-list1)))
	 (eqhyps     (mapcan #'(lambda (x)
				 (when (and (good-equality qe-sign (node~formula x))
					    (hth~subterm-matcher
					     (car (data~appl-arguments (node~formula x)))
					     act-goal (data~free-variables (car (data~appl-arguments (node~formula x))))))
				   (list x)))
			     ass-list))
	 )
    (format t "HYPS SIND: ~A~%" ass-list)
    (format t "Ergebinis ist: ~A~%" eqhyps)
    (format t "ACTGOAL IST: ~A~%" act-goal)
    ;; (error "Zum Testen")
    (when eqhyps
      (list (list (cons arg (car eqhyps)))))))








;(defun crule-aplction-ac ()
;  ;; Diese Bedingung besagt, dass zur Anwendung des Axiom is-component-step
;  ;; der entsprechende Ausdruck enthalten sein muss
;  (let* ((openform  (when cri*current-task (node~formula (agenda~task-node cri*current-task))))
;         (act-goal  (when cri*current-task (agenda~task-node cri*current-task)))
;         (fun       (post~read-object '((dvii1 glist) (dvii2 (o (o form))) (dviv3 (o form))) 
;                                      (pds~environment omega*current-proof-plan)
;                                      :variables))
;         (pattern   (post~read-object  '(= empty-cl
;                                         (crule-application res-res dvii1 dvii2))
;                                      (pds~environment omega*current-proof-plan)
;                                      :existing-term))
;         (goal-subst (when openform
;                       (multiple-value-bind (success subst)
;                           (term~alpha-match pattern openform :additional-bind
;                                             (data~free-variables pattern))
;                         (when success subst))))
;         (hyp-pat  (post~read-object  '(= (crule-application res-res dvii1 dvii2)
;                                          dviv3)
;                                      (pds~environment omega*current-proof-plan)
;                                      :existing-term)) 
;         (not-yet-applied (not (formula-is-assumption-of-goal hyp-pat act-goal)))
;         )
;;    (if (and goal-subst not-yet-applied)
;;        (format t "Bedingung ist erfuellt.")
;;      (format t "Fehler ist aufgetreten"))
;;    (error "Zum Testen")
;    (when (and goal-subst not-yet-applied) t)
;    ))
;
;
;(defun get_res-application (arg)
;  (list (list (cons arg 'res-application))))

;(defun inst-appl-rule-ac ()
;  (let* ((openform  (when cri*current-task (node~formula (agenda~task-node cri*current-task))))
;         (act-goal  (when cri*current-task (agenda~task-node cri*current-task)))
;         (fun       (post~read-object '((dvivi1 o)) 
;                                      (pds~environment omega*current-proof-plan)
;                                      :variables))
;         (pattern   (post~read-object  '(exists (lam (x crule) dvivi1))
;                                       (pds~environment omega*current-proof-plan)
;                                       :existing-term))
;         (goal-subst (when openform
;                       (multiple-value-bind (success subst)
;                           (term~alpha-match pattern openform :additional-bind
;                                             (data~free-variables pattern))
;                         (when success subst))))
;           )
;    (when (and goal-subst) t)
;    ))



 




(defun first-agenda-task (arg)
  (setf tester1 (agenda~task-node cri*current-task))
  (list (list (cons arg (agenda~task-node cri*current-task)))))

;; Bessere Version
		     
(defun ind-form (arity form)
  (if (= arity 0)
      form
    (ind-form (- arity 1) (data~abstr-range (first (data~appl-arguments form))))))


(meth~deffun induced-variables (formula para)
	      (declare (edited  "03-NOV-1999")
		       (authors Gebhard)
		       (input   )
		       (effect  )
		       (value   ))
 (let* ((paratyp (data~n-normalize (data~annotation para)))
		    (arity   (- (length (data~abstr-domain paratyp)) 1)))
	       (ind-vars arity formula)))

(defun ind-vars (arity form)
  
  (if (= arity 0)
      nil
    (cons (first (data~abstr-domain (first (data~appl-arguments form))))
	  (ind-vars (- arity 1) (data~abstr-range (first (data~appl-arguments form)))))))


(meth~deffun res-form (form vars quants abstrvar)
	     (declare (edited  "03-NOV-1999")
		      (authors Gebhard)
		      (input   )
		      (effect  )
		      (value   ))
  (let* ((eforall (env~lookup-object 'forall (th~env omega*current-theory))))
	       (result=form form  vars (cons eforall quants) abstrvar)))

(defun result=form (form vars quants abstrvar)
  (if vars
      (result=form
       (term~appl-create (car (last quants))
			 (list
			  (term~abstr-create (last vars) form)))
       (butlast vars)
       (butlast quants)
       abstrvar)
    (term~abstr-create (list abstrvar) form)))
    
    
(defun dsjl-2proove&simplified ()
  (let* ((openform (when cri*current-task (node~formula (agenda~task-node cri*current-task))))
	 (openline (when cri*current-task (agenda~task-node cri*current-task)))
	 (fun       (post~read-object '((dsjgkl (o form)))
				      (pds~environment omega*current-proof-plan)
				      :variables))
	 (pattern   (post~read-object
		     '(forall (lam (klm (o (o form))) (forall (lam (gkl (o form))
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
				 (and (klm kl) (card>2 kl))))))))))))
		     (pds~environment omega*current-proof-plan)
		     :existing-term)))
    (if (term~alpha-match pattern openform)
	;;(format t "Erfolg")
      t
      nil)
    ;;(error "Fehler zum Testen")
    ))

 
