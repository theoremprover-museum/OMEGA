#|
;; implemented meta-predicates:

;; already-applied-method-with-line (method line)
;; applied-with-last-method lines
;; assumption-matches (line term)
;; current-assumptions line
;; current-goal line
;; goal-matches (line term)
;; hyps-of-line (line hyps)
;; last-goal line
;; last-method method
;; latest-assumption line
;; matches (term term-that-matches?)
;; most-similar-subterm-in-asss-to-goal (goal ass subterm)
;; resulted-open-lines-from-last-method lines
;; second-last-goal-matches (line term)
;; sub-of-assumption (term line)
;; sub-of-latest-assumption (term line)
;; unwrapped-focus line

;; bei matches: Variablen des Typs num muessen mit n beginnen (n1 n2 na), des Typs o mit o

;; correct successor/predecessor in last ass (wenn Planer pds fuellt)

(in-package "OMEGA")

(defun last-method (method)
  ;; method is a name!
  ;; returns a method-name
  (let* ((last-alter (plan~state-method (first plan*planning-state)))
	 (last-method (when last-alter (plan~alter-method last-alter))))
    (when last-method
      (if (not (stringp method))
	  ; if the method is not a string, then it is a method name
	  (keim~equal last-method (meth~find-method method))
	   ;else bind method to the last-method -> ( ((method LAST-METHOD)) )
	(cons (cons (cons method (keim~name last-method)) ()) ())
	  )))
    )

(defun second-last-goal-matches (parameter)
  ; parameter is a list: (line term-that-matches?)
  ; value: a list of all goals that match term-that-matches?
  (let ((line (first parameter))
	(lines (plan~alter-addprems (plan~state-method (third plan*planning-state))))
	(formel (cri=build-formula (second parameter)))
	(result ()))
    (when formel
      (if (stringp line)
	  (dolist (line-to-check lines result)
	    (when (uni~match formel (node~formula line-to-check))
	      (push (cons (cons line line-to-check) ()) result)))
	(some #'(lambda (x) (uni~match formel (node~formula x))) lines)))))

(defun goal-matches (parameter)
  ; parameter is a list: (line term-that-matches?)
  ; value: a list of all goals that match term-that-matches?
  (let ((line (first parameter))
	(lines (plan~open-lines (first plan*planning-state)))
	(formel (cri=build-formula (second parameter)))
	(result ()))
    (when formel
      (if (stringp line)
	  (dolist (line-to-check lines result)
	    (when (uni~match formel (node~formula line-to-check))
	      (push (cons (cons line line-to-check) ()) result)))
	(some #'(lambda (x) (uni~match formel (node~formula x))) lines)))))
      


(defun matches (terms)
  ;; terms is a list: (term term-that-matches?)
  ;; return true if the first term matches term-that-matches
  (let ((term-to-match (cri=build-formula (second terms))))
    (and term-to-match (uni~match term-to-match (first terms)))))

(defun sub-of-latest-assumption (parameter)
  ;; parameter is a list, term and line
  ;; term may be bound or not, line must be unbound
  ;; value: all possible combinations of subterms and latest assumptions
  ;; latest assumptions are those lines with no pdsn~successor
  (let ((latest-assumptions (cri=find-latest-assumptions))
	(term (first parameter))
	(ass (second parameter)))
    (if (stringp term) ;; term is not bound:
	(mapcan #'(lambda (assumption)
		    (cri=combine-ass-terms assumption term ass))
		latest-assumptions)
      ;; term is bound, so it is either a real term or a symbol
      (let ((new-term (if (term~p term) term (env~lookup-object term (pds~environment
								      OMEGA*CURRENT-PROOF-PLAN)))))
	(mapcan #'(lambda (line)
		    (when (term~subterm-positions
			   new-term
			   (node~formula line))
		      (list (list (cons ass line)))))
		latest-assumptions)))))


(defun sub-of-assumption (parameter)
  ;; parameter is a list, term and line
  ;; term may be bound or not, line must be unbound
  ;; value: all possible combinations of subterms and assumptions
  (let ((assumptions (plan~support-lines (first plan*planning-state)))
	(term (first parameter))
	(ass (second parameter)))
    (if (stringp term) ;; term is not bound:
	(mapcan #'(lambda (assumption)
		    (cri=combine-ass-terms assumption term ass))
		assumptions)
      ;; term is bound:
      (mapcan #'(lambda (line)
		  (when (term~subterm-positions
			 term
			 (node~formula line))
		      (list (list (cons ass line)))))
	      assumptions))))



(defun cri=find-latest-assumptions ()
  (let* ((alter (plan~state-method (first plan*planning-state)))
	 (added-concs (if alter (plan~alter-addconcs alter))))
    added-concs))


(defun cri=combine-ass-terms (assumption line-name term-name)
  (mapcar #'(lambda (term)
	      (list (list line-name assumption)
		    (list term-name term)))
	  (cri=get-subterms (node~formula assumption))))
	      
	    
(defun cri=get-subterms (term)
  (cond
   ((listp term) (mapcar #'cri=get-subterms term))
   ((appl~p term) (cons term (cri=get-subterms (rest (term~subterms term)))))
   ((or (termc~existential-quantification-p term) (termc~universal-quantification-p term))
    (cons term (cri=get-subterms(termc~quantification-scope term))))
   (t term)))
    
    

(defun cri=build-formula (formula)
  ; build a term from the formula, replaces all occurences of strings with variables
;  (ignore-errors ;; needed to catch error when terms in the formula are not in the
		 ;; environment 
    (term~read (cri=replace-strings-with-vars formula)
	       (prob~proof-environment  omega*current-proof-plan)))
 ; )

(defun cri=replace-strings-with-vars (formula)
   ; replaces all occurences of strings with variables
  (mapcar #'(lambda (element)
	      (if (listp element) (cri=replace-strings-with-vars element)
		(if (stringp element)
		    (let ((new-var (intern element (find-package :omega))))
		      (unless (shsym~p new-var) ;; this var does not yet exist, so
			                        ;; create it
					;(poly~make-poly-sym (sym~env-enter-variable new-var (type~o); ???
			(sym~env-enter-variable new-var
						(cond
						 ((eq (char element 0) #\o)
						  (type~read-existing-type 'o
									   (prob~proof-environment omega*current-proof-plan)))
						 ((eq (char element 0) #\n)
						  (type~read-existing-type 'num
									   (prob~proof-environment omega*current-proof-plan))))
						(prob~proof-environment  omega*current-proof-plan)))
		      new-var)
		  element)))
	  formula))
		   
(defun applied-with-last-method (line)
  ;; parameters is a line
  ;; gives the lines to which the last method was applied
  ;; assumes that line is unbound
  (let ((alter (plan~state-method (first plan*planning-state))))
    (when alter
      (mapcar #'(lambda (x) (list (cons line x))) (plan~alter-pre alter)))))

(defun latest-assumption (line)
  ;; parameters is a line
  ;; gives the lines which resulted from the last application of the method
  ;; assumes that line is unbound
  (let ((alter (plan~state-method (first plan*planning-state))))
    (when alter
      (mapcar #'(lambda (x) (list (cons line x))) (plan~alter-addconcs alter)))))
    
(defun already-applied-method-with-line (parameters)
  ;; parameters is a list: Method and Line
  ;; checks if the method has already been applied to the line
    (let* ((method (meth~find-method (first parameters)))
	   (line (second parameters))
	   (alters (mapcar #'plan~state-method plan*planning-state))
	   (methods (cri=remove-unequal-methods method alters)))
      (mapcan #'(lambda (alter)
		  (mapcar #'(lambda (x) (list (cons line x))) (plan~alter-pre alter)))
	      methods)));)

(defun cri=remove-unequal-methods (method alters)
  (remove-if-not #'(lambda (x) (when x (equal method (plan~alter-method x)))) alters))

(defun assumption-matches (parameters)
  ;; parameters is a list, a node and a term (not a true term, may contain strings)
  (let ((term (second parameters))
	(node (first parameters)))
    (if (not (stringp node))
	(uni~match (cri=build-formula term) (node~formula node))
      (let ((formula (cri=build-formula term)))
	(mapcan #'(lambda (ass)
		    (when (uni~match formula (node~formula ass)) (list (list (cons node ass)))))
		(plan~support-lines (first plan*planning-state)))))))


(defun unwrapped-focus (prln)
  ;; return a line where the focus is unwrapped, if there is none -> nil.
  ;; assumes that prln is unbound
  

  (if (stringp prln) ;; prln is unbound
      (list (list (cons prln
			(some #'(lambda (node)
				  (if (eql (keim~name (term~at-position (node~formula node)
									(pos~list-position '(0))))
					   'focus)
				      node))
			      (union (plan~support-lines (first plan*planning-state))
				     (plan~open-lines (first plan*planning-state)))))))
    (keim~equal (term~at-position (node~formula prln)
				  (pos~list-position '(0)))
		(env~lookup-object 'focus (pds~environment OMEGA*CURRENT-PROOF-PLAN)))))
    


(defun cri=check-whether-unwrapped-in-lines (term prlns)
  ;; returns a line or nil, if the term occurs in one of the lines
  (if (null prlns) ()
    (when term
      (if (term~equal-p term (node~formula (first prlns)))
	  (first prlns)
	(cri=check-whether-unwrapped-in-lines term (rest prlns))))))

(defun last-goal (line)
  ;; if line is unbound, binds it to the open lines that were created by the last method
  (let* ((alter (plan~state-method (first plan*planning-state)))
	 (added-open (if alter (plan~alter-addprems alter))))
    (mapcar #'(lambda (x) (list (cons line x))) added-open)))


(defun most-similar-subterm-in-asss-to-goal (liste)
  ;; list has the form (goal ass subterm)
  ;; the function searches in the assumptions for the most similar subterm for goal
  ;; where the most similar subterm is definied as the term with shares the most
  ;; fct-symbols with the goal
  ;; goal can be bound, ass and subterm not
  (if (stringp (first liste)) ;; goal is not bound
      (mapcan #'(lambda (goal) (cri=most-similar-help (cons goal (rest liste))))
	      (plan~open-lines (first plan*planning-state)))
					;(plan~alter-addprems (plan~state-method (first plan*planning-state))))
    (cri=most-similar-help liste)))  
			   
(defun cri=most-similar-help (liste)
  (let ((analyzed-goal (cri=analyze (node~formula (first liste)) (make-hash-table :test #'equal)))
	(result '((() () -1)))
	(ass (second liste))
	(subterm (third liste))
	(goal-hyps (pdsn~hyps (first liste))))
    (dolist (x (mapcan #'(lambda (node)
			   (when (subsetp (pdsn~hyps node) goal-hyps) (list node)))
		       (plan~support-lines (first plan*planning-state))))
      (setf result
	    (cri=get-most-similar-subterm-of-ass analyzed-goal
						 x
						 result)))
    (when (> (third (first result)) -1)
      (reverse (mapcar #'(lambda (x)  ;;reverse ist ein kleiner Hack...
			   (list (cons ass (first x))
				 (cons subterm (pos~butlast (second x)))))
		       result)))))



(defun cri=get-most-similar-subterm-of-ass (analyzed-goal ass result)
  (let* ((positions (term~subterm-positions
		     (term~read 'less (prob~proof-environment omega*current-proof-plan))
		     (node~formula ass))))
    (if positions
      (dolist (pos positions result)
	(setf result (cri=extract-and-better? ass analyzed-goal (pos~add-end! 1 (pos~butlast pos)) result)))
      result)))

(defun cri=extract-and-better? (ass analyzed-goal pos result)
  (let ((new (cri=compare (term~at-position (node~formula ass) pos) analyzed-goal 0))
	(old (third (first result))))
    (if (not (second (first result)))
	(list (list ass pos new))
      (cond
       ((> new old) (list (list ass pos new)))
       ((< new old) result)
       (t
	(let ((new-depth (termc~depth (node~formula ass)))
	      (old-depth (termc~depth (node~formula (first (first result))))))
	  (cond
	   ((> new-depth old-depth) result)
	   ((< new-depth old-depth) (list (list ass pos new)))
	   (t (cons (list ass pos new) result)))))))))

(defun cri=compare (term analyzed-goal result)
  (if (not term) result
    (typecase term
      (cons (+ (cri=compare (first term) analyzed-goal result) (cri=compare (rest term) analyzed-goal result)))
      (appl+appl (+ (cri=get-value (appl~function term) analyzed-goal)
		    (cri=compare (appl~arguments term) analyzed-goal result)))
      (abstr+abstr (cri=compare (abstr~scope term) analyzed-goal result))
      (t 0))))

(defun cri=get-value (term analyzed-goal)
  (if (gethash (keim~name term) analyzed-goal) 1 0))

(defun cri=analyze (term hashtable)
  (if (not term) hashtable
    (typecase term
      (cons (cri=analyze (first term) hashtable) (cri=analyze (rest term) hashtable))
      (appl+appl (setf (gethash (keim~name (appl~function term)) hashtable) t)
		 (cri=analyze (appl~arguments term) hashtable))
      (abstr+abstr (cri=analyze (abstr~scope term) hashtable))
      )))

(defun hyps-of-line (line hyps)
  ;; gives the hypothesises of the line
  ;; line must be bound, hyps not.
  (list (list (cons hyps (pdsn~hyps line)))))

(defun current-goal (line)
  ;; gives the current-open-lines
  ;; line must be unbound
  (mapcar #'(lambda (open)
	      (list (cons line open)))
	  (plan~open-lines (first plan*planning-state))))

(defun current-assumptions (line)
  ;; gives the current assumptions
  ;; line must be unbound
  (mapcar #'(lambda (ass)
	      (list (cons line ass)))
	  (plan~support-lines (first plan*planning-state))))

(defun no-focus ()
  (notany #'(lambda (line)
	    (term~subterm-positions (env~lookup-object 'focus (pds~environment
							       OMEGA*CURRENT-PROOF-PLAN))
				    (node~formula line)))
	(append (plan~open-lines (first plan*planning-state))
		(plan~support-lines (first plan*planning-state)))))

(defun always-true ()
  t)

(defun latest-application-with-method (meth goals ass params)
  ;; searches for the latest application of method and binds goals, ass and params
  (let* ((method (meth~find-method meth))
	 (found-plan-state (some #'(lambda (plan-state)
				    (when (eql method
					       (plan~alter-method (plan~state-method
								   plan-state)))
				      plan-state))
				 plan*planning-state))
	 (subst (plan~alter-subst (plan~state-method found-plan-state))))
    (when found-plan-state
      (list
       (list
	(cons goals (mapcar #'(lambda (node)
				(subst~get-component node subst))
			    (meth~exist-conclusions method)))
	(cons ass (mapcar #'(lambda (node)
			      (subst~get-component node subst))
			  (meth~exist-premises method)))
	(cons params (mapcar #'(lambda (node)
				 (subst~get-component node subst))
			     (meth~parameters method))))))))
|#
