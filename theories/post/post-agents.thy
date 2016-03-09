(in-package :omega)

(eval-when (compile)
  (error "This file should not be compiled."))

;;; set-active-focus -------------------------------------------------

(agent~defmatrix set-active-focus
   (agents
    (c-predicate (for planline)
		 (definition t))
    (function  (for planline)
	       (multiple planline)
	       (definition (pds~open-nodes omega*current-proof-plan)))))

;;; foralli-sort* -------------------------------------------------

(agent~defmatrix foralli-sort* 
   (agents (c-predicate (for univ-line)
                        (uses )
                        (definition (pred0 univ-line)))
	   (function    (for param)
                        (uses univ-line)
                        (definition (func2 (:node univ-line)))))
   (predicates
    (pred0 (univ-line) (potac=forall-sort-formula? univ-line))
    (func2 (univ-line)
	   (when (pred0 univ-line)
	     (orules=generate-defaults-foralli univ-line (pds~environment omega*current-proof-plan))))))


;;; foralle-sort -------------------------------------------------

(agent~defmatrix foralle-sort
   (agents (s-predicate (for univ-line)
                        (uses )
                        (definition (pred0 univ-line)))
	   (function  (for term)
		      (uses so-line)
		      (definition (func2 so-line)))
	   (s-predicate (for so-line)			
                        (uses univ-line)
                        (definition (pred1 so-line univ-line))))
   (predicates
    (pred0 (univ-line) (potac=forall-sort-formula? univ-line))
    (pred1 (so-line univ-line)
	   (when (and (potac=forall-sort-formula? univ-line)
		      (= (length (data~appl-arguments univ-line)) 2))
	     (let* ((sort (second (data~appl-arguments  univ-line))))
	       (and (data~appl-p so-line)
		    (term~alpha-equal sort (data~appl-function so-line))))))
    (func2 (sort)
	   (when (data~appl-p sort) (car (data~appl-arguments sort))))))

;;; existse-sort -------------------------------------------------

(agent~defmatrix existse-sort
   (agents (s-predicate (for ex-line)
                        (uses )
                        (definition (pred0 ex-line)))
	   (function    (for param)
                        (uses ex-line)
                        (definition (func2 ex-line)))
	   (c-predicate (for line)
			(uses ex-line)
			(definition t)))
   (predicates
    (pred0 (univ-line) (potac=exists-sort-formula? univ-line))
    (func2 (univ-line)
	   (when (pred0 univ-line)
	     (let ((var (logic~quantification-bound-variable univ-line)))
	       (term~generate-term-primitive-with-new-name 
		(keim~name var) (term~type var) 'term+constant (pds~environment omega*current-proof-plan)))))))

;;; wellsorted -------------------------------------------------
(agent~defmatrix wellsorted 
   (agents (c-predicate (for line)
	        	(uses)
    			(definition (pred2 (:node line))))
           (function    (for premises)
                        (uses line)
                        (definition (pred1 (:node line)))))
   (predicates
    (pred2 (line)
	   (car (potac=wellsorted-prems (list line)
					(th~senv (prob~proof-theory omega*current-proof-plan)))))
    (pred1 (line)
	   (when line
	     (potac=wellsorted-prems (pds~node-supports line)
				     (th~senv (prob~proof-theory omega*current-proof-plan)))))))


;;; MBase and LML -------------------------------------------------

;;example: a(s(name:'=') a(s(_) '#'(a(s(name:'plus') v(_)#v(_))))#_)

(defgeneric poag=formula2pattern (form)
  (:method ((form term+appl))
	   (format nil "a(~A '#'~A)"
		   (poag=formula2pattern (data~appl-function form))
		   (mapcar #'poag=formula2pattern (data~appl-arguments form))))
  (:method ((form term+constant))
	   (if (env~lookup-object (keim~name form) (th~env (prob~theory omega*current-proof-plan)))
	       (format nil "s(name:exact('~A'))" (keim~name form))
	     "v(_)"))
  (:method ((form term+variable))
	   "v(_)")
  (:method ((form T)) (break t)))


(defun poag=formula2pattern-abstract (form)
  (let ((result  (poag=formula2pattern-abstr form 0)))
    (when (consp result) result)))

(defgeneric poag=formula2pattern-abstr (form depth)
  (:method ((form term+appl) (depth (eql 0)))
	   (let ((fct (poag=formula2pattern-abstr (data~appl-function form) (1+ depth)))
		 (new-args (mapcar #'(lambda (x) (poag=formula2pattern-abstr x (1+ depth )))
				   (data~appl-arguments form)))
		 (args (mapcar #'(lambda (x) (poag=formula2pattern-abstr x (1+ (1+ depth ))))
				   (data~appl-arguments form)))
		 output)
	     (omega~trace "new-args ~A  args ~A" new-args args)
	     (dotimes (n (length args) output)
	     (omega~trace "args ~A-~A-~A"
			  (subseq args 0 n)
			  (subseq new-args n (1+ n))
			  (subseq args (1+ n) (length args)))
	       (push (format nil "a(~A '#'~A)" fct (append (subseq args 0 n)
							   (subseq new-args n (1+ n))
							   (subseq args (1+ n) (length args))))
		     output))))
  (:method ((form term+appl) (depth (eql 1)))
	   (let ((fct (poag=formula2pattern-abstr (data~appl-function form) (1+ depth)))
		 (new-args (mapcar #'(lambda (x)  "v(_)") (data~appl-arguments form))))
	       (format nil "a(~A '#'~A)" fct new-args)))
  (:method ((form term+appl) (depth integer))
	   "v(_)")
  (:method ((form term+constant) (depth integer))
	   (if (env~lookup-object (keim~name form) (th~env (prob~theory omega*current-proof-plan)))
	       (format nil "s(name:exact('~A'))" (keim~name form))
	     "v(_)"))
  (:method ((form term+variable) (depth integer))
	   "v(_)")
  (:method ((form T) (depth integer)) (break t)))


(defun poag=get-from-mbase (patterns)
  (sys~handler-case
   (let* ((pattern   (if (consp patterns) patterns (list patterns)))
	  (mids  (remove-duplicates (mapcan #'(lambda (pat)
						(let ((mid (keim::th=mb-ofindtheorems nil pat)))
						  (if (stringp mid) nil mid)))
					    pattern)
				    :test #'equal))
	  (thms (mapcar #'keim::th=mb-ogetassertionbymid mids))
	  (thys (mapcar #'keim~name (cons
				     (prob~theory omega*current-proof-plan)
				     (th~imports-recursed (prob~theory omega*current-proof-plan)))))
	  (cleaned-thms (remove-if-not
			 #'(lambda (thm) (member
					  (second (find-if #'(lambda (tt) (when (consp tt) (eq (car tt) 'in))) thm))
					  thys)) thms)))
					;(omega~trace "###MBASE: Threorems from MBase ~A with pattern ~A" cleaned-thms pattern)
     (mapcar #'second cleaned-thms))
   (error (c) (omega~trace "~A" c))))

(defun poag=outputlml (text)
  (cond ((or opr*service-listener opr*loui-listener)
	 (opr~arrest-listener opr*loui-listener)
	 (opr~arrest-listener opr*service-listener)
	 (socket~write (format nil "browsePRES(\"~A<BR><BR>\")" text))
	 (opr~release-listener opr*loui-listener)
	 (opr~release-listener opr*service-listener))
	(t nil)))

(defun poag=stuff2formula (stuff)
  (typecase stuff
    (th+assumption (poag=stuff2formula (th~ass-node stuff)))
    (prob+problem (poag=stuff2formula (prob~conclusion stuff)))
    (symbol (poag=stuff2formula (th~find-assumption stuff (prob~proof-theory omega*current-proof-plan))))	       
    (string (poag=stuff2formula (th~find-assumption stuff (prob~proof-theory omega*current-proof-plan))))
    (node+node (node~formula stuff))
    (term+term stuff)))


;;; apply-theorem -------------------------------------------------
(defun poag=get-thms (line)
  (let* ((candidates (poag=get-thms-from-mbase line))
	 (cleaned-canditates (remove-if-not #'(lambda (cand) (potac=apply-theorem-test line nil cand))
						    candidates)))
     ;(omega~trace "###APPLY: applicable theorems ~A" cleaned-canditates)
    (poag=output-apply-thm line cleaned-canditates)
     cleaned-canditates))
    
(defun poag=output-apply-thm (goal thms)
  (mapc #'(lambda (theorem)
	      (poag=outputlml (format nil "apply-theorem <B>~A</B> on <TERM>~A</TERM><BR><TERM>~A</TERM>"
				      theorem
				      (parse=term (poag=stuff2formula goal))
				      (parse=term (poag=stuff2formula theorem)))))
				      thms))
  
(defun poag=get-thms-from-mbase (line)
  (let ((pattern (poag=formula2pattern line))
	(patterns (poag=formula2pattern-abstract line)))
   (poag=get-from-mbase (cons pattern patterns ))))


(agent~defmatrix apply-theorem
   (agents (c-predicate (for line)
	        	(uses)
			;(multiple line)
    			(definition (pred1 line)))
	   (function    (for thm)
			(uses line)
			(multiple thm)
                        (definition (poag=get-thms line)))
           (s-predicate (for premlist)
                        (uses line thm)
                        (definition (pred2 premlist line thm))))
   (predicates
    (pred1 (line) line)
    (pred2 (premlist line thm) ;;need s-pred-list here!
	   (list ))))


;;; rewrite-with -------------------------------------------------

(defun poag=get-rews (line dir)
  (let* ((substructs (remove-duplicates (data~all-substructs line) :test #'data~equal))
	 (thms (mapcan #'(lambda (sub)
			   (remove-if-not #'(lambda (thm)
					      (potac=rew-apply-term sub thm dir))
					  (poag=get-rews-from-mbase sub dir)))
				  substructs)))
    ;(omega~trace "###REWR: applicable rewrites ~A" thms)
    thms))

(defun poag=get-rews-from-mbase (formula dir)
  (when (data~appl-p formula)
     (let* ((pattern (poag=formula2pattern formula))
	    (construct-pattern (if (eq dir 'rl)
				   (format nil "a(s(name:'=') '#'(~A _))" pattern) 
				 (format nil "a(s(name:'=') '#'(_ ~A))" pattern))))
       (poag=get-from-mbase construct-pattern))))

(defun poag=get-poss (line ax dir)
  (when (and ax dir line)
    (let ((poss (mapcan #'(lambda (sub)
			    (remove-if-not #'(lambda (pos)
					       (potac=rew-apply line ax pos dir))
					   (data~substruct-positions sub line)))
			(data~all-substructs line))))
      (poag=output-rewrite-with line ax poss dir)
      poss)))

(defun poag=output-rewrite-with (line theorem poss dir)
  (mapc #'(lambda (pos)
	      (poag=outputlml (format nil "rewrite-with <B>~A</B> on term <TERM>~A</TERM> at <B>~A</B> in direction <B>~A</B><BR><TERM>~A</TERM>"
				      theorem
				      (parse=term (data~struct-at-position line pos))
				      pos
				      (if (eq dir 'rl) "<--" "-->")
				      (parse=term (poag=stuff2formula theorem)))))
				      poss))


(agent~defmatrix rewrite-with
   (agents (c-predicate (for oldline)
	        	(uses)
			(exclude newline)
    			(definition (pred1 oldline)))
	   (function    (for direction)
			(uses oldline)
			(multiple direction)
                        (definition (pred2 oldline)))
           (function    (for axiom)
                        (uses oldline direction)
                        (multiple axiom)
                        (definition (poag=get-rews oldline (:param direction))))
           (function    (for position)
                        (uses oldline axiom direction)
                        (multiple position)
                        (definition (poag=get-poss oldline (:param axiom)(:param direction))))
           (s-predicate (for newline)
			(exclude oldline)
			(definition (pred1 newline))))
   (predicates
    (pred1 (line) line)
    (pred2 (forget) (list 'lr 'rl))))


#|
(agent~defmatrix rewrite-with
   (agents (c-predicate (for oldline)
	        	(uses)
			(multiple oldline)
    			(definition (pred1 oldline)))
	   (function    (for direction)
			(uses oldline)
			(multiple direction)
                        (definition (pred2 oldline)))
           (function    (for direction)
                        (uses newline)
                        (multiple direction)
                        (definition (pred2 newline)))
           (function    (for axiom)
                        (uses oldline direction)
                        (multiple axiom)
                        (definition (poag=get-rews oldline (:param direction))))
           (function    (for axiom)
                        (uses newline direction)
                        (multiple axiom)
                        (definition (poag=get-rews newline (:param direction))))
           (function    (for position)
                        (uses oldline axiom direction)
                        (multiple position)
                        (definition (poag=get-poss oldline (:param axiom)(:param direction))))
           (function    (for position)
                        (uses newline axiom direction)
                        (multiple position)
                        (definition (poag=get-poss newline (:param axiom)(:param direction))))
           (s-predicate (for newline)
                        (exclude oldline)
                        (definition (pred1 newline))))
   (predicates
    (pred1 (line) line)
    (pred2 (forget) (list 'lr 'rl))))

|#
