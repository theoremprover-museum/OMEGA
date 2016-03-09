(in-package :omega)

(defclass pres+extnode (keim+name)
  ((formula :accessor pres~formula
	    :initarg :formula
	    :documentation "The formula for a proof node.")
   (justlist :accessor pres~justlist
	     :initarg :justlist
	     :initform nil
	     :documentation "The list of justifications for a proof node.")
   (explains :accessor pres~explains
	     :initarg :explains
	     :initform nil
	     :documentation "The set of nodes justified by this node.")
   (mark :accessor pres~mark
	 :initarg :mark
	 :initform nil
	 :documentation "The list of marks for the proof node."))
  (:documentation "A node in the plan data structure."))

(defclass pres+extjust (keim+object)
  ((method :accessor pres~method
	   :initform "Unspecified"
	   :initarg :method
	   :documentation "The inference method that justifies this proof node.")
   (premises :accessor pres~premises
	     :initform nil
	     :initarg :premises
	     :documentation "The list of premises from which this node is justified.")
   (mark :accessor pres~mark
	 :initarg :mark
	 :initform nil
	 :documentation "The list of marks for this justification."))
  (:documentation "The justification for a proof node."))

(defclass pres+premise (keim+object)
  ((node :accessor pres~node
	 :initarg :node
	 :documentation "The premise node")
   (mark :accessor pres~mark
	 :initarg :mark
	 :initform nil
	 :documentation "The list of marks for this premise."))
  (:documentation "A premise which consists of a node an some marks."))

(defclass pres+user ()
  ((aware :accessor pres~aware
	  :initarg :aware
	  :initform :nil
	  :documentation "List of formulas the user is aware of.")
   (abstract :accessor pres~abstract
	     :initarg :abstract
	     :initform nil
	     :documentation "List of keywords that describe the ability of the user ~
                             to understand certain abstractions. ~
                             Keywords are: ~
                             VARIABLE: Variables are abstracted into meta variables ~
                             CONSTANT: Constants are abstracted into a special placeholder ~
                             CONSTANTMULTIPLIER: Products of a constant and a variable ~
                               are abstracted into a meta variable. ~
                             EQUALITYOPERATOR: =,<,>,<=,>= are abstracted into a generic ~
                               equality operator. ~
                             NEGATION: Negations are deleted."))
  (:documentation "Profile of the mental capabilities of a user."))

(defclass pres+awareformula ()
  ((theory :accessor pres~theory
	   :initarg :theory
	   :initform nil
	   :documentation "The name of a theory which belongs to this formula")
   (formula :accessor pres~formula
	    :initarg :formula
	    :documentation "The formula the user is aware of"))
  (:documentation "A formula the user is aware of together with the theory to which it belongs"))

; Funktioniert
;; (setq pres*theoryinfo			; Information about multiplication and equivalence operators in different
;; 					; theories
;;       (let* ((fs (open "~/pres/theoryinfo.lisp" :direction :input))
;; 	     (file (read fs)))
;; 	(close fs)
;; 	file))

; Funktioniert
; Copy a node tree and turn it into an extnode tree
(defun pres~createextnodetree (node)
  (pres=createextnodetree node nil (make-hash-table)))

; Funktioniert
; Create an extnodetree from a node using a hash-table ht
(defun pres=createextnodetree (node parent ht)
  (if (second (multiple-value-list (gethash node ht))) ; Did we already process this node?
      (let ((enode (gethash node ht)))	; Get the extnode representation of it
	(setf (pres~explains enode) (cons parent (pres~explains enode))) ; Add the new parent
	enode)								 ; And give it back
    (let ((newnode (make-instance 'pres+extnode ; else create a new node
				  :name (keim~name node)
				  :formula (data~copy (node~formula node) :downto '(data+primitive))
				  :justlist nil
				  :explains (if (null parent) nil (list parent))
				  :mark nil)))
      (setf (gethash node ht) newnode)	; store it in the hash-table
      (setf (pres~justlist newnode) (pres=createjusttree node newnode ht)) ; process the justifications
      newnode)))							   ; and give it back

; Funktioniert
; Create a extjusttree from a node
(defun pres=createjusttree (node parent ht)
  (list (make-instance 'pres+extjust
		       :method (just~method (node~justification node))
		       :premises (mapcar #'(lambda (n) (make-instance 'pres+premise
								      :node (pres=createextnodetree n parent ht)
								      :mark nil))
					 (node~just-premises node))
		       :mark nil)))

; Funktioniert
(defun pres~mainterm (j)
  (if (null (pres~premises j))
      nil
    (pres~node (first (pres~premises j)))))

; Funktioniert
(defun pres~mainpremise (j)
  (if (null (pres~premises j))
      nil
    (first (pres~premises j))))

; Funktioniert
(defun pres~premisenodes (j)
  (mapcar #'(lambda (p) (pres~node p)) (pres~premises j)))

; Funktioniert
(defun pres~ordinary-premises (j)
  (if (null (pres~premises j))
      nil
    (cdr (pres~premises j))))

; Funktioniert
(defun pres~label2node (top label)
  (labels ((return-first-of-premises (li la)
				     (if (null li) nil
				       (let ((res (pres~label2node (pres~node (first li)) la)))
					 (if res res
					   (return-first-of-premises (cdr li) la)))))
	   (return-first-of-justlists (li la)
				      (if (null li) nil
					(let ((res (return-first-of-premises (pres~premises (first li)) la)))
					  (if res res
					    (return-first-of-justlists (cdr li) la))))))
    (cond ((string-equal (keim~name top) label) top)
	  (t (return-first-of-justlists (pres~justlist top) label)))))

; Funktioniert
(defun pres~cut-prop-tree (node user)
  (let ((ht (make-hash-table)))
    (labels ((cut-prop-tree (node)
			    (map nil #'(lambda (j)
					 (map nil #'(lambda (p) (cut-prop-tree (pres~node p)))
					      (pres~premises j)))
				 (pres~justlist node))
			    (if (not (second (multiple-value-list (gethash node ht))))
				(progn
				  (pres~cut-prop node user)
				  (setf (gethash node ht) t)))))
      (cut-prop-tree node))))

; Funktioniert
(defun pres~cut-prop (node user)				; node: extnode, user: user
  (let* ((concl (pres~formula node))		; concl: formula
	 (justset (remove-if #'(lambda (js) ; justset: list of extjust, js: extjust
				 (some #'(lambda (m) ; m: mark
					   (string-equal m 'summary)) (pres~mark js)))
			     (pres~justlist node))))
    (map nil
	 #'(lambda (j) ; j: extjust
	     (let* ((premises (pres~ordinary-premises j)) ; premises: list of premise
		    (main-term (pres~mainterm j))) ; main-term: extnode
	       (map nil
		    #'(lambda (p1) ; p1: premise
			(let* ((f1 (pres~formula (pres~node p1))) ; f1: formula, f: list of formula, p: extnode
			       (f (mapcar #'(lambda (n) (pres~formula (pres~node n)))
					  premises)))
					  ;(remove-if #'(lambda (p) (eq (pres~node p) (pres~node p1))) premises))))
			  (if (and (pres~aware-of user f1) (pres~able-infer user main-term f concl f1))
			      (progn (setf (pres~mark p1) (adjoin 'inferable (pres~mark p1) :test 'string-equal))
				     (print (concatenate 'string "Applying Cut Prop rule on "
							 (string (keim~name (pres~node p1)))))))))
		    premises)))
	 justset)))

; Funktioniert
(defun pres~cut-rule-tree (node user)
  (let ((ht (make-hash-table)))
    (labels ((cut-rule-tree (node)
			    (map nil #'(lambda (j)
					 (map nil #'(lambda (p) (cut-rule-tree (pres~node p)))
					      (pres~premises j)))
				 (pres~justlist node))
			    (if (not (second (multiple-value-list (gethash node ht))))
				(progn
				  (pres~cut-rule node user)
				  (setf (gethash node ht) t)))))
      (cut-rule-tree node))))

; Funktioniert
(defun pres~cut-rule (node user)				; node: extnode, user: user
  (let* ((concl (pres~formula node))		; concl: formula
	 (justset (remove-if #'(lambda (js) ; justset: list of extjust, js: extjust
				 (some #'(lambda (m) ; m: mark
					   (string-equal m 'summary)) (pres~mark js)))
			     (pres~justlist node))))
    (map nil
	 #'(lambda (j)											; j: extjust
	     (let* ((main-term (pres~mainterm j))) ; main-term: extnode
	       (if main-term
		   (let* ((r (pres~formula main-term)) ; r: formula
			  (premises (pres~ordinary-premises j)) ; premises: list of premise
			  (f (mapcar #'(lambda (n) (pres~formula (pres~node n))) ; f: list of formula, n: extnode
				     (remove-if #'(lambda (p) (some #'(lambda (m) ; p: premise, m: mark
									(string-equal m 'inferable))
								    (pres~mark p)))
						premises))))
		     (if (and (pres~aware-of user (pres~generic main-term))
			      (pres~able-infer user main-term f concl r))
			 (progn (setf (pres~mark (pres~mainpremise j))
				      (adjoin 'inferable (pres~mark (pres~mainpremise j))
					      :test #'(lambda (x y) (string-equal x y))))
				(print (concatenate 'string "Applying Cut Rule rule on "
						    (string (keim~name (pres~node (pres~mainpremise j))))))))))))
	 justset)))

(defun pres~compactification-tree (node user)
  (let ((ht (make-hash-table)))
    (labels ((compactification-tree (node)
				    (map nil #'(lambda (j)
						 (map nil #'(lambda (p) (compactification-tree (pres~node p)))
						      (pres~premises j)))
					 (pres~justlist node))
			    (if (not (second (multiple-value-list (gethash node ht))))
				(progn
				  (pres~compactification node user)
				  (setf (gethash node ht) t)))))
      (compactification-tree node))))

; pres~coherent, pres~subst, pres=addtoexplains noch nicht getestet
(defun pres~compactification (node user) ; node: extnode, user: user
  (let* ((concl (pres~formula node))		; concl: formula
	 (justset (remove-if #'(lambda (js) ; justset: list of extjust, js: extjust
				 (some #'(lambda (m) ; m: mark
					   (string-equal m 'summary)) (pres~mark js)))
			     (pres~justlist node))))
    (map nil
	 #'(lambda (j)	       ; j: extjust
	     (if (not (null (pres~premises j)))
		 (let* ((premises (remove-if
				   #'(lambda (pj)
				       (some #'(lambda (m) (string-equal m 'inferable))
					     (pres~mark pj)))
				   (pres~ordinary-premises j))) ; premises: list of premise
			(f (mapcar #'(lambda (n) (pres~formula (pres~node n))) ; f: list of formula, n: extnode
				   premises))
			(e (pres~explains node))) ; e: list of extnode
		   (map nil
			#'(lambda (e1)							; e1: extnode
			    (let* ((je (remove-if	; je: list of extjust
					#'(lambda (js) ; js: extjust
					    (notany #'(lambda (p) ; p: premise
							(eq (pres~node p) node))
						    (pres~ordinary-premises js)))
					(pres~justlist e1))))
			      (if (and (notany #'(lambda (l) (string-equal l 'chainable)) (pres~mark node))
				       (some #'(lambda (l) (string-equal l 'inferable)) (pres~mark (pres~mainpremise j)))
				       (pres~coherent user concl f))
				  (let* ((jn nil))	; jn: list of extjust
				    (map nil
					 #'(lambda (pj) ; pj: premise, m: mark
					     (progn
					       (setq jn (append jn (pres~subst node pj je)))
					       (print (concatenate 'string "Short cut: "
								   (string (keim~name e1)) "=>("
								   (string (keim~name node)) ")=>"
								   (string (keim~name (pres~node pj)))))))
					 premises)
				    (pres=addtoexplains jn e1)
				    (setf (pres~justlist e1) (append (pres~justlist e1) jn))
				    (map nil
					 #'(lambda (ej) ; ej: extjust
					     (map nil
						  #'(lambda (pr) ; pr: premise
						      (setf (pres~mark pr)
							    (adjoin 'short-cut (pres~mark pr)
								    :test #'(lambda (x y) (string-equal x y)))))
						  (pres~premises ej)))
					 jn)))))
			e))))
	 justset)))

(defun pres~restructuring-tree (node user)
  (let ((ht (make-hash-table)))
    (labels ((restructuring-tree (node)
				 (map nil #'(lambda (j)
					      (map nil #'(lambda (p) (restructuring-tree (pres~node p)))
						   (pres~premises j)))
				      (pres~justlist node))
				 (if (not (second (multiple-value-list (gethash node ht))))
				     (progn
				       (pres~restructuring node user)
				       (setf (gethash node ht) t)))))
      (restructuring-tree node))))

					; pres~no-expand noch nicht fertig
(defun pres~restructuring (node user)
  (labels ((search-ei (last-row p1)			; last-row: list of extnode, p1: premise, does breadth-first-search for some node
		      (let* ((new-row nil))			; new-row: list of extnode
			(map nil
			     #'(lambda (n) (setq new-row (append new-row (pres~explains n)))) ; n: extnode
			     last-row)
			(let* ((ei (some #'(lambda (e) ; e: extnode
					     (if (not (and ;(some #'(lambda (m) (string-equal m 'chainable)) (pres~mark e)) ; m: mark
						       (pres~no-expand e)
					;(some #'(lambda (j) (pres~no-expand e j p1)) (pres~justlist e)))) ; j: extjust
						       (pres~no-expand2 e p1))
						      e nil))
					     new-row))))
			  (if ei ei (search-ei new-row p1))))))
    (let* ((concl (pres~formula node))	; concl: formula
	   (justset (remove-if #'(lambda (js) ; justset: list of extjust, js: extjust
				   (some #'(lambda (m) ; m: mark
					     (string-equal m 'summary)) (pres~mark js)))
			       (pres~justlist node))))
      (map nil
	   #'(lambda (j)
	       (let* ((premises (pres~ordinary-premises j))) ; premises: list of premise
		 (map nil
		      #'(lambda (p1)					; p1: premise
			  (if (or ;(some #'(lambda (m) (string-equal m 'chainable)) (pres~mark node))
			          (pres~no-expand node)
				  (pres~no-expand2 node p1))
			      (let* ((ei (search-ei (list node) p1)) ; ei: node
				     (je (remove-if #'(lambda (js) ; je: list of extjust, js: extjust
							(notany #'(lambda (p) ; p: premise
								    (eq (pres~node p) node))
								(pres~ordinary-premises js)))
						    (pres~justlist ei))))
				(map nil
				     #'(lambda (ji) ; ji: extjust
					 (setf (pres~premises ji)
					       (append (pres~premises ji)
						       (list (make-instance 'pres+premise
									    :node (pres~node p1)
									    :mark nil))))
					 (setf (pres~explains (pres~node p1)) (adjoin ei (pres~explains (pres~node p1))
										      :test #'(lambda (n1 n2) (eq n1 n2))))
					 (print (concatenate 'string "Copying premise "
							     (string (keim~name (pres~node p1)))
							     " of node "
							     (string (keim~name node))
							     " to node "
							     (string (keim~name ei)))))
				     je)
				(setf (pres~mark p1) (adjoin 'co-reference (pres~mark p1) :test #'(lambda (x y) (string-equal x y)))))))
		      premises)))
	   justset))))

; Dateiformat:
; ('aware ((theory1 formula1) (theory2 formula2) ...)
;  'abstract (keyword1 keyword2 ...))

; Funktioniert
(defun pres~save-user (filename user)
  (let* ((awarelist
	  (let ((awl nil))
	    (map 'nil
		 #'(lambda (elem)					; Creates list elements of the form (theory formula)
		     (setq awl (cons (list (pres~theory elem)
					   (post~print (pres~formula elem) nil))
				     awl)))
		 (pres~aware user))
	    awl))
	 (abstractlist (pres~abstract user))
	 (userlist (list 'aware awarelist 'abstract abstractlist))
	 (fs (open filename :direction :output :if-exists :supersede)))
    (princ userlist fs)
    (close fs)))

; Funktioniert
(defun pres~load-user (filename)
  (let* ((fs (open filename :direction :input))
	 (file (read fs)))
    (close fs)
    (let* ((awareoffset (search '(aware) file))
	   (awarelist (if (not (null awareoffset)) (nth (+ awareoffset 1) file) nil))
	   (awarelist2 nil)
	   (abstractoffset (search '(abstract) file))
	   (abstractlist (if (not (null abstractoffset)) (nth (+ abstractoffset 1) file) nil)))
      (map nil #'(lambda (elem)					; elem of form (theory formula)
		   (let* ((theory (th~find-theory (first elem))))
		     (if (not (null theory))
			 (setq awarelist2
			       (cons (make-instance 'pres+awareformula
						    :theory (first elem)
						    :formula (post~read-object
							      (second elem)
							      (th~env theory)
							      :existing-term))
				     awarelist2)))))
	   awarelist)
      (make-instance 'pres+user
		     :aware awarelist2
		     :abstract abstractlist))))

; Funktioniert
(defun pres~aware-of (user term)
  (if (not (null term))
      (not (null (search (list term) (pres~aware user)
			 :test #'(lambda (el1 el2) ; el1: term, el2: pres+awareformula
				   (uni~unify-two-terms el1 (pres~formula el2))))))))

; pres~abstracted noch nicht getestet
; erstes and funktioniert
(defun pres~able-infer (user mainterm premises conclusion unknown) ; mainterm: extnode, premises: list of formula
					; conclusion: formula, unknown: formula
  (and (or (and (data~equal unknown (pres~formula mainterm))
		(pres~instantiated (pres~compose (adjoin conclusion premises :test #'(lambda (x y) (data~equal x y)))
						 (pres~formula mainterm))))
           (and (member unknown premises :test #'(lambda (x y) (data~equal x y)))
		(pres~instantiated (pres~compose (adjoin conclusion
							 (remove-if #'(lambda (f) (data~equal f unknown))
								    premises))
						 (pres~formula mainterm)))))
       (some #'(lambda (f) (pres~match f (pres~generic mainterm)))
	     (pres~abstracted user (pres~formula mainterm)))))

; Funktioniert
(defun pres~multiplier-p (user function)
  (pres=multiplier-p user function pres*theoryinfo))

; Zu testen
(defun pres=multiplier-p (user function restlist) ; restlist: (thname thinfo thname thinfo ...)
					; thinfo: (... 'multipliers (m1 m2 ...) ...)
  (cond ((null restlist) nil)						; Theoryinfo completely searched => not found
	((th~find-theory (first restlist)) ; Is first theory in list present?
	 (let* ((multiplieroffset (search '(multipliers) (second restlist))) ; Get the multiplier info
		(multipliers (if (not (null multiplieroffset))
				 (nth (+ multiplieroffset 1) (second restlist))
			       nil)))
	   (if (search (list function) multipliers
		       :test #'(lambda (x y) (string-equal (keim~name x) y))) ; Is function among the multipliers?
	       t												; Found it!
	     (pres=multiplier-p user function (cddr restlist))))) ; Otherwise try the next theory
	(t (pres=multiplier-p user function (cddr restlist))))) ; Theory is not present => try the next one

; Funktioniert
(defun pres~equalityoperator-p (user function)
  (pres=equalityoperator-p user function pres*theoryinfo))

; Zu testen
(defun pres=equalityoperator-p (user function restlist) ; restlist: (thname thinfo thname thinfo ...)
					; thinfo: (... 'equalityoperators (e1 e2 ...) ...)
  (cond ((null restlist) nil)						; Theoryinfo completely searched => not found
	((th~find-theory (first restlist)) ; Is first theory in list present?
	 (let* ((eqopoffset (search '(equalityoperators) (second restlist))) ; Get the equalityoperator info
		(eqoperators (if (not (null eqopoffset))
				 (nth (+ eqopoffset 1) (second restlist))
			       nil)))
	   (if (search (list function) eqoperators
		       :test #'(lambda (x y) (string-equal (keim~name x) y))) ; Is function among the equalityoperators?
	       t												; Found it!
	     (pres=equalityoperator-p user function (cddr restlist))))) ; Otherwise try the next theory
	(t (pres=equalityoperator-p user function (cddr restlist))))) ; Theory is not present => try the next one

; Funktioniert
(defun pres~negation-p (user function)
  (pres=negation-p user function pres*theoryinfo))

; Zu testen
(defun pres=negation-p (user function restlist) ; restlist: (thname thinfo thname thinfo ...)
					; thinfo: (... 'negations (e1 e2 ...) ...)
  (cond ((null restlist) nil)						; Theoryinfo completely searched => not found
	((th~find-theory (first restlist)) ; Is first theory in list present?
	 (let* ((negationoffset (search '(negations) (second restlist))) ; Get the negation info
		(negations (if (not (null negationoffset))
				 (nth (+ negationoffset 1) (second restlist))
			       nil)))
	   (if (search (list function) negations
		       :test #'(lambda (x y) (string-equal (keim~name x) y))) ; Is function among the negations?
	       t												; Found it!
	     (pres=negation-p user function (cddr restlist))))) ; Otherwise try the next theory
	(t (pres=negation-p user function (cddr restlist))))) ; Theory is not present => try the next one

; Scheint zu funktionieren
(defun pres~instantiated (term)
  (cond ((data~appl-p term) (every #'pres~instantiated (data~appl-arguments term)))
	((data~abstr-p term) (and (every #'pres~instantiated (data~abstr-domain term))
				  (pres~instantiated (data~abstr-range term))))
	((meta~p term) nil)
	(t t)))

; Scheint zu funktionieren
(defun pres=unify-with-subterms (flist formula subst)
  (if (null flist) subst
    (let* ((positions (data~substruct-positions (car flist)
						formula :test #'(lambda (x y) (uni~unify-two-terms x y :subst subst)))))
      (if (null positions) 'error
	(pres=subterm-in-formula flist formula positions subst)))))

; Scheint zu funktionieren
(defun pres=subterm-in-formula (flist formula positions subst)
  (if (null positions) 'error
    (let* ((newsubst (uni~unify-two-terms (car flist) (data~struct-at-position formula (car positions)) :subst subst))
	   (newsubst2 (subst~compose-substitution subst newsubst))
	   (unification (pres=unify-with-subterms (cdr flist) formula newsubst2)))
      (if (symbolp unification)
	  (pres=subterm-in-formula flist formula (cdr positions) subst)
	unification))))

; Scheint zu funktionieren
(defun pres~compose (flist formula)
  (let* ((variables nil)
	 (variables2 (make-hash-table))
	 (unification (pres=unify-with-subterms flist formula (subst~create nil nil)))
	 (covered-vars (if (symbolp unification) nil (subst~domain unification))))
    (labels
					; Function to collect variables of term into "variables"
	((get-variables (term) (cond ((data~appl-p term)
				      (map nil #'(lambda (x) (get-variables x))
					   (data~appl-arguments term)))
				     ((data~abstr-p term)
				      (get-variables (data~abstr-range term)))
				     ((data~variable-p term)
				      (setq variables (adjoin term variables :test #'(lambda (x y) (data~equal x y)))))
				     (t nil)))
					; Function that creates a new term by replacing the variables as given in the hash-table "variables2"
	 (replace-variables (term) (cond ((data~appl-p term)
					  (data~appl-create (data~copy (data~appl-function term))
							    (mapcar #'replace-variables
								    (data~appl-arguments term))))
					 ((data~abstr-p term)
					  (data~abstr-create (mapcar #'replace-variables (data~abstr-domain term))
							     (replace-variables (data~abstr-range term))))
					 ((data~variable-p term)
					  (let ((v (gethash term variables2)))
					    (if v v term)))
					 (t term))))
					; Collect variables of formula into "variables"
      (get-variables formula)
					; Delete variables in "variables" that are covered by a formula in "flist"
      (setq variables (remove-if #'(lambda (v) (member v covered-vars :test #'data~equal)) variables))
					; Create hash-table "variables2" that maps variables of the formula into meta-variables
      (map nil #'(lambda (v) (setf (gethash v variables2)
				   (meta~variable-create (keim~name v) (data~annotation v))))
	   variables)
					; Replace the variables of the formula by meta-variables
      (replace-variables formula))))

; Könnte klappen
(defun pres~match (formula1 formula2)
  (cond ((and (data~appl-p formula1) (string-equal (keim~name (data~appl-function formula1)) 'forall))
	 (pres~match (data~abstr-range (first (data~appl-arguments formula1))) formula2))
	((and (data~appl-p formula2) (string-equal (keim~name (data~appl-function formula2)) 'forall))
	 (pres~match formula1 (data~abstr-range (first (data~appl-arguments formula2)))))
	(t (uni~unify-two-terms formula1 formula2))))

; Scheint zu funktionieren
(defun pres~genericnode (node)
					; Get generic form of axiom from a list of justifications
  (labels ((getgeneric (justlist)
		       (cond ((null justlist) nil) ; List empty->no generic form
					; The first justification has foralle justification->could lead to axiom
			     ((string-equal (keim~name (pres~method (car justlist))) 'foralle)
					; Try to get an axiom out of this
			      (let ((gen (pres~genericnode (pres~node (car (pres~premises (car justlist)))))))
					; If successful give it back, else retry with cdr of list
				(if gen gen (getgeneric (cdr justlist)))))
					; else try with cdr of list
			     (t (getgeneric (cdr justlist))))))
    (if (null (pres~premises (car (pres~justlist node)))) node
      (getgeneric (pres~justlist node)))))

(defun pres~generic (node)
  (let ((genericnode (pres~genericnode node)))
    (if genericnode (pres~formula genericnode) nil)))

(defun pres~axiom (node)
  (let ((genericnode (pres~genericnode node)))
    (if genericnode (keim~name genericnode) nil)))

; Scheint zu funktionieren
; Vergleichsoperatoren fehlen noch
; Constant-Multiplier fehlt noch
(defun pres~abstracted (user formula)
  (cond ((and (member 'variable (pres~abstract user)) ; Does the user understand abstracted variables?
	      (data~variable-p formula)) ; Is the term a variable?
	 (list (meta~variable-create (keim~name formula) (data~annotation formula)) ; Abstract it
	       (data~copy formula)))		; and also leave it
	((and (member 'constant (pres~abstract user)) ; Does the user understand abstracted constants?
	      (data~constant-p formula)) ; Ist the term a constant?
	 (list (term~constant-create 'constantph (data~annotation formula))
	       (data~copy formula)))
	((and (member 'constantmultiplier (pres~abstract user)) ; Does user underst. abstr. of const. mult.?
	      (data~appl-p formula)			; Is term an application?
	      (pres~multiplier-p user (data~appl-function formula)) ; Is function a multiplier?
	      (or (and (data~constant-p (first (data~appl-arguments formula))) ; Is one of the args a
		       (data~variable-p (second (data~appl-arguments formula)))) ; constant and the other
		  (and (data~constant-p (second (data~appl-arguments formula))) ; a variable?
		       (data~variable-p (first (data~appl-arguments formula))))))
	 (append (list (meta~variable-create (gensym))) ; Abstract it
		 (pres=abstract-arguments user formula))) ; and also abstract the arguments
	((and (member 'equalityoperator (pres~abstract user)) ; Does the user underst. abstr. of equality operators?
	      (data~appl-p formula)          ; Is term an application?
	      (pres~equalityoperator-p user (data~appl-function formula)))   ; Is term an equality operator?
	 (append (pres=abstract-arguments user (data~appl-create        ; Abstract with a place holder as the operator
						(term~constant-create 'equalityph
								      (data~annotation (data~appl-function formula)))
						(data~appl-arguments formula)))
		 (pres=abstract-arguments user formula)))               ; Also abstract with the original operator
	((and (member 'negation (pres~abstract user)) ; Does the user underst. abstr. of negations?
	      (data~appl-p formula)          ; Is term an application?
	      (pres~negation-p user (data~appl-function formula)))   ; Is term a negation?
	 (append (pres~abstracted user (first (data~appl-arguments formula)))   ; Remove the negation
		 (pres=abstract-arguments user formula)))               ; also abstract with the negation left in
	((data~appl-p formula)					; Recursion into the arguments of an application
	 (pres=abstract-arguments user formula))
;	((data~abstr-p formula)
;	 (append (pres~abstracted user (data~abstr-range formula))
;		 (list (data~copy formula))))
	(t (list (data~copy formula)))))

; Scheint zu funktionieren
(defun pres=abstract-arguments (user formula)
  (cond ((data~appl-p formula)
	 (let* ((list1 (mapcar #'(lambda (el) (pres~abstracted user el)) (data~appl-arguments formula)))
		(result (list nil)))
	   (mapc #'(lambda (l)					; Create combinations of arguments (e.g ((a b) (c d e))=>
					; ((a c) (b c) (a d) (b d) (a e) (b e))
		     (setq result
			   (let* ((dummy nil))
			     (mapc #'(lambda (el)
				       (setq dummy (append dummy
							   (mapcar #'(lambda (el2) (append el2 (list el)))
								   result))))
				   l)
			     dummy)))
		 list1)
	   (mapcar #'(lambda (args)
		       (data~appl-create (data~appl-function formula) args))
		   result)))))

; Generalisation und Instance fehlen noch
; zu testen
(defun pres~coherent (user formula g)		; formula: formula, g: list of formula
  (labels ((listofands (formula)
		       (cond ((and (data~appl-p formula)
				   (string-equal (keim~name (data~appl-function formula)) 'and))
			      (append (listofands (first (data~appl-arguments formula)))
				      (listofands (second (data~appl-arguments formula)))))
			     (t (list formula))))
	   (andgequalsf (g formula)
			(let* ((listoffs (listofands formula)))
			  (and (every #'(lambda (el) (some #'(lambda (el2) (data~equal el el2)) g)) listoffs)
			       (every #'(lambda (el) (some #'(lambda (el2) (data~equal el el2)) listoffs)) g)))))
    (or (and (> (length g) 1) (andgequalsf g formula))
	(and (= (length g) 1)
	     (or (data~substruct-p formula (first g))
		 (some #'(lambda (x) (some #'(lambda (y) (pres~match x y))
					   (pres~abstracted user formula)))
		       (pres~abstracted user (first g))))))))

; Zu testen
(defun pres~subst (n p es)							; n: Node, p: Premise, es: List of ExtJust
  (mapcar #'(lambda (e)									; e: ExtJust
	      (make-instance
	       'pres+extjust
	       :method (pres~method e)
	       :mark (adjoin 'short-cut (pres~mark e) :test 'string-equal)
	       :premises (mapcar #'(lambda (p2) ; p2: Premise
				     (if (eq (pres~node p2) n)
					 (make-instance
					  'pres+premise
					  :mark (pres~mark p)
					  :node (pres~node p))
				       (make-instance
					'pres+premise
					:mark (pres~mark p2)
					:node (pres~node p2))))
				 (pres~premises e))))
	  es))

; Zu testen
(defun pres=addtoexplains (es n)				; es: List of ExtJust, n: ExtNode
  (map nil #'(lambda (e)								; e: ExtJust
	       (map nil #'(lambda (p)		; p: Premise
			    (setf (pres~explains (pres~node p))
				  (adjoin n (pres~explains (pres~node p)) :test #'(lambda (n1 n2) (eq n1 n2)))))
		    (pres~premises e)))
       es))

; Zu testen
(defun pres~no-expand2 (l p)						; l: extnode, p: premise
  (labels ((in-subtree (n1 n2)					; n1: extnode, n2: extnode
		       (cond ((eq n1 n2) t)
			     ((some #'(lambda (j)	; j: extjust
					(some #'(lambda (p) ; p: premise
						  (in-subtree n1 (pres~node p)))
					      (pres~premises j)))
				    (pres~justlist n2))
			      t)
			     (t nil))))
    (not (null (remove-if #'(lambda (s)	; s: extnode
			      (in-subtree s l))
			  (pres~explains (pres~node p)))))))

(defun pres~no-expand (l)
  (some #'(lambda (m) (string-equal m 'chainable)) (pres~mark l)))
