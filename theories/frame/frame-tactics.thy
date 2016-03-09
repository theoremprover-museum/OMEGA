 ;;; -*- syntax: common-lisp; package: OMEGA; base: 10; mode: Keim -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Equality Substitution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; =subst

(infer~deftactic =subst
		 (outline-mappings (((nonexistent existent existent) =subst-f)
                                    ((existent existent existent) =subst-a)
                                    ((existent existent nonexistent) =subst-l)
				    ((existent nonexistent existent) =subst-r)
                                    ))
		 (parameter-types position)
		 (expansion-function batac=expand-=subst)
		 (help "Replacement property of equality."))


(tac~deftactic =subst-f =subst (in frame)
   (parameters (position pos+position "A position."))
   (premises L1 L2)
   (conclusions L3)
   (computations (L3 (batac=equality-subst-create-f
                      (formula L1) (formula L2) position)))
   (sideconditions (batac=equality-subst-f-p
		    (formula L1) (formula L2) position))
   (description "Forward application equality-substitution."))


(tac~deftactic =subst-a =subst (in frame)
   (parameters (position pos+position "A position."))
   (premises L1 L2)
   (conclusions L3)
   (computations)
   (sideconditions (batac=equality-subst-a-p
                    (formula L3) (formula L1) (formula L2) position))
   (description "Closing equality substitution."))

(tac~deftactic =subst-l =subst (in frame)
   (parameters (position pos+position "A position."))
   (premises L1 L2)
   (conclusions L3)
   (computations (L2 (batac=equality-subst-create-l (formula L3) (formula L1) position))) 
   (sideconditions (batac=equality-subst-l-p (formula L3) (formula L1) position))
   (description "Creating equation for a substitution."))

(tac~deftactic =subst-r =subst (in frame)
   (parameters (position pos+position "A position."))
   (premises L1 L2)
   (conclusions L3)
   (computations (L1 (batac=equality-subst-create-f (formula L3) (formula L2) position))) 
   (sideconditions (batac=equality-subst-f-p
                    (formula L3) (formula L2) position))
   (description "Creating equation for a substitution."))

(defun batac=equality-subst-f-p (term equal-term position)
  (when (frame~equality? equal-term)
    (let* ((arg1 (first (data~appl-arguments equal-term)))
	   (arg2 (second (data~appl-arguments equal-term)))
	   (positions-of-arg1 (data~substruct-positions arg1 term :test 'data~equal))
	   (positions-of-arg2 (data~substruct-positions arg2 term :test 'data~equal)))
      (or (find position positions-of-arg1 :test 'keim~equal)
	  (find position positions-of-arg2 :test 'keim~equal)))))


(defun batac=equality-subst-create-f (term equal-term position)
  (let* ((term-at-position (data~struct-at-position term position))
	 (args (data~appl-arguments equal-term)))
    (cond ((data~equal term-at-position (first args))
	   (data~replace-at-position term position (second args)))
	  (t
	   (data~replace-at-position term position (first args))))))
	  

(defun batac=equality-subst-a-p (conclusion term equal-term position)
  (and (batac=equality-subst-f-p term equal-term position)
       (term~alpha-equal (batac=equality-subst-create-f term equal-term position) conclusion)))

(defun batac=equality-subst-l-p (conclusion term position)
  (let ((positions-of-conc (data~positions conclusion #'(lambda (arg) 't)))
	(positions-of-term (data~positions term #'(lambda (arg) 't))))
    (when (and (find position positions-of-conc :test 'keim~equal)
	       (find position positions-of-term :test 'keim~equal))
      (term~alpha-equal (data~replace-at-position conclusion
						  position
						  (data~struct-at-position term position))
			term))))
	     
(defun batac=equality-subst-create-l (conclusion term position)
  (term~appl-create (env~lookup-object := (pds~environment omega*current-proof-plan))
		    (list (data~struct-at-position conclusion position)
			  (data~struct-at-position term position))))



(defun batac=expand-=subst (outline parameters &optional (rec nil))
  (let* ((precond (second outline))
	 (equation (third outline))
	 (conc (first outline))
	 (=def (th~find-assumption "=" (prob~theory omega*current-proof-plan)))
	 (definiendum (th~definition-constant =def))
	 (definiens (data~copy (th~ass-node =def) :downto '(term+constant type+primitive)))
	 (pos (first parameters))
	 (inst1 (ignore-errors (data~appl-function (beta~expand (node~formula precond) (list pos)))))
	 (term-at-pos (ignore-errors (data~struct-at-position (node~formula precond) pos)))
	 (term-at-pos2 (ignore-errors (data~struct-at-position (node~formula conc) pos)))
	 (left-eq (car (data~appl-arguments (node~formula equation))))
	 (right-eq (cadr (data~appl-arguments (node~formula equation)))))
    (cond ((and term-at-pos term-at-pos2
		(data~equal term-at-pos left-eq);; precond: Ca  equation: a=b  conc: Cb
		(data~equal term-at-pos2 right-eq))
	   (tacl~init outline)
	   (tacl~sequence
	    (defne-res ('defne (list nil equation) (list definiendum definiens
							 (pos~add-front 0))))      ;;; leibab equation ...
	    (foralle-res ('foralle (list nil (car defne-res)) (list inst1)))  ;;; ((lamC)a => (lamC)b) leibab
                      ;;; nun rueckwaerts
	    (impe-res ('impe (list conc  precond nil) nil))    ;;; Cb Ca (Ca => Cb)
	    (lambda-res ('lambda (list (third impe-res) (first foralle-res)) nil)))		     
	   (tacl~end))
	  ((and term-at-pos term-at-pos2
		(data~equal term-at-pos right-eq);; precond: Ca  equation: b=a  conc: Cb
		(data~equal term-at-pos2 left-eq))
	   (tacl~init outline)   ;;;; das hier direct expandieren
	   (tacl~sequence
	    (=sym-res ('=sym (list nil equation) nil))
	    (=subst-res ('=subst (list conc precond (car =sym-res)) parameters)))
	   (tacl~end))
	  ((not rec)               ;;;;;;;;; Hack for non-compatible numerical functions!!!!!!!!!   VS
	   (batac=expand-=subst (mapcar #'natac~num2func-line outline) parameters 1))
	  ((= rec 1)               ;;;;;;;;; Hack for non-compatible numerical functions!!!!!!!!!   VS
	   (batac=expand-=subst (mapcar #'natac~func2num-line outline) parameters t))
	  (t (omega~error "Application of =-Subst cannot be expanded. The problematic terms are:~% ~A   <--->   ~A~% OR~% ~A   <--->   ~A~%In case numbers are involved try to establish the same representation everywhere!" term-at-pos left-eq term-at-pos right-eq)))))

	    

(com~defcommand =subst
  (argnames line1 line2 equality-line position)
  (argtypes ndline ndline ndline position)
  (arghelps "The substituted line" "The unsubstituted line"
            "The equation to be applied." "A position.")
  (function batac==subst)
  (frag-cats tactics base-equality)
  (defaults ((oc~default-current-planline) (com~unspecified) (com~unspecified) (com~unspecified)))
  (level 1)
  (log-p T)
  (help "Equality-Substitution."))

(defun batac==subst (P P2 P3 position)
  (infer~compute-outline '=subst (list P P2 P3) (list position)))



;;; =subst*

(infer~deftactic =subst*
		 (outline-mappings (((nonexistent existent existent) =subst*-f)
                                    ((existent existent existent) =subst*-a)
                                    ((existent existent nonexistent) =subst*-l)
				    ((existent nonexistent existent) =subst*-r)
                                    ))
		 (parameter-types position-list)
		 (expansion-function batac=expand-=subst*)
		 (help "Simultaneous replacement of several subterms with one equality."))


(tac~deftactic =subst*-f =subst* (in frame)
   (parameters (positions cons "A list of positions."))
   (premises L1 L2)
   (conclusions L3)
   (computations (L3 (batac=equality-subst*-create-f
                      (formula L1) (formula L2) positions)))
   (sideconditions (batac=equality-subst*-f-p
		    (formula L1) (formula L2) positions))
   (description "Forward application of simultaneous equality-substitution."))


(tac~deftactic =subst*-a =subst* (in frame)
   (parameters (positions cons "A list of positions."))
   (premises L1 L2)
   (conclusions L3)
   (computations)
   (sideconditions (batac=equality-subst*-a-p
                    (formula L3) (formula L1) (formula L2) positions))
   (description "Closing simultaneous equality substitution."))

(tac~deftactic =subst*-l =subst* (in frame)
   (parameters (positions cons "A list of positions."))
   (premises L1 L2)
   (conclusions L3)
   (computations (L2 (batac=equality-subst*-create-l (formula L3) (formula L1) positions))) 
   (sideconditions (batac=equality-subst*-l-p (formula L3) (formula L1) positions))
   (description "Creating equation for a simultaneous substitution."))

(tac~deftactic =subst*-r =subst* (in frame)
   (parameters (positions cons "A list of positions."))
   (premises L1 L2)
   (conclusions L3)
   (computations (L1 (batac=equality-subst*-create-f (formula L3) (formula L2) positions))) 
   (sideconditions (batac=equality-subst*-f-p
                    (formula L3) (formula L2) positions))
   (description "Backward application of simultaneous equality-substitution."))

(defun batac=equality-subst*-f-p (term equal-term positions)
  (when (frame~equality? equal-term)
    (let* ((arg1 (first (data~appl-arguments equal-term)))
	   (arg2 (second (data~appl-arguments equal-term)))
	   (positions-of-arg1 (data~substruct-positions arg1 term :test 'data~equal))
	   (positions-of-arg2 (data~substruct-positions arg2 term :test 'data~equal)))
      (every #'(lambda (position) (or (find position positions-of-arg1 :test 'keim~equal)
				      (find position positions-of-arg2 :test 'keim~equal)))
	     positions))))


(defun batac=equality-subst*-create-f (term equal-term positions)
  (let* ((terms-at-position (mapcar #'(lambda (position) (data~struct-at-position term position)) positions))
	 (args (data~appl-arguments equal-term))
	 (farg (car args))
	 (sarg (cadr args)))
    (labels ((recursive-subst (term repterms positions)
			      (if (and repterms positions)
				  (let ((fterm (car repterms))
					(fpos (car positions)))
				    (if (data~equal fterm farg)
					(recursive-subst
					 (data~replace-at-position term fpos sarg)
					 (cdr repterms) (cdr positions))
				      (recursive-subst
				       (data~replace-at-position term fpos farg)
				       (cdr repterms) (cdr positions))))
				term)))
      (recursive-subst term terms-at-position positions))))
	  

(defun batac=equality-subst*-a-p (conclusion term equal-term positions)
  (and (batac=equality-subst*-f-p term equal-term positions)
       (term~alpha-equal (batac=equality-subst*-create-f term equal-term positions) conclusion)))

(defun batac=equality-subst*-l-p (conclusion term positions)
  (let ((positions-of-conc (data~positions conclusion #'(lambda (arg) 't)))
	(positions-of-term (data~positions term #'(lambda (arg) 't))))
    (when (every #'(lambda (position) (and (find position positions-of-conc :test 'keim~equal)
					   (find position positions-of-term :test 'keim~equal)))
		 positions)
      (let* ((term-list (mapcar #'(lambda (pos) (data~struct-at-position term pos)) positions))
	     (conc-list (mapcar #'(lambda (pos) (data~struct-at-position conclusion pos)) positions))
	     (members (remove-duplicates (append term-list conc-list) :test #'data~equal)))
	(and (or (= (length members) 1)
		 (and
		  (= (length members) 2)
		  (let ((fm (first members))
			(sm (second members)))
		    (every #'(lambda (t1 t2)
			       (or (and (data~equal t1 fm) (data~equal t2 sm))
				   (and (data~equal t2 fm) (data~equal t1 sm))))
			   term-list conc-list))))
	     (labels ((recursive-subst (conc positions terms)
				       (if (and positions terms)
					   (recursive-subst
					    (data~replace-at-position conc
								      (car positions)
								      (car terms))
					    (cdr positions) (cdr terms))
					 conc)))
	       (term~alpha-equal (recursive-subst conclusion positions term-list) term)))))))
	     
(defun batac=equality-subst*-create-l (conclusion term positions)
  (term~appl-create (env~lookup-object := (pds~environment omega*current-proof-plan))
		    (list (data~struct-at-position conclusion (car positions))
			  (data~struct-at-position term (car positions)))))

(defun batac=expand-=subst* (outline parameters)
  (let* ((precond (second outline))
	 (equation (third outline))
	 (conc (first outline))
	 (pos-list (first parameters)))
    (tacl~init outline)
    (dolist (pos (butlast pos-list))
      (setf conc (second (tacl~apply '=subst (list conc nil equation) (list pos)))))
    (tacl~apply '=subst (list conc precond equation) (last pos-list))
    (tacl~end)))
	  

(com~defcommand =subst*
  (argnames line1 line2 equality-line position)
  (argtypes ndline ndline ndline position-list)
  (arghelps "The substituted line" "The unsubstituted line"
            "The equation to be applied" "A list of positions")
  (function batac==subst*)
  (frag-cats tactics base-equality)
  (defaults ((oc~default-current-planline) (com~unspecified) (com~unspecified) (com~unspecified)))
  (level 1)
  (log-p T)
  (help "Equality-Substitution in several sub-terms."))

(defun batac==subst* (P P2 P3 positions)
  (infer~compute-outline '=subst* (list P P2 P3) (list positions)))


;;; =subst**

(infer~defwild-tactic =subst**
		 (outline-mappings (((nonexistent list) =subst**-f)
                                    ((existent list) =subst**-b)
                                    ))  ;;; The a-direction is subsumed by the b-direction!
		 (parameter-types position-list-list)
		 (expansion-function batac=expand-=subst**)
		 (help "Simultaneous replacement of several subterms with respect to a list of equalities."))


(defun =subst**-f (concs prems parameters)
  (declare (ignore concs))
  (let ((prem (car prems))
	(equations (cdr prems))
	(positions (car parameters)))
    (when (and (= (length equations) (length positions))
	       (batac=equality-subst**-f-p prem equations positions))
      (values (list (batac=equality-subst**-create-f prem equations positions)) nil nil))))

(defun batac=equality-subst**-f-p (term equations positions)
  (every #'(lambda (eq pos) (batac=equality-subst*-f-p term eq pos))
	 equations positions))

(defun batac=equality-subst**-create-f (term equations positions)
  (let ((new-term term))
    (mapc #'(lambda (eq pos)
	      (setf new-term (batac=equality-subst*-create-f new-term eq pos)))
	  equations positions)
    new-term))

(defun =subst**-b (concs prems parameters)
  (let ((conc (car concs))
	(positions (car parameters)))
    (multiple-value-bind (prem equations)
	(batac==subst**-prems&equations prems (length positions))
      (cond ((and prem equations)                                 ;;; the a-direction
	     (when (batac=equality-subst**-a-p conc prem equations positions)
	       t))
	    (equations                                      ;;; the actual b-direction
	     (when (batac=equality-subst**-f-p conc equations positions)
	       (values nil (list (batac=equality-subst**-create-f conc equations positions)) nil)))
  ))))

(defun batac==subst**-prems&equations (list n)
  (declare (edited  "09-MAR-2000")
	   (authors Sorge)
	   (input   "A list and a number.")
	   (effect  "None.")
	   (value   "Two values: the car and the cdr of LIST if the length of LIST is N+1,"
		    "NIL and LIST if the length of LIST is equal to N, otherwise NIL."))
  (cond ((= (length list) (1+ n)) (values (car list) (cdr list)))
	((= (length list) n) (values nil list))))
  

(defun batac=equality-subst**-a-p (conclusion term equations positions)
  (and (batac=equality-subst**-f-p term equations positions)
       (term~alpha-equal (batac=equality-subst**-create-f term equations positions) conclusion)))

(defun batac=expand-=subst** (conclusions premises parameters)
  (let* ((precond (first premises))
	 (equations (rest premises))
	 (conc (first conclusions))
	 (positions (first parameters)))
    (tacl~init (append conclusions premises))
    (mapc #'(lambda (eq pos)
	      (setf conc (second (tacl~apply '=subst* (list conc nil eq) (list pos)))))
	  (butlast equations) (butlast positions))
    (tacl~apply '=subst* (list conc precond (car (last equations))) (last positions))
    (tacl~end)))
	  

(com~defcommand =subst**
  (argnames line1 line2 equality-lines position-lists)
  (argtypes ndline ndline ndline-list position-list-list)
  (arghelps "The substituted line" "The unsubstituted line"
            "A list of equations to be applied" "A list of position-lists")
  (function batac==subst**)
  (frag-cats tactics base-equality)
  (defaults ((oc~default-current-planline) (com~unspecified) (com~unspecified) (com~unspecified)))
  (level 1)
  (log-p T)
  (help "Equality-Substitution in several sub-terms with respect to a list of equations."))

(defun batac==subst** (P P2 P3 positions)
  (infer~compute-outline '=subst** (list P (if p2 (cons P2 P3) p3)) (list positions)))

