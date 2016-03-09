(in-package :keim)



(mod~defmod TERM+ 
            :uses (data env keim meta mod pos post subst type)
            :documentation "Datastructures with additional functionaltiy for special representations."
            :exports (
		      term+annotated-constant
		      term+annotated-term
		      term+markup
                      term+appl
                      term+block
                      term+constant
                      term+cyc
                      term+ddots
                      term+ellipsis
                      term+hdots
                      term+list
                      term+matrix
                      term+multi-set
                      term+set
                      term+term
                      term+tuple
                      term+vdots
                      term+vector
                      
                      term~abstr-create
                      term~alpha-equal
                      term~annotated-constant-p
                      term~annotated-constant-specified
                      term~annotation
                      term~appl-create
                      term~block-columns
                      term~block-p
                      term~block-rows
                      term~constant-p
                      term~create-block
                      term~create-matrix
                      term~create-set
                      term~cyc-p
                      term~ddots-p
                      term~disagreement-set
                      term~ellipsis-begin
                      term~ellipsis-computable
                      term~ellipsis-element
                      term~ellipsis-end
                      term~ellipsis-p
                      term~ellipsis-range
                      term~generate-term-primitive-with-new-name
                      term~hdots-p
                      term~list-p
                      term~matrix-columns
                      term~matrix-p
                      term~matrix-rows
                      term~matrix-update-ellipses
                      term~multi-set-p
                      term~normalform
                      term~number-p
                      term~schema-create
                      term~set-p
                      term~special-p
                      term~tuple-p
                      term~type
                      term~vdots-p
                      term~vector-dimension
                      term~vector-p

		      term~annotated-term-p 
		      term~markup-p
		      term~gen-cyc-p
		      term~gen-set-p 
                      ))




(eval-when (load compile eval)
  (defclass term+annotated-constant (term+constant)
    ((annotation :initform nil
		 :initarg :annotation
		 :accessor term~annotation)
     (specified :initform nil
		:initarg :specified
		:accessor term~annotated-constant-specified))
    (:documentation "The superclass of annotated constants."))
  (defclass term+cyc (term+annotated-constant)
    ()
    (:documentation "The class of elements of a cycle."))
  (defclass term+list (term+annotated-constant)
    ()
    (:documentation "The class of elements of a list."))
  (defclass term+set (term+annotated-constant)
    ()
    (:documentation "The class of elements of a set."))
  (defclass term+multi-set (term+annotated-constant)
    ()
    (:documentation "The class of elements of a multi-set."))
  (defclass term+tuple (term+annotated-constant)
    ()
    (:documentation "The class of tuples."))
  (defclass term+vector (term+annotated-constant)
    ((dimension :initform 0
		:initarg :dimension
		:accessor term~vector-dimension))
    (:documentation "The class of vectors."))
  (defclass term+matrix (term+annotated-constant)
    ((rows :initform 0
	   :initarg :rows
	   :accessor term~matrix-rows)
     (columns :initform 0
	      :initarg :columns
	      :accessor term~matrix-columns))
    (:documentation "The class of matrices."))
  (defclass term+block (term+annotated-constant)
    ((rows :initform 0
	   :initarg :rows
	   :accessor term~block-rows)
     (columns :initform 0
	      :initarg :columns
	      :accessor term~block-columns))
    (:documentation "The class of blocks."))
)


(eval-when (load compile eval) 
  (defclass term+annotated-term (term+appl)
    ()
    (:documentation "The class of annotated terms."))

  (defclass term+markup (term+constant)
    ()
    (:documentation "The class of mark-up terms for annotated terms."))
  )



(defun term~annotated-constant-p (thing)
  (typep thing 'term+annotated-constant))

(defun term~set-p (thing)
  (typep thing 'term+set))

(defun term~multi-set-p (thing)
  (typep thing 'term+multi-set))

(defun term~list-p (thing)
  (typep thing 'term+list))

(defun term~cyc-p (thing)
  (typep thing 'term+cyc))

(defun term~tuple-p (thing)
  (typep thing 'term+tuple))

(defun term~vector-p (thing)
  (typep thing 'term+vector))

(defun term~matrix-p (thing)
  (typep thing 'term+matrix))

(defun term~block-p (thing)
  (typep thing 'term+block))

(defun term~special-p (thing)
  (or (term~number-p thing)
      (term~annotated-constant-p thing)))

;;; method has to be used with care, since it does not check for correctness of input !! 

; list or set??
; problems with term-constant "nil"!
#|
(defmethod term~constant-create ((set list) (type type+type))
  (let ((tconst (data~constant-create 'term+set :name set)) )
    (setf (data~annotation tconst) type)
    (setf (data~constant-origin tconst) tconst)
    tconst))

;;; not sure whether that function should even exist. VS
(defmethod term~constant-create ((set list) type)
  (declare (ignore type))
  (let* ((types (mapcar #'term~type set))
	 (ftype (car types)))
    (if (every #'(lambda (x) (keim~equal ftype x)) types)
	(let ((tconst (data~constant-create 'term+set :name set)))
	  (setf (data~annotation tconst) (type~func-create (type~o) ftype))
	  (setf (data~constant-origin tconst) tconst)
	  tconst)
      (error ";; TERM~~CONSTANT-CREATE: Elements of ~A must be of the same type." set))))

|#

(defmethod post~read-object (term (env env+environment)
				  (indicator (eql :existing-term)))
  (cond ((symbolp term)
	 
	 ;; primitiver Term -> aus Environment lesen
	 
	 (or
	  ;; if the symbol denotes an ellipese
	  (post~read-object term env :ellipsis)
	  ;; otherwise it is a constant
	  (term=env-lookup-symbol term env)))
	
	((and (not (listp (first term)))
	      (string= (string (first term)) "GROUNDED"))

	 ;; zu groundender Term

	 (let* ((ground-types (mapcar #'(lambda (type-list)
					  (let* ((type (post~read-object type-list env :existing-type)))
					    (if (type~type-variables type)
						(error "~% Type ~A contains type-variables but has to be ground.")
					      type)))
				      (second term)))
		(term-to-ground (post~read-object (third term) env :existing-term)))

	   (if (null (data~schema-p term-to-ground))
	       term-to-ground
	     (let* ((domain (data~schema-domain term-to-ground))
		    (min-length (min (length domain) (length ground-types)))
		    (domain-vars (subseq domain 0 min-length))
		    (according-ground-types (subseq ground-types 0 min-length))
		    (rest-domain-vars (subseq domain min-length))
		    (new-term (data~replace-free-variables (data~schema-range term-to-ground) domain-vars according-ground-types)))

	       (if (null rest-domain-vars)
		   new-term
		 (term~schema-create new-term :kappas rest-domain-vars))))))
	
	((and (not (listp (first term)))
	      (string= (string (first term)) "ALL-TYPES"))

	 ;; Kappa Term
	 
	 (let* ((syn-type-variables (rest (butlast term)))
		(rest-term (first (last term)))
		(type-vars (post~read-object syn-type-variables env :type-constants-multiple))
		(the-term (post~read-object rest-term env :existing-term))                         
		(new-term (if (data~schema-p the-term) (data~schema-range the-term) the-term)))    ;; (all-types aa const) MP


	   ;; type-vars werden zunaechst als constanten eingelesen, damit sie waehrend des Termaufbaus nicht gebunden werden koennen
	   ;; jetzt aber werden wir sie in type-variablen umaendern !!

	   (mapcar #'(lambda (type-var)
		       (change-class type-var 'type+variable))
		   type-vars)
	   
	   ;; rauswerfen der type-variables aus dem environment
	   (mapcar #'(lambda (var)
		       (env~remove var env))
		   syn-type-variables)

	   (term~schema-create new-term
			       :kappas type-vars)
	   ))
	((and (not (listp (first term)))
	      (string= (string (first term)) "LAM"))

	 ;; Lambda Abstraktion
	 
	 (let* ((vars-des (rest (reverse (rest (reverse term)))))
		(normal-vars (remove-if-not #'(lambda (var-decl)
						(= (length var-decl) 2))
					    vars-des))
		(ss-vars (remove-if-not #'(lambda (var-decl)
					    (and (= (length var-decl) 3)
						 (not (equal (third var-decl) :lookup))))
					vars-des))
		(lookup-vars (remove-if-not #'(lambda (var-decl)
						(and (= (length var-decl) 3)
						     (equal (third var-decl) :lookup)))
					    vars-des))
		
		(variables (post~read-object normal-vars env :variables-multiple))
		(ss-variables (post~read-object ss-vars env :ss-variables))
		(lookup-variables (post~read-object lookup-vars env :lookup-variables))
		(type-var-subst (if (env~lookup-object 'type-var-subst env)
				    (env~lookup-object 'type-var-subst env)
				  (subst~create nil nil)))
		(scope (post~read-object (first (last term)) env :existing-term))
		(new-term (term~abstr-create (append variables ss-variables lookup-variables) scope
					     :mode :n-normalize
					     :tnormalize 't  
					     )))
	   (mapcar #'(lambda (var)
		       (env~remove (first var) env))
		   normal-vars)
	   new-term))
	 ;; SETS
	((and (not (listp (first term))) (string= (string (first term)) "SET"))
	 	 (post~read-object (rest term) env :set))
	 ;; LISTS
	((and (not (listp (first term))) (string= (string (first term)) "LIST"))
	 (post~read-object (rest term) env :list))
	 ;; CYCLES
	((and (not (listp (first term))) (string= (string (first term)) "CYC"))
	 (post~read-object (rest term) env :cycle))
	 ;; TUPLES
	((and (not (listp (first term))) (string= (string (first term)) "TUPLE"))
	 (post~read-object (rest term) env :tuple))
	 ;; VECTORS
	((and (not (listp (first term))) (string= (string (first term)) "VEC"))
	 (post~read-object (rest term) env :vector))
	 ;; MATRICES
	((and (not (listp (first term))) (string= (string (first term)) "MAT"))
	 (post~read-object (rest term) env :matrix))
	 ;; BLOCKS
	((and (not (listp (first term))) (string= (string (first term)) "BLOC"))
	 (post~read-object (rest term) env :block))
	 ;; MULTI-SETS
	((and (not (listp (first term))) (string= (string (first term)) "MULTI-SET"))
	 	 (post~read-object (rest term) env :multi-set))
	 ;; Application
	(t
	 
	 (let* ((type-var-subst (if (env~lookup-object 'type-var-subst env)
				    (env~lookup-object 'type-var-subst env)
				  (subst~create nil nil)))
		(sub-terms (mapcar #'(lambda (sub-term)
				       (post~read-object sub-term env :existing-term))
				   term)))
	   (term~appl-create (first sub-terms) (rest sub-terms)
			     :mode :n-normalize
			     :tnormalize 't 
			     )))))

(defmethod post~read-object ((elements list)
			     (env env+environment) 
			     (indicator (eql :set)))
  (term~create-set (mapcar #'(lambda (elem) (post~read-object elem env :existing-term)) elements) env))

(defun term~create-set (elems env)
  (let* ((elements (remove-duplicates
		 elems
		 :test #'term~alpha-equal))
	 (types (mapcar #'term~type elems))
	 (ftype (car types)))
    (if (every #'(lambda (x) (data~equal ftype x)) types)
	(if (some #'term~free-variables elements)
	    (term~annotated-term-create elements (type~func-create  ftype (type~o)) env :set)
	  (let ((tconst (data~constant-create 'term+set :name elems)))
	    (setf (data~annotation tconst) (type~func-create  ftype (type~o)))
	    (setf (data~constant-origin tconst) tconst)
	    tconst))
      (error ";; TERM~~CREATE-SET: Elements of ~A must be of the same type." elements))))

(defmethod post~read-object ((elements list)
			     (env env+environment) 
			     (indicator (eql :multi-set)))
  (let ((elems  (mapcar #'(lambda (elem)  (post~read-object elem env :existing-term)) elements)))
    (if (some #'term~free-variables elems)
	(term~annotated-term-create elems (type~multi-set) env :multi-set)
      (let ((tconst (data~constant-create 'term+multi-set :name elems)))
	(setf (data~annotation tconst) (type~multi-set))
	(setf (data~constant-origin tconst) tconst)
	tconst))))

(defmethod post~read-object ((elements list)
			     (env env+environment) 
			     (indicator (eql :list)))
  (let ((elems  (mapcar #'(lambda (elem)  (post~read-object elem env :existing-term)) elements)))
    (if (some #'term~free-variables elems)
	(term~annotated-term-create elems (type~list) env :list)
      (let ((tconst (data~constant-create 'term+list :name elems)))
	(setf (data~annotation tconst) (type~list))
	(setf (data~constant-origin tconst) tconst)
	tconst))))

(defmethod post~read-object ((elements list)
			     (env env+environment) 
			     (indicator (eql :cycle)))
  (let ((elems  (mapcar #'(lambda (elem)  (post~read-object elem env :existing-term)) elements)))
    (if (some #'term~free-variables elems)
	(term~annotated-term-create elems (type~cyc) env :cyc)
      (let ((tconst (data~constant-create 'term+cyc :name elems)))
	(setf (data~annotation tconst) (type~cyc))
	(setf (data~constant-origin tconst) tconst)
	tconst))))

(defmethod post~read-object ((elements list)
			     (env env+environment) 
			     (indicator (eql :tuple)))
  (let* ((elems  (mapcar #'(lambda (elem)  (post~read-object elem env :existing-term)) elements))
	 (annotation (term=create-tuple-type elems)))
    (if (some #'term~free-variables elems)
	(term~annotated-term-create elems annotation env :tuple)
      (let ((tconst (data~constant-create 'term+tuple :name elems)))
	(setf (data~annotation tconst) annotation)
	(setf (data~constant-origin tconst) tconst)
	tconst))))

(defun term=create-tuple-type (elems)
  (declare (edited  "05-JUN-2003")
	   (authors Vxs)
	   (input   "A list of terms.")
	   (effect  "Creates a new tuple type recursively from the types of the elements.")
	   (value   "The newly created type."))
  (cond ((<= (length elems) 1) (error "Too few elements when creating a tuple type!"))
	((= (length elems) 2)
	 (type~appl-create (type~tuple) (list (term~type (car elems)) (term~type (cadr elems)))))
	(t (type~appl-create (type~tuple)
			     (list (term~type (car elems))
				   (term=create-tuple-type (cdr elems)))))))

;;;;  VS: Annotated Terms inserted up to here.


(defmethod post~read-object ((elements list)
			     (env env+environment) 
			     (indicator (eql :vector)))
  (let* ((elems  (mapcar #'(lambda (elem)  (post~read-object elem env :existing-term)) elements))
	 (types (mapcar #'term~type elems))
	 (ftype (car types)))
    (if (every #'(lambda (x) (data~equal ftype x)) types)
	(let ((tconst (data~constant-create 'term+vector :name elems))
	      (num-type (type~num)))
	  (setf (term~vector-dimension tconst) (length elems))
	  (setf (data~annotation tconst) (type~func-create  num-type ftype))   ;;; This should be num type
	  (setf (data~constant-origin tconst) tconst)
	  tconst)
      (error ";; POST~~READ-OBJECT: Elements of vector ~A must be of the same type." elements))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stuff for Matrices
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod post~read-object ((elements list)
			     (env env+environment) 
			     (indicator (eql :matrix)))
  (term~create-matrix (mapcar #'(lambda (rows)
				  (mapcar #'(lambda (elem) (post~read-object elem env :existing-term)) rows))
			      elements)
		      env))

(defun term~create-matrix (matrix env)
  (declare (edited  "15-JUN-2004")
	   (authors Sorge)
	   (input   "A matrix representation and an environment.")
	   (effect  "Create a new instence of TERM+MATRIX.")
	   (value   "The newly created instance."))
  (if (every #'listp matrix)
      (let ((columns (length (first matrix))))
	(if (every #'(lambda (x) (= (length x) columns)) matrix)
	    (multiple-value-bind (ellipses computable)
		(term~matrix-update-ellipses matrix env)
	      (if (and ellipses (not computable))
		  (error ";; TERM~~CREATE-MATRIX: Not all ellipses in matrix ~A are computable." matrix)
		(let* ((types (mapcar #'term~type (apply #'append matrix)))
		       (ftype (car types)))
		  (if (every #'(lambda (x) (data~equal ftype x)) types)
		      (let ((tconst (data~constant-create 'term+matrix :name matrix))
			    (num-type (type~num)))
			(unless ellipses
			  (setf (term~matrix-rows tconst) (length matrix))
			  (setf (term~matrix-columns tconst) columns))
			(setf (data~annotation tconst) (type~func-create  (list num-type num-type) ftype)) ;;; This should be num type
			(setf (data~constant-origin tconst) tconst)
			 tconst)
		    (error ";; TERM~~CREATE-MATRIX: Elements of matrix ~A must be of the same type." matrix)))))
	  (error ";; TERM~~CREATE-MATRIX: Rows of matrix ~A must be of equal length." matrix)))
    (error ";; TERM~~CREATE-MATRIX: Rows of matrix ~A must be lists of elements." matrix)))


(defun term~matrix-update-ellipses (matrix env)
  (declare (edited  "14-MAY-2004")
	   (authors Sorge)
	   (input   "A list of lists representing a matrix. The elements are already terms or ellipses."
		    "Additonally an environment.")
	   (effect  "Updates the paremeters of possibly contained ellipses.")
	   (value   "The updated matrix list."))
  (let ((i 0) ellipses)
    (dolist (row matrix)
      (incf i)
      (let ((j 0))
      (dolist (col row)
	(incf j)
	(when (term~ellipsis-p col)
	  (push (list (list i j) col) ellipses)))))
    (dolist (el (reverse ellipses) matrix)
      (term=matrix-update-single-ellipsis (cadr el) (caar el) (cadar el) matrix env))
    (let ((ells (mapcar #'second ellipses)))
      (values ells
	      (every #'term~ellipsis-computable ells)))))

(defun term=matrix-update-single-ellipsis (ellipsis row column matrix env)
  (declare (edited  "14-MAY-2004")
	   (authors Sorge)
	   (input   "An ellipsis, its row and its column position in the matrix, which is given as"
		    "a list of lists representing a matrix. Additionally an environment.")
	   (effect  "Updates slots of ellipsis.")
	   (value   "The updated ellipsis."))
  ;; In the following little functions x are the columns and y are the rows.
  (let (begin end)     ;;; these are the coordinates that store the beginning and the end of the ellipsis
  (flet ((left-elem (x y)
		    (when (and (> x 1) (> y 0))
		      (nth (- x 2) (nth (1- y) matrix))))
	 (right-elem (x y)
		     (when (and (> x 0) (> y 0))
		       (nth x (nth (1- y) matrix))))
	 (up-elem (x y)
		  (when (and (> x 0) (> y 1))
		    (nth (1- x) (nth (- y 2) matrix))))
	 (down-elem (x y)
		    (when (and (> x 0) (> y 0))
		      (nth (1- x) (nth y matrix))))
	 (up-left-elem (x y)
		       (when (and (> x 1) (> y 1))
			 (nth (- x 2) (nth (- y 2) matrix))))
	 (down-right-elem (x y)
			  (when (and (> x 0) (> y 0))
			    (nth x (nth y matrix))))
	 (finalise-ellipsis (ellipsis)
			    (cond ((term~ellipsis-element ellipsis) 
				   (setf (term~type ellipsis) (term~type (term~ellipsis-begin ellipsis)))
				   (setf (data~constant-origin ellipsis) ellipsis)
				   (setf (term~ellipsis-computable ellipsis) t))
				  ((and (term~ellipsis-begin ellipsis) (term~ellipsis-end ellipsis))
				   (multiple-value-bind (element range)
				       (term=ellipsis-element&range (term~ellipsis-begin ellipsis) (term~ellipsis-end ellipsis)
								    env)
				     (if element
					 (progn
					   (setf (term~ellipsis-element ellipsis) element)
					   (setf (term~ellipsis-range ellipsis) range)
					   (setf (term~type ellipsis) (term~type (term~ellipsis-begin ellipsis)))
					   (setf (data~constant-origin ellipsis) ellipsis)
					   (setf (term~ellipsis-computable ellipsis) t))
				       (setf (term~ellipsis-computable ellipsis) nil))))
				  (t (setf (term~ellipsis-computable ellipsis) nil)))
			    ellipsis))
    (macrolet ((walk-matrix (initial direction predicate side end)
			    `(do* (,@initial
				   (elem ,direction ,direction))
				 ((or (null elem)
				      (and (not (,predicate elem))
					   (term~ellipsis-p elem))
				      (,side ellipsis))
				  ,end)
			       (if (,predicate elem)
				   (setf (,side ellipsis) (,side elem)
					 (term~ellipsis-range ellipsis) (term~ellipsis-range elem)
					 (term~ellipsis-element ellipsis) (term~ellipsis-element elem))
				 (setf (,side ellipsis) elem)))))
      (cond ((term~hdots-p ellipsis)
	     ;; We check to the left and to the right. Go over other hdots.
	     ;; In case we reach a different kind of ellipsis it is undefined.
	     (walk-matrix ((x column (1- x))) (left-elem x row) term~hdots-p term~ellipsis-begin (setf begin (list (1- x) row)))
	     (walk-matrix ((x column (1+ x))) (right-elem x row) term~hdots-p term~ellipsis-end (setf end (list (1+ x) row)))
	     (finalise-ellipsis ellipsis))
	    ((term~vdots-p ellipsis)
	     ;; We check above and below. Go over other vdots.
	     ;; In case we reach a different kind of ellipsis it is undefined.
	     (walk-matrix ((y row (1- y))) (up-elem column y) term~vdots-p term~ellipsis-begin (setf begin (list column (1- y))))
	     (walk-matrix ((y row (1+ y))) (down-elem column y) term~vdots-p term~ellipsis-end (setf end (list column (1+ y))))
	     (finalise-ellipsis ellipsis))
	    ((term~ddots-p ellipsis)
	     ;; We check diagonal left above and diagonal right below. Go over other ddots.
	     ;; In case we reach a different kind of ellipsis it is undefined.
	     (walk-matrix ((y row (1- y)) (x column (1- x))) (up-left-elem x y) term~ddots-p term~ellipsis-begin
			  (setf begin (list (1- x) (1- y))))
	     (walk-matrix ((y row (1+ y)) (x column (1+ x))) (down-right-elem x y) term~ddots-p term~ellipsis-end
			  (setf end (list (1+ x) (1+ y))))
	     (finalise-ellipsis ellipsis)))))))
  

(defun term=ellipsis-element&range (begin end env)
  (declare (edited  "09-JUN-2004")
	   (authors Vxs)
	   (input   "Two terms and an environment.")
	   (effect  "None.")
	   (value   "1. An element describing the elements the ellipsis substitutes for."
		    "2. The range which the element covers."))
  (if (keim~equal begin end)
      (values begin nil)
    (let* ((disagreement-set (term~disagreement-set begin end))
	   (ranges (term=get-ranges begin end disagreement-set))
	   (franges (mapcar #'first ranges))
	   (meta-vars (mapcar #'(lambda (x)
				  (term~generate-term-primitive-with-new-name :el (term~type x) 'meta+variable env))
			      franges)))
      (when (and disagreement-set ranges
		 ;;; (term=check-range-compatility ranges)    ;;; here could go another elaborate check for the ranges...
		 )
	(values (term=replace-structs-at-positions begin meta-vars disagreement-set)
		(mapcar #'cons meta-vars ranges))))))

(defun term=replace-structs-at-positions (term structs positions)
  (declare (edited  "15-JUN-2004")
	   (authors Sorge)
	   (input   "A term, a list of terms and a list of positions.")
	   (effect  "None.")
	   (value   "The term with all subterms at the given positions replaced by structs."))
  (let ((result term))
    (mapc #'(lambda (struct pos)
	      (setf result (data~replace-at-position result pos struct)))
	  structs positions)
    result))


	
	

;; I don't trust the embedded macro yet....
;; 	   (do* ((x column (1- x))
;; 		 (elem (left-elem x row) (left-elem x row)))
;; 	       ((or (null elem)
;; 		    (and (not (term~hdots-p elem))
;; 			 (term~ellipsis-p elem))
;; 		    (term~ellipsis-begin ellipsis)))
;; 	     (if (term~hdots-p elem)
;; 		 (setf (term~ellipsis-begin ellipsis) (term~ellipsis-begin elem))
;; 	       (setf (term~ellipsis-begin ellipsis) elem)))
;; 	   (do* ((x column (1+ x))
;; 		 (elem (right-elem x row) (right-elem x row)))
;; 	       ((or (null elem)
;; 		    (and (not (term~hdots-p elem))
;; 			 (term~ellipsis-p elem))
;; 		    (term~ellipsis-end ellipsis)))
;; 	     (if (term~hdots-p elem)
;; 		 (setf (term~ellipsis-end ellipsis) (term~ellipsis-end elem))
;; 	       (setf (term~ellipsis-end ellipsis) elem)))
;; 	   (do* ((y row (1- y))
;; 		 (elem (up-elem column y) (up-elem column y)))
;; 	       ((or (null elem)
;; 		    (and (not (term~vdots-p elem))
;; 			 (term~ellipsis-p elem))
;; 		    (term~ellipsis-begin ellipsis)))
;; 	     (if (term~vdots-p elem)
;; 		 (setf (term~ellipsis-begin ellipsis) (term~ellipsis-begin elem))
;; 	       (setf (term~ellipsis-begin ellipsis) elem)))
;; 	   (do* ((y row (1+ y))
;; 		 (elem (down-elem column y) (down-elem column y)))
;; 	       ((or (null elem)
;; 		    (and (not (term~vdots-p elem))
;; 			 (term~ellipsis-p elem))
;; 		    (term~ellipsis-end ellipsis)))
;; 	     (if (term~vdots-p elem)
;; 		 (setf (term~ellipsis-end ellipsis) (term~ellipsis-end elem))
;; 	       (setf (term~ellipsis-end ellipsis) elem)))
;; 	   (do* ((y row (1- y))
;; 		 (x column (1- x))
;; 		 (elem (up-left-elem x y)
;; 		       (up-left-elem x y)))
;; 	       ((or (null elem)
;; 		    (and (not (term~ddots-p elem))
;; 			 (term~ellipsis-p elem))
;; 		    (term~ellipsis-begin ellipsis)))
;; 	     (if (term~ddots-p elem)
;; 		 (setf (term~ellipsis-begin ellipsis) (term~ellipsis-begin elem))
;; 	       (setf (term~ellipsis-begin ellipsis) elem)))
;; 	   (do* ((y row (1+ y))
;; 		 (x column (1+ x))
;; 		 (elem (down-right-elem x y)
;; 		       (down-right-elem x y)))
;; 	       ((or (null elem)
;; 		    (and (not (term~ddots-p elem))
;; 			 (term~ellipsis-p elem))
;; 		    (term~ellipsis-end ellipsis)))
;; 	     (if (term~ddots-p elem)
;; 		 (setf (term~ellipsis-end ellipsis) (term~ellipsis-end elem))
;; 	       (setf (term~ellipsis-end ellipsis) elem)))

;;;; Some Auxiliary Functions for Matrices

(defun term=matrix-elements (mat)
  (declare (edited  "09-JUN-2004")
	   (authors Vxs)
	   (input   "A matrix.")
	   (effect  "None.")
	   (value   "A list containing the single elements of the matrix."))
  (apply #'append (keim~name mat)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Stuff for Block-Matrices 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(defmethod post~read-object ((elements list)
			     (env env+environment) 
			     (indicator (eql :block)))
  (term~create-block (mapcar #'(lambda (elem) (post~read-object elem env :existing-term)) elements)))

;; currently only 2 x 2 blocks  
;; (a b c d) = |a b|
;;             |c d|
;; the a b c d are either matrices or double-indexed functions
;; TODO: allow constants, vectors, blocks containing blocks

(defun term~create-block (block)
  (if (= (length block) 4)
	(let ((dimension (term=block-dimension block)))
	  (if (every #'(lambda (x) (integerp x)) dimension)
	    (let* ((types (mapcar #'term~type block)))
	      (if (every #'(lambda (x) (data~equal (car types) x)) (rest types))
		       (let ((tconst (data~constant-create 'term+block :name block)))
			 (setf (term~block-rows tconst) (list (third dimension)(fourth dimension)))
			 (setf (term~block-columns tconst) (list (first dimension)(second dimension)))
			 (setf (data~annotation tconst) (car types))
			 (setf (data~constant-origin tconst) tconst)
			 tconst)
		     (error ";; TERM~~CREATE-BLOCK: Elements of block ~A must be of the same type ~A." block types)))
	      (error ";; TERM~~CREATE-BLOCK: Problem with the dimension of subblocks ~A." block)))
	(error ";; TERM~~CREATE-BLOCK: ~A is not a 2 x 2 block." block)))

(defun term=block-dimension (block)
  (destructuring-bind (a b c d)
      block
    (mapcar #'(lambda (dim)
		(if (and (integerp (car dim))
			 (integerp (cadr dim))
			 (= (car dim)(cadr dim)))
		    (car dim)
		  (or (car dim)(cadr dim))))
	    (list (mapcar #'term=block-columns (list a c))
		  (mapcar #'term=block-columns (list b d))
		  (mapcar #'term=block-rows (list a b))
		  (mapcar #'term=block-rows (list c d))))))

(defun term=block-rows (obj)
  (if (term~matrix-p obj) (term~matrix-rows obj) nil))

(defun term=block-columns (obj)
  (if (term~matrix-p obj) (term~matrix-columns obj) nil))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Normalforms
;; ===========
;;
;; For certain annotated constants there exist particular normalform.
;; They will be used for the definition-expansion or when determining
;; equality of annotated constants.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgeneric term~normalform (term)
  (declare (edited  "12-SEP-2002")
	   (authors Pollet)
	   (input   "A terms.")
	   (effect  "-")
	   (value   "For special terms a normalform of it, else the term itself."))
  (:method ((term term+term))
	   term)
  (:method ((term term+set))
	   (term=set-nf (keim~name term)))
  (:method ((term term+multi-set))
	   (term=multi-set-nf (keim~name term)))
  (:method ((term term+cyc))
	   (term=cycle-nf (keim~name term)))
  (:method ((term term+annotated-constant))
	   (keim~name term))
  (:method ((term term+annotated-term))
	   (data~appl-arguments term))
  )

(defun term=cycle-nf (termlist)
  (declare (edited  "12-SEP-2002")
	   (authors Pollet)
	   (input   "A list of terms representing a cycle.")
	   (effect  "-")
	   (value   "A list that is the normal of the cycle, with a minimal element with"
		    "respect to 'string<' as first element."))
  (labels ((minimum (restlist current)
		    (cond ((null restlist) current)
			  ((cycle< (car restlist) current)
			   (minimum (rest restlist) (car restlist)))
			  (t (minimum (rest restlist) current))))
	   (cycle< (x y) (string< (post~string x) (post~string y)))
	   (cycle= (x y) (string= (post~string x) (post~string y))))
    (let ((pos (position (minimum (rest termlist)(car termlist)) termlist :test #'cycle=)))
      (if pos (append (subseq termlist pos) (subseq termlist 0 pos)) termlist))))

(defun term=set-nf (set)
  (declare (edited  "12-SEP-2002")
	   (authors Pollet)
	   (input   "A list of terms representing a set.")
	   (effect  "-")
	   (value   "A list that is the normal of the set, with elements sorted"
		    "with respect to 'string<'."))
  (sort
   (remove-duplicates set :test #'term~alpha-equal)
   #'(lambda (x y)
       (string< (post~string x) (post~string y)))))

(defun term=multi-set-nf (termlist)
  (declare (edited  "13-JUL-2004")
	   (authors Vxs)
	   (input   "A list of terms representing a multi-set.")
	   (effect  "None.")
	   (value   "A list that is the normal form of that multi-set, i.e. the elements are"
		    "ordered with respect to 'string<'."))
  (sort termlist #'(lambda (x y) (string< (post~string x) (post~string y)))))
  

; Equality on lists and sets: Lists and sets are basically annotated constants,
; the annotation is stored as name. If the annotation of the set/list/cycle are
; set-equal/equal/cycle-equal to the annotation of the other set/list, then it
; denotes the same object. 

(defun term=set-test (term1 term2 test)
  (declare (edited  "12-SEP-2002")
	   (authors Pollet)
	   (input   "Two term+set and a test-function.")
	   (effect  "-")
	   (value   "T if the two sets are equal with respect to test."
		    "else nil."))
  (and (subsetp (keim~name term1) (keim~name term2) :test test)
       (subsetp (keim~name term2) (keim~name term1) :test test)))

(defmethod data~equal ((term1 term+set) (term2 term+set))
  (term=set-test term1 term2 #'data~equal))

(defmethod term~alpha-equal ((term1 term+set) (term2 term+set) &key subst additional-bind)
  (term=set-test term1 term2 #'(lambda (x y) (term~alpha-equal x y :subst subst :additional-bind additional-bind ))))

(defmethod term=alpha-equal ((term1 term+set) (term2 term+set) bdbl)
  (term=set-test term1 term2 #'(lambda (x y) (term=alpha-equal x y bdbl))))

(defmethod data~equal-p ((term1 term+set) (term2 term+set) &key mode destructive)
  (term=set-test term1 term2 #'(lambda (x y) (data~equal-p x y :mode mode :destructive destructive))))

(defmethod keim~equal ((term1 term+set) (term2 term+set))
  (term=set-test term1 term2 #'keim~equal))

(defun term=multi-set-test (term1 term2 test)
  (declare (edited  "13-JUL-2004")
	   (authors Vxs)
	   (input   "Two term+multi-set and a test-function.")
	   (effect  "None.")
	   (value   "T if the two multi-sets are equal with respect to test."
		    "else nil."))
  (let* ((terms1 (term~normalform term1))
	 (terms2 (term~normalform term2)))
    (and (= (length terms1)(length terms2))
	 (every #'(lambda (x y) (apply test (list x y))) terms1 terms2))))

(defmethod data~equal ((term1 term+multi-set) (term2 term+multi-set))
  (term=multi-set-test term1 term2 #'data~equal))

(defmethod term~alpha-equal ((term1 term+multi-set) (term2 term+multi-set) &key subst additional-bind)
  (term=multi-set-test term1 term2 #'(lambda (x y) (term~alpha-equal x y :subst subst :additional-bind additional-bind ))))

(defmethod term=alpha-equal ((term1 term+multi-set) (term2 term+multi-set) bdbl)
  (term=multi-set-test term1 term2 #'(lambda (x y) (term=alpha-equal x y bdbl))))

(defmethod data~equal-p ((term1 term+multi-set) (term2 term+multi-set) &key mode destructive)
  (term=multi-set-test term1 term2 #'(lambda (x y) (data~equal-p x y :mode mode :destructive destructive))))

(defmethod keim~equal ((term1 term+multi-set) (term2 term+multi-set))
  (term=multi-set-test term1 term2 #'keim~equal))

(defun term=list-test (term1 term2 test)
  (declare (edited  "12-SEP-2002")
	   (authors Pollet)
	   (input   "Two term+list and a test-function.")
	   (effect  "-")
	   (value   "T if the two lists are equal with respect to test."
		    "else nil."))
  (let ((terms1 (keim~name term1))
	(terms2 (keim~name term2)))
    (and (= (length terms1)(length terms2))
	 (every #'(lambda (x y) (apply test (list x y)))
		terms1 term2))))

(defmethod data~equal ((term1 term+list) (term2 term+list))
  (term=list-test term1 term2 #'data~equal))

(defmethod term~alpha-equal ((term1 term+list) (term2 term+list) &key subst additional-bind)
  (term=list-test term1 term2 #'(lambda (x y) (term~alpha-equal x y :subst subst :additional-bind additional-bind ))))

(defmethod term=alpha-equal ((term1 term+list) (term2 term+list) bdbl)
  (term=list-test term1 term2 #'(lambda (x y) (term=alpha-equal x y bdbl))))

(defmethod data~equal-p ((term1 term+list) (term2 term+list) &key mode destructive)
  (term=list-test term1 term2 #'(lambda (x y) (data~equal-p x y :mode mode :destructive destructive))))

(defmethod keim~equal ((term1 term+list) (term2 term+list))
  (term=list-test term1 term2 #'keim~equal))

(defun term=cycle-test (term1 term2 test)
  (declare (edited  "12-SEP-2002")
	   (authors Pollet)
	   (input   "Two term+cyc and a test-function.")
	   (effect  "-")
	   (value   "T if the two cycles are equal with respect to test."
		    "else nil."))
  (let* ((terms1 (term~normalform term1))
	 (terms2 (term~normalform term2)))
    (and (= (length terms1)(length terms2))
	 (every #'(lambda (x y) (apply test (list x y))) terms1 terms2))))

(defmethod data~equal ((term1 term+cyc) (term2 term+cyc))
  (term=cycle-test term1 term2 #'data~equal))

(defmethod term~alpha-equal ((term1 term+cyc) (term2 term+cyc) &key subst additional-bind)
  (term=cycle-test term1 term2 #'(lambda (x y) (term~alpha-equal x y :subst subst :additional-bind additional-bind ))))

(defmethod term=alpha-equal ((term1 term+cyc) (term2 term+cyc) bdbl)
  (term=cycle-test term1 term2 #'(lambda (x y) (term=alpha-equal x y bdbl))))

(defmethod data~equal-p ((term1 term+cyc) (term2 term+cyc) &key mode destructive)
  (term=cycle-test term1 term2 #'(lambda (x y) (data~equal-p x y :mode mode :destructive destructive))))

(defmethod keim~equal ((term1 term+cyc) (term2 term+cyc))
  (term=cycle-test term1 term2 #'keim~equal))

(defun term=tuple-test (term1 term2 test)
  (declare (edited  "05-JUN-2003" "12-SEP-2002")
	   (authors Vxs Pollet)
	   (input   "Two term+tuple and a test-function.")
	   (effect  "-")
	   (value   "T if the two tuples are equal with respect to test."
		    "else nil."))
  (let ((ann1 (keim~name term1))
	(ann2 (keim~name term2)))
    (and (= (length ann1) (length ann2))
	 (every #'(lambda (x y) (apply test (list x y)))
		ann1 ann2))))
	     

(defmethod data~equal ((term1 term+tuple) (term2 term+tuple))
  (term=tuple-test term1 term2 #'data~equal))

(defmethod term~alpha-equal ((term1 term+tuple) (term2 term+tuple) &key subst additional-bind)
  (term=tuple-test term1 term2 #'(lambda (x y) (term~alpha-equal x y :subst subst :additional-bind additional-bind ))))

(defmethod term=alpha-equal ((term1 term+tuple) (term2 term+tuple) bdbl)
  (term=tuple-test term1 term2 #'(lambda (x y) (term=alpha-equal x y bdbl))))

(defmethod data~equal-p ((term1 term+tuple) (term2 term+tuple) &key mode destructive)
  (term=tuple-test term1 term2 #'(lambda (x y) (data~equal-p x y :mode mode :destructive destructive))))

(defmethod keim~equal ((term1 term+tuple) (term2 term+tuple))
  (term=tuple-test term1 term2 #'keim~equal))

(defun term=vector-test (term1 term2 test)
  (declare (edited  "05-MAY-2004" "05-JUN-2003" "12-SEP-2002")
	   (authors Sorge Vxs Pollet)
	   (input   "Two term+vector and a test-function.")
	   (effect  "-")
	   (value   "T if the two vectors are equal with respect to test."
		    "else nil."))
  (and (= (term~vector-dimension term1) (term~vector-dimension term2))
       (every #'(lambda (x y) (apply test (list x y)))
	      (keim~name term1) (keim~name term2))))

(defmethod data~equal ((term1 term+vector) (term2 term+vector))
  (term=vector-test term1 term2 #'data~equal))

(defmethod term~alpha-equal ((term1 term+vector) (term2 term+vector) &key subst additional-bind)
  (term=vector-test term1 term2 #'(lambda (x y) (term~alpha-equal x y :subst subst :additional-bind additional-bind ))))

(defmethod term=alpha-equal ((term1 term+vector) (term2 term+vector) bdbl)
  (term=vector-test term1 term2 #'(lambda (x y) (term=alpha-equal x y bdbl))))

(defmethod data~equal-p ((term1 term+vector) (term2 term+vector) &key mode destructive)
  (term=vector-test term1 term2 #'(lambda (x y) (data~equal-p x y :mode mode :destructive destructive))))

(defmethod keim~equal ((term1 term+vector) (term2 term+vector))
  (term=vector-test term1 term2 #'keim~equal))

(defun term=matrix-test (term1 term2 test)
  (declare (edited  "05-MAY-2004" "05-JUN-2003" "12-SEP-2002")
	   (authors Sorge Vxs Pollet)
	   (input   "Two term+matrix and a test-function.")
	   (effect  "-")
	   (value   "T if the two matrices are equal with respect to test."
		    "else nil."))
  (and (= (term~matrix-rows term1) (term~matrix-rows term2))
       (= (term~matrix-columns term1) (term~matrix-columns term2))
       (every #'(lambda (row1 row2)
		  (every #'(lambda (elem1 elem2)
			     (apply test (list elem1 elem2)))
			 row1 row2))
	      (keim~name term1) (keim~name term2))))

(defmethod data~equal ((term1 term+matrix) (term2 term+matrix))
  (term=matrix-test term1 term2 #'data~equal))

(defmethod term~alpha-equal ((term1 term+matrix) (term2 term+matrix) &key subst additional-bind)
  (term=matrix-test term1 term2 #'(lambda (x y) (term~alpha-equal x y :subst subst :additional-bind additional-bind ))))

(defmethod term=alpha-equal ((term1 term+matrix) (term2 term+matrix) bdbl)
  (term=matrix-test term1 term2 #'(lambda (x y) (term=alpha-equal x y bdbl))))

(defmethod data~equal-p ((term1 term+matrix) (term2 term+matrix) &key mode destructive)
  (term=matrix-test term1 term2 #'(lambda (x y) (data~equal-p x y :mode mode :destructive destructive))))

(defmethod keim~equal ((term1 term+matrix) (term2 term+matrix))
  (term=matrix-test term1 term2 #'keim~equal))

(defun term=block-test (term1 term2 test)
  (declare (edited  "05-MAY-2004" "05-JUN-2003" "12-SEP-2002")
	   (authors Sorge Vxs Pollet)
	   (input   "Two term+block and a test-function.")
	   (effect  "-")
	   (value   "T if the two matrices are equal with respect to test."
		    "else nil."))
  (and (every #'(lambda (x y)(= x y)) (term~block-rows term1) (term~block-rows term2))
       (every #'(lambda (x y)(= x y)) (term~block-columns term1) (term~block-columns term2))
       (every #'(lambda (elem1 elem2)
			     (apply test (list elem1 elem2)))
	      (keim~name term1) (keim~name term2))))

(defmethod data~equal ((term1 term+block) (term2 term+block))
  (term=block-test term1 term2 #'data~equal))

(defmethod term~alpha-equal ((term1 term+block) (term2 term+block) &key subst additional-bind)
  (term=block-test term1 term2 #'(lambda (x y) (term~alpha-equal x y :subst subst :additional-bind additional-bind ))))

(defmethod term=alpha-equal ((term1 term+block) (term2 term+block) bdbl)
  (term=block-test term1 term2 #'(lambda (x y) (term=alpha-equal x y bdbl))))

(defmethod data~equal-p ((term1 term+block) (term2 term+block) &key mode destructive)
  (term=block-test term1 term2 #'(lambda (x y) (data~equal-p x y :mode mode :destructive destructive))))

(defmethod keim~equal ((term1 term+block) (term2 term+block))
  (term=block-test term1 term2 #'keim~equal))

; Printing

(defmethod post~print ((sym term+list) stream)
  (format stream "(LIST~{ ~A~})" (keim~name sym)))

(defmethod post~print ((sym term+set) stream)
  (format stream "(SET~{ ~A~})" (keim~name sym)))

(defmethod post~print ((sym term+multi-set) stream)
  (format stream "(MULTI-SET~{ ~A~})" (keim~name sym)))

(defmethod post~print ((sym term+cyc) stream)
  (format stream "(CYC~{ ~A~})" (keim~name sym)))

(defmethod post~print ((sym term+tuple) stream)
  (format stream "(TUPLE~{ ~A~})" (keim~name sym)))

(defmethod post~print ((sym term+vector) stream)
  (format stream "(VEC~{ ~A~})" (keim~name sym)))

(defmethod post~print ((sym term+matrix) stream)
  (format stream "(MAT~{ (~{~A~^ ~})~})" (keim~name sym)))

(defmethod post~print ((sym term+block) stream)
  (format stream "(BLOC~{ ~A~})" (keim~name sym)))

(defmethod print-object ((sym term+list) stream)
  (format stream "(LIST~{ ~A~})" (keim~name sym)))

(defmethod print-object ((sym term+set) stream)
  (format stream "(SET~{ ~A~})" (keim~name sym)))

(defmethod print-object ((sym term+cyc) stream)
  (format stream "(CYC~{ ~A~})" (keim~name sym)))

(defmethod print-object ((sym term+tuple) stream)
  (format stream "(TUPLE~{ ~A~})" (keim~name sym)))

(defmethod print-object ((sym term+vector) stream)
  (format stream "(VEC~{ ~A~})" (keim~name sym)))

(defmethod print-object ((sym term+matrix) stream)
  (format stream "(MAT~{ (~{~A~^ ~})~})" (keim~name sym)))

(defmethod print-object ((sym term+block) stream)
  (format stream "(BLOC~{ ~A~})" (keim~name sym)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; An attempt at ellipses etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eval-when (load compile eval)
  (defclass term+ellipsis (term+constant)
    ((computable :initform nil
		 :initarg :computable
		 :type boolean
		 :accessor term~ellipsis-computable)
     (begin :initform nil
	    :initarg :begin
	    :accessor term~ellipsis-begin)
     (end :initform nil
	  :initarg :end
	  :accessor term~ellipsis-end)
     (element :initform nil
	      :initarg :element
	      :accessor term~ellipsis-element)
     (range :initform nil                     ;;; we will need this, but will we need step width or assume simply it's always 1?
	    :initarg :range
	    :accessor term~ellipsis-range))
    (:documentation "The superclass of ellipses."))
  (defclass term+hdots (term+ellipsis)
    ()
    (:documentation "The class of horizontal ellipses."))
  (defclass term+vdots (term+ellipsis)
    ()
    (:documentation "The class of horizontal ellipses."))
  (defclass term+ddots (term+ellipsis)
    ()
    (:documentation "The class of horizontal ellipses."))
)


(defun term~ellipsis-p (thing)
  (typep thing 'term+ellipsis))

(defun term~hdots-p (thing)
  (typep thing 'term+hdots))

(defun term~vdots-p (thing)
  (typep thing 'term+vdots))

(defun term~ddots-p (thing)
  (typep thing 'term+ddots))


(defmethod post~read-object ((ellipsis symbol)
			     (env env+environment) 
			     (indicator (eql :ellipsis)))
  (cond ((string-equal ellipsis :hdots) (data~constant-create 'term+hdots :name ellipsis))
	((string-equal ellipsis :vdots) (data~constant-create 'term+vdots :name ellipsis))
	((string-equal ellipsis :ddots) (data~constant-create 'term+ddots :name ellipsis))))

#|
(setq envi (env~create (th~env :matrix)))

(post~read-object '((a i) (b i) (c i)) envi :constants)

(setq m1 (post~read-object '(mat (  a     b   hdots   b  )
				 (  c   ddots ddots vdots)  
				 (vdots ddots ddots   b  )
				 (  c   hdots   c     a  )) envi :existing-term))

(post~read-object '((n num) (m num)) envi :constants)
(post~read-object '(d (i num num)) envi :constant)

(setq m2 (post~read-object '(mat ((d 1 1) hdots (d 1 n)) (vdots ddots vdots) ((d m 1) hdots (d m n))) envi :existing-term))

(setq m3 (post~read-object '(mat (1 2 3) (4 5 6) (7 8 9)) envi :existing-term))

(post~read-object '((e i) (f i)) envi :constants)

(setq m4 (post~read-object '(mat (  a     b   hdots   b  )
				 (  c   ddots ddots vdots)  
				 (vdots ddots ddots   b  )
				 (  c   hdots   c     a  )
				 (  e     f   hdots   f  )) envi :existing-term))

(setq m5 (post~read-object '(mat (  a     b   hdots   b  )
				 (  c   ddots ddots vdots)  
				 (vdots ddots ddots   b  )
				 (  c   hdots   c     a  )
				 (  e   (d 1 1) hdots (d 1 n))) envi :existing-term))

(setq m6 (post~read-object '(mat (  a     b   hdots   b  )
				 (  c     a   ddots vdots)  
				 (vdots ddots   a     b  )
				 (  c   hdots   c     a  )) envi :existing-term))


|#





;; ------ CAUTION! CONSTRUCTION AHEAD ------

(data~defgeneric term~disagreement-set ((term1) (term2) &key position)
  (declare (edited  "14-JUN-2004")
	   (authors Vxs)
	   (input   "Two term.")
	   (effect  "None.")
	   (value   "A set of maximal term positions where the terms disagree."
		    "If the terms are not of the same type, NIL is returned."
		    "If the two terms are equal, T is returned."))
  (:method :around ((term1 term+term) (term2 term+term) &key (position (pos~empty)))
	   (declare (ignore position))
	   (cond ((keim~equal term1 term2)
		  t)
		 ((keim~equal (term~type term1) (term~type term2))
		  (call-next-method))
		 (t nil)))
  (:method ((term1 term+term) (term2 term+term) &key (position (pos~empty)))
	   (list position))
  (:method ((term1 term+annotated-constant) (term2 term+annotated-constant) &key (position (pos~empty)))
	   ;;; what should we do there???
	   (list position)
	   )
  (:method ((term1 term+appl) (term2 term+appl) &key (position (pos~empty)))
	   (let* ((func1 (data~appl-function term1))
		  (func2 (data~appl-function term2))
		  (dis (term~disagreement-set func1 func2 :position (pos~add-end 0 position))))
	     (flet ((true-p (thing)
			    (eq thing 't)))
	       (if (null dis)             ;;; maximal disagreement leave
		   (list position)
		 (let* ((dis2
			 (do* ((args1 (data~appl-arguments term1) (cdr args1))
			       (args2 (data~appl-arguments term2) (cdr args2))
			       (pos 1 (1+ pos))
			       (result t))
			     ((null args1) result)
			   (let ((res (term~disagreement-set (car args1) (car args2) :position (pos~add-end pos position))))
			     (cond ((and (listp res) (true-p result))
				    (setf result res))
				   ((and (listp res) (listp result))
				    (setf result (append result res))))))))
		   (if (true-p dis) dis2
		     (append dis dis2)))))))
  )


(defun term=get-ranges (term1 term2 positions &optional (result nil))
  (declare (edited  "15-JUN-2004")
	   (authors Sorge)
	   (input   "Two terms and a list of positions.")
	   (effect  "None.")
	   (value   "A list of ranges corresponding to the subterms at the given positions."
		    "NIL if something goes wrong."))
  (if (null positions)
      (reverse result)
    (let ((range (term=get-range term1 term2 (car positions))))
      (unless (null range)
	(term=get-ranges term1 term2 (cdr positions) (cons range result))))))
	  
			   

(defun term=get-range (term1 term2 position)
  (declare (edited  "15-JUN-2004")
	   (authors Sorge)
	   (input   "Two terms and a position.")
	   (effect  "None.")
	   (value   "A set with two elements specifying an index range. The beginning and end of"
		    "range have to be either constants of type NUM or integer numbers."
		    "Otherwise, the range is not valid and NIL is returned."))
  (flet ((num-const (term)
		   (and
		    (not (term~number-p term))
		    (term~constant-p term)
		    (type~num-p (term~type term))))
	 (term-integer-p (term)
			 (and (term~number-p term)
			      (integerp (keim~name term)))))
    (let* ((begin (data~struct-at-position term1 position))
	   (end (data~struct-at-position term2 position)))
      (when (and (or (term-integer-p begin) (num-const begin))     ;;; Here is the test that restricts the range to integers.
		 (or (term-integer-p end) (num-const end)))
	(list begin end)))))
		  

;; ------ END OF CONSTRUCTION ------



;; ------ CAUTION! CONSTRUCTION AHEAD ------


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Annotated Terms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#{
\subsection{Annotated Terms}

Annotated terms are a generalisation of annotated constants. They allow
for the use of variables within an annotated term.

The are implemented as a specialisation of the term+appl datastructure.

#}


  
(defun term~annotated-term-p (thing)
  (typep thing 'term+annotated-term))

(defun term~markup-p (thing)
  (typep thing 'term+markup))

(defun term~gen-cyc-p (thing)
  (and (term~annotated-term-p thing)
       (term~markup-p (data~appl-function thing))
       (string-equal "cyc" (subseq (keim~name (data~appl-function thing)) 0 3))))

(defun term~gen-set-p (thing)
  (and (term~annotated-term-p thing)
       (term~markup-p (data~appl-function thing))
       (string-equal "set" (subseq (keim~name (data~appl-function thing)) 0 3))))

(defun term~annotated-term-create (arguments type env symbol)
  (declare (edited  "09-JUN-2005")
	   (authors Sorge)
	   (input   "A list of terms, a type signifying the codomain of the final functional type,"
		    "an environment, and a symbol.")
	   (effect  "Creates an annotated term object.")
	   (value   "The newly created object."))
  (let* ((final-type (type~func-create (mapcar #'term~type arguments) type))
	 (markup (term~markup-create env symbol (length arguments) final-type))
	 (dummy (setq nix markup))
	 (new-object (term~appl-create markup arguments)))
    (change-class new-object 'term+annotated-term)))

(defun term~markup-create (env symbol arity type)
  (declare (edited  "09-JUN-2005")
	   (authors Sorge)
	   (input   "An environment, a symbol, a non-negative integer, and a type.")
	   (effect  "Possibly creates a new markup symbol.")
	   (value   "The markup symbol."))
  (let* ((name (format nil "~A~A" symbol arity))
	 (markup (term=find-markup-symbol name type env)))
    (if (term~markup-p markup) markup
      (let ((tconst (data~constant-create 'term+markup :name markup)))
	(setf (data~annotation tconst) type)
	(setf (data~constant-origin tconst) tconst)
	(env~enter markup tconst env)
	tconst))))
  


(defun term=find-markup-symbol (symbol type env)
  (declare (edited  "10-JUN-2005")
	   (authors Sorge)
	   (input   "A symbol, a type, and an environment.")
	   (effect  "None.")
	   (value   "The best corresponding markup symbol in the environment. Possibly a new one."))
  (let ((env-symbol (env~lookup-object symbol env)))
    (cond ((and env-symbol (term~markup-p env-symbol) (keim~equal (term~type env-symbol) type))
	   env-symbol)
	  ((null env-symbol) symbol)
	  (t
	   (do* ((n 1 (1+ n))
		 (new-name (intern (format nil "~A~A" symbol n)
				   (find-package :keim))
			   (intern (format nil "~A~A" symbol n)
				   (find-package :keim)))
		 (env-symbol (env~lookup-object new-name env) (env~lookup-object new-name env)))
	       ((or (and env-symbol (term~markup-p env-symbol) (keim~equal (term~type env-symbol) type))
		    (null env-symbol))
		(if (null env-symbol) new-name env-symbol)))))))


;;; Equality methods for annotated terms


(defmethod data~equal-p ((term1 term+annotated-term) (term2 term+annotated-term) &key mode destructive)
  (and (data~equal-p (data~appl-function term1) (data~appl-function term2) :mode mode :destructive destructive)
       (data~equal-p (data~appl-arguments term1) (data~appl-arguments term2) :mode mode :destructive destructive)))

(defmethod keim~equal ((term1 term+annotated-term) (term2 term+annotated-term))
  (and (keim~equal (data~appl-function term1) (data~appl-function term2))
       (keim~equal (data~appl-arguments term1) (data~appl-arguments term2))))

;; (defmethod data~equal ((term1 term+markup) (term2 term+markup))
;;   (data~equal term1 term2 #'data~equal))

(defmethod term~alpha-equal ((term1 term+markup) (term2 term+markup) &key subst additional-bind)
  (term~alpha-equal (term~type term1) (term~type term2) :subst subst :additional-bind additional-bind))

(defmethod term=alpha-equal ((term1 term+markup) (term2 term+markup) bdbl)
  (data~equal-p (term~type term1) (term~type term2)))

(defmethod data~equal-p ((term1 term+markup) (term2 term+markup) &key mode destructive)
  (data~equal-p (term~type term1) (term~type term2) :mode mode :destructive destructive))

(defmethod keim~equal ((term1 term+markup) (term2 term+markup))
  (keim~equal (term~type term1) (term~type term2)))


;; ------ END OF CONSTRUCTION ------


