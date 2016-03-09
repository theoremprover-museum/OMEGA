(in-package :omega)

(defun repr=env ()
  (if omega*current-proof-plan
      (pds~environment omega*current-proof-plan)
    (th~env omega*current-theory)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The following functions transform special/annotated constanst to lambda-terms
;;; The functions below need an omega*current-proof-plan!

(defgeneric repr~special2term (obj)
  (declare (edited  "12-SEP-2002")
	   (authors Pollet)
	   (input   "A special representation: numbers, lists, sets, cycles.")
	   (effect  "-")
	   (value   "A term representation."))
  (:method ((obj term+number))
	   (natac=numbers-2-function obj))
  (:method ((obj term+list))
	   (omega~error "Currently no terms for lists"))
  (:method ((obj term+cyc))
	   (repr=cyc2term (term~normalform obj)))
  (:method ((obj term+set))
	   (repr=set2term (term~normalform  obj)))
  (:method ((obj term+markup))
	   (let* ((name (keim~name obj))
		  (domaintypes (data~abstr-domain (term~type obj)))
		  (vars (mapcar #'(lambda (type) (term~variable-create (term~generate-new-name 'x (repr=env)) type))
			       				      domaintypes)))
	   (term~abstr-create vars
			      (cond ((string-equal (subseq name 0 3) "set")  
				     (repr=set2term vars))
				    ((string-equal (subseq name 0 3) "cyc")  
				     (repr=cyc2term vars))))))
  (:method ((obj term+annotated-term))
	   (let ((function (repr~special2term (data~appl-function obj))))
	     (beta~normalize (data~appl-function function
						 (data~appl-arguments obj)))))		   
  (:method ((obj term+tuple))
	   (let* ((elems (term~normalform obj))
		  (pair (env~lookup-object 'pair (repr=env))))
	     (reduce #'(lambda (x y) (term~appl-create pair (list y x))) (reverse elems) )))
  (:method ((obj term+multi-set))
	   (let* ((elems (term~normalform obj))
		  (mscons (env~lookup-object 'mscons (repr=env)))
		  (msnil (env~lookup-object 'msnil (repr=env))))
	     (reduce #'(lambda (x y) (term~appl-create mscons (list y x))) (cons msnil (reverse elems)))))
  (:method ((obj term+vector))
	   (repr~vector2term (keim~name obj)))
  (:method ((obj term+matrix))
	   (repr~matrix2term (keim~name obj))))

(defgeneric repr~term2special (obj)
  (declare (edited  "12-SEP-2002")
	   (authors Pollet)
	   (input   "A term representation.")
	   (effect  "-")
	   (value   "A special representation: numbers, lists, sets, cycles."))
  (:method ((obj term+number))
	    (natac=function-2-numbers obj)))

(defun repr=cyc2term (elems)
  (let  ((empty (env~lookup-object 'nil (repr=env)))
	 (cons (env~lookup-object 'cons (repr=env))))
    (reduce #'(lambda (x y) (term~appl-create cons (list y x))) (reverse elems) :initial-value empty )))
  

(defun repr=set2term (elems)
  (let* ((var (term~variable-create 'e (term~type (car elems))))
	 (= (logic~equality-constant))
	 (equalities (mapcar #'(lambda (el) (term~appl-create =
							      (list var el)))
		      elems)))
    (term~abstr-create (list var)
		       (reduce #'(lambda (x y) (term~appl-create
						(logic~disjunction-constant)
						(list x y))) equalities))))
  

; we don't have to store the definitions for special terms, but can generate them
; from their keim~name

(defun repr~definition (obj)
  (declare (edited  "12-SEP-2002")
	   (authors Pollet)
	   (input   "A constant.")
	   (effect  "-")
	   (value   "If it is a special repr. a generated definition."
		    "Else nil."))
  (typecase obj
      (integer (repr~definition (post~read-object obj (repr=env) :existing-term)))
      (symbol  (repr~definition (post~read-object obj (repr=env) :existing-term)))
      (cons    (repr~definition (post~read-object obj (repr=env) :existing-term)))
      (string  (repr~definition (read-from-string obj)))
      (T (when (or (term~special-p obj)
		   (term~annotated-term-p obj)
		   (term~markup-p obj))
	   (make-instance 'th+def
			  :name (post~string obj)
			  :constant obj
			  :node  (repr~special2term obj))))))


(defgeneric repr~all-substructs (term)
  (declare (edited  "16-SEP-2002")
	   (authors Pollet)
	   (input   "A term.")
	   (effect  "-")
	   (value   "Returns all substructures of the term including the substructures contained special terms."))
  (:method ((term term+term))
	   (mapcan #'(lambda (sub)
		       (if (and (term~special-p sub)(not (term~number-p sub))) ;;here we have some possible substructs
			   (repr~all-substructs sub)
			 (list sub)))
	   (data~all-substructs term)))
  (:method ((term term+list))
	   (cons term (mapcan #'repr~all-substructs (term~normalform term))))
  (:method ((term term+cyc))
	   (cons term (mapcan #'repr~all-substructs (term~normalform term))))
  (:method ((term term+set))
	   (cons term (mapcan #'repr~all-substructs (term~normalform term))))
  (:method ((term term+tuple))
	   (cons term (mapcan #'repr~all-substructs (term~normalform term)))))



; defnitions are the definitions of the theory and every special object has a definition
; (generated on the fly by repr~definiton, since the definition contained in the object)
; the following function corresponds to th~find-assumption but includes definitions for
; special terms

(defun repr~find-definition (name theory)
  (declare (edited  "18-SEP-2002")
	   (authors Pollet)
	   (input   "An assumption name, and that of a theory (symbol, string or even a theory).")
	   (effect  "None.")
	   (value   "The definition of the special term or of the theory with this name or NIL if none exists."))
	(or (repr~definition name)
	    (th~find-assumption name theory)))
;;            (let ((thy (th~find-theory theory)))
;;              (if thy
;;                  (let ((ass (gethash (keim::th=read-string name) (keim::th=assumptions thy))))
;;                    (if (and ass (th~definition-p ass)) ass
;;                      (some #'(lambda (x) (th~find-assumption name x)) (th~imports theory))))
;;              (error "RERP~~FIND-DEFINITION: The theory ~A does not exist. ~A" theory thy)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; An evaluation-mechanism for annotated constants
;;; This can be interpreted as procedural annotations to the function-symbols.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dealing with Vectors and Matrices
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defvar repr*matrix-optimization :none
  "Variable containing the default matrix optimization strategy.")


(defun repr~vector2term (vector &key (optimization repr*matrix-optimization))
  (declare (edited  "10-MAY-2004")
	   (authors Sorge)
	   (input   "A nested list representing a matrix and a symbol representing the desired optimization strategy.")
	   (effect  "None.")
	   (value   "The matrix given as a lambda abstraction."))
  (let* ((i 0)
	 (indexed-list (mapcar #'(lambda (elem) (incf i) (list i elem)) vector)))
    (repr=vector2ifthen-repr indexed-list :optimization optimization)))


(defun repr=vector2ifthen-repr (vector &key (optimization repr*matrix-optimization))
  (declare (edited  "10-MAY-2004")
	   (authors Sorge)
	   (input   "A list of indices value pairs and a type.")
	   (effect  "Creates a new keim object.")
	   (value   "The vector given as a lambda abstraction."))
  (let* ((env (repr=env))
	 (num-type (env~lookup-object 'num env))
	 (i (term~variable-create 'i num-type))
	 (ifthen (env~lookup-object 'ifthen env))
	 (= (logic~equality-constant)))
    (flet ((equality (var val) (term~appl-create = (list var (term~constant-create val num-type)))))
      (labels ((make-ifthen-term (ind-list)
				 (cond ((= (length ind-list) 1) (cadar ind-list))
				       ((= (length ind-list) 2)
					(term~appl-create ifthen (list (equality i (caar ind-list))
								       (cadar ind-list)
								       (cadadr ind-list))))
				       (t (term~appl-create ifthen (list (equality i (caar ind-list))
									 (cadar ind-list)
									 (make-ifthen-term (cdr ind-list))))))))
	(term~abstr-create (list i) (make-ifthen-term vector))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Optimization strategies for matrices
;; ====================================
;; 
;; 0) None
;;    All elements of the matrix are explicitly given
;;
;; 1) Rows
;;    If a whole row contains exactly one element this is collapsed
;;    into a single case
;; 
;; 2) Columns
;;    If a whole column contains exactly one element this is collapsed
;;    into a single case
;; 
;; 3) Diagonals
;;    If diagonals main diagonal or upper or lower secondary diagonal
;;    contain exactly one element this is collapsed into a single case
;; 
;; 4) Most occuring elements/zero elements
;;
;; 5) Triangles 
;;
;; 6) Blocks
;;
;; not yet implemented...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(defun repr~matrix2term (matrix &key (optimization repr*matrix-optimization))
  (declare (edited  "10-MAY-2004")
	   (authors Sorge)
	   (input   "A nested list representing a matrix and a symbol representing the desired optimization strategy.")
	   (effect  "None.")
	   (value   "The matrix given as a lambda abstraction."))
  (let* ((indexed-list (repr=matrix2indexed-list matrix :optimization optimization)))
    (repr=matrix2ifthen-repr indexed-list :optimization optimization)))



(defun repr=matrix2indexed-list (matrix &key (optimization repr*matrix-optimization))
  (declare (edited  "10-MAY-2004")
	   (authors Sorge)
	   (input   "A nested list representing a matrix and a symbol representing the desired optimization strategy.")
	   (effect  "None.")
	   (value   "A list of elements associating index pairs with values."))
  (let ((i 0) result)
    (dolist (row matrix (reverse result))
      (incf i)
      (let ((j 0))
	(dolist (col row)
	(incf j)
	(push (list (list i j) col) result))))))


(defun repr=matrix-translate-ellipses (matrix &key (optimization repr*matrix-optimization))
  (declare (edited  "07-OCT-2004")
	   (authors Vxs)
	   (input   "A list representing a matrix and an indicator.")
	   (effect  "None.")
	   (value   "Don't know yet..."))
  (let ((indexed-matrix (repr=matrix2indexed-list matrix))
	combinations      ;; Combinations of the same ellipses
	vdim              ;; Vertical dimensions
	hdim              ;; Horizontal dimensions
	diagonals         ;; The position of diagonal ellipses in the matrix
	matrix1           ;; Matrix after the first pass: Horizontal and veritcal ellipses are considered
	matrix2           ;; Matrix after the second pass: Diagonal ellipses are considered
	matrix3           ;; Matrix after the third pass: Indices are collapsed
	)
    (flet ((check&adjust-hdim (row col)
			      (if (= row 1)
				  (let ((new-var (gensym "row")))
				    (setf hdim (append hdim (list new-var)))
				    new-var)
				(print "adjusting horizontal dimension")))
	   )
			      
    (do* ((element (car indexed-matrix) (car run-matrix))
	  (run-matrix (cdr indexed-matrix) (cdr run-matrix)))
	((null element) matrix1)
      (let ((row (caar element))
	    (col (cadar element))
	    (value (second element)))
	;; check index
	(format t "~% (~A ~A): ~A" row col value)
	;; add dimension
	;;;;; don't know yet....

	;; go through ellipsis cases
	(cond ((and (term~vdots-p value) (term~ellipsis-range value))
	       (when (= row 1) (error "Vertical ellipsis in row 1????"))
	       (print 1)
	       ())
	      ((term~vdots-p value)
	       (when (= row 1) (error "Vertical ellipsis in row 1????"))
	       (print 2)
	       )
	      ((and (term~hdots-p value) (term~ellipsis-range value))
	       (print 3)
	       (if (= row 1)
		   (let* ((range (term~ellipsis-range value))      ;;; this should be enhanced, i.e. get the real range variable
			  (previous (car (last matrix1)))
			  (start (cadar previous))
			  (next (do* ((next (pop run-matrix) (pop run-matrix))
				      (result (list next) (push next result)))
				    ((not (term~hdots-p (cadr next))) (reverse result)))))
		     (setf matrix1
			   (append (butlast matrix1)
				   (mapcar #'(lambda (x) (list (list (caar x) (list start range)) (cadr x)))
					   (cons previous (cons element next)))))
		     (setf hdim (append hdim (make-list (1+ (length next)) :initial-element range)))
		     )
		 (print "Adjust ....")))
	      ((term~hdots-p value)
	       (print 4)
	       (if (= row 1)
		   (let* ((new-var (gensym "row"))
			  (previous (car (last matrix1)))
			  (start (cadar previous))
			  (next (do* ((next (pop run-matrix) (pop run-matrix))
				      (result (list next) (push next result)))
				    ((not (term~hdots-p (cadr next))) (reverse result)))))
		     (setf matrix1
			   (append (butlast matrix1)
				   (mapcar #'(lambda (x) (list (list (caar x) (list start new-var)) (cadr x)))
					   (cons previous (cons element next)))))
		     (setf hdim (append hdim (make-list (1+ (length next)) :initial-element new-var)))
		     )
		 (print "Adjust ....")		 
		))
	      ((term~ddots-p value)
	       (when (= row 1) (error "Diagonal ellipsis in row 1????"))
	       (print 5)
	       (push (list row col) diagonals)
	       (setf matrix1 (append matrix1 (list element)))
	       )
	      (t (print 6)
		 (if (= row 1)
		     (setf hdim (append hdim (list col)))
		   (print "Adjust ...."))
		 (setf matrix1 (append matrix1 (list element)))))
      ;; collate new dimension changes
    ))
    (print hdim)
    (print vdim)
    (print diagonals)
    (print matrix1)
    matrix1
    )))

(let ((ellipses2variables (make-hash-table :test #'eq))
      (variable-counter 0))
  
  (defun repr=matrix-reset-variable-mapping ()
    (declare (edited  "10-OCT-2004")
	     (authors Sorge)
	     (input   "None.")
	     (effect  "Resets the local variables: The hash table mapping ellipses to variables"
		      "and the variable counter")
	     (value   "The reset hash table."))
    (setf variable-counter 0)
    (setf ellipses2variables (make-hash-table :test #'eq))
    )

  (defun repr=matrix-ellipsis2variable (ellipsis &optional parent-ellipsis)
    (declare (edited  "10-OCT-2004")
	     (authors Sorge)
	     (input   "An ellipsis and a optionally an ellipsis that is the beginning of a line of several ellipses.")
	     (effect  "The ellipsis is mapped to a variable that is its unique identifier."
		      "If PARENT-ELLIPSIS is given it will be mapped to that variable."
		      "If the ellipsis cannot be mapped to an already existing variable, then a new variable is created.")
	     (value   "The variable the ellispsis is mapped to."))
    (let ((variable (gethash ellipsis ellipses2variables))
	  (parent-var (when parent-ellipsis (gethash parent-ellipsis ellipses2variables))))
      (flet ((new-ellipsis-name ()
				(cond ((term~hdots-p ellipsis) 
				       (format nil "h-ellipsis~A" (incf variable-counter)))
				      ((term~vdots-p ellipsis) 
				       (format nil "v-ellipsis~A" (incf variable-counter)))
				      ((term~ddots-p ellipsis) 
				       (format nil "d-ellipsis~A" (incf variable-counter))))))
      (cond ((and parent-ellipsis parent-var variable (not (string-equal variable parent-var)))
	     (omega~warn ";;;REPR=MATRIX-ELLIPSIS2VARIABLE: Ellipses on the same straight are mapped to distinct variables. Possible inconsistency in hashtable.")
	     variable)
	    ((and parent-ellipsis parent-var variable)
	     variable)
	    ((and parent-ellipsis parent-var)
	     (setf (gethash ellipsis ellipses2variables) parent-var))
	    ((and parent-ellipsis variable)
	     (omega~warn ";;;REPR=MATRIX-ELLIPSIS2VARIABLE: Parent ellipses is not mapped to a variable. Possible inconsistency in hashtable.")
	     (setf (gethash parent-ellipsis ellipses2variables) variable))
	    (parent-ellipsis
	     (omega~warn ";;;REPR=MATRIX-ELLIPSIS2VARIABLE: Neither parent nor regular ellipses are mapped to a variable. Possible inconsistency in hashtable.")
	     (setf (gethash parent-ellipsis ellipses2variables)
		   (setf (gethash ellipsis ellipses2variables)
			 (new-ellipsis-name))))
	    (variable variable)
	    (t (setf (gethash ellipsis ellipses2variables) (new-ellipsis-name)))))))

  ;; Diagonals might have to be treated differently since they can stand for different horizontal and vertical length!!!

  (defun repr=matrix-get-row-constraints (matrix)
    (declare (edited  "10-OCT-2004")
	     (authors Sorge)
	     (input   "A list representing a matrix.")
	     (effect  "None.")
	     (value   "Two lists of constraints:"
		      "1. The general flexible constraints for horizontal length of ellipses."
		      "2. The concrete fixed constraints mapping single ellipsis to a constant length."))
    (let (fixed-constraints
	  (max-col (length (car matrix))))
      (values
       (loop for row from 1 to (length matrix)
	     collect
	     (do* ((row-val row (if (term~ddots-p element) (1+ row-val) row-val))
		   (col 1 (1+ col))
		   (old-element nil element)
		   (element (repr=element-at-position matrix row-val col)
			    (when (<= col max-col) (repr=element-at-position matrix row-val col)))
		   result)
		 ((> col max-col) (reverse result))
	       (cond ((and (or (term~hdots-p element) (term~ddots-p element))
			   (eq (type-of element) (type-of old-element)))
		      nil)
		     ((or (term~hdots-p element) (term~ddots-p element))
		      (let ((variable (repr=matrix-ellipsis2variable element))
			    (range (term~ellipsis-range element)))
			(push variable result)
			(when range 
			  (push (list variable range) fixed-constraints))))
		     (t (push 1 result)))))
       fixed-constraints)))
  
  
  (defun repr=matrix-get-column-constraints (matrix)
    (declare (edited  "10-OCT-2004")
	     (authors Sorge)
	     (input   "A list representing a matrix.")
	     (effect  "None.")
	     (value   "Two lists of constraints:"
		      "1. The general flexible constraints for vertical length of ellipses."
		      "2. The concrete fixed constraints mapping single ellipsis to a constant length."))
    (let (fixed-constraints
	  (max-row (length matrix)))
      (values
       (loop for col from 1 to (length (car matrix))
	     collect
	     (do* ((col-val col (if (term~ddots-p element) (1+ col-val) col-val))
		   (row 1 (1+ row))
		   (old-element nil element)
		   (element (repr=element-at-position matrix row col-val)
			    (when (<= row max-row) (repr=element-at-position matrix row col-val)))
		   result)
		 ((> row max-row) (reverse result))
	       (print element)
	       (cond ((and (or (term~vdots-p element) (term~ddots-p element))
			   (eq (type-of element) (type-of old-element)))
		      nil)
		     ((or (term~vdots-p element) (term~ddots-p element))
		      (let ((variable (repr=matrix-ellipsis2variable element))
			    (range (term~ellipsis-range element)))
			(print variable)
			(push variable result)
			(when range 
			  (push (list variable range) fixed-constraints))))
		     (t (push 1 result)))))
       fixed-constraints)))
  

  (defun repr=matrix-solve-constraint-problem (constraints)
    (declare (edited  "11-OCT-2004")
	     (authors Sorge)
	     (input   "A list of constraints.")
	     (effect  "None.")
	     (value   "The solution for the constraint problem. Format????"))
    ;;; first simplify
    (do* ((constraints constraints (mapcar #'(lambda (x) (remove 1 x :count 1)) constraints)))
	((notevery #'(lambda (x) (find 1 x)) constraints) constraints))
	  
    )
  
  
  (defun repr=matrix-translate-ellipses (matrix &key (optimization repr*matrix-optimization))
    (declare (edited  "07-OCT-2004")
	     (authors Vxs)
	     (input   "A list representing a matrix and an indicator.")
	     (effect  "None.")
	     (value   "Don't know yet..."))
    ;; Steps:
    ;; 1) get row constraints
    ;; 2) get column constraints
    ;; 3) solve constraints
    ;; 4) check possible problems: all ellipsis are mapped to variables, constraints are consistent etc.
    ;; 5) Transform constraints into indices
    ;; 6) Construct lambda term

    (repr=matrix-reset-variable-mapping)
    (multiple-value-bind (row-constraints fixed-constraints1)
	(repr=matrix-get-row-constraints (keim~name matrix))
      (multiple-value-bind (column-constraints fixed-constraints2)
	  (repr=matrix-get-column-constraints (keim~name matrix))
	(print row-constraints)
	(print column-constraints)
	(print (append fixed-constraints1 fixed-constraints2))
    )))
  
  )

(defun repr=element-at-position (matrix row col)
  (declare (edited  "07-OCT-2004")
	   (authors Sorge)
	   (input   "A list of lists representing a matrix, two integers.")
	   (effect  "None.")
	   (value   "The entry at the given coordinates."))
  (if (and (>= row 1) (>= col 1))
      (or (nth (1- col) (nth (1- row) matrix))
	  (omega~error "Row ~A and column ~A does not exist in matrix ~A." row col matrix))
    (omega~error "Can't have non-positive rows (~A) and columns (~A) in a matrix." row col)))
  
  

(defun repr=matrix2ifthen-repr (matrix &key (optimization repr*matrix-optimization))
  (declare (edited  "10-MAY-2004")
	   (authors Sorge)
	   (input   "A list of indices value pairs and an indicator.")
	   (effect  "Creates a new keim object.")
	   (value   "The matrix given as a lambda abstraction."))
  (let* ((env (repr=env))
	 (num-type (env~lookup-object 'num env))
	 (i (term~variable-create 'i num-type))
	 (j (term~variable-create 'j num-type))
	 (ifthen (env~lookup-object 'ifthen env))
	 (= (logic~equality-constant))
	 (and (logic~conjunction-constant)))
    (flet ((equality (var val) (term~appl-create = (list var (term~constant-create val num-type))))
	   (conjunction (conj1 conj2) (term~appl-create and (list conj1 conj2))))
      (labels ((make-ifthen-term (ind-list)
				 (cond ((= (length ind-list) 1) (cadar ind-list))
				       ((= (length ind-list) 2)
					(term~appl-create ifthen (list (conjunction (equality i (caaar ind-list))
										    (equality j (cadaar ind-list)))
								       (cadar ind-list)
								       (cadadr ind-list))))
				       (t (term~appl-create ifthen (list (conjunction (equality i (caaar ind-list))
										      (equality j (cadaar ind-list)))
									 (cadar ind-list)
									 (make-ifthen-term (cdr ind-list))))))))
	(term~abstr-create (list i j) (make-ifthen-term matrix))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eval functions (simple version)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric repr~eval (term)
  (declare (edited  "07-MAY-2004")
	   (authors V1mpolle)
	   (input   "A function term.")
	   (effect  "-")
	   (value   "2 values: a term where all evaluations were applied and a list with preconditions."))
  (:method ((term term+appl))
	   (multiple-value-bind (fct premsfct)
	       (repr~eval (data~appl-function term))
	     (multiple-value-bind (args premsargs)
		 (repr~eval  (data~appl-arguments term))
	       (multiple-value-bind (result prems)
		   (case (length args)
		     (1 (apply #'repr=eval1 (cons fct args)))
		     (2 (apply #'repr=eval2 (cons fct args)))
		     (3 (apply #'repr=eval3 (cons fct args)))
		     (4 (apply #'repr=eval4 (cons fct args)))
					;	       (5 (apply #'repr=eval5 (cons fct args)))
		     (otherwise term))
		 (values result (append premsfct premsargs prems))))))
  (:method ((term term+constant))
	   (cond
	    ((term~matrix-p term)
	       (term~create-matrix (repr~eval (term~normalform term)) (repr=env)))
	    ((term~set-p term)
	       (term~create-set (repr~eval (term~normalform term)) (repr=env)))
	    (T  term)))
  (:method ((term cons))
	   (let* (precond
		  (result
		   (mapcar #'(lambda (x) (multiple-value-bind (res pre)
					     (repr~eval x)
					   (push pre precond)
					   res))
			   term)))
	     (values result (apply #'append precond))))
  (:method ((term term+term)) term))



(defgeneric repr=eval1 (fct arg1)
  (declare (edited  "07-MAY-2004")
	   (authors V1mpolle)
	   (input   "A function (2-ary) and its arguments")
	   (effect  "-")
	   (value   "2 values: a term where all evaluations where applied and a list with preconditions."))
  (:method ((fct (eql 'transpose))(arg1 term+matrix))
	   (let ((cols (term~matrix-columns arg1)))
	     (term~create-matrix
	      (mapcar #'(lambda (col)
			  (repr=nth-column col arg1))
		      (reverse (maplist #'length  (make-list cols)))) (repr=env))))
  (:method ((fct (eql 'singleton))(arg1 term+term))
	   (term~create-set (list arg1) (repr=env)
	   ))
  (:method ((fct (eql 'cardinality))(arg1 term+set))
	   (let* ((elements (term~normalform arg1))
		  (different? (every #'(lambda (x) x)
				     (maplist #'(lambda (elems)
						  (if (= (length elems) 1)
						      T
						    (every #'(lambda (elem)
							       (repr=check-not2 '= (car elems) elem))
							   (rest elems))))
					      elements))))
 	     (if different? (repr=make-term (length elements))
	       (repr=make-term fct arg1))))
  (:method ((fct term+constant) (arg1 t))
	   (repr=eval1 (keim~name fct) arg1))
  (:method ((fct t) (arg1 t))
	   (repr=make-term fct arg1)))

(defgeneric repr=eval2 (fct arg1 arg2)
  (declare (edited  "07-MAY-2004")
	   (authors V1mpolle)
	   (input   "A function (2-ary) and its arguments")
	   (effect  "-")
	   (value   "2 values: a term where all evaluations where applied and a list with preconditions."))
  (:method ((fct term+constant)(arg1 term+number)(arg2 term+number))
	   ;reuse the existing eval for natac
	   (natac~compute fct (list arg1 arg2) (repr=env)))
  (:method ((fct (eql 'union))(arg1 term+set)(arg2 term+set))
	   (term~create-set (union (keim~name arg1)(keim~name arg2))(repr=env)))
  (:method ((fct term+constant) (arg1 t)(arg2 t))
	   (repr=eval2 (keim~name fct) arg1 arg2))
  (:method ((fct t) (arg1 t)(arg2 t))
	   (repr=make-term fct  arg1 arg2)))

(defgeneric repr=eval3 (fct arg1 arg2 arg3)
  (declare (edited  "07-MAY-2004")
	   (authors V1mpolle)
	   (input   "A function (3-ary) and its arguments")
	   (effect  "-")
	   (value   "2 values: a term where all evaluations where applied and a list with preconditions."))
  (:method ((fct (eql 'matplus))(arg1 term+term)(arg2 term+matrix)(arg3 term+matrix))
	   (if  (and
		 (data~equal (term~type arg2)(term~type arg3))
		 (= (term~matrix-rows arg2) (term~matrix-rows arg3))
		 (= (term~matrix-columns arg2) (term~matrix-columns arg3)))
	       (let* (precond
		      (newmatrix   (term~create-matrix
				    (mapcar #'(lambda (row1 row2)
						(mapcar #'(lambda (elem1 elem2)
							    (multiple-value-bind (result pre)
								(repr~eval (term~appl-create arg1 (list elem1 elem2)))
							      (push pre precond)
							      result))
							row1 row2))
					    (keim~name arg2) (keim~name arg3)) (repr=env))))
		 (values
		  newmatrix
		  (append (apply #'append precond)
			  (repr=make-terms
			   (list 'subset (term~create-set (union (repr=matrix-elems arg2)(repr=matrix-elems arg3) :test #'term~alpha-equal)
							  (repr=env))
				 'F)
			   (list 'ring 'F arg1 'mul)))))
	     (omega~error "something wrong with sum of matrices ~A and ~A." arg2 arg3)))
  (:method ((fct (eql 'scalartimes))(op term+term)(scal term+term)(mat term+matrix))
	   (let* (precond
		  (newmatrix   (term~create-matrix
				(mapcar #'(lambda (row1)
					    (mapcar #'(lambda (elem1)
							(multiple-value-bind (result pre)
							    (repr~eval (term~appl-create op (list scal elem1)))
							  (push pre precond)
							  result))
						    row1))
					(keim~name mat)) (repr=env))))
					
	     (values
	      newmatrix
	      (append (apply #'append precond)
		      (repr=make-terms
		       (list 'subset (term~create-set (repr=matrix-elems mat) (repr=env)) 'F)
		       (list 'ring 'F 'mul op))))))
  (:method ((fct term+constant) (arg1 t)(arg2 t)(arg3 t))
	   (repr=eval3 (keim~name fct) arg1 arg2 arg3))
  (:method ((fct t) (arg1 t)(arg2 t)(arg3 t))
	   (repr=make-term fct  arg1 arg2 arg3)))



(defgeneric repr=eval4 (fct arg1 arg2 arg3 arg4)
  (declare (edited  "07-MAY-2004")
	   (authors V1mpolle)
	   (input   "A function (4-ary) and its arguments")
	   (effect  "-")
	   (value   "2 values: a term where all evaluations where applied and a list with preconditions."))
  (:method ((fct (eql 'mattimes))(arg1 term+term)(arg2 term+term)(arg3 term+matrix)(arg4 term+matrix))
	   (multiple-value-bind (term pre)
	       (repr~eval (repr=matrix-multiply arg1 arg2 arg3 arg4))
	     	     (values
		      term
		      (append pre
			      (repr=make-terms
			       (list 'subset (term~create-set (union (repr=matrix-elems arg3)(repr=matrix-elems arg4) :test #'term~alpha-equal)
							      (repr=env)) 'F)
			       (list 'ring 'F arg2 arg1))))))
  (:method ((fct term+constant) (arg1 t)(arg2 t)(arg3 t)(arg4 t))
	   (repr=eval4 (keim~name fct) arg1 arg2 arg3 arg4))
  (:method ((fct t) (arg1 t)(arg2 t)(arg3 t)(arg4 t))
	   (repr=make-term fct arg1 arg2 arg3 arg4)))
  

;; predicates and relations
;; loop detection?

(defgeneric repr~check (term)
  (declare (edited  "07-MAY-2004")
	   (authors V1mpolle)
	   (input   "A prediate term.")
	   (effect  "Calls annotated evaluation on subterms.")
	   (value   "A list with subgoals: empty list means that the property holds,"
		    "list containing the original proposition means no reduction possible."))
  (:method ((term term+appl))
	   (multiple-value-bind (evalterm prems)
	       (repr~eval term)
	       (let* ((fct (data~appl-function evalterm))
		      (args (data~appl-arguments evalterm))
		      (result
		       (if (eql (keim~name fct) 'not)
			   (repr=check-not (car args))
			 (case (length args)
			   (1 (apply #'repr=check1 (cons fct args)))
			   (2 (apply #'repr=check2 (cons fct args)))
					;(3 (apply #'repr=check3 (cons fct args)))
					;(4 (apply #'repr=check4 (cons fct args)))
					;(5 (apply #'repr=check5 (cons fct args)))
			   (otherwise nil)))))
		 (cond
		  ((null result) (if (term~alpha-equal evalterm term)
				     (list term)
				   (cons evalterm (repr~check prems))))
		  ((consp result) (if (member evalterm result :test #'term~alpha-equal)
				      (list term)
				    (repr~check (append result prems))))
		  (T (repr~check prems))))))
  (:method ((term term+term))
	   (list term))
  (:method ((term list))
	   (when (consp term)
	       (append (repr~check (first term))
		       (repr~check (rest term))))))
	     

	     
(defgeneric repr=check-not (term)
  (declare (edited  "07-MAY-2004")
	   (authors V1mpolle)
	   (input   "A prediate term.")
	   (effect  "-")
	   (value   "Checks whether negated property holds."))
  (:method ((term term+appl))
	   (let ((fct (data~appl-function term))
		 (args (data~appl-arguments term)))
	     (case (length args)
	       ;(1 (apply #'repr=check-not1 (cons fct args)))
	       (2 (apply #'repr=check-not2 (cons fct args)))
	       ;(3 (apply #'repr=check-not3 (cons fct args)))
	       ;(4 (apply #'repr=check-not4 (cons fct args)))
					;(5 (apply #'repr=check-not5 (cons fct args)))
	       (otherwise nil))))
  (:method ((term term+term)) nil))

;; the following functions either return
;; nil : property could not be shown
;; list with proposition : the new subgoals
;; else (T or any atom): property holds

(defgeneric repr=check1 (fct arg1)
  (:method ((fct term+set)(arg1 term+term))
	   (member arg1 (keim~name fct) :test #'term~alpha-equal))
  (:method ((fct (eql 'nat))(arg1 term+number))
	   (and (integerp (keim~name arg1))(> (keim~name arg1) 0)))
  (:method ((fct (eql 'int))(arg1 term+number))
	   (integerp (keim~name arg1)))
  (:method ((fct term+constant) (arg1 t))
	   (repr=check1 (keim~name fct) arg1))
  (:method ((fct t) (arg1 t))
	   nil))

	       
(defgeneric repr=check2 (fct arg1 arg2)
  (:method ((fct (eql 'in))(arg1 term+term)(arg2 term+term))
	   (repr=check1 arg2 arg1))
  (:method ((fct (eql 'subset))(arg1 term+set)(arg2 term+term))
	   (mapcar #'(lambda (sub) (repr=make-term 'in sub arg2)) (keim~name arg1)))
  (:method ((fct term+constant) (arg1 t)(arg2 t))
	   (repr=check2 (keim~name fct) arg1 arg2))
  (:method ((fct t) (arg1 term+constant)(arg2 t))
	   (repr=check2 fct (keim~name arg1) arg2))
  (:method ((fct t) (arg1 t)(arg2 term+constant))
	   (repr=check2 fct arg1 (keim~name arg2)))
  (:method ((fct t) (arg1 t)(arg2 t))
	   nil))

(defgeneric repr=check-not2 (fct arg1 arg2)
  (:method ((fct (eql '=))(arg1 integer)(arg2 integer))
	   (not (= arg1 arg2)))
  (:method ((fct (eql 'in))(arg1 term+term)(arg2 term+set))
	   (every #'(lambda (elem) (repr=check-not2 '= arg1 elem))(term~normalform arg2)))
  (:method ((fct (eql 'subset))(arg1 term+set)(arg2 term+set))
	   (some #'(lambda (elem) (repr=check-not2 'in elem arg2))(term~normalform arg1)))
  (:method ((fct (eql '=))(arg1 term+set)(arg2 term+set))
	   (or (repr=check-not2 'subset arg1 arg2)
	       (repr=check-not2 'subset arg2 arg1)))
  (:method ((fct term+constant) (arg1 t)(arg2 t))
	     (repr=check-not2 (keim~name fct) arg1 arg2))
  (:method ((fct t) (arg1 term+constant)(arg2 t))
	     (repr=check-not2 fct (keim~name arg1) arg2))
  (:method ((fct t) (arg1 t)(arg2 term+constant))
	     (repr=check-not2 fct arg1 (keim~name arg2)))
  (:method ((fct t) (arg1 t)(arg2 t))
	   nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evaluation for generalised terms (disjunctive with supports version)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass repr+sequent ()	
  ((supports     :initarg :supports
		 :initform nil
		 :accessor repr=supports)
   (processed     :initarg :processed
		   :initform nil
		   :accessor repr=processed)
   (goals     :initarg :goals
	      :initform nil
	      :accessor repr=goals)))


(defun repr=make-sequent (goals supps &optional processed)
  (make-instance 'repr+sequent
		:supports supps
		:goals (if (listp goals) goals (list goals))
		:processed processed))
		
(defgeneric repr~general-check (goal supps &optional max-depth)
  (:method ((goal node+node) (supps list) &optional max-depth)
	   (repr~general-check (node~formula goal) (mapcar #'node~formula supps) :until max-depth))
  (:method ((goal term+term) (supps list) &optional max-depth)
	   (repr=general-check (repr=make-sequent goal supps) :until max-depth)))

(defun repr=general-check (seq &key until depth)
  (let ((oldgoals (repr=goals seq)))
	;; generate supports
        (repr=generate-supports seq) 
	;; eval supports
	; (todo)
        ;; eval-fct: reuse old eval, then apply the new eval
	(omega~trace "~A ~A" (repr=goals seq)(repr=supports seq))
	(setf (repr=goals seq) (multiple-value-bind (evalterm prems)
				   (repr~eval (repr=goals seq))
				 (cons evalterm prems)))
	(omega~trace "~A ~A" (repr=goals seq)(repr=supports seq))
        (setf (repr=goals seq) (multiple-value-bind (evalterm prems)
				   (repr=general-eval (repr=supports seq)
						      (repr=goals seq))
				 (cons evalterm prems)))
	;; check-preds
	(omega~trace "~A ~A" (repr=goals seq)(repr=supports seq))
	;; newgoals = goals - supps

	;; again?
	(if (or
	     (and until (>= depth until))  ;max-depth reached
	     (and (every #'(lambda (mem)   ;nothing new
			     (member mem oldgoals :test #'term~alpha-equal))
			 (repr=goals seq))
		  (every #'(lambda (mem)   ;nothing new
			     (member mem  (repr=goals seq) :test #'term~alpha-equal))
			 oldgoals))) 
	    seq
	  (repr=general-check seq :until until :depth (1+ depth)))))


;; generate supps: get information from the terms

(defun repr=generate-supports (seq)
  (let ((goals (repr=goals seq)))
    (mapc #'(lambda (goal) (repr=generate-supps goal seq)) goals)
    seq))

(defun repr=generate-supps (term seq)
  (if (member term (repr=processed seq))
      seq
    (let ((new-supps (repr=gen-supps term)))
      (when new-supps
	(push new-supps (repr=supports seq))
	(push term (repr=processed seq)))
      (when (data~appl-p term)
	(mapc #'(lambda (term) (repr=generate-supps term seq)) 
	      (cons (data~appl-function term)
		    (data~appl-arguments term))))
      seq)))

      
(defgeneric  repr=gen-supps (term)
  (:method ((term term+annotated-term))
	   (let ((members (term~normalform term)))
	   (cond ((some #'term~gen-cyc-p members)
		  (let* (all-elems non-numbers equs) ;done like this because of problems with desctructive effects
		    (mapc #'(lambda (cyc)
			      (mapc #'(lambda (ele)
					(push ele all-elems)
					(unless (term~number-p ele) (push ele non-numbers)))
				    (term~normalform cyc)))
			  members)
		    (mapc #'(lambda (x)
				(mapc #'(lambda (y)
						 (push (repr=make-term 'not (list '=  x y)) equs))
					(remove x all-elems :test #'term~alpha-equal)))
			  non-numbers)
		    equs)))))
  (:method ((term t))
	   nil))
		  

;; general eval with respect to supps

(defgeneric repr=general-eval (supps term)
  (:method ((supps list)(term term+appl))
	   (multiple-value-bind (fct premsfct)
	       (repr=general-eval supps (data~appl-function term))
	     (multiple-value-bind (args premsargs)
		 (repr=general-eval  supps (data~appl-arguments term))
	       (multiple-value-bind (result prems)
		   (case (length args)
		     (1 (apply #'repr=geval1 (cons supps (cons fct args))))
		     (2 (apply #'repr=geval2 (cons supps (cons fct args))))
		     (3 (apply #'repr=geval3 (cons supps (cons fct args))))
		     (4 (apply #'repr=geval4 (cons supps (cons fct args))))
					;	       (5 (apply #'repr=eval5 (cons supps (cons fct args))))
		     (otherwise term))
		 (values result (append premsfct premsargs prems))))))
  (:method ((supps list)(term term+constant))
	   (cond
	    ((term~matrix-p term)
	       (term~create-matrix (repr=general-eval (term~normalform term)) (repr=env)))
	    ((term~set-p term)
	       (term~create-set (repr=general-eval (term~normalform term)) (repr=env)))
	    (T  term)))
  (:method ((supps list)(term cons))
	   (let* (precond
		  (result
		   (mapcar #'(lambda (x) (multiple-value-bind (res pre)
					     (repr=general-eval supps x)
					   (push pre precond)
					   res))
			   term)))
	     (values result (apply #'append precond))))
  (:method ((supps list)(term term+term)) term))


(defgeneric repr=geval1 (supps fct arg1)
  (:method ((supps list)(fct term+constant)(arg1 t))
	   (repr=geval1 supps (keim~name fct) arg1))
  (:method ((supps list)(fct t) (arg1 t))
	   (repr=make-term fct arg1)))
    
(defgeneric repr=geval2 (supps fct arg1 arg2)
  (:method ((supps list)(fct term+constant) (arg1 t)(arg2 t))
	   (repr=geval2 supps (keim~name fct) arg1 arg2))
  (:method ((supps list)(fct t) (arg1 t)(arg2 t))
	   (repr=make-term fct arg1 arg2)))

(defgeneric repr=geval3 (supps fct arg1 arg2 arg3)
  (:method ((supps list)(fct term+constant) (arg1 t)(arg2 t)(arg3 t))
	   (repr=geval3 supps (keim~name fct) arg1 arg2 arg3))
  (:method ((supps list)(fct t) (arg1 t)(arg2 t)(arg3 t))
	   (repr=make-term fct arg1 arg2 arg3)))

(defgeneric repr=geval4 (supps fct arg1 arg2 arg3 arg4)
  (:method ((supps list)(fct term+constant) (arg1 t)(arg2 t)(arg3 t)(arg4 t))
	   (repr=geval4 supps (keim~name fct) arg1 arg2 arg3 arg4))
  (:method ((supps list)(fct t) (arg1 t)(arg2 t)(arg3 t)(arg4 t))
	   (repr=make-term fct arg1 arg2 arg3 arg4)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; useful
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun repr=make-term (fct &rest args)
  (car  (repr=make-terms (cons fct args))))

(defun repr=make-terms (&rest lists)
  (let (mvs)
    (mapcar #'(lambda (list)
		(destructuring-bind (fct &rest args) list
		  (let ((fct (if (term~p fct) fct (repr=read (if (stringp fct) (read-from-string fct) fct )))))
		    (if args
			(let* ((type (data~abstr-domain (term~type
							 (if (data~schema-p fct)
							     (data~schema-range  fct)
							   fct))))
			       (argterms (mapcar #'(lambda (arg typ)
						     (if (term~p arg) arg
						       (or (rest (assoc arg mvs))
							   (cdar (push (cons arg (meta~variable-create arg typ)) mvs)))))
						 args type)))
			  (term~appl-create fct argterms))
		      fct))))
	    lists)))

(defun repr=read (obj)
  (post~read-object obj (repr=env) :existing-term))
	   



;; stuff for matrices

(defun repr=matrix-elems (matrix)
  (apply 'append (keim~name matrix)))

(defun repr=matrix-set (matrix)
  (term~create-set (repr=matrix-elems matrix)(repr=env)))

(defun repr=nth-row (n matrix)
  (nth (1- n) (keim~name matrix)))
  
(defun repr=nth-column (n matrix)
  (mapcar #'(lambda (row)
	      (nth (1- n) row))
	  (keim~name matrix)))
	      
(defun repr=matrix-multiply (times plus aij bij)
  (if  (and
	(data~equal (term~type aij)(term~type bij))
	(= (term~matrix-columns aij) (term~matrix-rows bij)))
      (let*  ((rows (term~matrix-rows aij))
	      (cols (term~matrix-columns bij)))
	(term~create-matrix
	 (mapcar #'(lambda (row)
		     (mapcar #'(lambda (elem)
				 (reduce  #'(lambda (x y)
					      (omega~trace "step ~A ~A" x y)
					      (term~appl-create
					       plus
					       (list x
						     y)))
					  (mapcar #'(lambda (x y)
						      (term~appl-create times (list x y)))
						  (repr=nth-row elem aij)
						  (repr=nth-column row bij))))
			     (reverse (maplist #'length (make-list rows)))))
		 (reverse (maplist #'length ' (make-list cols))))
	 (repr=env)))
    (omega~error "Matrices ~A and ~A can't be multiplied" aij bij)))






















#|
(setf plus (post~read-object 'plus (th~env 'matrix) :existing-term))
(setf times (post~read-object 'times (th~env 'matrix) :existing-term))
(setf mat1 (post~read-object '(mat (1 2 3 5)(6 8 9 0)) (th~env 'matrix) :existing-term))
(setf mat2 (post~read-object '(mat (1 2 99)(3 5 99)(6 8 99)(9 99 999)) (th~env 'matrix) :existing-term))
(setf check (post~read-object '(in 1 (set 1 2 3 4)) (th~env 'matrix) :existing-term))
(setf check (post~read-object '(not (in 1 (set 1 2 3 4))) (th~env 'integer) :existing-term))

(setf general-perm (data~abstr-n-scope (post~read-object '(lam (x num) (lam (y num) (SET (CYC x 10) (CYC y 8) (CYC 3 11) (CYC 5 7))))
		   (th~env perm*permutation-theory) :existing-term)))
|#

