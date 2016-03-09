

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stuff for learning, for MXJ
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package :omega)


(mod~defmod LEARN 
            :uses (arg com data infer meth omega param tac tacl tactic)
            :documentation "Learning new methods and tactics from given specifications."
            :exports (
                      
                      learn~compile-learned-stuff
                      learn~convert-expression
                      learn~convert-expression-to-expansion-function
                      learn~convert-expression-to-function
                      learn~create-tactic+method
                      learn~get-tactic
                      learn~interpret-learned-stuff
                      learn~produce-method
                      learn~produce-tactic
                      learn~save-learned-stuff
                      learn~save-learned-stuff-separately
                      learn~sequence
                      learn~star
                      learn~xor
                      
                      learn*direction-order
                      learn*sequence
                      learn*star
                      learn*xor))

;;; The following functions are internal in other modules and should not be used:
;;; (tac=computations tac=sideconds)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A couple of global constants to ease future changes to the syntax
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant learn*star '*         "Symbol for method repetition.")
(defconstant learn*sequence '&     "Symbol for method sequence.")
(defconstant learn*xor  '^         "Symbol for method either-or.")

(defconstant learn*direction-order '(:f :b :a)
  "A default order for tactic or method directions to be created.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The actual interface function
;; ------------------------------
;; These are the ones you should use outside this module, since they
;; should enable you to conveniently apply the code-producing parts
;; in the module....
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun learn~compile-learned-stuff (code)
  (declare (edited  "25-MAY-2000")
	   (authors Vxs)
	   (input   "A list of code for methods and tactics.")
	   (effect  "Compiles the code in the running lisp environment.")
	   (value   "Undefined."))
  (multiple-value-bind (funcs tacs infs)
      (learn=split-code-list code)
    (dolist (sexp funcs)
      (compile (eval sexp)))
    (dolist (sexp (append infs tacs))
      (eval sexp))))
  
(defun learn~interpret-learned-stuff (code)
  (declare (edited  "25-MAY-2000")
	   (authors Vxs)
	   (input   "A list of code for methods and tactics.")
	   (effect  "Interprets the code in the running lisp environment.")
	   (value   "Undefined."))
  (multiple-value-bind (funcs tacs infs)
      (learn=split-code-list code)
    (dolist (sexp (append funcs infs tacs))
      (eval sexp))))

(defun learn~save-learned-stuff (code file-name &optional (action :append))
  (declare (edited  "25-MAY-2000")
	   (authors Vxs)
	   (input   "A list of code for methods and tactics, a file-name and optionally a symbol"
		    "describing the action if the given file already exists.")
	   (effect  "Interprets the code in the running lisp environment.")
	   (value   "Undefined."))
  (with-open-file (file file-name
			:direction :output
			:if-exists action
			:if-does-not-exist :create)
		  (dolist (sexp code)
		    (terpri file)
		    (format file "~A" sexp)
		    (terpri file))))

(defun learn~save-learned-stuff-separately (code tactic-file method-file &optional (action :append))
  (declare (edited  "25-MAY-2000")
	   (authors Vxs)
	   (input   "A list of code for methods and tactics, two file-names and optionally a symbol"
		    "describing the action if the given file already exists.")
	   (effect  "Interprets the code in the running lisp environment.")
	   (value   "Undefined."))
  (multiple-value-bind (tactic-code method-code)
      (learn=split-code-list2 code)
    (with-open-file (file tactic-file
			  :direction :output
			  :if-exists action
			  :if-does-not-exist :create)
		    (dolist (sexp tactic-code)
		      (terpri file)
		      (format file "~A" sexp)
		      (terpri file)))
    (with-open-file (file method-file
			  :direction :output
			  :if-exists action
			  :if-does-not-exist :create)
		    (dolist (sexp method-code)
		      (terpri file)
		      (format file "~A" sexp)
		      (terpri file)))))

(defun learn=split-code-list (list)
  (declare (edited  "25-MAY-2000")
	   (authors Vxs)
	   (input   "A list of sexpressions.")
	   (effect  "None.")
	   (value   "Three values: 1 -- a list of functions, 2 -- a list of method and tactic definitions,"
		    "and 3 -- a list of inference and command definitions."))
  (let (functions meth+tacs inferences)
    (dolist (sexp list)
      (let ((fexp (car sexp)))
      (cond ((string-equal fexp 'defun) (push sexp functions))
	    ((or (string-equal fexp 'infer~defmethod)
		 (string-equal fexp 'infer~deftactic)
		 (string-equal fexp 'com~defcommand))
	     (push sexp inferences))
	    ((or (string-equal fexp 'tac~deftactic)
		 (string-equal fexp 'meth~defmethod))
	     (push sexp meth+tacs)))))
    (values functions meth+tacs inferences)))
				   
(defun learn=split-code-list2 (list)
  (declare (edited  "26-MAY-2000")
	   (authors Vxs)
	   (input   "A list of sexpressions.")
	   (effect  "None.")
	   (value   "Two values:"
		    "1 -- A list of definitions and functions needed for defining a tactic."
		    "2 -- A list of definitions and functions needed for defining a method."))
  (let ((div (position 'infer~deftactic list :test #'string-equal :key #'car)))
    (values (subseq list div) (subseq list 0 div))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specifications are of the form:
;; (& (^ (inv-r func1)
;;       (inv-l func2))
;;    (* (^ (a-r func3)
;;          (a-l func4))
;;       tfunc1)
;;    (* (unit func5) tfunc2))
;; where &,^,* are the combination connectives,
;; inv-r, etc. are tactics,
;; func1, etc. are functions to compute parameters,
;; tfunc1, etc. are termination functions for the * operator.
;;
;; The list of parameters has to be of the form: '((p position pos+position pos) (t term term+term))
;; All functions and termination functions of the specification need to be applicable to the
;; parameters of the given type.

(defun learn~create-tactic+method (specification parameters theory outer-direction &key
						 (tac-name (learn=make-gensym-name "TAC"))
						 (meth-name (learn=make-gensym-name "METH")))
  (declare (edited  "17-MAY-2000")
	   (authors Vxs)
	   (input   "A learned specification, a list of triples specifying parameter-types,types, and method-types,"
		    "a symbol specifying the theory of the tactic/method to be created, and a symbol indicating"
		    "the direction the tactic/method is to be applied."
		    "Optionally two symbols specifying the tactic and the method name.")
	   (effect  "Creates tactics, methods, plenty of code, etc.")
	   (value   "Don't know yet."))
  (when (and (learn=check-specification specification)
	     (learn=check-parameters parameters)
	     (learn=check-direction outer-direction))
    (let* ((param+names (pairlis (mapcar #'(lambda (x) (learn=make-gensym-name (car x))) parameters) parameters))
	   (directions (list outer-direction :a))
	   (functions (mapcar #'(lambda (direction)
				  (learn~convert-expression-to-function
				   specification direction outer-direction :name-prefix tac-name))
			      directions))
	   (func-names (mapcar #'second functions))
	   (expansion (learn~convert-expression-to-expansion-function specification outer-direction :name-prefix tac-name))
	   (exp-name (second expansion))
	   (dummy (print exp-name))
	   (tactic-code (learn~produce-tactic tac-name theory func-names exp-name param+names directions))
	   (dummy2 (print 'hier2))
	   (method-code (learn~produce-method meth-name theory tac-name param+names directions))
	   )
      (append method-code tactic-code functions (list expansion))
      )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Creating the Method Code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun learn~produce-method (name theory tactic parameters &optional (directions learn*direction-order))
  (declare (edited  "18-MAY-2000")
	   (authors Vxs)
	   (input   "Three symbols and a list of parameter tuples.")
	   (effect  "None.")
 	   (value   "A list containing the code for the method. In detail:"
		    "The inference method code and the code for the methods in three application directions."))
  (let* ((methods (mapcar #'(lambda (direction)
			      (learn=produce-method name theory tactic parameters direction))
			  directions))
	 (meth-names (mapcar #'cadr methods))
	 (inference (learn=produce-inference name meth-names (mapcar #'second parameters) :method directions)))
    (cons inference methods)))

(defgeneric learn=produce-method (name theory tactic parameters direction)
  (declare (edited  "18-MAY-2000")
	   (authors Vxs)
	   (input   "Three symbols, a list of parameter tuples, and a symbol indicating the application direction.")
	   (effect  "None.")
	   (value   "The code for a single tactic depending on the application direction."))
  (:method ((name symbol) (theory symbol) (tactic symbol) (parameters list) (direction (eql :f)))
	   (learn=produce-method-code '(- L2) '(+ L1) '((L1 o prln) (Phi o))
				      '((L2 () Phi)) '(mlist (mnil) L2)
				      '((L1 (mfirst dummy)))				       
				      direction))
  (:method ((name symbol) (theory symbol) (tactic symbol) (parameters list) (direction (eql :b)))
	   (learn=produce-method-code '(+ L2) '(- L1) '((L2 o prln) (Psi o))
				      '((L1 () Psi)) '(mlist L1 (mnil))
				      '((L2 (msecond dummy)))				       
				      direction))
  (:method ((name symbol) (theory symbol) (tactic symbol) (parameters list) (direction (eql :a)))
	   (learn=produce-method-code '(- L2) '(- L1) '((Phi o) (Psi o))
				      '((L2 () Phi) (L1 () Psi)) '(mlist L1 L2) nil direction))
  (:method ((name string) theory tactic parameters direction)
	   (learn=produce-method (read-from-string name) theory tactic parameters direction))
  (:method (name (theory string) tactic parameters direction)
	   (learn=produce-method name (read-from-string theory) tactic parameters direction))
  (:method ((name string) theory tactic parameters direction)
	   (learn=produce-method name theory tactic parameters (read-from-string direction))))

;; The following macro is rather delicate and should be read with caution and only by those
;; not too afraid of nested back-quote syntax....    VS
(defmacro learn=produce-method-code (premise conclusion meth-vars decl-cont call computations direction)
  (declare (edited  "25-MAY-2000")
	   (authors Vxs)
	   (input   "Some specifications for a method.")
	   (effect  "None.")
	   (value   "Inserts function code that produces method code."))
  `(let* ((meth-name (read-from-string (format nil "~A-~A" name ,direction)))
	  (para-decl (mapcar #'(lambda (para) (list (first para) 'o (fourth para))) parameters))
	  (para-names (mapcar #'car parameters))
	  (boundps (read-from-string (format nil "(~{(bound-p ~A) ~})" para-names))))
     `(meth~defmethod ,meth-name ,name (in ,theory)
		      (parameters ,@para-names)
		      (reasoning :planning)
		      (rating 0)
		      (declarations
		       (sorted-meta-variables ,@,meth-vars ,@para-decl))
		      (premises ,,premise)
		      (application-condition
		       (mand ,@boundps
			     (test-tactic (:symbol ,tactic)
					  ,,call
					  (mlist ,@para-names))))
		      (conclusions ,,conclusion)
		      (outline-computations
		       (dummy
			(apply-tactic (:symbol ,tactic)
				      ,,call
				      (mlist ,@para-names)))
		       ,@,computations)
		      (decl-content ,@,decl-cont)
		      (proc-content apply-tactic))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Creating the Tactic Code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun learn~produce-tactic (name theory functions exp-function parameters &optional (directions learn*direction-order))
  (declare (edited  "18-MAY-2000")
	   (authors Vxs)
	   (input   "Two symbol, a list of function names, a function name, and a list of parameter tuples.")
	   (effect  "None.")
	   (value   "A list containing the code for the tactic definition:"
		    "An inference definition, three tactic definitions, and a command definition"
		    "together with its execution function."))
  (let* ((tactics (mapcar #'(lambda (function direction)
			      (learn=produce-tactic name theory function parameters direction))
			  functions directions))
	 (tac-names (mapcar #'cadr tactics))
	 (inference (learn=produce-inference name (cons exp-function tac-names)
					     (mapcar #'second parameters) :tactic directions))
	 (command (learn=produce-command name theory (cadr inference) parameters)))
    (cons inference (append tactics command))))

(defun learn=produce-command (name theory inference parameters)
  (declare (edited  "18-MAY-2000")
	   (authors Vxs)
	   (input   "Three symbols and a list of tuples representing parameter names and types.")
	   (effect  "None.")
	   (value   "A list containing the code for a command with the name and a function applying an"
		    "inference method of the same name."))
  (let* ((func (read-from-string (format nil "execute-~A" name)))
	 (function (list :function func))
	 (para-names (mapcar #'first parameters))
	 (para-types (mapcar #'second parameters))
	 (helps (cons "Support Line" (cons "Open Line" (make-list (length parameters) :initial-element "")))))
    (list
     `(com~defcommand ,name
		      (argnames line1 line2 ,@para-names)
		      (argtypes ndline ndline ,@para-types)
		      (arghelps ,@helps)
		      ,function
		      (frag-cats tactics ,theory)
		      (log-p t))
     `(defun ,func (l1 l2 ,@para-names)
	(infer~compute-outline ',inference (list l2 l1) (list ,@para-names))))))

(defgeneric learn=produce-inference (name tactics parameters type &optional (directions learn*direction-order))
  (declare (edited  "18-MAY-2000")
	   (authors Vxs)
	   (input   "A symbol, a list of symbols representing tactics, a list of symbols parameter types,"
		    "and a symbol indicating the type of the inference method.")
	   (effect  "None.")
	   (value   "The code for defining an appropriate inference method."))
  (:method ((name symbol) (tactics list) (parameters list) (type (eql :tactic))
	    &optional (directions learn*direction-order))
	   (let ((exp-function (car tactics))
		 (tacs (cdr tactics)))
	     `(infer~deftactic ,name
			       (outline-mappings ,(learn=outline-pattern2applications directions tacs))
			       (expansion-function ,exp-function)
			       (parameter-types ,@parameters))))
  (:method ((name symbol) (methods list) (parameters list) (type (eql :method))
	    &optional (directions learn*direction-order))
	     `(infer~defmethod ,name
			       (outline-mappings ,(learn=outline-pattern2applications directions methods))
			       (parameter-types ,@parameters))))

(defun learn=outline-pattern2applications (directions appls)
  (declare (edited  "24-MAY-2000")
	   (authors Vxs)
	   (input   "Two lists of symbols.")
	   (effect  "None.")
	   (value   "A piece of code to map outline-patterns for each given direction to the applications."))
  (let ((fpos (position :f directions))
	(bpos (position :b directions))
	(apos (position :a directions)))
    (remove-if #'null
	       (list (learn=outline-pattern2application fpos appls :f)
		     (learn=outline-pattern2application bpos appls :b)
		     (learn=outline-pattern2application apos appls :a))
    )))

(defgeneric learn=outline-pattern2application (position appls direction)
  (declare (edited  "24-MAY-2000")
	   (authors Vxs)
	   (input   "A non-negative integer and a list of symbols.")
	   (effect  "None.")
	   (value   "A piece of code to map from a certain outline to an application"))
  (:method ((position number) (appls list) (direction (eql :f)))
	   `((nonexistent existent) ,(nth position appls)))
  (:method ((position number) (appls list) (direction (eql :b)))
	   `((existent nonexistent) ,(nth position appls)))
  (:method ((position number) (appls list) (direction (eql :a)))
	   `((existent existent) ,(nth position appls)))
  (:method ((position null) (appls list) direction)
	   (declare (ignore direction))
	   (values)))


(defgeneric learn=produce-tactic (name theory function parameters direction)
  (declare (edited  "18-MAY-2000")
	   (authors Vxs)
	   (input   "Three symbols, a list of parameter tuples, and a symbol indicating the application direction.")
	   (effect  "None.")
	   (value   "The code for a single tactic depending on the application direction."))
  (:method ((name symbol) (theory symbol) (function symbol) (parameters list) (direction (eql :f)))
	   (let ((tac-name (read-from-string (format nil "~A-f" name)))
		 (parameters (mapcar #'(lambda (param) (list (first param) (third param))) parameters))
		 (para-names (mapcar #'car parameters)))
	     `(tac~deftactic ,tac-name ,name (in ,theory)
			     (parameters ,@parameters)
			     (premises L1)
			     (conclusions L2)
			     (computations (L2 (,function (formula L1) ,@para-names)))
			     (sideconditions (,function (formula L1) ,@para-names)))))
  (:method ((name symbol) (theory symbol) (function symbol) (parameters list) (direction (eql :b)))
	   (let ((tac-name (read-from-string (format nil "~A-b" name)))
		 (parameters (mapcar #'(lambda (param) (list (first param) (third param))) parameters))
		 (para-names (mapcar #'car parameters)))
	     `(tac~deftactic ,tac-name ,name (in ,theory)
			     (parameters ,@parameters)
			     (premises L1)
			     (conclusions L2)
			     (computations (L1 (,function (formula L2) ,@para-names)))
			     (sideconditions (,function (formula L2) ,@para-names)))))
  (:method ((name symbol) (theory symbol) (function symbol) (parameters list) (direction (eql :a)))
	   (let ((tac-name (read-from-string (format nil "~A-a" name)))
		 (parameters (mapcar #'(lambda (param) (list (first param) (third param))) parameters))
		 (para-names (mapcar #'car parameters)))
	     `(tac~deftactic ,tac-name ,name (in ,theory)
			     (parameters ,@parameters)
			     (premises L1)
			     (conclusions L2)
			     (sideconditions (,function (formula L1) (formula L2) ,@para-names)))))
  (:method ((name string) theory function parameters direction)
	   (learn=produce-tactic (read-from-string name) theory function parameters direction))
  (:method (name (theory string) function parameters direction)
	   (learn=produce-tactic name (read-from-string theory) function parameters direction))
  (:method ((name string) theory function parameters direction)
	   (learn=produce-tactic name theory function parameters (read-from-string direction))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Producing Functions for sideconditions and computations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric learn~convert-expression-to-function (learn-expr direction outer-direction &key (name-prefix 'mxj))
  (declare (edited  "18-MAY-2000")
	   (authors Ameier)
	   (input  "Three arguments: a learn-expr, a direction (b,a,f) and a name prefix for the function.")
	   (effect "None." )  
	   (value  "The code for the new function."))

  (:method ((learn-expr list) (direction symbol) (outer-direction symbol) &key (name-prefix 'mxj))
	   (let* ((function-code (learn~convert-expression learn-expr direction))
		  (new-name (learn=make-gensym-name name-prefix)))
	     
	     `(defun ,new-name (input-formula &rest param-list)
		(funcall ,function-code input-formula param-list))))

  (:method ((learn-expr list) (direction (eql :a)) (outer-direction symbol) &key (name-prefix 'mxj))
	   (let* ((new-name (learn=make-gensym-name name-prefix)))
	     ;;; here needs to go the dependency wrt. the direction of the learned tactic
	     (if (string-equal outer-direction :b)
		 `(defun ,new-name (prem conc &rest param-list)
		    (data~equal prem (funcall ,(learn~convert-expression learn-expr :b) conc param-list)))
	       `(defun ,new-name (prem conc &rest param-list)
		  (data~equal conc (funcall ,(learn~convert-expression learn-expr :f) prem param-list)))))))


(defun learn~convert-expression-to-expansion-function (learn-expr outer-direction &key (name-prefix 'mxj))
  (declare (edited  "21-MAY-2000")
	   (authors Ameier)
	   (input   "Three arguments: a learn-expr, an outer direction (b,f) and a name prefix for the function.")
	   (effect  "None.")
	   (value   "The code for the new expansion function."))
  (let* ((new-name (learn=make-gensym-name name-prefix)))
	     ;;; here needs to go the dependency wrt. the direction of the learned tactic
    (if (string-equal outer-direction :b)
	`(defun ,new-name (outline &rest param-list)
	   (tacl~init outline)
	   (let* ((result (funcall ,(learn~convert-expression learn-expr :b :expansion 't) (first outline) param-list)))
	     (tacl~apply 'weaken (list result (second outline)) nil))
	   (tacl~end))
      `(defun ,new-name (outline &rest param-list)
	 (tacl~init outline)
	 (let* ((result (funcall ,(learn~convert-expression learn-expr :f :expansion 't) (second outline) param-list)))
	   (tacl~apply 'weaken (list (first outline) result) nil))
	 (tacl~end)))))
;;
;;
;; Syntax for learn-expr:
;;
;; (& list-of(comp-pairs))                                -> also called sequence-expression
;; (^ list-of(comp-pairs))                                -> also called xor-expression
;; (* comp-pair termination-condition)                    -> also called star-expression
;;
;; Thereby:
;; termination-condition is a function with two arguments: formula and list of (external parameters)
;; comp-pair is a pair consisting of a computation-expression and a parameter-expression
;; A computation-expression is either a tactic (in this case the parameter-expression has to be a function with two
;; arguments: formula and list of (external parameters), that returns a list of internal parameters) or again a
;; learn-expr (in this case the parameter-expression is ignored).
;;
;;
;; Example:
;;
;;(& (* (^ (A-L internal-param-A-L) (A-R internal-param-A-R))
;;      Assoc-term-cond)
;;   (^ (INV-L internal-param-INV-L) (INV-R internal-param-INV-R))
;;   (^ (UNIT-L internal-param-UNIT-L) (UNIT-R internal-param-UNIT-R))
;;   )
;;
;; Note that none of the given nils (for empty internal parameter computation) is necessary!
;;
;;
;; learn~convert-expression-to-function produces a function of two arguments: a formula and a list of parameters
;;
;;

(defun learn~convert-expression (comp-expression direction &key (expansion nil))
  (declare (edited  "18-MAY-2000")
	   (authors Ameier)
	   (input  "Two arguments: a computation-expression and a direction (b,a,f).")
	   (effect "None." )  
	   (value  "Multiple-value:"
		   "First: A code part corresponding to the computation-expression is computed, this is a lisp exprssion"
		   "       which is a lambda function on two arguments: input-formula and list-of-params."
		   "Second: A sidecomdition for this application."))
  (if (listp comp-expression)
      ;; -> interleaved application
      (let* ((head (first comp-expression)))
	(cond ((string-equal head learn*star)
	       (values (learn=convert-star-expression-to-function-code comp-expression direction :expansion expansion)
		       nil))
	      ((string-equal head learn*sequence)
	       (values (learn=convert-sequence-expression-to-function-code comp-expression direction :expansion expansion)
		       nil))
	      ((string-equal head learn*xor)
	       (values (learn=convert-xor-expression-to-function-code comp-expression direction :expansion expansion)
		       nil))))
    ;; -> basic tactic application
    (learn=convert-tactic-expression comp-expression direction :expansion expansion)))

(defun learn=convert-star-expression-to-function-code (star-expression direction &key (expansion nil)) 
  (declare (edited  "18-MAY-2000")
	   (authors Ameier)
	   (input  "Two arguments: a star-expression and a direction (b,a,f).")
	   (effect "None." )  
	   (value  "A code part corresponding to the star-expression is computed, this is a lisp exprssion"
		   "which is a lambda function on two arguments: input-formula and list-of-params."))
  
  (let* ((sign (first star-expression))
	 (expression (second star-expression))
	 (termination-condition (third star-expression)))
    
    (if (null (string-equal sign learn*star))
	(omega~error "~%Function learn\~convert-star-expression-to-function-code called with a non-star expression: ~A"
		     star-expression)

      (let* ((interleaved-p (and (listp expression)
				 (find (first expression) (list learn*star learn*sequence learn*xor) :test #'string-equal)))
	     (comp-expression (if interleaved-p
				  ;; interleaved case
				  expression
				;; tactic basic case
				(first expression))) 
	     (internal-param-function (if interleaved-p
					  ;; interleaved case
					  nil
					;; tactic basic case
					(second expression))))
	(multiple-value-bind
	    (code-of-comp-function code-of-sidecond-function)
	    (learn~convert-expression comp-expression direction :expansion expansion)
	  
	  `#'(lambda (input-formula list-of-params)
	       (learn~star input-formula
			   (list ,(learn=add-quote-if-function-name code-of-comp-function)
				 ,(learn=add-quote-if-function-name code-of-sidecond-function)
				 ,(learn=add-quote-if-function-name internal-param-function))
			   list-of-params
			   ,(learn=add-quote-if-function-name termination-condition))))))))  ;;; removed one learn=add-quote-.....


(defun learn=convert-xor-expression-to-function-code (xor-expression direction &key (expansion expansion)) 
  (declare (edited  "18-MAY-2000")
	   (authors Ameier)
	   (input  "Two arguments: a xor-expression and a direction (b,a,f).")
	   (effect "None." )  
	   (value  "A code part corresponding to the xor-expression is computed, this is a lisp exprssion"
		   "which is a lambda function on two arguments: input-formula and list-of-params."))
  
  (let* ((sign (first xor-expression))
	 (comp-pair-list (rest xor-expression)))
    
    (if (null (string-equal sign learn*xor))
	(omega~error "~%Function learn\~convert-xor-expression-to-function-code called with a non-xor expression: ~A"
		     xor-expression)
      
      (let* ((internal-code (learn=convert-comp-pair-list comp-pair-list direction :expansion expansion)))
	
	`#'(lambda (input-formula list-of-params)
	     (learn~xor input-formula
			,internal-code
			list-of-params))))))

(defun learn=convert-sequence-expression-to-function-code (sequence-expression direction &key (expansion expansion)) 
  (declare (edited  "18-MAY-2000")
	   (authors Ameier)
	   (input  "Two arguments: a sequence-expression and a direction (b,a,f).")
	   (effect "None." )  
	   (value  "A code part corresponding to the sequnece-expression is computed, this is a lisp exprssion"
		   "which is a lambda function on two arguments: input-formula and list-of-params."))
  
  (let* ((sign (first sequence-expression))
	 (comp-pair-list (rest sequence-expression)))
    
    (if (null (string-equal sign learn*sequence))
	(omega~error "~%Function learn\~convert-sequence-expression-to-function-code called with a non-sequence expression: ~A"
		     sequence-expression)
      
      (let* ((internal-code (learn=convert-comp-pair-list comp-pair-list direction :expansion expansion)))
	
	`#'(lambda (input-formula list-of-params)
	     (learn~sequence input-formula
			     ,internal-code
			     list-of-params))))))

(defun learn=convert-comp-pair-list (comp-pair-list direction &key (expansion expansion))
  (do* ((current-code (list 'list))
	(rest-comp-pair-list comp-pair-list (rest rest-comp-pair-list)))
      ((null rest-comp-pair-list)
       current-code)
    (let* ((expression (first rest-comp-pair-list))
	   (interleaved-p (and (listp expression)
			       (find (first expression) (list learn*star learn*sequence learn*xor) :test #'string-equal)))
	   (comp-expression (if interleaved-p
				;; interleaved case
				expression
			      ;; tactic basic case
			      (first expression))) 
	   (internal-param-function (if interleaved-p
					;; interleaved case
					nil
				      ;; tactic basic case
				      (second expression))))
      (multiple-value-bind
	  (code-of-comp-function code-of-sidecond-function)
	  (learn~convert-expression comp-expression direction :expansion expansion)
	
	(setf current-code  (append current-code `((list ,(learn=add-quote-if-function-name code-of-comp-function)
							 ,(learn=add-quote-if-function-name code-of-sidecond-function)
							 ,(learn=add-quote-if-function-name internal-param-function)))))))))

(defun learn=add-quote-if-function-name (function-code)
  (if (symbolp function-code)
      `(quote ,function-code)
    function-code))

(defun learn=convert-tactic-expression (tactic-expression direction &key (expansion nil))
  (declare (edited  "18-MAY-2000")
	   (authors Ameier)
	   (input  "Two arguments: a tactic-expression and a direction (b,a,f).")
	   (effect "None." )  
	   (value  "Multiple-value:"
		   "First: A code part corresponding to the tactic-expression is computed, this is a lisp exprssion"
		   "       which is a lambda function on two arguments: input-formula and list-of-params."
		   "Second: the side-condition function."))
  (if expansion
      (if (string-equal direction :b)
	  (values
	   `#'(lambda (input-formula list-of-params)
		(second (tacl~apply ',tactic-expression (list input-formula nil) list-of-params)))
	   nil)
	(values
	 `#'(lambda (input-formula list-of-params)
	      (first (tacl~apply ',tactic-expression (list nil input-formula) list-of-params)))
	 nil))
    (let* ((tactic tactic-expression)
	   (tac (learn~get-tactic tactic direction))
	   (tac-object (if (tac~p tac)
			   tac
			 (tac~find-tactic tac))))
      (multiple-value-bind
	  (tactic-computation-function tactic-sidecondition-function)
	  (learn=extract-comp-func-and-side-cond-from-tactic tac-object)
	(values tactic-computation-function
		tactic-sidecondition-function)))))

(defun learn=extract-comp-func-and-side-cond-from-tactic (tactic)
  (declare (edited  "18-MAY-2000")
	   (authors Ameier)
	   (input  "An object of type TAC+TACTIC")
	   (effect "None." )  
	   (value  "Multiple-value:"
		   "First: the first computation function used in the computations slot of the tactic (if there is"
		   "       more than one computation function a warning message is given, but all except the first"
		   "       one are ignored."
		   "Second: if there is exactly one sidecondition then the function of this sidecondition is returned."
		   "        otherwise the list of sideconditions (nil is possible) are combined into one function"
		   "        on two arguments: formula and list of parameters. Then the code of this function is"
		   "        returned."))
  (let* ((computations (tac=computations tactic))
	 (sideconditions (tac=sideconds tactic))
	 (comp-function (first (second (first computations))))
	 (sidecond-function (cond ((null sideconditions)
				   `#'(lambda (input-formula list-of-parameters)
				      t))
				  ((= (length sideconditions) 1)
				   (first (first sideconditions)))
				  (t
				   (learn=produce-side-cond-function-from-list sideconditions)))))
    (values comp-function
	    sidecond-function)))

(defun learn=produce-side-cond-function-from-list (sidecondition-list)
  `#'(lambda (input-formula param-list)
     ,(do* ((current-formula '(and))
	    (rest-sideconds sidecondition-list (rest rest-sideconds)))
	  ((null rest-sideconds)
	   current-formula)
	(setf current-formula (append current-formula
				      (list (list (first (first rest-sideconds)) 'input-formula 'param-list)))))))
				      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic Elements for Translating Specifications to Code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun learn~xor (input-formula comp-triple-list external-parameters &rest resti)
  (declare (edited  "18-MAY-2000")
	   (authors Ameier)
	   (input  "Three arguments: a input formula, a list of computation triples, and a list of external"
		   "parameters.")
	   (effect "None." )  
	   (value  "If the xor is executable a formula, otherwise nil."))
  (do* ((rest-comp-triple-list comp-triple-list (rest rest-comp-triple-list))
	(back-formula nil))
      ((or back-formula
	   (null rest-comp-triple-list))
       
       (cond (back-formula
	      ;; one of the xor components was applied and produced back-formula
	      ;; -> return back-formula
	      back-formula)
	     ((null rest-comp-triple-list)
	      ;; xor list finished and none of the components could be applied
	      ;; -> return nil
	      nil)))
    
    (let* ((next-comp-triple (first rest-comp-triple-list))
	   (next-comp-func (first next-comp-triple))
	   (next-comp-sidecond (second next-comp-triple))
	   (next-param-func (third next-comp-triple))
	   
	   ;; Notice that both next-comp-sidecond and next-param-func are optional arguments!
	   ;; If here are computation function of a tactic should be called directly
	   ;; then they should be both given (since such application depend on sidecondition
	   ;; which have to be checked first and on parameters)
	   ;; If here a interleaved application of other tacticals has to to be applied
	   ;; these interleaved stuff has neither parameters nor sideconditions.
	   ;; In this case the external-parameters are inherited as internal-parameters
	   
	   (internal-parameters (if next-param-func
				    (funcall next-param-func input-formula external-parameters)
				  external-parameters))
	   ;; -> a list of parameters to call the next function and sidecondition
	   
	   (sidecond (if next-comp-sidecond
			 (funcall next-comp-sidecond input-formula internal-parameters)
		       nil))
	   ;; Is the sidecondition fulfilled?
	   )
      
      (when (or sidecond
		(null next-comp-sidecond))
	;; when sidecond is fulfilled or not given, apply computation function and set back-formula to
	;; the result of this application to sign success of xor (see do loop)
	(setf back-formula (funcall next-comp-func input-formula internal-parameters))))))



;;
;; An example for learn~xor:
;;
;;(learn~xor 3 
;;	   (list (list #'(lambda (a internalparamlist) 
;;			   (+ a (first internalparamlist)))  
;;		       #'(lambda (a internalparamlist) 
;;			   (evenp a)) 
;;		       #'(lambda (a externalparamlist)
;;			   (if (evenp (first externalparamlist))
;;			       (list 2)
;;			     (list 3))))
;;		 (list #'(lambda (a internalparamlist) 
;;			   (+ a (first internalparamlist)))  
;;		       #'(lambda (a internalparamlist) 
;;			   (oddp a)) 
;;		       #'(lambda (a externalparamlist)
;;			   (if (evenp (first externalparamlist))
;;			       (list 7)
;;			     (list 15)))))
;;	   (list 2))
;;
;; Syntax for learn~xor:
;;
;; 1. Argument: Input Formula
;; 2. Argument: List of tuples of: Computation-Function (computes the new formel from the input formula + internal
;;                                                        parameterlist)
;;                                  Sidecondition-Function (checks whether Computation-Function applicable wrt. input
;;                                                          formala and internal parameterlist)
;;                                  InternalParameter-Function (computes the internal parameterlist wrt. the input formula
;;                                                              and the external parameterlist)
;; 3. Argument: list of external parameters
;;
;; learn~xor is executable if one of the tuples can be executed.
;; One of the tuples can be executed if its Sidecondition-Function evaluates to true.
;; The result of the execution of a triple is the result of the appliaction of its computation-function.
;; The first executable triple (wrt. the order in the list) is executed and its result is returned as result of learn~xor.
;; If none of the tuples can be executed nil is returned as result of learn~xor.
;;
;; Notice: a computation-function has to be a function with two arguments: formula + internal-parameterlist
;;         a sidecondition-function has to be a function with two arguments: formula + internal-parameterlist
;;         a InternalParameter-Function has to be a function with two arguments: formula + external-parameterlist
;; Notice: We can ommit to give the Sidecondition-Function and InternalParameter-Function (or write nil for them) if:
;;         In case of Sidecondition-Function: the sidecondition is always true (nil is interpreted as no sidecondition
;;         exists)
;;         In case of InternalParameter-Function: The Computation-Function and the InternalParameter-Function need no
;;         internal parameters
;;



(defun learn~sequence (input-formula comp-triple-list external-parameters &rest resti)
  (declare (edited  "18-MAY-2000")
	   (authors Ameier)
	   (input  "Three arguments: a input formula, a list of computation tuples, and a list of external"
		   "parameters.")
	   (effect "None." )  
	   (value  "If the sequence is executable a formula, otherwise nil."))
  (do* ((rest-comp-triple-list comp-triple-list (rest rest-comp-triple-list))
	(current-formula input-formula))
      ((or (null current-formula)
	   (null rest-comp-triple-list))
       
       (cond ((null current-formula)
	      ;; one of the sequence components failed -> whole sequence fails
	      ;; -> return nil
	      nil)
	     ((null rest-comp-triple-list)
	      ;; sequence is finished and all components could be applied successfully
	      ;; -> return current-fomula
	      current-formula)))
    
    (let* ((next-comp-triple (first rest-comp-triple-list))
	   (next-comp-func (first next-comp-triple))
	   (next-comp-sidecond (second next-comp-triple))
	   (next-param-func (third next-comp-triple))
	   
	   ;; Notice that both next-comp-sidecond and next-param-func are optional arguments!
	   ;; If here are computation function of a tactic should be called directly
	   ;; then they should be both given (since such application depend on sidecondition
	   ;; which have to be checked first and on parameters)
	   ;; If here a interleaved application of other tacticals has to to be applied
	   ;; these interleaved stuff has neither parameters nor sideconditions
	   ;; In this case the external-parameters are inherited as internal-parameters
	   
	   (internal-parameters (if next-param-func
				    (funcall next-param-func current-formula external-parameters)
				  external-parameters))
	   ;; -> a list of parameters to call the next function and sidecondition

	   (sidecond (if next-comp-sidecond
			 (funcall next-comp-sidecond current-formula internal-parameters)
		       nil))
	   ;; Is the sidecondition fulfilled?
	   )
      
      (if (or sidecond
	      (null next-comp-sidecond))
	  ;; sidecondition fulfilled or not given -> apply the compution function on the current-formula and set the
	  ;; current formula to the result of this computation
	  (setf current-formula (funcall next-comp-func current-formula internal-parameters))
	;; sidecondition not fulfilled -> set current-formula nil to sign failure (see do loop)
	(setf current-formula nil)))))



;; An example for learn~sequence:
;;
;;(learn~sequence 2 
;;		(list (list #'(lambda (a internalparamlist) 
;;				(+ a (first internalparamlist)))  
;;			    #'(lambda (a internalparamlist) 
;;				(evenp a)) 
;;			    #'(lambda (a externalparamlist)
;;				(if (evenp (first externalparamlist))
;;				    (list 3)
;;				  (list 10))))
;;		      (list #'(lambda (a internalparamlist) 
;;				(+ a (first internalparamlist)))  
;;			    #'(lambda (a internalparamlist) 
;;				(oddp a)) 
;;			    #'(lambda (a externalparamlist)
;;				(if (evenp (first externalparamlist))
;;				    (list 7)
;;				  (list 12)))))
;;		(list 2))
;;
;;
;; Syntax for learn~sequence
;; 1. Argument: Input Formula
;; 2. Argument: List of tuples of: Computation-Function (see above)
;;                                  Sidecondition-Function (see above)
;;                                  InternalParameter-Function (see above)
;; 3. Argument: list of external parameters
;;
;; learn~sequence is executable if each of the tuples can be executed (when one triple can be executed see above).
;; Thereby, the input formula for the first triple is the input formula of learn~sequence, the input formula for the second
;; triple is the result of the execution of the first triple (for result of a the execution of a triple see above), ...
;; The result of the execution learn~sequence is the result of the execution of the last triple.
;; If one of the tuples cannot be executed the result of learn~sequence is nil
;;
;;


(defun learn~star (input-formula comp-triple external-parameters termination-condition &rest resti)
  (declare (edited  "18-MAY-2000")
	   (authors Ameier)
	   (input  "Four arguments: a input formula, a list of computation triples, a list of external"
		   "parameters, and a termination condition.")
	   (effect "None." )  
	   (value  "If the star is executable a formula, otherwise nil."))
  (let* ((comp-func (first comp-triple))
	 (comp-sidecond (second comp-triple))
	 (param-func (third comp-triple)))
    
    (do* ((current-formula input-formula))
	((or (null current-formula)
	     (funcall termination-condition current-formula external-parameters))
	 
	 (cond ((null current-formula)
		;; the operation is not longer applicable but the termination condition is not fulfilled
		;; -> whole star operation fails
		;; -> return nil
		nil)
	       ((funcall termination-condition current-formula external-parameters)
		;; if the termination condition is fulfilled return the current-formula
		current-formula)))
      (let* (
	     ;; Notice that both next-comp-sidecond and next-param-func are optional arguments!
	     ;; If here are computation function of a tactic should be called directly
	     ;; then they should be both given (since such application depend on sidecondition
	     ;; which have to be checked first and on parameters)
	     ;; If here a interleaved application of other tacticals has to to be applied
	     ;; these interleaved stuff has neither parameters nor sideconditions
	     ;; In this case the external-parameters are inherited as internal-parameters
	     
	     (internal-parameters (if param-func
				      (funcall param-func current-formula external-parameters)
				    external-parameters))
	     ;; -> a list of parameters to call the next function and sidecondition
	     
	     (sidecond (if comp-sidecond
			   (funcall comp-sidecond current-formula internal-parameters)
			 nil))
	     ;; Is the sidecondition fulfilled?
	     )
	
	(if (or sidecond
		(null comp-sidecond))
	    ;; sidecondition fulfilled or not given -> apply the compution function on the current-formula and
	    ;; set the current formula to the result of this computation
	    (setf current-formula (funcall comp-func current-formula internal-parameters))
	  ;; sidecondition not fulfilled -> set current-formula nil to sign failure (see do loop)
	  (setf current-formula nil))))))

  

;;
;; An example for lear~star:
;;
;;(learn~star 2
;;	    (list #'(lambda (a internalparamlist) 
;;		      (+ a (first internalparamlist)))  
;;		  #'(lambda (a internalparamlist) 
;;		      (numberp a)) 
;;		  #'(lambda (a externalparamlist)
;;		      (if (evenp (first externalparamlist))
;;			  (list 1)
;;			(list 4))))
;;	    (list 3)
;;	    #'(lambda (a externalparamlist) 
;;		(> a 10)))
;;
;; Syntax for learn~star
;; 1. Argument: Input Formula
;; 2. Argument: One triple of: Computation-Function (see above)
;;                             Sidecondition-Function (see above)
;;                             InternalParameter-Function (see above)
;; 3. Argument: list of external parameters
;; 4. Argument: Termination Condition: A function that returns t/nil called on the current-formula and the external-
;;                                     parameters
;;
;; learn~star is executable if the triple can be executed until the termination condition holds for the current formula and
;; the external parameters.
;; Thereby, the current formula is as first the input formula of learn~sequence, then the result of the execution of the
;; computation-function of the triple on the current-formula, ... until the termination condition holds for the current
;; formula and the external parameters.
;; The result of the execution learn~star is the result of the last execution of the triple.
;; If the triple cannot be executed although the termination condition is not fulfilled nil is returned as result of the
;; execution of learn~star
;;


;;
;; An example of a combined application of learn~xor, learn~star, learn~sequence
;;
;; (learn~sequence 2
;;		(list (list #'(lambda (a internalparamlist)
;;				(learn~xor a
;;					   (list (list #'(lambda (a internalparamlist) 
;;							   (+ a (first internalparamlist)))  
;;						       #'(lambda (a internalparamlist) 
;;							   (evenp a)) 
;;						       #'(lambda (a externalparamlist)
;;							   (if (evenp (first externalparamlist))
;;							       (list 2)
;;							     (list 3))))
;;						 (list #'(lambda (a internalparamlist) 
;;							   (+ a (first internalparamlist)))  
;;						       #'(lambda (a internalparamlist) 
;;							   (oddp a)) 
;;						       #'(lambda (a externalparamlist)
;;							   (if (evenp (first externalparamlist))
;;							       (list 7)
;;							     (list 15)))))
;;					   internalparamlist))
;;			    nil
;;			    nil)
;;		      (list #'(lambda (a internalparameter)
;;				(learn~star a
;;					    (list #'(lambda (a internalparamlist) 
;;						      (+ a (first internalparamlist)))  
;;						  #'(lambda (a internalparamlist) 
;;						      (numberp a)) 
;;						  #'(lambda (a externalparamlist)
;;						      (if (evenp (first externalparamlist))
;;							  (list 1)
;;							(list 4))))
;;					    (list 3)
;;					    #'(lambda (a externalparamlist) 
;;						(> a 20))))
;;			    nil
;;			    nil))
;;		(list 2))
;;
;;
;; Some Remarks about interleaving of learn~star, learn~sequence, and learn~xor
;; 1.) A interleaved application of these function has to be done within additional functions, since a computation-function
;;     has only two arguments (formula + internal-parameters) whereas learn~star, learn~sequence, and learn~xor themselves
;;     have more arguments!
;; 2.) Only basic computation-functions have sidecondition-functions and internalparameter-functions. Interleaved
;;     applications of learn~star, learn~sequence, and learn~xor have these function not, therefor we can write nil instead
;;     of such functions or ommit at all to give the second and third argument in the triple. 
;; 3.) The external-parameters have to be written explicitly only once: in the outmost application of learn~star,
;;     learn~sequence, and learn~xor.
;;     The others interleaved applications of learn~star, learn~sequence, and learn~xor inherit these external-parameters
;;     if: -) Their internalparameter-functions are nil or not given.
;;         -) The interleaved applications of learn~star, learn~sequence, and learn~xor use the interal-parameter argument
;;            of their computation-function as internal parameter argument
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auxiliary Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun learn=check-specification (specification)
  (declare (edited  "17-MAY-2000")
	   (authors Vxs)
	   (input   "A learned specification.")
	   (effect  "None.")
	   (value   "T if the specification is valid, o/w NIL."))
  (if (and specification (listp specification))
      (let ((fspec (car specification)))
	(cond ((equal fspec learn*star)
	       (and (every #'learn=check-specification (butlast (cdr specification)))
		    (learn=funcallable-p (car (last specification)))))
	      ((equal fspec learn*xor)
	       (every #'learn=check-specification (cdr specification)))
	      ((equal fspec learn*sequence)
	       (every #'learn=check-specification (cdr specification)))
	      (t (and (= (length specification) 2)
		      (infer~find-method (car specification))
		      (learn=funcallable-p (cadr specification))))))
    t))

(defun learn=funcallable-p (symbol)
  (declare (edited  "17-MAY-2000")
	   (authors Vxs)
	   (input   "A symbol.")
	   (effect  "None.")
	   (value   "T if the symbol represents a funcallable function."))
  (ignore-errors (symbol-function symbol)))

(defun learn=check-parameters (parameters)
  (declare (edited  "17-MAY-2000")
	   (authors Vxs)
	   (input   "A list of parameters.")
	   (effect  "None.")
	   (value   "T if the list is valid, o/w NIL."))
  (every #'(lambda (param)
	     (and (arg~find-argtype (car param))
		  (find-class (cadr param))))
	 parameters))

(defun learn=check-direction (direction)
  (declare (edited  "24-MAY-2000")
	   (authors Vxs)
	   (input   "A direction.")
	   (effect  "None.")
	   (value   "T if it is a valid direction, o/w NIL."))
  (or (string-equal direction :b)
      (string-equal direction :f)))

(defun learn=make-gensym-name (&optional name)
  (read-from-string (symbol-name
		     (intern (gensym (etypecase name
				       (string name)
				       (symbol (symbol-name name))))))))

(defgeneric learn~get-tactic (name direction)
  (declare (edited  "18-MAY-2000")
	   (authors Vxs)
	   (input   "A name of an inference method and a symbol indicating an application direction.")
	   (effect  "None.")
	   (value   "The tactic responsible belonging to the inference method and the direction."))
  (:method ((name symbol) (direction (eql :f)))
	   (let* ((inference (infer~find-method name))
		  (outline-pattern (list infer*non-existent infer*existent)))
	     (when inference
	       (tac~find-tactic
		(infer~outline-pattern2application inference outline-pattern)))))
  (:method ((name symbol) (direction (eql :b)))
	   (let* ((inference (infer~find-method name))
		  (outline-pattern (list infer*existent infer*non-existent)))
	     (when inference
	       (tac~find-tactic
		(infer~outline-pattern2application inference outline-pattern)))))
  (:method ((name symbol) (direction (eql :a)))
	   (let* ((inference (infer~find-method name))
		  (outline-pattern (list infer*existent infer*existent)))
	     (when inference
	       (tac~find-tactic
		(infer~outline-pattern2application inference outline-pattern)))))
  (:method ((name string) direction)
	   (learn~get-tactic (symbol-name name) direction)))

