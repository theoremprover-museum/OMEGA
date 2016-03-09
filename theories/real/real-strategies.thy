(in-package :omega)

(th~require-completely 'zmz) ;; for massertion

;;All the prime stuff is in integer.
;;(th~require-completely 'prime) ;; for the prime things!

#| ------------------------------------------------ COND FUNCS ----------------------------------------------- |#


(meth~deffun add-th-ass (ass theory)
	     (declare (edited  "06-JUN-2001")
		      (authors Sorge)
		      (input   "A theory assertion.")
		      (effect  "Inserts the assertion into the proof.")
		      (value   "The newly created proof line."))
	     (let* ((th-ass (th~find-assumption ass (th~find-theory theory))))
	       (pds~add-thy-assertion th-ass)))
	     

(meth~deffun find-int-supps (node)
	     (declare (edited  "06-JUN-2001")
		      (authors AMEIER)
		      (input   "An open line.")
		      (effect  "None.")
		      (value   "Returns the list of all support-nodes of node that have a formula:"
			       "(int X)."))
	     (let* ((supports (pds~node-supports node)))
	       (remove-if-not #'(lambda (supp)
				  (let* ((formula (node~formula supp)))
				    (and (data~appl-p formula)
					 (keim~equal (data~appl-function formula)
						     (env~lookup-object 'int (pds~environment omega*current-proof-plan))))))
			      supports)))
					 


(meth~defcond simplifyable-p (args cmapp)
	      (declare (edited  "18-APR-2000")
		       (authors Ameier)
		       (input   "A list of args consisting of two terms t11 and t12 of type num, and two variables t21-var and t22-var.")
		       (effect  "None.")
		       (value   "If the equation represented by the two terms t11 and t12 can be simplified (see below), then"
				"then the mapping is extended with t21-var -> simplified-t11 and t22-var -> simplified-t12"
				"and <c,mapp'> is returned. Otehrwise, <nil,cmapp> is returned."))
	      (let* ((t11 (first args))
		     (t12 (second args))
		     (t21-var (third args))
		     (t22-var (fourth args)))
		(multiple-value-bind
		    (t21 t22)
		    (methhelp=simplifyable-equation-p t11 t12)
		  (if (or (null (keim~equal t21 t11))
			  (null (keim~equal t22 t12)))
		      (progn
			(meth~mapp-extend-mapp cmapp t21-var t21)
			(meth~mapp-extend-mapp cmapp t22-var t22))
		    (meth~mapp-new-constraint cmapp nil)))))

(meth~new-relational-function 'simplifyable-p)

(defun methhelp=simplifyable-equation-p (t11 t12)
  (multiple-value-bind
      (t11-after1 t12-after1)
      (methhelp=simplify-case1 t11 t12)
    (multiple-value-bind
	(t11-after2 t12-after2)
	(methhelp=simplify-case2 t11-after1 t12-after1)
      (multiple-value-bind
	  (t11-after3 t12-after3)
	  (methhelp=simplify-case3 t11-after2 t12-after2)
	(multiple-value-bind
	    (t11-after4 t12-after4)
	    (methhelp=simplify-case4 t11-after3 t12-after3)
	  
	  (if (or (null (keim~equal t11 t11-after4))
		  (null (keim~equal t12 t12-after4)))
	      (methhelp=simplifyable-equation-p t11-after4 t12-after4)
	    (values t11 t12)))))))

(defun methhelp=simplify-case1 (t11 t12)
  ;; if one term has the form (times (sqrt X) Y) or (times (power X (div M N)) Y), then we apply a power 2 or N to both sides
  (let* ((times-it (env~lookup-object 'times (th~env 'rational)))
	 (power-it (env~lookup-object 'power (th~env 'rational))))

    (multiple-value-bind
	(success x y)
	(methhelp=sqrt-check t11)

      (if success
	  (values (term~appl-create times-it
				    (list x
					  (term~appl-create power-it
							    (list y (post~read-object 2 (th~env 'real) :existing-term)))))
		  (term~appl-create power-it
				    (list t12 (post~read-object 2 (th~env 'real) :existing-term))))

	
	(multiple-value-bind
	    (success x m n y)
	    (methhelp=power-check t11)

	  (if success
	      (values (term~appl-create times-it
					(list (term~appl-create power-it
								(list x m))
					      (term~appl-create power-it
								(list y n))))
		      (term~appl-create power-it
					(list t12 n)))
	    
	    (values t11 t12)))))))
				       
  
(defun methhelp=power-check (term)
  (let* ((times-it (env~lookup-object 'times (th~env 'rational)))
	 (power-it (env~lookup-object 'power (th~env 'rational)))
	 (div-it (env~lookup-object 'div (th~env 'rational))))
    (if (and (data~appl-p term)
	     (keim~equal (data~appl-function term) times-it)
	     (= (length (data~appl-arguments term)) 2))
	(let* ((arg1 (first (data~appl-arguments term)))
	       (arg2 (second (data~appl-arguments term))))
	  (if (and (data~appl-p arg1)
		   (keim~equal (data~appl-function arg1) power-it)
		   (= (length (data~appl-arguments arg1)) 2))
	      (let* ((power1 (first (data~appl-arguments arg1)))
		     (power2 (second (data~appl-arguments arg1))))
		(if (and (data~appl-p power2)
			 (keim~equal (data~appl-function power2) div-it)
			 (= (length (data~appl-arguments power2)) 2))
		    (let* ((div1 (first (data~appl-arguments power2)))
			   (div2 (second (data~appl-arguments power2))))
		      (values 't power1 div1 div2 arg2))
		  (values nil nil nil nil nil)))
	    (values nil nil nil nil nil)))
      (values nil nil nil nil nil))))
	    
(defun methhelp=sqrt-check (term)
  (let* ((times-it (env~lookup-object 'times (th~env 'rational)))
	 (sqrt-it (env~lookup-object 'sqrt (th~env 'real))))
    (if (and (data~appl-p term)
	     (keim~equal (data~appl-function term) times-it)
	     (= (length (data~appl-arguments term)) 2))
	(let* ((arg1 (first (data~appl-arguments term)))
	       (arg2 (second (data~appl-arguments term))))
	  (if (and (data~appl-p arg1)
		   (keim~equal (data~appl-function arg1) sqrt-it))
	      (values 't (first (data~appl-arguments arg1)) arg2) 
	    (values nil nil nil)))
      (values nil nil nil))))

    
      
(defun methhelp=simplify-case3 (t11 t12)
  ;; if both t11 has the form (times num1 rest1) and t12 has the form (times num2 rest2) then we kuerzen the term
  (let* ((times-it (env~lookup-object 'times (th~env 'rational))))
    (if (and (data~appl-p t11)
	     (data~appl-p t12)
	     (keim~equal (data~appl-function t11) times-it)
	     (keim~equal (data~appl-function t12) times-it)
	     (= (length (data~appl-arguments t11) 2))
	     (= (length (data~appl-arguments t12) 2))
	     (term~number-p (first (data~appl-arguments t11)))
	     (term~number-p (first (data~appl-arguments t12))))
	(let* ((num1 (keim~name (first (data~appl-arguments t11))))
	       (num2 (keim~name (first (data~appl-arguments t12))))
	       (rest1 (second (data~appl-arguments t11)))
	       (rest2 (second (data~appl-arguments t12))))
	  (values (if (< num1 num2)
		      rest1
		    rest2)
		  (term~appl-create times-it
				    (list (if (< num1 num2)
					      (post~read-object (/ num2 num1) (th~env 'rational) :existing-term)
					    (post~read-object (/ num1 num2) (th~env 'rational) :existing-term))
					  (if (< num1 num2)
					      rest2
					    rest1)))))
      (values t11 t12))))

(defun methhelp=simplify-case2 (t11 t12)
  ;; If t11 or t12 have the form (power (times x y) z), then it is simplified to (times (power x z) (power y z))
  (values (methhelp=unwrap-power t11)
	  (methhelp=unwrap-power t12)))

(defun methhelp=simplify-case4 (t11 t12)
  ;; Each subterm within t11 or t12 that contains only numerical expressions which are connected by +,-,*,power, and div and
  ;; can be evaluated, are evaluated.
  (values (methhelp=evaluate-subterm t11)
	  (methhelp=evaluate-subterm t12)))

(defun methhelp=unwrap-power (term)
  (let* ((power-it (env~lookup-object 'power (th~env 'rational)))
	 (times-it (env~lookup-object 'times (th~env 'rational))))
    (if (data~appl-p term)
	(let* ((functor (data~appl-function term))
	       (args (data~appl-arguments term)))
	  (if (and (keim~equal functor power-it)
		   (= (length args) 2))
	      (let* ((arg1 (first args))
		     (arg2 (second args)))
		(if (and (data~appl-p arg1) 
			 (keim~equal (data~appl-function arg1) times-it)
			 (= (length (data~appl-arguments arg1) 2)))
		    (term~appl-create times-it
				      (list (term~appl-create power-it
							      (list (first (data~appl-arguments arg1))
								    arg2))
					    (term~appl-create power-it
							      (list (second (data~appl-arguments arg1))
								    arg2))))
		  (term~appl-create functor
				    (list (methhelp=unwrap-power arg1)
					  (methhelp=unwrap-power arg2)))))
	    (term~appl-create functor
			      (mapcar #'methhelp=unwrap-power args))))
      term)))

		   


(defun methhelp=evaluate-subterm (term)
  (let* ((plus-it (env~lookup-object 'plus (th~env 'rational)))
	 (minus-it (env~lookup-object 'minus (th~env 'rational)))
	 (times-it (env~lookup-object 'times (th~env 'rational)))
	 (div-it (env~lookup-object 'div (th~env 'rational)))
	 (power-it (env~lookup-object 'power (th~env 'rational))))
    (if (data~appl-p term)
	(let* ((functor (data~appl-function term))
	       (args (data~appl-arguments term)))
	  (if (and (or (keim~equal functor plus-it)
		       (keim~equal functor minus-it)
		       (keim~equal functor times-it)
		       (keim~equal functor power-it)
		       (keim~equal functor div-it))
		   (= (length args) 2)
		   (term~number-p (first args))
		   (term~number-p (second args)))
	      (post~read-object (cond ((keim~equal functor plus-it)
				       (+ (keim~name (first args)) (keim~name (second args))))
				      ((keim~equal functor minus-it)
				       (- (keim~name (first args)) (keim~name (second args))))
				      ((keim~equal functor times-it)
				       (* (keim~name (first args)) (keim~name (second args))))
				      ((keim~equal functor div-it)
				       (/ (keim~name (first args)) (keim~name (second args))))
				      ((keim~equal functor power-it)
				       (expt (keim~name (first args)) (keim~name (second args)))))
				(th~env 'rational)
				:existing-term)
	    (term~appl-create functor
			      (mapcar #'methhelp=evaluate-subterm args))))
      term)))
				      


   
(meth~defcond diff-divisors-of (args cmapp)
	      (declare (edited  "18-APR-2000")
		       (authors Ameier)
		       (input   "A list of args consisting of a ndline, a term t, and two variables assumplist-var and divs-var.")
		       (effect  "None.")
		       (value   "If some of the assumption lines are of the form '(divisor X t)' or '(prime-divisor X t)'"
				"then the mapping is extended with divs -> divisors-list, where all elements of divisors list"
				"have to be different, and <c,mapp'> is returned. Otehrwise, <nil,cmapp> is returned."))
	      (let* ((line (first args))
		     (term (second args))
		     (assumplist-var (third args))
		     (divs-var (fourth args)))
		(multiple-value-bind
		    (success divs used-assumps)
		    (methhelp=find-different-divisors (pds~node-supports line) term)
		  (if success
		      (progn
			(meth~mapp-extend-mapp cmapp assumplist-var used-assumps)
			(meth~mapp-extend-mapp cmapp divs-var divs))
		    (meth~mapp-new-constraint cmapp nil)))))

(meth~new-relational-function 'diff-divisors-of)

(defun methhelp=find-different-divisors (assumplist term)
  (let* ((divisor-item (env~lookup-object 'divisor (th~env 'rational)))
	 (prime-divisor-item (env~lookup-object 'prime-divisor (pds~environment omega*current-proof-plan)))
	 (divs-and-assumps (apply #'append (mapcar #'(lambda (assump)
						       (let* ((formula (node~formula assump)))
							 (if (and (data~appl-p formula)
								  (or (keim~equal (data~appl-function formula) divisor-item)
								      (keim~equal (data~appl-function formula) prime-divisor-item))
								  (= (length (data~appl-arguments formula)) 2))
							     (let* ((arg1 (first (data~appl-arguments formula)))
								    (arg2 (second (data~appl-arguments formula))))
							       (if (keim~equal arg2 term)
								   (list (list arg1 assump))
								 nil))
							   nil)))
						   assumplist)))
	 (divs-and-assumps-clear (remove-duplicates divs-and-assumps :test #'(lambda (it1 it2)
									       (keim~equal (first it1) (first it2))))))
    (if divs-and-assumps
	(values 't
		(mapcar #'first divs-and-assumps-clear)
		(mapcar #'second divs-and-assumps-clear))
      (values nil nil nil))))

(meth~deffun compute-divisor (divs)
	     (declare (edited  "18-APR-2000")
		      (authors Ameier)
		      (input   "A non-empty list of terms of type num.")
		      (effect  "None.")
		      (value   "The formula where all the elements of the input list are connected with times."
			       "If the input list consists only of one item it, then it is retunred."))
	     (methhelp=connect-by-times divs))

(defun methhelp=connect-by-times (divs)
  (if (= (length divs) 1)
      (first divs)
    (let* ((divs-rest (methhelp=connect-by-times (rest divs))))
      (term~appl-create (env~lookup-object 'times (th~env 'rational))
			(list (first divs)
			      divs-rest)))))
					   
(meth~defcond  prime-factors (args cmapp)
	      (declare (edited  "18-APR-2000")
		       (authors Ameier)
		       (input   "A list of args consisting of a term, which is an integer n, and a variable"
				"called primes.")
		       (effect  "None.")
		       (value   "If Maple succeeds to compute a prime factorization of n, then the mapping is"
				"extended with prime -> factorization-list and <c,mapp'> is returned. Otehrwise,"
				"<nil,cmapp> is returned."))
	      (let* ((number (first args))
		     (prime-var (second args)))
		(multiple-value-bind
		    (success primes)
		    (methhelp=call-maple-to-get-prime-fac number)

		  (if success
		      (meth~mapp-extend-mapp cmapp prime-var primes)
		    (meth~mapp-new-constraint cmapp nil)))))

(meth~new-relational-function 'prime-factors)

(defun methhelp=call-maple-to-get-prime-fac (number)
  (let* ((primes-string (maple~call-maple (list "with(numtheory,factorset)" (format nil "~A" (keim~name number))) :syntax 'post2maple)))
    (if (or (string= primes-string "error")
	    (string= primes-string "[{}]"))
	(values nil nil)
      (values t  (mapcar #'atptop~parse-number
			 (atptop~divide-string primes-string #\, :ignore-char-list (list #\[ #\] #\{ #\} #\SPACE)))))))

(meth~deffun compute-prime-fac-lines (line expr primes)
	     (declare (edited  "05-MAR-2001")
		      (authors Ameier)
		      (input   "An assumption line, a numerical expression, a list of prime numbers.")
		      (effect  "None.")
		      (value   "A list of formulas with the hyps of line, and as formula:"
			       "(prime-divisor a prime)"))
	     (methhelp=new-prime-nodes line expr primes))

(defun methhelp=new-prime-nodes (line expr primes)
  (let* ((hyps (pdsn~hyps line)))
    (mapcar #'(lambda (prime)
		(let* ((label (pds~new-node-name omega*current-proof-plan))
		       (formula (term~appl-create (env~lookup-object 'prime-divisor (pds~environment omega*current-proof-plan))
						  (list (post~read-object prime (pds~environment omega*current-proof-plan) :existing-term)
							expr))))
		  (pdsn~open-node-create formula hyps label)))
	    primes)))

(meth~deffun compute-prime-fac-prems (expr)
	     (declare (edited  "22-AUG-2002")
		      (authors Pollet))
		 (multiple-value-bind (concs prems)
		     (real=primefacs-product-p expr)
		   prems))

(meth~defcond natnumber-p (args cmapp)
	      (declare (edited  "18-APR-2000")
		       (authors Ameier)
		       (input   "A list of args (a term with num).")
		       (effect  "None.")
		       (value   "<c,mapp> if the term is a integer number larger 0, <nil,cmapp> otherwise."))
	      (meth~mapp-new-constraint cmapp (and (integerp (keim~name (first args)))
						   (> (keim~name (first args)) 0))))

(meth~deffun th-def (string)
	     (declare)
	     (th~find-assumption string (prob~theory omega*current-proof-plan)))


(meth~deffun num2func (num)
	     (declare)
	     (natac=numbers-2-function num))

(meth~deffun expand-primefacs-param (prem)
	     (declare)
	     (multiple-value-bind (newconc newprems)
		 (real=expand-primefacs-product-p prem)
	       newprems))

(meth~deffun compute-conjunction-for-prime-facs (expr primes)
	     (declare (edited  "05-MAR-2001")
		      (authors Ameier)
		      (input   "A numerical expression, a list of prime numbers.")
		      (effect  "None.")
		      (value   "A conjunction consisting of all terms (prime-divisor expr prime)."))
	     (let* ((and-obj (env~lookup-object 'and (th~env 'base)))
		    (primediv-obj (env~lookup-object 'prime-divisor (pds~environment omega*current-proof-plan))))
				  
	       (do* ((back-conj nil)
		     (rest-primes primes (rest rest-primes)))
		   ((null rest-primes)
		    back-conj)
		 (let* ((new-term (term~appl-create primediv-obj
						    (list (post~read-object (first rest-primes)
									    (pds~environment omega*current-proof-plan) :existing-term)
							  expr))))
		   (if (null back-conj)
		       (setf back-conj new-term)
		     (setf back-conj (term~appl-create and-obj (list back-conj new-term))))))))


(meth~defcond find-num-times-p (args cmapp)
	      (declare (edited  "18-APR-2000")
		       (authors Ameier)
		       (input   "A list of args consisting of two terms left-handside (lf) and right-handside (rh) and three variables a,b,n.")
		       (effect  "None.")
		       (value   "If either lf or rh has the form (times n b) where n is an integer then the variables a,b,n are bound"
				"correspondingly (i.e., a is bound to the other side."))
	      (let* ((lf (first args))
		     (rh (second args))
		     (a-var (third args))
		     (n-var (fourth args))
		     (b-var (fifth args)))
		(if (and (data~appl-p lf)
			 (keim~equal (data~appl-function lf) (env~lookup-object 'times (pds~environment omega*current-proof-plan)))
			 (term~constant-p (first (data~appl-arguments lf)))
			 (integerp (keim~name (first (data~appl-arguments lf)))))
		    (progn
		      (meth~mapp-extend-mapp cmapp a-var rh)
		      (meth~mapp-extend-mapp cmapp n-var (first (data~appl-arguments lf)))
		      (meth~mapp-extend-mapp cmapp b-var (second (data~appl-arguments lf))))
		  (if (and (data~appl-p rh)
			   (keim~equal (data~appl-function rh) (env~lookup-object 'times (pds~environment omega*current-proof-plan)))
			   (term~constant-p (first (data~appl-arguments rh)))
			   (integerp (keim~name (first (data~appl-arguments rh)))))
		      (progn
			(meth~mapp-extend-mapp cmapp a-var lf)
			(meth~mapp-extend-mapp cmapp n-var (first (data~appl-arguments rh)))
			(meth~mapp-extend-mapp cmapp b-var (second (data~appl-arguments rh))))
		    (meth~mapp-new-constraint cmapp nil)))))

(meth~new-relational-function 'find-num-times-p)

(meth~defcond sort-check-int-p (args cmapp)
	      (declare (edited  "18-APR-2000")
		       (authors Ameier)
		       (input   "A list of args consisting of a term term and a variable needed.")
		       (effect  "None.")
		       (value   "First the list sortlist of all nodes in the proof with formula '(int X)' is computed, where X is a constant."
				"Then, it is checked whether from this list of ndlines (int term) can be derived. If this is the case,"
				"then the variable needed is bound to the subset of sortlist of lines that are actually needed to do so."))
	      
	      (let* ((term (first args))
		     (needed-var (second args))
		     (sortlist (remove-if-not #'(lambda (line)
						  (let* ((formula (node~formula line)))
						    (if (and (data~appl-p formula)
							     (keim~equal (data~appl-function formula)
									 (env~lookup-object 'int (pds~environment omega*current-proof-plan)))
							     (term~constant-p (first (data~appl-arguments formula))))
							't
						      nil)))
					      (prob~proof-steps omega*current-proof-plan)))
		     (new-open-line (pdsn~open-node-create (term~appl-create (env~lookup-object 'int (pds~environment omega*current-proof-plan))
									     (list term))
							   (remove-duplicates (apply #'append (mapcar #'pdsn~hyps sortlist)))
							   'ui)))
		
		
		(potac=wellsorted-outline new-open-line sortlist)

		(let* ((just (node~justification new-open-line)))
		  (if (pdsj~open-p just)
		      (meth~mapp-new-constraint cmapp nil)
		    (progn
		      (mapcar #'(lambda (open-node)
				  (pds~delete-sponsors open-node (list new-open-line)))
			      (pds~open-nodes omega*current-proof-plan))
		      (keim::pds=remove-plan-step! omega*current-proof-plan (pds~last-plan-step omega*current-proof-plan))
		      (meth~mapp-extend-mapp cmapp needed-var (just~premises just)))))))

(meth~new-relational-function 'sort-check-int-p)

(meth~deffun unite-hyps (line-list)
	     (declare (edited  "05-MAR-2001")
		      (authors Ameier)
		      (input   "A list of lines.")
		      (effect  "None.")
		      (value   "The list containing all hyps of the lines in the input list (duplicates are removed)."))
	     (progn
	       ;;(format t "~%UNITE-HYPS IN: ~A" line-list)
	       (let* ((out (remove-duplicates (apply #'append (mapcar #'pdsn~hyps line-list)))))
		 ;;(format t "~%UNITE-HYPS OUT: ~A" out)
		 out)))

#| ------------------------------------------------ Methods ----------------------------------------------- |#

(infer~defmethod UReflex-m
                (outline-mappings (((existent) Ureflex-m-b)))
                (help "Closes a goal of the form 'a = a'"))

(meth~defmethod Ureflex-m-b Ureflex-m
               (in base)
               (rating 50)
               (reasoning :planning :middle-out)
               (declarations
                (type-variables aa)
                (sorted-meta-variables
                 (phi aa term)
                 (psi aa term)))
               
               (premises )
               (conclusions (- L1))
               
               (application-condition
                (unify phi psi))
               
               (decl-content
                (l1 () (= phi psi) ("=ref" (phi) ())))
	       (manual (author "AMeier")
		       (examples "ZMZ, Homo")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PrimeFacs-Product-m
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; given: assumption line with equation X = n * Y where n is an integer
;; output: new assumption lines with formulas (prime-divisor p X) where p is a prime divisor of n
;; 



(infer~defmethod PrimeFacs-Product-m
		 (outline-mappings (((nonexistent existent) PrimeFacs-Product-m-f)))
		 (help ""))

(meth~defmethod PrimeFacs-Product-m-f PrimeFacs-Product-m
		(in rational)
		(rating 60)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  (phi o term)
		  (a num term)
		  (b num term)
		  (n num term)
		  (aprime num term)
		  (bprime num term)
		  (sortlist o prlnlist)
		  (conjunction o)
		  )
		 )
		
		(application-condition
		 (mand (find-num-times-p aprime bprime a n b)    ;; one side of equation is a, the other side is n*b where n is a number
		       (sort-check-int-p a neededa) 
		       (sort-check-int-p b neededb) 
		       (prime-factors n primes)
		       (bound-p primes)))
		
		(outline-computations
		 (conjunction (compute-conjunction-for-prime-facs a primes))
		 (hyps (unite-hyps (mcons l1 (mappend neededa neededb))))
		 )

		(expansion-computations
		 (prems (mcons l1 (mappend neededa neededb))))
		
		(premises L1)
		(conclusions (+ L2))
		
		(decl-content
		 (L1 () (= aprime bprime))
		 (L2 (hyps) conjunction           ("Expand-primefacs-product" () prems))
		 )
		(manual) 
		)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PrimeDiv-Power-m
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; given: assumption line with formula (prime-divisor n (power X m)) where m is a natural number
;; output: new assumption line with formula (prime-divisor n X)
;;

#|
(infer~defmethod PrimeDiv-Power-m
		 (outline-mappings (((nonexistent existent existent) PrimeDiv-Power-m-f)))
		 (help ""))

(meth~defmethod PrimeDiv-Power-m-f PrimeDiv-Power-m
		(in real)
		(rating 60)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  (a num term)
		  (m num term)
		  (n num term)
		  )
		 )
		
		(application-condition
		 (natnumber-p m))
		
		(premises (- L3) l2)
		(conclusions (+ l5))
		
		(expansion-computations
		 (powerth (add-th-ass (:symbol PRIME-DIVISOR-POWER) (:symbol INTEGER)))
		 (welln (msecond (wellsorted-prepara (:term (nat n)) (mnil))))
		 (wellm (msecond (wellsorted-prepara (:term (nat m)) (mnil))))
		 (term-list (mlist m n a))
		 )
		
		(decl-content
		 (L0 () (nat m)                                ("Wellsorted" (wellm) ()))
		 (L1 () (nat n)                                ("Wellsorted" (welln) ()))
		 (L2 () (int a)                                )
		 (L3 () (prime-divisor n (power a m))          )
		 (L4 () (implies (prime-divisor n (power a m))
				 (prime-divisor n a))          ("Foralle-sort*" (term-list) (powerth l0 l1 l2)))
		 (L5 () (prime-divisor n a)                    ("ImpE" () (L4 L3)))
		 )
		(manual) 
		)|#

(infer~defmethod PrimeDiv-Power-m
		 (outline-mappings (((nonexistent existent) PrimeDiv-Power-m-f)))
		 (help ""))

(meth~defmethod PrimeDiv-Power-m-f PrimeDiv-Power-m
		(in real)
		(rating 60)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  (a num term)
		  (m num term)
		  (n num term)
		  (sortline o)
		  (neededa o prlnlist)
		  )
		 )
		
		(application-condition
		 (mand (natnumber-p m)
		       (natnumber-p n)
		       (sort-check-int-p a neededa)))
		
		(premises (- L3))
		(conclusions (+ l5))

		(outline-computations
		 (hyps (unite-hyps (mcons l3 neededa))))
		 
		(expansion-computations
		 (powerth (add-th-ass (:symbol PRIME-DIVISOR-POWER) (:symbol INTEGER)))
		 (welln (msecond (wellsorted-prepara (:term (nat n)) (mnil))))
		 (wellm (msecond (wellsorted-prepara (:term (nat m)) (mnil))))
		 ;;(wella (msecond (wellsorted-prepara (:term (int a)) neededa))) ;; -> if nil clash!
		 (wella (mlist (mnil)))
		 (term-list (mlist m n a))
		 (hypsa (unite-hyps neededa))
		 )
		
		(decl-content
		 (L0 ()      (nat m)                                 ("Wellsorted" (wellm) ()))
		 (L1 ()      (nat n)                                 ("Wellsorted" (welln) ()))
		 (L2 (hypsa) (int a)                                 ("Wellsorted" wella neededa))   ;; -> (wella) -> but clash!
		 (L3 ()      (prime-divisor n (power a m))           )
		 (L4 (hypsa) (implies (prime-divisor n (power a m))
				      (prime-divisor n a))           ("Foralle-sort*" (term-list) (powerth l0 l1 l2)))
		 (L5 (hyps)  (prime-divisor n a)                     ("ImpE" () (L4 L3)))
		 )
		(manual) 
		)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CollectPrimeDivs-m
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; given: list of assumption lines with formulas (prime-divisor n_i X) where all n_i are different
;; output: new assumption line with formula (X = (times n_1 ... n_k Y))
;;

(meth~deffun compute-new-hyp-lines (a constants composed-divs)
	     (declare (edited  "18-APR-2000")
		      (authors Ameier)
		      (input   "A term, a list of constants, a list of comosed divisors.")
		      (effect  "None.")
		      (value   "A list of new hyp-lines."))
	     (let* ((env (pds~environment omega*current-proof-plan))
		    (and-obj (env~lookup-object 'and env))
		    (int-obj (env~lookup-object 'int env))
		    (=-obj (env~lookup-object '= env))
		    (times-obj (env~lookup-object 'times env)))
	       ;;(format t "~%HERE")
	       (mapcar #'(lambda (const composed-div)
			   (let* ((formula (term~appl-create and-obj
							     (list (term~appl-create int-obj (list const))
								   (term~appl-create =-obj
										     (list a
											   (term~appl-create times-obj
													     (list composed-div const))))))))
			     
			     (pdsn~make-hypothesis formula (pds~new-node-name))))
		       constants composed-divs)))

(meth~deffun newconsts-of-type (type listi)
	     (declare (edited  "18-APR-2000")
		      (authors Ameier)
		      (input   "A type symbol and a list.")
		      (effect  "None.")
		      (value   "Creates a new constant of type type for each member of the list."))
	     (mapcar #'(lambda (item)
			 (term~generate-term-primitive-with-new-name 'const type 'term+constant
								     (pds~environment
								      omega*current-proof-plan)))
		     listi))
	     
(meth~deffun compute-divisors (divs)
	     (declare (edited  "18-APR-2000")
		      (authors Ameier)
		      (input   "A non-empty list of terms of type num.")
		      (effect  "None.")
		      (value   "The formula where all subsets of elements of the input list are connected with times."
			       "If the input list consists only of one item it, then it is retunred."))
	     (mapcar #'methhelp=connect-by-times (methhelp=compute-all-non-empty-subsets divs)))

(defun methhelp=compute-all-non-empty-subsets (set)
  (cond ((null set)
	 nil)
	((= (length set) 1)
	 (list set))
	(t
	 (let* ((head (first set))
		(subsets-of-rest (methhelp=compute-all-non-empty-subsets (rest set))))
	   (append (list (list head))
		   subsets-of-rest
		   (mapcar #'(lambda (setti)
			       (cons head setti))
			   subsets-of-rest))))))


(infer~defmethod CollectDivs-m
		 (outline-mappings (((existent nonexistent) CollectDivs-m-b)))
		 (parameter-types term)
		 (help ""))

(meth~defmethod CollectDivs-m-b CollectDivs-m
		(in integer)
		(rating 60)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  (assumplist o prlnlist)
		  (int-supps o prlnlist)
		  (a num term)
		  (c num term)
		  (formula o term)
		  (divs o)
		  (composed-div num term)
		  )
		 )

		(parameters a)

		(application-condition
		 (mand
		  (bound-p a)
		  (diff-divisors-of l10 a assumplist divs)))
		
		(outline-computations
		 (int-supps (find-int-supps l10))
		 (composed-divs (compute-divisors divs))
		 (cs (newconsts-of-type (:type num) composed-divs))
		 (hyp-lines (compute-new-hyp-lines a cs composed-divs))
		 )
		
		(premises (+ l9) assumplist)
		(conclusions (- l10))

		(outline-actions
		 (l9 (sponsor hyp-lines)))
		
		;;(expansion-computations
		;; (exlines ...)
		;;  )
		
		(decl-content
		 (l9 (hyp-lines) formula                                               ("Open" () ()))
		 (l10 ()         formula                                               ("ExistsE-Sort**" cs (exlines l9)))
		 )
		(manual) 
		)


#|
(infer~defmethod CollectDivs-m
		 (outline-mappings (((existent nonexistent) CollectDivs-m-b)))
		 (parameter-types term)
		 (help ""))

(meth~defmethod CollectDivs-m-b CollectDivs-m
		(in integer)
		(rating 60)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  (assumplist o prlnlist)
		  (int-supps o prlnlist)
		  (a num term)
		  (c num term)
		  (formula o term)
		  (divs o)
		  (composed-div num term)
		  )
		 )

		(parameters a)

		(application-condition
		 (mand
		  (bound-p a)
		  (diff-divisors-of l10 a assumplist divs)))
		
		(outline-computations
		 (int-supps (find-int-supps l10))
		 (composed-div (compute-divisor divs))
		 (c (type-newconst (:type num)))
		 ;;(exline (mfirst (mfirst (apply-tactic (:symbol :expand-collectdivs) (mlist (mnil) assumplist) (mlist a)))))
		 )

		;;(outline-actions
		;; (l9 (sponsor exline)))
		
		(premises (+ l9) assumplist)
		(conclusions (- l10))

		(expansion-computations
		 ;;(exline (mfirst (mfirst (apply-tactic (:symbol :expand-collectdivs) (mlist (mnil) assumplist) (mlist a)))))
		 ;; (exjust (computeexjust c exline l9))
		 )
		
		(decl-content
		 (l3 () (exists-sort (lam (z num)
		 			  (= a (times composed-div c)))
		 		     int)                                                  ("Expand-Collectdivs" (a) (assumplist)))
		 (l4 () (and (int c)
			     (= a (times composed-div c)))                                 ("Hyp" () ()))
		 (l9 (l4) formula                                                          ("Open" () ()))
		 (l10 () formula                                                           ("ExistsE-Sort" (c) (l3 l9)))
		 )
		(manual) 
		)

|#

#|
(infer~defmethod CollectDivs-m
		 (outline-mappings (((existent nonexistent) CollectDivs-m-b)))
		 (parameter-types term)
		 (help ""))

(meth~defmethod CollectDivs-m-b CollectDivs-m
		(in integer)
		(rating 60)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  (assumplist o prlnlist)
		  (a num term)
		  (c num term)
		  (formula o term)
		  (divs o)
		  (composed-div num term)
		  )
		 )

		(parameters a)

		(application-condition
		 (mand
		  (bound-p a)
		  (diff-divisors-of l10 a assumplist divs)))
		
		(outline-computations
		 (composed-div (compute-divisor divs))
		 (c (type-newconst (:type num))))
		 
		(premises (+ l9) assumplist)
		(conclusions (- l10))

		;;(outline-actions
		;; (l9 (sponsor l5))
		;; (l9 (sponsor l6))
		;; (l9 (unsponsor assumplist)))
		
		(expansion-computations
		 (posi (:position ()))
		 ((def1 def2) (th-get-def :divisor)))
		
		(decl-content
		 (l1 () (divisor composed-div a)                                      ("TacticBla" (a) assumplist))
		 (l2 () (and (and (in composed-div int)
				  (int  a ))
			     (exists-sort (lam (z num)
					       (= a (times composed-div z))) int))    ("DefnE" (def1 def2 posi) (l1)))
		 (l3 () (exists-sort (lam (z num)
					  (= a (times composed-div z))) int)          ("Ander" () (l2)))
		 (l4 () (and (int c)
			     (= a (times composed-div c)))                         ("Hyp" () ()))
		 (l5 (l4) (int c)                                                  ("Andel" () (l4)))
		 (l6 (l4) (= a (times composed-div c))                             ("Ander" () (l4)))
		 (l9 (l4) formula                                                  ("Open" () ()))
		 (l10 () formula                                                   ("ExistsE-sort" (c) (l3 l9)))
		 )
		(manual) 
		)

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TransformEquation-m
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; given: assumption with equation formula
;; output: new assumption line with changed formula, if necessary
;;
;; This method is clearly cheating ... we form the terms in the way we need them ...
;; 


(infer~defmethod TransformEquation-m
		 (outline-mappings (((nonexistent existent) TransformEquation-m-f)))
		 (help ""))

(meth~defmethod TransformEquation-m-f TransformEquation-m
		(in integer)
		(rating 60)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  (t11 num term) (t12 num term)
		  (t21 num term) (t22 num term)
		  )
		 )
		
		(application-condition
		 (simplifyable-p t11 t12 t21 t22))
		
		(premises (- L1))
		(conclusions (+ l2))
		
		(decl-content
		 (L1 () (= t11 t12))
		 (L2 () (= t21 t22)    ("By-Computation" () (l1)))
		 )
		(manual) 
		)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ContraDictionCommonDivisor-m
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Find two expressions (prime-divisor X c1) and (prime-divisor X c2) to contradict that c1 and c2 have no common divisor
;;

#|
(infer~defmethod ContraDictionCommonDivisor-m
		 (outline-mappings (((existent closed closed closed closed closed) ContraDictionCommonDivisor-m-b)))
		 (help ""))

(meth~defmethod ContraDictionCommonDivisor-m-b ContraDictionCommonDivisor-m
		(in integer)
		(rating 60)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  (n num term) 		  (nsuc num term)
		  (c1 num term)
		  (c2 num term)
		  (sym o) (well o)
		  (pridiv o )(divint o)(leqeq o)
		  )
		 )

		(application-condition
		 (intnumber-p n))
		
		(premises l0 l1 l2 l10 l20)
		(conclusions (- l3))
		
		(decl-content
		 (L10 () (int c1))
		 (L20 () (int c2))
		 (L0 () (not (exists-sort (lam (x num var)
					       (common-divisor c1 c2 x))
					  int)))
		 (e02 () (not (exists
			       (lam (x num var)
				    (AND (INT X)                       
					 (AND (AND (INT C1) (INT C2))           
					      (AND (INT X)                     
						   (AND (NOT (= 1 X))              
							(AND                            
							 (DIVISOR X C1)            
							 (DIVISOR X C2))))))))) ("Defse" (divint) (L0)))
		 (L1  () (prime-divisor n c1))
		 (e10 () (AND (DIVISOR N C1) (PRIME N)) ("Defse"  (pridiv) (L1)))

		 (L2  () (prime-divisor n c2))
		 (e20 () (AND (DIVISOR N C2) (PRIME N)) ("Defse" (pridiv) (L2)))

		 (e35 () (less (s zero)  nsuc)         ("arith-simplify" (les) ()))
		 (e34 () (less 1 n)                    ("defn-contract-nums" () (e35)))
		 (e33 () (and (leq 1 n)(not (= 1 n)))  ("defse" (leqeq) (e34)))
		 (e30 () (not (= 1 n))                 ("ander" () (e33)))

		 (e40 () (int n)                       ("wellsorted" (well) ()))
		 
		 (L3 () FALSE                          ("Otter" (sym) (e10 e20 e02 e30 e40  L10 L20 )))
		 )
		
		(expansion-computations
		 (sym (:symbol t))
		 (well (msecond (wellsorted-prepara (:term (int n)) (mnil))))
		 (divint (mlist  (th-def "divisor") (th-def "int") (th-def "=")))
		 (pridiv (mlist  (th-def "divisor") (th-def "prime")))
		 (leqeq (mlist  (th-def "leq")(th-def "=")))
		 (les (:symbol less))
		 (nsuc  (num2func n)))

		(manual) 
		)
|#

(infer~defmethod ContraDictionCommonDivisor-m
		 (outline-mappings (((existent closed closed closed) ContraDictionCommonDivisor-m-b)))
		 (help ""))

(meth~defmethod ContraDictionCommonDivisor-m-b ContraDictionCommonDivisor-m
		(in integer)
		(rating 60)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  (n num term) 		  (nsuc num term)
		  (c1 num term)
		  (c2 num term)
		  (sym o) (well o)
		  (pridiv o ) (divint o) (leqeq o)
		  (neededc1 o prlnlist) (neededc2 o prlnlist)
		  )
		 )
		
		(application-condition
		 (mand (intnumber-p n)
		       (sort-check-int-p c1 neededc1)
		       (sort-check-int-p c2 neededc2)))
		
		(premises l0 l1 l2)
		(conclusions (- l3))

		(decl-content
		 (L10 (hypsc1) (int c1)                              ("Wellsorted" wellc1 neededc1))
		 (L20 (hypsc2) (int c2)                              ("Wellsorted" wellc2 neededc2))
		 (L0 () (not (exists-sort (lam (x num var)
					       (common-divisor c1 c2 x))
					  int)))
		 (e02 () (not (exists
			       (lam (x num var)
				    (AND (INT X)                       
					 (AND (AND (INT C1) (INT C2))           
					      (AND (INT X)                     
						   (AND (NOT (= 1 X))              
							(AND                            
							 (DIVISOR X C1)            
							 (DIVISOR X C2))))))))) ("Defse" (divint) (L0)))
		 (L1  () (prime-divisor n c1))
		 (e10 () (AND (DIVISOR N C1) (PRIME N)) ("Defse"  (pridiv) (L1)))

		 (L2  () (prime-divisor n c2))
		 (e20 () (AND (DIVISOR N C2) (PRIME N)) ("Defse" (pridiv) (L2)))

		 (e35 () (less (s zero)  nsuc)         ("arith-simplify" (les) ()))
		 (e34 () (less 1 n)                    ("defn-contract-nums" () (e35)))
		 (e33 () (and (leq 1 n)(not (= 1 n)))  ("defse" (leqeq) (e34)))
		 (e30 () (not (= 1 n))                 ("ander" () (e33)))

		 (e40 () (int n)                       ("wellsorted" (well) ()))
		 
		 (L3 () FALSE                          ("Otter" (sym) (e10 e20 e02 e30 e40  L10 L20 )))
		 )
		
		(expansion-computations
		 (sym (:symbol t))
		 (well (msecond (wellsorted-prepara (:term (int n)) (mnil))))
		 (divint (mlist  (th-def "divisor") (th-def "int") (th-def "=")))
		 (pridiv (mlist  (th-def "divisor") (th-def "prime")))
		 (leqeq (mlist  (th-def "leq")(th-def "=")))
		 (les (:symbol less))
		 (nsuc  (num2func n))
		 (wellc1 (mlist (mnil)))
		 (wellc2 (mlist (mnil)))
		 (hypsc1 (unite-hyps neededc1))
		 (hypsc2 (unite-hyps neededc2))
		 )
		
		(manual) 
		)




#| ------------------------------------------------------------------- STRATEGY ----------------------------------------------- |#


(defun notrat-goal-p (task)
  (if (agenda~goal-or-goal-schema-task-p task)
      (let* ((node (agenda~task-node task))
	     (formula (node~formula node))
	     (env (th~env 'real)))
	(if (and (logic~negation-p formula)
		 (data~appl-p (first (data~appl-arguments formula))))
	    (let* ((functor (data~appl-function (first (data~appl-arguments formula)))))
	      (if (keim~equal functor (env~lookup-object 'rat env))
		  't
		nil))
	  nil))
    nil))

(strat~define-strategy-ks
 (name NotRatByPrimDivisors)
 (refinement-algorithm PPlanner)
 (condition notrat-goal-p)
 (methods (ContraDictionCommonDivisor-m-b
	   notI-m-b
	   UREFLEX-M-B
	   MAssertion-m-b
	   Existse-Sort-m-f
	   ande-m-f
	   TransformEquation-m-f
	   PrimeFacs-Product-m-f
	   PrimeDiv-Power-m-f
	   CollectDivs-m-b
	   =subst-m-f
	   ))
 (normalization-methods (ande-m-f
			 TransformEquation-m-f
			 PrimeFacs-Product-m-f
			 PrimeDiv-Power-m-f
			 ))
 (restriction-methods (UReflex-m-b
		       ))
 (control-rules (ratbyprim-indirect-if-not
		 ratbyprim-apply-rat-criterion
		 ratbyprim-apply-collectprimedivs
		 ratbyprim-apply-=subst
		 ratbyprim-prefer-primediv
		 ratbyprim-reject-if-already-done-collectdivs
		 loop-reject
		 )) 
 (loop-detection )
 (randomization-rules nil)
 (termination-check no-further-goal-p)
 (selection waterfall)
 (remark "The goal <LISP>(verbalize-start-task state-des)</LISP><BR>is tackled by the NotRatByPRimDivs Technique.")
 (print "Strategy-KS NotRatByPrimDivisors: Offer to proof task ~A by testing for NotRatByPrimDivs."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ratbyprim-prefer-primediv
;;
;; -> prefers facts created by primediv-power-m
;;

;; This control rule is purely technical to create a ''better'' proof object.
;; For search control it is obsolete.
;; When primediv-power-m derives that a constant c has a prime divisor p (prime-divisor p c) then afterwards
;; collectdivs can create a new hypotheses that c = p * ... * p_n c' where p, ..., p_n are all found prime
;; divisors of c. Afterwards simplify-equations priduces c = P*c' where P is the evaluated product of p* ... *p_n
;; now PrimeFacs-Product can again compute that p is a prime factor of c although this is not the desired way
;; c = P*c' should used.
;; Therefore, the following control rules always prefers lines with formula (prime-divisor p c) that are
;; derived by an application of primediv-power-m


(cri~def-control-rule ratbyprim-prefer-primediv
		      (kind supports)
		      (if (order-supps-with-prime-divisor-by-div-power-in-front-p "new-supps"))
		      (then
		       (select (("new-supps")))))

(defun order-supps-with-prime-divisor-by-div-power-in-front-p (new-supps)
  (let* ((supps (first (first cri*current-original-alternative-list)))
	 (supps-with-prime-divisor-from-div-power (remove-if-not #'crihelp=node-is-prime-divisor-from-div-power-p supps))
	 (other-supps (remove-if #'crihelp=node-is-prime-divisor-from-div-power-p supps)))
    (if supps-with-prime-divisor-from-div-power
	(list (list (cons new-supps (append supps-with-prime-divisor-from-div-power other-supps))))
      nil)))
	 
							    
(defun crihelp=node-is-prime-divisor-from-div-power-p (node)
  (let* ((formula (node~formula node))
	 (just (node~justification node)))
    (if (and (data~appl-p formula)
	     (keim~equal (data~appl-function formula)
			 (env~lookup-object 'prime-divisor (pds~environment omega*current-proof-plan))))
	(let* ((just-method (keim~name (just~method just))))
	  ;;(format t "~%HACK1")
	  (if (string-equal just-method 'primediv-power-m)
	      (progn
		;;(format t "~%HACK2")
		't)
	    (progn
	      ;;(format t "~%HACK3")
	      nil)))
      (progn
	;;(format t "~%HACK4")
	nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ratbyprim-apply-=subst
;;
;; -> applies ratbyprim-apply-=subst
;; 

(cri~def-control-rule ratbyprim-apply-=subst
		      (kind methods)
		      (if (find-in-power-equation "mother" "equation" "position"))
		      (then
		       (prefer (ContraDictionCommonDivisor-m-b
				(=subst-m-f ("mother" "equation") ("position"))))))

(defun find-in-power-equation (mother equation position)
  ;; We are looking for supports  c = (times X Y) such that there are other supports with the subterm (power c Z)
  
  (let* ((task cri*current-task)
	 (node (agenda~task-node task))
	 (supports (pds~node-supports node))
	 (times-it (env~lookup-object 'times (th~env 'rational)))
	 (power-it (env~lookup-object 'power (th~env 'rational)))
	 (eq-it (data~schema-range (env~lookup-object '= (th~env 'rational))))
	 (eq-supports-with-c (remove-if-not #'crihelp=node-has-formula-c=times supports))
	 (triples (apply #'append (mapcar #'(lambda (eq-support-with-c)
					      (crihelp=find-mothers-for-c-support eq-support-with-c supports))
					  eq-supports-with-c))))
    (if triples
	(mapcar #'(lambda (triple)
		    (list (cons mother (second triple))
			  (cons equation (first triple))
			  (cons position (pos~add-end 1 (third triple)))))
		triples)
      nil)))


(defun crihelp=find-mothers-for-c-support (c-support supports)
  (let* ((c (first (data~appl-arguments (node~formula c-support))))
	 (mothers-for-c (apply #'append (mapcar #'(lambda (support)
						    (crihelp=find-mother-for-c support c))
						supports))))
    (mapcar #'(lambda (mother-for-c)
		(cons c-support mother-for-c))
	    mothers-for-c)))
    
	 
(defun crihelp=find-mother-for-c (support c)
  (let* ((power-it (env~lookup-object 'power (th~env 'rational)))
	 (positions (data~positions (node~formula support) #'(lambda (term)
							       (and (data~appl-p term)
								    (keim~equal (data~appl-function term) power-it)
								    (= (length (data~appl-arguments term)) 2)
								    (eq (first (data~appl-arguments term)) c))))))
    (mapcar #'(lambda (pos)
		(list support pos))
	    positions)))

					    
(defun crihelp=node-has-formula-c=times (support)
  (let* ((times-it (env~lookup-object 'times (th~env 'rational)))
	 (eq-it (data~schema-range (env~lookup-object '= (th~env 'rational))))
	 (formula (node~formula support)))
    (if (and (data~appl-p formula)
	     (keim~equal (data~appl-function formula) eq-it)
	     (= (length (data~appl-arguments formula)) 2))
	(let* ((arg1 (first (data~appl-arguments formula)))
	       (arg2 (second (data~appl-arguments formula))))
	  (if (and (term~constant-p arg1)
		   (data~appl-p arg2)
		   (keim~equal (data~appl-function arg2) times-it)
		   (= (length (data~appl-arguments arg2)) 2))
	      't
	    nil))
      nil)))
	 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ratbyprim-apply-collectprimedivs
;;
;; -> applies collectprimedivs
;; 

(cri~def-control-rule ratbyprim-apply-collectprimedivs
		      (kind methods)
		      (if (find-div-factor "factor"))
		      (then
		       (prefer (ContraDictionCommonDivisor-m-b
				;;PrimeDiv-Power-m-f
				(CollectDivs-m-b () ("factor"))))))


(defun find-div-factor (factor)
  (let* ((task cri*current-task)
	 (node (agenda~task-node task))
	 (supports (pds~node-supports node))
	 (prime-divisor-it (env~lookup-object 'prime-divisor (pds~environment omega*current-proof-plan)))
	 (divisor-it (env~lookup-object 'divisor (th~env 'real)))
	 (supports-with-divisors (remove-if-not #'(lambda (support)
						    (let* ((formula (node~formula support)))
						      (if (and (data~appl-p formula)
							       (or (keim~equal (data~appl-function formula) prime-divisor-it)
								   (keim~equal (data~appl-function formula) divisor-it))
							       (= (length (data~appl-arguments formula)) 2)
							       (term~number-p (first (data~appl-arguments formula))))
							  't
							nil)))
						supports)))
    (if supports-with-divisors 
	(mapcar #'(lambda (support)
		    (let* ((formula (node~formula support)))
		      (list (cons factor (second (data~appl-arguments formula))))))
		supports-with-divisors)
      nil)))
		  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ratbyprim-apply-rat-criterion
;;
;; -> applies as first the rat-criterion
;;

(cri~def-control-rule ratbyprim-apply-rat-criterion
		      (kind methods)
		      (if (and (current-task-is-not-rat)
			       (find-rat-criterion-p "theorem")))
		      (then
		       (select ((MAssertion-m-b () ("theorem"))))))

(defun find-rat-criterion-p (theorem)
  (let* ((current-th (th~find-theory 'rational))
	 (theorems (th~theorems current-th))
	 (rat-criterion-theorem (first (remove-if-not #'(lambda (theorem)
							  (string-equal (keim~name theorem) 'rat-criterion))
						      theorems))))
    (if rat-criterion-theorem
	(list (list (cons theorem rat-criterion-theorem)))
      nil)))

(defun current-task-is-not-rat ()
  (let* ((task cri*current-task)
	 (node (agenda~task-node task))
	 (formula (node~formula node))
	 (env (th~env 'real)))
    (if (and (logic~negation-p formula)
	     (data~appl-p (first (data~appl-arguments formula))))
	(let* ((functor (data~appl-function (first (data~appl-arguments formula)))))
	  (if (keim~equal functor (env~lookup-object 'rat env))
	      (list (list (cons T T)))
	    nil))
      nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ratbyprim-indirect-if-not
;;
;; -> applies as indirectII-m-b if the task is a negation
;;

(cri~def-control-rule ratbyprim-indirect-if-not
		      (kind methods)
		      (if (current-task-is-negation))
		      (then
		       (select (notI-m-b))))

(defun current-task-is-negation ()
  (let* ((task cri*current-task)
	 (node (agenda~task-node task))
	 (formula (node~formula node))
	 (env (th~env 'real)))
    (if (logic~negation-p formula)
	(list (list (cons T T)))
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; removing loops with collectdivs
;;
;; 
;;


(cri~def-control-rule ratbyprim-reject-if-already-done-collectdivs
		      (kind mmatchings)
		      (if (already-applied-collectdivs-p "match"))
		      (then
		       (reject ("match"))))

(defun already-applied-collectdivs-p (match)
  (let* ((mmatchings cri*mmatchings)
	 (mmatchings-with-collectdivs (remove-if-not #'(lambda (mmatching)
							 (if (keim~equal (pplan~matched-method mmatching)
									 (meth~find-method 'collectdivs-m-b))
							     't
							   nil))
						     mmatchings))
	 (const-divs-pairs-of-old-steps (apply #'append (mapcar #'crihelp=application-of-collectdivs (pds~plan-steps)))))

    (if const-divs-pairs-of-old-steps
	(let* ((mmatchings-with-collectdivs-and-corresponding-old-step
		(remove-if-not #'(lambda (mm)
				   (crihelp=mmathing-corresponds-to-old-pairs mm const-divs-pairs-of-old-steps))
			       mmatchings-with-collectdivs)))
	  (if mmatchings-with-collectdivs-and-corresponding-old-step
	      (mapcar #'(lambda (mm)
			  (list (cons match mm)))
		      mmatchings-with-collectdivs-and-corresponding-old-step)
	    nil))
      nil)))

									   
(defun crihelp=mmathing-corresponds-to-old-pairs (mmatching const-divs-pairs-of-old-steps)
  (let* ((subst (meth~mapp-subst (pplan~mmatch-mmapp mmatching)))
	 (mapp (meth~mapp-mapp (pplan~mmatch-mmapp mmatching)))
	 (const-divs-pair (crihelp=const-divs-pair-of-subst subst mapp))
	 (const (first const-divs-pair))
	 (divs (second const-divs-pair))
	 (corresponding-old-appl (remove-if-not #'(lambda (const-divs-pair-old-step)
						    (let* ((const-old (first const-divs-pair-old-step))
							   (divs-old (second const-divs-pair-old-step)))
						      (if (and (eq const const-old)
							       ;; new divisors are subset of old-divisors
							       (every #'(lambda (it)
									  (find it divs-old :test #'keim~equal))
								      divs))
							  (progn
							    (format t "~%<< Removing Matching for CollectDivs with Const ~A and Divs ~A since there is already a step with Divs ~A >>~%" const divs divs-old)
							    't)
							nil)))
						const-divs-pairs-of-old-steps)))
    (if corresponding-old-appl	  
	't
      nil)))

				    
(defun crihelp=application-of-collectdivs (plan-step)
  (let* ((just (pdsc~an-just plan-step))
	 (method (just~method just))
	 (meth-mapping (pdsj~subst just))
	 (subst (meth~mapp-subst meth-mapping))
	 (mapp (meth~mapp-mapp meth-mapping)))
    (if (eq method (infer~find-method 'collectdivs-m))
	(let* ((a-divs-pair-of-subst (crihelp=const-divs-pair-of-subst subst mapp)))
	  (list a-divs-pair-of-subst))
      nil)))

(defun crihelp=const-divs-pair-of-subst (subst mapp)
  (let* ((sdomain (subst~domain subst))
	 (scodomain (subst~codomain subst))
	 (sassoc-list (mapcar #'list sdomain scodomain))
	 (mdomain (mapp~domain mapp))
	 (mcodomain (mapp~codomain mapp))
	 (massoc-list (mapcar #'list mdomain mcodomain))
	 (a (second (first (remove-if-not #'(lambda (pair)
					      (string-equal 'a (keim~name (first pair))))
					  sassoc-list))))
	 (divs (second (first (remove-if-not #'(lambda (pair)
						 (string-equal 'divs (keim~name (first pair))))
					     massoc-list)))))
    (list a divs)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general loop-detection for forward methods
;;
;; 
;;

(cri~def-control-rule loop-reject
		      (kind mmatchings)
		      (if (already-applied-forward-methods-p  "match"))
		      (then
		       (reject ("match"))))


(defun already-applied-forward-methods-p (match)
  (let* ((tasks
	  (mapcan #'(lambda (task) (when (agenda~task-node task) (list (agenda~task-node task))))
		      (agenda~all-tasks (pds~agenda cri*current-pds))))
	  ;it should be cri*current-task-node, but it does not work. Take all tasks instead.
	 (match-inferences (mapcar #'(lambda (m)
				       (meth~inference (pplan~matched-method m)))
				   cri*mmatchings))
	 (supports (remove-if-not #'(lambda (just)
				      (some #'(lambda (x) (keim~equal x
								      (just~method just)))  match-inferences ))
				  (remove-duplicates
				   (mapcan #'(lambda (task)
					       (mapcar #'node~justification (pds~node-supports task))) tasks))))
	 (remove-matching (remove-if-not #'(lambda (matching)
					     (some #'(lambda (sup)
						       (and (keim~equal (just~method sup)
									(meth~inference (pplan~matched-method matching)))
							    (meth~mapp-submap? (pplan~mmatch-mmapp matching)
									       (pdsj~subst sup))))
						       supports))
					     cri*mmatchings)))
	 (omega~trace "!!!LOOP: ~A ~A ~A ~A" match-inferences supports remove-matching cri*mmatchings)
	 (setf my (list cri*current-task-node cri*current-task  match-inferences supports remove-matching cri*mmatchings))
	 (when (and (stringp match))
	   (mapcar #'(lambda (rmm) (list (cons match rmm)))
		   remove-matching))))



(defun meth~mapp-submap? (mmap1 mmap2)
  (let ((map1 (meth~mapp-mapp mmap1))
	(map2 (meth~mapp-mapp mmap2))
	(subst1 (meth~mapp-subst mmap1))
	(subst2 (meth~mapp-subst mmap2)))
    (and (every #'(lambda (do1 co1)
		    (meth=compare co1 (mapp~get-component do1 subst2 )))
		(mapp~domain subst1)
		(mapp~codomain subst1))
	 (every #'(lambda (do1 co1)
		    (meth=compare co1 (mapp~get-component do1 map2 )))
		(mapp~domain map1)
		(mapp~codomain map1)))))


(defgeneric meth=compare (obj1 obj2)
  (:method ((obj1 term+term)(obj2 term+term))
	   (term~alpha-equal obj1 obj2))
  (:method ((obj1 pdsn+node)(obj2 pdsn+node))
	   (meth=compare (node~formula obj1)
			 (node~formula obj2)))
  (:method (obj1 obj2 )
	   (keim~equal obj1 obj2)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                                    ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;; SETTINGS                                                           ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                                    ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf sod*current-strategies '(NotRatByPrimDivisors
			       ))

(setf sod*current-strategic-control-rules '())












































































