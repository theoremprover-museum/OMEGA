(in-package :omega)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatically generating a large testbed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun agplan~generate-set-examples (num-of-vars depth &optional
						 (set-ops (list 'union 'intersection 'setminus 'exclunion)))
  (declare (edited  "21-DEC-2000")
	   (authors Sorge Chris)
	   (input   "Two integers indicating the number of variables and the nesting depth for"
		    "the construed expressions. Optionally a list of set operations.")
	   (effect  "None.")
	   (value   "A list of post expressions."))
  (let* ((vars (do ((num 1 (1+ num))
		    (varlist nil (acons num (read-from-string (format nil "x~A" num)) varlist)))
		   ((> num num-of-vars) (reverse varlist))))
	 (terms-of-depth-n (agplan=construe-terms-of-depth-n vars set-ops depth)))
    (do* ((i 0 (1+ i))
	  (result nil
		  (append result (list (agplan=formulate-problem (pop terms-of-depth-n) i)))))
	((null terms-of-depth-n) result))))

(defun agplan=formulate-problem (conc n)
  (declare (edited  "23-DEC-2000")
	   (authors Sorge)
	   (input   "A formula and an integer.")
	   (effect  "None.")
	   (value   "A problem formulation with the given formula as conclusion."))
  (let ((name (read-from-string (format nil "set-ex-~A" n))))
    (list 'th\~defproblem name '(in typed-set)
	  (list 'conclusion 'conc conc))))

(defun agplan=construe-terms-of-depth-n (vars set-ops depth)
  (let* ((permutations (cdr (agplan=explicit-permutations (length vars) depth)))
	 (operation-list (agplan=make-operation-permutations set-ops (1- depth)))
	 (combined-list (agplan=combine-variables&operations permutations operation-list))
	 (terms (mapcan #'(lambda (pair) (agplan=build-terms pair vars)) combined-list))
	 (uniq (mapcan #'(lambda (pair)
			   (agplan=add-uniqueness-conditions (first pair) (second pair)))
		       terms)))
    (mapcar #'(lambda (pair)
		(agplan=add-all-quantification (first pair) (second pair)))
	    uniq)))

(defun agplan=add-uniqueness-conditions (term variables)
  (if (> (length variables) 1)
      (let ((conditions (agplan=make-uniq-conditions variables)))
	(mapcar #'(lambda (cond) (if cond
				     (list (list 'implies cond term) variables)
				   (list term variables)))
		conditions))
    (list (list term variables))))

(defun agplan=make-uniq-conditions (variables)
  (let ((sets (agplan=construct-powerset
	       (remove-duplicates
		(remove-if #'(lambda (x) (equal (first x) (second x))) (rcl=permute2pairs variables))
		:test #'(lambda (x y) (or (and (equal (first x) (second y)) (equal (second x) (first y)))))))))
    (flet ((make-equation (pair)
			  (list 'not (list '= (first pair) (second pair)))))
      (labels ((make-conjunction (conj)
				 (cond ((< (length conj) 2) conj)
				       ((= (length conj) 2) (list 'and (first conj) (second conj)))
				       (t (list 'and (first conj) (make-conjunction (rest conj)))))))
	(mapcar #'(lambda (x)
		    (cond ((null x) nil)
			  ((= (length x) 1) (make-equation (car x)))
			  (t (make-conjunction
			      (mapcar #'make-equation x)))))
		sets)))))

(defun agplan=construct-powerset (set &key (separate nil))
  (declare (edited  "03-JAN-2005")
           (authors Sorge)
           (input   "A set of elements.")
           (effect  "None.")
           (value   "The powerset of SET without emptyset and SET itself."
                    "If SEPARATE is set the subsets are grouped by length.")
           (example "Input: '(0 1 2 3)"
                    "Output (with separate): '(((0) (1) (2) (3)) ((0 1) (0 2) (0 3) (1 2) (1 3) (2 3))"
                    "          ((0 1 2) (0 1 3) (0 2 3) (1 2 3)))")) 
  (if separate 
      (loop for n from 0 to (length set)
            collect     (agplan=all-subsets-of-length set n))
    (loop for n from 0 to (length set)
          append (agplan=all-subsets-of-length set n))))

(defun agplan=all-subsets-of-length (set n)
  (declare (edited  "03-JAN-2005")
           (authors Sorge)
           (input   "A set of elements and an integer.")
           (effect  "None.")
           (value   "A list containing all possible subsets of set of length n."))
  (cond ((= n 0) (list nil))
        ((null set) set)
        ((< (length set) n) nil)
        ;;((= (length set) n) set)
        ((= n 1) (mapcar #'list set))
        (t (let ((element (car set))
                 (n-1-subsets (agplan=all-subsets-of-length (cdr set) (1- n))))
             (append
              (mapcar #'(lambda (subset) (cons element subset)) n-1-subsets)
              (agplan=all-subsets-of-length (cdr set) n))))))

(defun agplan=add-all-quantification (term variables)
  (declare (edited  "22-DEC-2000")
	   (authors Sorge)
	   (input   "A term and a list of variables.")
	   (effect  "None.")
	   (value   "The term with all quantification over the given variables."))
  (if variables
      (list 'forall (list 'lam (list (car variables) '(o i))
			  (agplan=add-all-quantification term (cdr variables))))
    term))
	  

(defun agplan=build-terms (pair var-mapping)
  (declare (edited  "22-DEC-2000")
	   (authors Sorge)
	   (input   "A list pairing variable permutations and operation permutations.")
	   (effect  "None.")
	   (value   "A list of pairs containing a term and its occurring variables."))
  (let ((vars (first pair))
	(ops (mapcan #'agplan=split-at-equality (second pair))))
    (mapcar #'(lambda (p)
		(agplan=insert-correct-variables (second p) (first p) var-mapping))
	    (agplan=cross-product vars ops))))

(defun agplan=insert-correct-variables (term-structure vars var-mapping)
  (declare (edited  "22-DEC-2000")
	   (authors Sorge)
	   (input   "A term structure, a list of numbers and a association list mapping numbers to variables.")
	   (effect  "None.")
	   (value   "A pair consisting of the substituted term and the variables in the term."))
  (let* ((varlist (mapcar #'(lambda (x) (cdr (assoc x var-mapping :test #'=))) vars))
	 (bkpvl (sort (remove-duplicates varlist) #'string<)))
    (labels ((subst-term (term)
			 (cond ((null term) nil)
			       ((consp term) (mapcar #'subst-term term))
			       ((equal term '*) (pop varlist))
			       (t term))))
      (list (subst-term term-structure) bkpvl))))

(defun agplan=split-at-equality (ops)
  (declare (edited  "22-DEC-2000")
	   (authors Sorge)
	   (input   "A list of operations.")
	   (effect  "None.")
	   (value   "A list of terms."))
  (let* ((pos (position '= ops :test #'string-equal))
	(l1 (subseq ops 0 pos))
	(l2 (subseq ops (1+ pos)))
	(lterms (agplan=bracket-term l1))
	(rterms (agplan=bracket-term l2)))
    (cond ((and (null lterms) (null rterms))
	   '((= * *)))
	  ((null lterms)
	   (mapcar #'(lambda (term) (list '= '* term)) rterms))
	  ((null rterms)
	   (mapcar #'(lambda (term) (list '= term '*)) lterms))
	  (t (mapcar #'(lambda (pair)
			 (list '= (car pair) (cadr pair)))
		     (agplan=cross-product lterms rterms))))))

(defun agplan=bracket-term (ops)
  (declare (edited  "22-DEC-2000")
	   (authors Sorge)
	   (input   "A list of operations.")
	   (effect  "None.")
	   (value   "A list of terms bracketed in all possible ways."))
  (cond ((null ops) nil)
	((null (cdr ops)) (list (list (car ops) '* '*)))
	((null (cddr ops))
	 (let ((fop (car ops))
	       (result (car (agplan=bracket-term (cdr ops)))))
	   (list (list fop '* result) (list fop result '*))))
	(t (let* ((poslists (agplan=split-in-pairs (cdr ops)))
		  (result (mapcar #'(lambda (x)
				      (list (agplan=bracket-term (car x))
					    (agplan=bracket-term (cadr x))))
				  poslists))
		  (fop (car ops)))
	     (mapcan #'(lambda (pair)
			 (if (null (second pair))
			     (mapcan #'(lambda (elem)
					 (list (list fop '* elem)
					       (list fop elem '*)))
				     (first pair))
			   (mapcar #'(lambda (npair)
				       (list fop (first npair) (second npair)))
				   (agplan=cross-product (first pair) (second pair)))))
		     result)
	     ))))


(defun agplan=cross-product (list1 list2)
  (declare (edited  "22-DEC-2000")
	   (authors Sorge Chris)
	   (input   "Two lists.")
	   (effect  "None.")
	   (value   "A list containing the cross product of the two lists."))
  (cond ((null list1) list2)
	((null list2) list1)
	(t (mapcan #'(lambda (x)
		       (mapcar #'(lambda (y) (list x y))
			       list2))
		   list1))))
  

(defun agplan=split-in-pairs (list)
  (declare (edited  "22-DEC-2000")
	   (authors Sorge)
	   (input   "A list.")
	   (effect  "None.")
	   (value   "A list of pairs consisting of the input lists split in all possible two subsets."))
  (do* ((i 0 (1+ i))
	(reslist nil (cons (list (subseq list 0 i)
				 (subseq list i))
			   reslist)))
      ((= i (length list)) reslist)))

(defun agplan=combine-variables&operations (vars ops)
  (declare (edited  "22-DEC-2000")
	   (authors Sorge)
	   (input   "A list of variable permutations and a list of operations permutations.")
	   (effect  "None.")
	   (value   "A pair list of variable permutations and operations permutations that can be combined."))
  (when (and vars ops)
    (let* ((length (length (car vars)))
	   (var-pos (position-if #'(lambda (x) (> (length x) length)) vars))
	   (var-list (if var-pos (subseq vars 0 var-pos) vars))
	   (op-pos (position-if #'(lambda (x) (= (length x) length)) ops))
	   (op-list (if op-pos (subseq ops 0 op-pos) ops)))
      (cons (list var-list op-list)
	     (agplan=combine-variables&operations
	      (subseq vars (length var-list))
	      (subseq ops (length op-list)))))))

(defun agplan=make-operation-permutations (ops depth)
  (declare (edited  "21-DEC-2000")
	   (authors Sorge)
	   (input   "A list of operations and an integer.")
	   (effect  "None.")
	   (value   "A list of operation permutations including equality."))
  (agplan=add-equality-to-perms
   (do* ((i 0 (1+ i))
	 (result (list nil)
		 (append result (agplan=make-permutation-of-length ops i))))
       ((= i (1- depth)) result))))



(defun agplan=add-equality-to-perms (oplist)
  (declare (edited  "21-DEC-2000")
	   (authors Sorge)
	   (input   "A list of permutation list of operations.")
	   (effect  "None.")
	   (value   "The expanded list where equality is placed in each possible position."))
  (when oplist
    (append (agplan=add-equality-to-single-perm (car oplist))
	    (agplan=add-equality-to-perms (cdr oplist)))))

(defun agplan=add-equality-to-single-perm (opperm &optional (equality '=))
  (declare (edited  "21-DEC-2000")
	   (authors Sorge)
	   (input   "A permutation of operations.")
	   (effect  "None.")
	   (value   "The expanded permutation where equality is placed in each possible position."))
  (flet ((insert-at-pos (n)
			(cond ((= n 0) (cons equality opperm))
			      ((< n (length opperm)) (append (subseq opperm 0 n)
							     (cons equality (subseq opperm n))))
			      (t (append opperm (list equality))))))
    (do* ((i (length opperm) (1- i))
	  (result (list (insert-at-pos i))
		  (cons (insert-at-pos i) result)))
	((= i 0) result))))

(defun agplan=make-permutation-of-length (elems length)
  (declare (edited  "21-DEC-2000")
	   (authors Sorge)
	   (input   "A list of elements of the permuation and its length.")
	   (effect  "None.")
	   (value   "A list of permutations of the elements of given length."))
  (if (> length 1)
      (let ((result (agplan=make-permutation-of-length elems (1- length))))
	(apply #'append
	       (mapcar #'(lambda (x)
			   (mapcar #'(lambda (y) (cons y x))
				   elems))
		       result)))
    (mapcar #'list elems)))
	
(defun agplan=explicit-permutations (n depth)
  (declare (edited  "21-DEC-2000")
	   (authors Sorge)
	   (input   "Two integers with N>=1 and DEPTH>=2.")
	   (effect  "None.")
	   (value   "A list of permutations of length DEPTH with N different elements."))
  (cond ((= depth 0) nil)
	((= depth 1) (list (list 1)))
	(t (let ((result (agplan=explicit-permutations n (1- depth))))
	     (append result
		     (agplan=single-explicit-permutation 
		      (remove-if-not #'(lambda (x) (= (length x) (1- depth))) result)
		      n))))))

(defun agplan=single-explicit-permutation (perms n)
  (declare (edited  "21-DEC-2000")
	   (authors Sorge)
	   (input   "A list of permutations and an integer.")
	   (effect  "None.")
	   (value   "The extended list of permutations."))
  (when perms
    (append (agplan=extend-permutation (car perms) n)
	      (agplan=single-explicit-permutation (cdr perms) n))))

(defun agplan=extend-permutation (perm n)
  (declare (edited  "21-DEC-2000")
	   (authors Sorge)
	   (input   "A single permutation and an integer.")
	   (effect  "None.")
	   (value   "A list of all possible extensions of the permutation."))
  (let* ((different (length (remove-duplicates perm :test #'=)))
	 (extlist (do* ((i 0 (1+ i))
			(reslist nil (cons i reslist)))
		      ((or (= i (1+ different)) (= i n)) (reverse reslist)))))
    (mapcar #'(lambda (x) (append perm (list x))) extlist)))

(defun agplan~analyse-automatic-generated-set-examples
  (num-of-vars depth constructors &optional (filename "/tmp/set-ex") (resultfile "/tmp/set-ex-out"))
  (let* ((list (agplan~generate-set-examples num-of-vars depth constructors))
	 (pathname1 (pathname filename))
	 (pathname2 (pathname resultfile))
	 (stream (open pathname1 :direction :output :if-exists :rename-and-delete)))
    (format stream ";;; Automatically Generated Set Examples~%;;; Author: Benzmueller\&Sorge~%;;; Number of Variables ~A ~%;;; Construction Depth ~A ~%;;; Constructors ~A ~%;;; Number of generated Examples ~A~%~%" num-of-vars depth constructors (length list))
    (dolist (ex list nil)
	  (format stream "~A~%" ex))
    (close stream)
    (load pathname1)
    (omega~message "~%~% Automatically created ~A set equations:~%" (length list))
    (dolist (ex list list)
      (omega~message "~A" ex))))
    ;(sex~explore-testbed pathname1 pathname2)))
    
#| replaced by lea=extract-tolearn  MP but needed for set-equations!	|#				  
(defun agplan~pds2outline (&optional (pds pds*current-proof-plan))
  (labels ((agplan=recurse (node)
			   (when node
			     (let ((premises (pdsn~just-premises node)))
			       (cons (intern (string-upcase (string (agplan~name (pdsn~just-method node)))))
				     (when premises
				       (if (= (length premises) 1)
					   (agplan=recurse (car premises))
					 (mapcar #'agplan=recurse
						 premises))))))))
    (let* ((res (agplan=recurse (prob~proof-root pds)))
	   (string  (do* ((count 1 (incf count))
			  (comlist res (cdr comlist))
			  (com (agplan~find-corresponding-command (car comlist)) (agplan~find-corresponding-command (car comlist)))
			  (formatted-res (format nil "\"~A\"," com)
					 (concatenate 'string formatted-res (format nil "\"~A\"," com))))
			((>= count (- (length res) 1))
			 (format nil "[~A]"
				 (concatenate 'string formatted-res (format nil "\"~A\"" (agplan~find-corresponding-command (car (last comlist))))))))))
      (omega~message "OUTLINE of current PDS: ~A" string)
      (setq pds*outlines (if pds*outlines
			     (if (OHLP=FIND-SUBSTRING-LIST string (list pds*outlines))
				  pds*outlines  
			       (concatenate 'string pds*outlines  (format nil ",~A" string)))
			   (format nil "~A" string)))
      (omega~message "All outlines so far : [~A]" pds*outlines)
      (omega~message "Writing outlines so far to file ~A." (pathname "/tmp/set-ex-outlines"))
      (let ((stream (open (pathname "/tmp/set-ex-outlines") :direction :output
				    :if-exists :rename-and-delete)))
	(format stream "[~A]" pds*outlines)
	(close stream))
      string)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatically exploring set examples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar sex*explore-process nil)

(defun sex~explore-testbed (file outfile)
  (setf sex*explore-process
        (proc~create :name "Set Examples Exploration"
                     :function #'sex=run-function
                     :args (file outfile))))

(defun sex=run-function (file outfile)
  (let ((stream (open outfile :direction :output
		      :if-exists :rename-and-delete))
	(count*pos 0)
	(count*neg 0))
    (format stream "This file reports on the validity/invalidity of the examples defined
in ~A~% Authors: Benzmueller & Sorge ~%" file)
    (do ((result (keim::th=read-next-sexp file) (keim::th=read-next-sexp)))
	((null result) (print "Done exploring!"))
      (let* ((problem (eval result)))	; (print "hier")
	
	(opr~enqueue-command (opr~normalize-command 'prove
						    :args (list problem) :process
						    (proc~actual-process)))
	(opr~enqueue-command (opr~normalize-command 'automate :process (proc~actual-process)))
	(proc~sleep 8)
	(proc~wait "Waiting for automated proof search to finish!"
		   #'(lambda () (not (proc~is-active auto*process))))
	(format stream "~% ~A: ~A" (second result)
		(cond ((find-if #'(lambda (x) (string-equal (keim~name (just~method (node~justification x)))
							 "COUNTEREXAMPLE-BY-SATCHMO"))
				(prob~proof-steps pds*current-proof-plan))
		       (print "test-chris1")
		       (progn (setf count*neg (1+ count*neg))
			      (omega~message "Counterexample ~A" (sex~print-model (agplan~current-counterexample))) 
			      (format nil "Counterexample ~A" (sex~print-model (agplan~current-counterexample)))))
		      ((find-if #'(lambda (x) (string-equal (keim~name (just~method (node~justification x)))
							 "OPEN"))
				(prob~proof-steps pds*current-proof-plan))
		       (print "test-chris2")
		       (progn (setf count*neg (1+ count*neg)) "UNKNOWN"))
		      (T (print "test-chris3")
			 (progn (setf count*pos (1+ count*pos)) (format nil "Proof ~A" (AGPLAN~PDS2OUTLINE))))))
	(force-output stream)
	(omega~message "Removing files in ~A" (atptop~default-directory))
	))
    ;;;(sys~call-system (format nil "\\rm -rf ~A*" (atptop~default-directory)))))
    (format stream "~%~% Valid examples: ~A ~% Invalid examples: ~A ~% All examples: ~A" count*pos count*neg (+ count*pos count*neg))
    (omega~message "~%Result of case study written to file ~A ~% Valid examples: ~A ~% Invalid examples: ~A ~% All examples: ~A" outfile count*pos count*neg (+ count*pos count*neg))
    (close stream)))


(defun sex=pprint-model-entry (string)
  ;;;; model entry strings look like (ob_sk_0_x1_181418_1(ob_sk_0_g181454_181480_2).  )
  (labels ((get-after-third-_ (str)
			       (let* ((str1 (subseq str (1+ (position #\_ str))))
				      (str2 (subseq str1 (1+ (position #\_ str1))))
				      (str3 (subseq str2 (1+ (position #\_ str2))))
				      (obj (subseq str3 0 (position #\_ str3))))
				 obj))
	    (get-elem (str)
		      (let ((str1 (subseq str (1+ (position #\( str)))))
			str1)))
    (omega~message "~A in ~A" (get-after-third-_ (get-elem string)) (get-after-third-_ string)) ;;message 
    (format nil "~A in ~A" (get-after-third-_ (get-elem string)) (get-after-third-_ string))    ;;and return string
    ))

(defun sex=pp-model-string (string)
  (let* ((line-strings (atptop~divide-string string #\Newline))
	 (flag nil)
	 (res-string nil))
    (remove ""
	    (dolist (str line-strings res-string)
	      (if flag (setf res-string (append res-string (list str)))
		(when (string= "%% Model found:" str)
		  (setf flag t))))
	    :test #'string=)))

(defun sex~print-model (string)
    ;;;; these strings look like
    ;;;; (ob_sk_0_x1_183389_1(ob_sk_0_g183425_183451_2). ob_sk_0_x2_183404_3(ob_sk_0_g183425_183451_2).)
  (let* ((string-list (sex=pp-model-string string))
	 (res nil))
    (dolist (el string-list res)
      (setq res (if res (format nil "~A AND <~A>" res (sex=pprint-model-entry el))
		   (format nil "<~A>" (sex=pprint-model-entry el)))))))
      
      
  

(defun grp~settings-group-examples ()
  (csm~set-considered-classifiers nil)
  (csm~set-considered-commands (cons 'solved-by-fo-atp nic*command-list))
  (setf rsrc*use-resources NIL)
  (bb~set-command-suggestion-ordering '(:group-examples))
  (bb~set-parameter-suggestion-ordering '(:status :length))
  (bb~set-command-suggestion-equality '(:command))
  (bb~set-parameter-suggestion-equality '(:mapping))
  )
  
(defmethod bb~command-suggestion-leq-p
  ((sugg1 bb+command-suggestion) (sugg2 bb+command-suggestion) (indicator (eql :group-examples)))
  (or (find :extern (mapcar #'agplan~name (com~categories (bb~entry-command sugg2))) :test #'string-equal)
      (and (not (find :extern (mapcar #'agplan~name (com~categories (bb~entry-command sugg1))) :test #'string-equal))
	   (let ((pos1 (position (agplan~name (bb~entry-command sugg1)) nic*command-list :test #'string-equal))
		 (pos2 (position (agplan~name (bb~entry-command sugg2)) nic*command-list :test #'string-equal)))
	     (print pos1)
	     (print pos2)
	     (> pos1 pos2)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generate Uniqueness examples (in parts by Manfred Kerber)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(setq max-lisp-eval-depth 2000)
;;(setq print-level 30)

(defun agplan~generate-set-examples-new (number filename &optional
						(maxcounter 3)
						(max-uniq 20)
						(vars '(x1 x2 x3 x4 x5 x6))
						(functions (list 'union 'intersection 'setminus))
						(predicates '(= subset)))
  (declare (edited  "07-FEB-2007")
	   (authors Sorge)
	   (input   "Two integers indicating the number of variables and the nesting depth for"
		    "the construed expressions. Optionally a list of set operations.")
	   (effect  "None.")
	   (value   "A list of post expressions."))
  (with-open-file (output-stream filename :direction :output
				 :if-does-not-exist :create
				 :if-exists :supersede)
		  (let ((i 0))
		    (dotimes (n number)
		      (multiple-value-bind (formula variables)
			  (agplan=make-formula maxcounter functions predicates vars)
			(dolist (problem
				 (mapcar #'(lambda (pair)
					     (agplan=add-all-quantification (first pair) (second pair)))
					  (agplan=add-uniqueness-conditions-random formula variables max-uniq)))
			  (incf i)
			  (format output-stream "~%~A"  (agplan=formulate-problem problem i))))))))




(defun agplan~generate-set-examples-unique (number filename &optional
						(maxcounter 3)
						(vars '(x1 x2 x3 x4 x5 x6))
						(functions (list 'union 'intersection 'setminus))
						(predicates '(= subset)))
  (declare (edited  "07-FEB-2007")
	   (authors Sorge)
	   (input   "Two integers indicating the number of variables and the nesting depth for"
		    "the construed expressions. Optionally a list of set operations.")
	   (effect  "None.")
	   (value   "A list of post expressions."))
  (with-open-file (output-stream filename :direction :output
				 :if-does-not-exist :create
				 :if-exists :supersede)
		  (let ((i 0))
		    (dotimes (n number)
		      (multiple-value-bind (formula variables)
			  (agplan=make-formula maxcounter functions predicates vars)
			(incf i)
			(format output-stream "~%~A~%"  (agplan=formulate-problem
						       (agplan=add-all-quantification
							(first (agplan=add-full-uniqueness-conditions formula variables))
							variables) i)))))))

(defun agplan~generate-set-examples-disjoint (number filename &optional
						     (maxcounter 3)
						     (vars '(x1 x2 x3 x4 x5 x6))
						     (functions (list 'union 'intersection 'setminus))
						     (predicates '(= subset)))
  (declare (edited  "07-FEB-2007")
	   (authors Sorge)
	   (input   "Two integers indicating the number of variables and the nesting depth for"
		    "the construed expressions. Optionally a list of set operations.")
	   (effect  "None.")
	   (value   "A list of post expressions."))
  (with-open-file (output-stream filename :direction :output
				 :if-does-not-exist :create
				 :if-exists :supersede)
		  (let ((i 0))
		    (dotimes (n number)
		      (multiple-value-bind (formula variables)
			  (agplan=make-formula maxcounter functions predicates vars)
			(incf i)
			(format output-stream "~%~A~%"  (agplan=formulate-problem
						       (agplan=add-all-quantification
							(first (agplan=add-full-disjointness-conditions formula variables))
							variables) i)))))))

(defun agplan=set-add(el l)
  (if (member el l)
      l
      (cons el l)))


(let (occurring-vars)
  
  (defun agplan=make-term (counter maxcounter functions vars)
    (let* ((fl (length functions))
	   (vl (length vars))
	   (r (random (1+ fl))))
      (if (or (= r fl) (> counter maxcounter))
	  (let ((var (nth (random vl) vars)))
	    ;;(setq occurring-vars (agplan=set-add var occurring-vars))
	    (pushnew var occurring-vars)
	    var)
	(let* ((func (nth r functions))
	       (left (agplan=make-term (1+ counter) maxcounter functions vars))
	       (right (agplan=make-term (1+ counter) maxcounter functions vars)))
	  (loop while (tree-equal left right)
		do (setf right (agplan=make-term (1+ counter) maxcounter functions vars)))
	  (list func left right)))))
	

  (defun agplan=make-formula (maxcounter functions predicates vars)
    (setf occurring-vars nil)
    (let* ((pl (length predicates))
	   (pred (nth (random pl) predicates))
	   (left (agplan=make-term 0 maxcounter functions vars))
	   (right (agplan=make-term 0 maxcounter functions vars)))
      (values (list pred left right)
	      occurring-vars)))

  )


(defun agplan=permute2unordered-pairs (elements)
  (declare (edited  "07-JUN-2000")
	   (authors Sorge)
	   (input   "A list of elements.")
	   (effect  "None.")
	   (value   "A list of all possible distinct unordered pairs of the elements."))
  (flet ((make-pairs (elems)
		     (mapcar #'(lambda (x) (list (car elems) x)) (cdr elems))))
    (apply #'append (maplist #'make-pairs elements))))



(defun agplan=add-full-uniqueness-conditions (term variables)
  (flet ((make-equation (pair)
			(list 'not (list '= (first pair) (second pair)))))
    (labels ((make-conjunction (conj)
			       (cond ((< (length conj) 2) conj)
				     ((= (length conj) 2) (list 'and (first conj) (second conj)))
				     (t (list 'and (first conj) (make-conjunction (rest conj)))))))
      
      (cond ((<= (length variables) 1) (list (list term variables)))
	    ((= (length variables) 2) (list (list 'implies (make-equation variables) term) variables))
	    (t (list (list 'implies
			   (make-conjunction
			    (mapcar #'make-equation
				    (agplan=permute2unordered-pairs variables)))
			   term)
		     variables))))))

(defun agplan=add-full-disjointness-conditions (term variables)
  (flet ((make-equation (pair)
			(list '= (list 'intersection (first pair) (second pair)) 'emptyset)))
    (labels ((make-conjunction (conj)
			       (cond ((< (length conj) 2) conj)
				     ((= (length conj) 2) (list 'and (first conj) (second conj)))
				     (t (list 'and (first conj) (make-conjunction (rest conj)))))))
      
      (cond ((<= (length variables) 1) (list (list term variables)))
	    ((= (length variables) 2) (list (list 'implies (make-equation variables) term) variables))
	    (t (list (list 'implies
			   (make-conjunction
			    (mapcar #'make-equation
				    (agplan=permute2unordered-pairs variables)))
			   term)
		     variables))))))


;;; Randomly add some uniqueness conditions.

(defun agplan=random-numbers-in-interval (max number)
  (declare (edited  "07-FEB-2007")
	   (authors Sorge)
	   (input   "Two integers.")
	   (effect  "None.")
	   (value   "Generates NUMBER of different random elements in the intercal [0,max]."))
  (do* ((result (list (random max)) (pushnew (random max) result)))
      ((= (length result) number) (sort result #'<))))

(defun agplan=add-uniqueness-conditions-random (term variables number)
  (if (> (length variables) 1)
      (let ((conditions (agplan=make-uniq-conditions-random variables number)))
	(mapcar #'(lambda (cond) (if cond
				     (list (list 'implies cond term) variables)
				   (list term variables)))
		conditions))
    (list (list term variables))))

(defun agplan=make-uniq-conditions-random (variables number)
  (let* ((all-sets (agplan=construct-powerset
		   (remove-duplicates
		    (remove-if #'(lambda (x) (equal (first x) (second x))) (rcl=permute2pairs variables))
		    :test #'(lambda (x y) (or (and (equal (first x) (second y)) (equal (second x) (first y))))))))
	(sets (mapcar #'(lambda (n) (nth n all-sets)) (agplan=random-numbers-in-interval (length all-sets) number)))
	)
    (flet ((make-equation (pair)
			  (list 'not (list '= (first pair) (second pair)))))
      (labels ((make-conjunction (conj)
				 (cond ((< (length conj) 2) conj)
				       ((= (length conj) 2) (list 'and (first conj) (second conj)))
				       (t (list 'and (first conj) (make-conjunction (rest conj)))))))
	(mapcar #'(lambda (x)
		    (cond ((null x) nil)
			  ((= (length x) 1) (make-equation (car x)))
			  (t (make-conjunction
			      (mapcar #'make-equation x)))))
		sets)))))


(defun agplan~problem-file2tptp-files (in-file out-directory)
  (with-open-file (in in-file
		      :direction :input
		      :if-does-not-exist :error)
		  (do* ((x (read in nil) (read in nil))
			(result (list x) (cons x result)))
		      ((null x)
		       (mapcar #'(lambda (expr)
				   (f2tptp~problem2tptp-file (eval expr) out-directory))
			       (reverse (cdr result)))))))

(defun agplan~apply-vampire-to-file (in-file out-directory)
  
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Solved by Vampire ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun vamp~program ()
  "~/omega-new/atp/bin/linux/vampire"
  ;;"/project/omega/bin/vampire"
  )

(com~defcommand solved-by-vampire
  (argnames node)
  (argtypes ndline)
  (arghelps "Node to prove with VAMPIRE")
  (frag-cats extern)
  (function agplan=solved-by-vampire)
  (log-p T) 
  (help ""))

(defun agplan=solved-by-vampire (line)
  ;;;(declare (ignore parameters))
  (infer~compute-outline 'solved-by-vampire (list line) nil)
  ;;(agplan~call-vampire-new line)
  )

(infer~deftactic solved-by-vampire
		 (outline-mappings (((existent) solved-by-vampire-a)))
		 ;;;(parameter-types boolean)
                 )


(tac~deftactic solved-by-vampire-a solved-by-vampire (in base)
               (conclusions L1)
	       ;;;(parameters (fo-proof boolean "A flag."))
               ;;;(sideconditions (agplanleo=solved-by-vampire-a-p fo-proof))
               (description "Application of solved-by-vampire tactic."))



(defun agplan~test-with-vampire (line &key (resource 120))
  (declare (edited  "14-OCT-2004")
	   (authors Sorge)
	   (input   "A line. A time resource")
	   (effect  "Calls Vampire externally.")
	   (value   "If vampire finds a proof the message 'proof found' is printed and t is returned, otherwise"
		    "nil is returned. If vampire finds a saturation the message 'saturation found' is printed."))
  (let* ((vampire-input (f2tptp~node2tptp line))
	 (problem-dir "/tmp/omega-atp-dir/"))
    (unless (probe-file (namestring problem-dir))
      (sys~call-system (format nil "mkdir ~A" (string-right-trim '(#\/) (namestring problem-dir)))))
  (multiple-value-bind (time output)
      (f2tptp=test-with-vampire vampire-input problem-dir resource)
    (let ((success (vamp~check-success output)))
      (when success
	(omega~message "Vampire's time: ~%~A" time)
	(let* ((divided (atptop~divide-string output #\Newline))
	       (generated (nth (1+ (position "=== Generated clauses:" divided :test #'string-equal)) divided))
	       (vampire-time (nth (1+ (position "=== General:" divided :test #'string-equal)) divided)))
	  (omega~message "~A" vampire-time)
	  (omega~message "~A" generated)
;; 	  (leo~special-output (list 'format 'nil (format nil "& ~A & ~A & ~A " clause-number
;; 							 (car (atptop~parse-only-numbers time :float t))
;; 							 (car (atptop~parse-only-numbers generated))))
;; 			      3)
	  (values t vampire-time generated)
	  ))))))


(defun f2tptp=test-with-vampire (vampire-problem vampire-problem-dir ressource)
  (declare (edited  "26-MAR-1998")
	   (authors Ameier)
	   (input   "A vampire-problem, the directory for input, output and a time-ressource.")
	   (effect  "Writes the in-string of the vampire-problem to the file vampire.in in the"
		    "directory, calls vampire on it, reads the file vampire.out from the directory"
		    "and writes it into the out-string of the vampire-problem.")
	   (value   "1. The time needed by the Vampire proces."
		    "2. A string containing the Vampire output if one exists."))

  (let* ((problem-name (gensym "vampire-"))
	 (in-file (merge-pathnames (format nil "~A.in" problem-name) vampire-problem-dir))
	 (temp-out-file (merge-pathnames (format nil "~A-tmp.out" problem-name) vampire-problem-dir))
	 (out-file (merge-pathnames (format nil "~A.out" problem-name) vampire-problem-dir)))

    (atptop~print-string-in-file vampire-problem in-file)

    (setf atptop*interactivity-allowed nil)
    (let* ((*trace-output* (make-string-output-stream))
	   (call-flag (time (atptop~call-with-time-ressource (format nil "~A ~A >! ~A; mv ~A ~A &"
								     (vamp~program) in-file temp-out-file
								     temp-out-file out-file)
							     out-file
							     "vampire"
							     vampire-problem-dir
							     ressource
							     ))))
      (values
       (get-output-stream-string *trace-output*)
       (if (null call-flag)
	   (omega~message "~% Vampire was not able to find a proof in the given time resource.")
	 (atptop~read-file-as-string out-file)
	)))))


(defun agplan~call-vampire (line &key (resource 20))
  (declare (edited  "14-OCT-2004")
	   (authors Sorge)
	   (input   "A line. A time resource")
	   (effect  "Calls Vampire externally.")
	   (value   "If vampire finds a proof the message 'proof found' is printed and t is returned, otherwise"
		    "nil is returned. If vampire finds a saturation the message 'saturation found' is printed."))
  (setf atptop*interactivity-allowed nil)
  (let* ((vampire-input (f2tptp~node2tptp line))
	 (problem-dir "/tmp/omega-atp-dir/"))
    (unless (probe-file (namestring problem-dir))
      (sys~call-system (format nil "mkdir ~A" (string-right-trim '(#\/) (namestring problem-dir)))))
    (proc~create :name (format nil "Vampire")
		 ;;;:priority 300
		 :function #'agplan=run-parallel-vampire
		 :args (vampire-input problem-dir resource))
    ))

(defun agplan=run-parallel-vampire (atp-problem problem-dir resource)
  ;; parsen des vampire-beweises
  (multiple-value-bind (time output)
      (f2tptp=call-vampire! atp-problem problem-dir resource)
    (let ((success (vamp~check-success output)))
      (when success
	(omega~message "Vampire's time: ~%~A" time)
	(let* ((divided (atptop~divide-string output #\Newline))
	       (generated (nth (1+ (position "=== Generated clauses:" divided :test #'string-equal)) divided)))
	  (omega~message "~A" generated)
;; 	  (leo~special-output (list 'format 'nil (format nil "& ~A & ~A & ~A " clause-number
;; 							 (car (atptop~parse-only-numbers time :float t))
;; 							 (car (atptop~parse-only-numbers generated))))
;; 			      3)
	  )))))
      

(defun agplan~call-vampire-new (line &key (resource 10))
  (declare (edited  "13-MAR-2007" "14-OCT-2004")
	   (authors Vxs Sorge)
	   (input   "A line. A time resource")
	   (effect  "Calls Vampire externally.")
	   (value   "If vampire finds a proof the message 'proof found' is printed and t is returned, otherwise"
		    "nil is returned. If vampire finds a saturation the message 'saturation found' is printed."))
  (setf atptop*interactivity-allowed nil)
  (let* ((vampire-input (f2tptp~node2tptp line))
	 (problem-dir "/tmp/omega-atp-dir/"))
    (unless (probe-file (namestring problem-dir))
      (sys~call-system (format nil "mkdir ~A" (string-right-trim '(#\/) (namestring problem-dir)))))
  (multiple-value-bind (time output)
      (f2tptp=call-vampire! vampire-input problem-dir resource)
    (let ((success (vamp~check-success output)))
      (when success
	(omega~message "Vampire's time: ~%~A" time)
	(let* ((divided (atptop~divide-string output #\Newline))
	       (generated (nth (1+ (position "=== Generated clauses:" divided :test #'string-equal)) divided))
	       (vampire-time (nth (1+ (position "=== General:" divided :test #'string-equal)) divided)))
	  (omega~message "~A" vampire-time)
	  (omega~message "~A" generated)
;; 	  (leo~special-output (list 'format 'nil (format nil "& ~A & ~A & ~A " clause-number
;; 							 (car (atptop~parse-only-numbers time :float t))
;; 							 (car (atptop~parse-only-numbers generated))))
;; 			      3)
	  t
    ))))))


(defun f2tptp=call-vampire! (vampire-problem vampire-problem-dir ressource)
  (declare (edited  "26-MAR-1998")
	   (authors Ameier)
	   (input   "A vampire-problem, the directory for input, output and a time-ressource.")
	   (effect  "Writes the in-string of the vampire-problem to the file vampire.in in the"
		    "directory, calls vampire on it, reads the file vampire.out from the directory"
		    "and writes it into the out-string of the vampire-problem.")
	   (value   "1. The time needed by the Vampire proces."
		    "2. A string containing the Vampire output if one exists."))

  (let* ((problem-name (gensym "vampire-"))
	 (in-file (merge-pathnames (format nil "~A.in" problem-name) vampire-problem-dir))
	 (temp-out-file (merge-pathnames (format nil "~A-tmp.out" problem-name) vampire-problem-dir))
	 (out-file (merge-pathnames (format nil "~A.out" problem-name) vampire-problem-dir)))

    (atptop~print-string-in-file vampire-problem in-file)

    (let* ((*trace-output* (make-string-output-stream))
	   (call-flag (time (agplan~call-with-time-ressource (format nil "~A ~A >! ~A; mv ~A ~A &"
								     (vamp~program) in-file temp-out-file
								     temp-out-file out-file)
							     out-file
							     "vampire"
							     vampire-problem-dir
							     ressource
							     :agent-name "solved-by-vampire"))))
      (values
       (get-output-stream-string *trace-output*)
       (if (null call-flag)
	   (omega~message "~% Vampire was not able to find a proof in the given time resource.")
	 (atptop~read-file-as-string out-file)
	 )))))


(defun agplan~call-with-time-ressource (shell-call out-file-name program-name directory resource-in-sec
						   &key (agent-name nil))
  (declare (edited  "28-FEB-2007" "29-JAN-1998")
	   (authors Sorge ChrisSorge AMEIER)
	   (input   "The command to call in the shell as a string, the name of the out-file (as string),"
		    "the name of the program (as string), the working directory and a"
		    "time resource in seconds.")
	   (effect  "First the outfile is removed, then the command is started in the shell, with all"
		    "things, that this may will cause ...")
	   (value   "T if the out-file is written by the processes themself after they had finished, nil"
		    "if the processes doesn't stopped during the time given by the time resource and had"
		    "to be killed."))
  ;;;(mixin~save-and-set-input)
  (when (probe-file out-file-name)
    (sys~call-system (format nil "\\rm ~A" out-file-name)))

  (print 1)
  (multiple-value-bind
      (stream-a dummy-a dummy-b) 
      (excl::run-shell-command shell-call
			       :wait nil :output :stream)
    (declare (ignore dummy-a dummy-b))
    (print 2)
    (setf streami stream-a)
    
    (let* ((time (get-universal-time))
	   (killing-time (+ resource-in-sec time))
	   (dummy (print 'dummy1))
	   (pid (atptop~get-pid-from-stream stream-a))
	   ;;(dummy (print pid))
	   (dummy (print 'dummy2))
	   (agent)) 

;;       (when agent-name
;; 	(progn
;; 	 (setf agent (agent~get-agent agent-name))
;; 	 (setf (agent~proc-id agent) (append (agent~proc-id agent) (list (list pid
;; 									       directory))))
;; 	 ;; add agent to list with all agents for external
;; 	 ;; provers
;; 	 (setf agent*external-prover-agents (cons agent agent*external-prover-agents))))
	 
      (omega~message "~% Calling ~A process ~A with time resource ~Asec ."
		     program-name
		     pid
		     resource-in-sec)
      
      (omega~message "~% ~A Time Resource in seconds: " program-name)
      (print 3)

      (proc~wait "Waiting for external system to terminate."
 		 #'(lambda () (or
			       (probe-file out-file-name)
			       (< killing-time (get-universal-time)))))
      (print 4)
      
      (let* ((pids-of-children (atptop~get-pids-of-children pid directory))
	     (dummy (print pids-of-children))
	     (proof-found-flag (if (probe-file out-file-name)
				   ;; prueft noch ein letztes mal ob outfile inzwischen da 
				   't
				 (progn (atptop~kill pids-of-children)
					nil))))
	;;;(mixin~reset-input)
	proof-found-flag))))

(agent~defagent solved-by-vampire c-predicate
                (for node)
                (uses )
                (level 1)
                (definition (agplan~call-vampire-new (:node node))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Counterexample with Paradox ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun paradox~program ()
  "/project/omega/bin/paradox"
  "~/omega-new/atp/bin/linux/paradox"
  )

(com~defcommand counterexample-by-paradox
  (argnames node)
  (argtypes ndline)
  (arghelps "Node to find counterexample for with Paradox")
  (frag-cats extern)
  (function agplan=counterexample-by-paradox)
  (log-p T) 
  (help ""))

(defun agplan=counterexample-by-paradox (line)
  ;;;(declare (ignore parameters))
  (infer~compute-outline 'counterexample-by-paradox (list line) nil)
  )

(infer~deftactic counterexample-by-paradox
		 (outline-mappings (((existent) counterexample-by-paradox-a)))
		 ;;;(parameter-types boolean)
                 )


(tac~deftactic counterexample-by-paradox-a counterexample-by-paradox (in base)
               (conclusions L1)
	       ;;;(parameters (fo-proof boolean "A flag."))
               ;;;(sideconditions (agplanleo=counterexample-by-paradox-a-p fo-proof))
               (description "Application of counterexample-by-paradox tactic."))



(defun agplan~test-with-paradox (line &key (resource 120))
  (declare (edited  "14-OCT-2004")
	   (authors Sorge)
	   (input   "A line. A time resource")
	   (effect  "Calls Vampire externally.")
	   (value   "If vampire finds a proof the message 'proof found' is printed and t is returned, otherwise"
		    "nil is returned. If vampire finds a saturation the message 'saturation found' is printed."))
  (let* ((vampire-input (f2tptp~node2tptp line))
	 (problem-dir "/tmp/omega-atp-dir/"))
    (unless (probe-file (namestring problem-dir))
      (sys~call-system (format nil "mkdir ~A" (string-right-trim '(#\/) (namestring problem-dir)))))
    (setf atptop*interactivity-allowed nil)
    (multiple-value-bind (time output)
	(f2tptp=test-with-paradox vampire-input problem-dir resource)
      (let ((success (paradox~check-success output)))
	(when success
	  (omega~message "Paradox's time: ~%~A" time)
	(let* ((divided (atptop~divide-string output #\Newline))
	       (generated (nth (1+ (position "== Result ==================================================================" divided :test #'string-equal)) divided)))
	  (omega~message "~A" generated)
;; 	  (leo~special-output (list 'format 'nil (format nil "& ~A & ~A & ~A " clause-number
;; 							 (car (atptop~parse-only-numbers time :float t))
;; 							 (car (atptop~parse-only-numbers generated))))
;; 			      3)
	  
	  (values t generated)
	  ))))))


(defun f2tptp=test-with-paradox (paradox-problem paradox-problem-dir ressource)
  (declare (edited  "26-MAR-1998")
	   (authors Ameier)
	   (input   "A paradox-problem, the directory for input, output and a time-ressource.")
	   (effect  "Writes the in-string of the paradox-problem to the file paradox.in in the"
		    "directory, calls paradox on it, reads the file paradox.out from the directory"
		    "and writes it into the out-string of the paradox-problem.")
	   (value   "1. The time needed by the Paradox proces."
		    "2. A string containing the Paradox output if one exists."))

  (let* ((problem-name (gensym "paradox-"))
	 (in-file (merge-pathnames (format nil "~A.in" problem-name) paradox-problem-dir))
	 (temp-out-file (merge-pathnames (format nil "~A-tmp.out" problem-name) paradox-problem-dir))
	 (out-file (merge-pathnames (format nil "~A.out" problem-name) paradox-problem-dir)))

    (atptop~print-string-in-file paradox-problem in-file)

    (setf atptop*interactivity-allowed nil)
    (let* ((*trace-output* (make-string-output-stream))
	   (call-flag (time (atptop~call-with-time-ressource (format nil "~A ~A >! ~A; mv ~A ~A &"
								     (paradox~program) in-file temp-out-file
								     temp-out-file out-file)
							     out-file
							     "paradox"
							     paradox-problem-dir
							     ressource
							     ))))
      (values
       (get-output-stream-string *trace-output*)
       (if (null call-flag)
	   (omega~message "~% Paradox was not able to find a counterexample in the given time resource.")
	 (atptop~read-file-as-string out-file)
	)))))

(defun agplan~call-paradox (line &key (resource 10))
  (declare (edited  "13-MAR-2007" "14-OCT-2004")
	   (authors Vxs Sorge)
	   (input   "A line. A time resource")
	   (effect  "Calls Paradox externally.")
	   (value   "If paradox finds a counterexample the message 'counterexample found' is printed and t is returned, otherwise"
		    "nil is returned."))
  (setf atptop*interactivity-allowed nil)
  (let* ((paradox-input (f2tptp~node2tptp line))
	 (problem-dir "/tmp/omega-atp-dir/"))
    (unless (probe-file (namestring problem-dir))
      (sys~call-system (format nil "mkdir ~A" (string-right-trim '(#\/) (namestring problem-dir)))))
  (multiple-value-bind (time output)
      (f2tptp=call-paradox! paradox-input problem-dir resource)
    (let ((success (paradox~check-success output)))
      (when success
	(omega~message "Paradox's time: ~%~A" time)
	(let* ((divided (atptop~divide-string output #\Newline))
	       (generated (nth (1+ (position "== Result ==================================================================" divided :test #'string-equal)) divided)))
	  (omega~message "~A" generated)
;; 	  (leo~special-output (list 'format 'nil (format nil "& ~A & ~A & ~A " clause-number
;; 							 (car (atptop~parse-only-numbers time :float t))
;; 							 (car (atptop~parse-only-numbers generated))))
;; 			      3)

	  t
	  ))))))


(defun f2tptp=call-paradox! (paradox-problem paradox-problem-dir ressource)
  (declare (edited  "26-MAR-1998")
	   (authors Ameier)
	   (input   "A paradox-problem, the directory for input, output and a time-ressource.")
	   (effect  "Writes the in-string of the paradox-problem to the file paradox.in in the"
		    "directory, calls paradox on it, reads the file paradox.out from the directory"
		    "and writes it into the out-string of the paradox-problem.")
	   (value   "1. The time needed by the Paradox proces."
		    "2. A string containing the Paradox output if one exists."))

  (let* ((problem-name (gensym "paradox-"))
	 (in-file (merge-pathnames (format nil "~A.in" problem-name) paradox-problem-dir))
	 (temp-out-file (merge-pathnames (format nil "~A-tmp.out" problem-name) paradox-problem-dir))
	 (out-file (merge-pathnames (format nil "~A.out" problem-name) paradox-problem-dir)))

    (atptop~print-string-in-file paradox-problem in-file)

    (let* ((*trace-output* (make-string-output-stream))
	   (call-flag (time (agplan~call-with-time-ressource (format nil "~A ~A >! ~A; mv ~A ~A &"
								     (paradox~program) in-file temp-out-file
								     temp-out-file out-file)
							     out-file
							     "paradox"
							     paradox-problem-dir
							     ressource
							     :agent-name "counterexample-by-paradox"))))
      (values
       (get-output-stream-string *trace-output*)
       (if (null call-flag)
	   (omega~message "~% Paradox was not able to find a counterexample in the given time resource.")
	 (atptop~read-file-as-string out-file)
	)))))


(defun paradox~check-success (string)
  (let* ((line-strings (atptop~divide-string string #\Newline))
	 (proof-flag
	  (let* ((result (member "== Result ==================================================================" line-strings :test 'string=)))
	    (when result
	      (let* ((sat-unsat (atptop~divide-string (string-trim '(#\Space) (second result)) #\Space)))
		(cond ((string-equal (third sat-unsat) "SATISFIABLE")
		       'sat)
		      ((string-equal (third sat-unsat) "CONTRADICTION")
		       'unsat)
		      (t (error "An unknown case has occurred: ~A." result))))))))
    (cond ((equal proof-flag 'sat)
	   (omega~message "~% Paradox has found a model.~%")
	   t)
	  ((equal proof-flag 'unsat)
	   (omega~message "~% Paradox has found a contradiction.~%")
	   nil)
	  (t
	   (omega~message "~% Paradox has not found a model.~%")
	   nil))))

(agent~defagent counterexample-by-paradox c-predicate
                (for node)
                (uses )
                (level 1)
                (definition (agplan~call-paradox (:node node))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatically exploring the new set examples
;; Here we do three things:
;; 1.) Attempt the problem with 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun sex~explore-testbed-new (file outfile)
  (setf sex*explore-process
        (proc~create :name "Set Examples Exploration"
                     :function #'sex=run-function-new
                     :args (file outfile))))

(defun sex=run-function-new (file outfile)
  (let ((stream (open outfile :direction :output
		      :if-exists :rename-and-delete))
	(count*pos 0)
	(count*neg 0))
    (format stream "This file reports on the validity/invalidity of the examples defined
in ~A~% Authors: Benzmueller & Sorge ~%" file)
    (csm~set-considered-commands '(
			       foralli*
			       set-ext-contract*
			       defsi
			       ;;;PRENEX-FORM
			       solved-by-vampire
			       counterexample-by-paradox
			       ))
    (do ((result (keim::th=read-next-sexp file) (keim::th=read-next-sexp)))
	((null result) (print "Done exploring!"))
      (let* ((problem (eval result))
	     measure-time)	; (print "hier")

	(opr~enqueue-command (opr~normalize-command 'prove
						    :args (list problem) :process
						    (proc~actual-process)))
	(proc~sleep 2)
	(format stream "~%~A" (keim~name problem))
	(format stream " & $~A$" (sex~formula2latex (node~formula (prob~conclusion problem))))
	(multiple-value-bind (success time generated)
	    (agplan~test-with-vampire (pds~label2node 'conc))
	  (if success
	      (format stream "& Proved & ~A & ~A" time generated)
	    (format stream "& Not proved & --- &---")))
        (multiple-value-bind (success time)
            (agplan~test-with-paradox (pds~label2node 'conc))
          (if success
              (format stream "& Model & ~A" time)
            (format stream "& No model & ---" time)))
	(opr~enqueue-command (opr~normalize-command 'automate :process (proc~actual-process)))
	(setq measure-time (get-universal-time))
	(proc~sleep 8)
	(proc~wait "Waiting for automated proof search to finish!"
		   #'(lambda () (not (proc~is-active auto*process))))
	(setq measure-time (- (get-universal-time) measure-time))
	(format stream "& ~A & ~A \\\\" 
		(cond ((find-if #'(lambda (x) (string-equal (keim~name (just~method (node~justification x)))
							    "COUNTEREXAMPLE-BY-PARADOX"))
				(prob~proof-steps pds*current-proof-plan))
		       (progn (setf count*neg (1+ count*neg))
			      (omega~message "Counterexample")
			      "Counterexample"))
		      ((find-if #'(lambda (x) (string-equal (keim~name (just~method (node~justification x)))
							 "OPEN"))
				(prob~proof-steps pds*current-proof-plan))
		       (progn (setf count*neg (1+ count*neg)) "UNKNOWN")
		       "Unknown")
		      (T (print "test-chris3")
			 (progn (setf count*pos (1+ count*pos))
				"Proof")))
		measure-time)
	(force-output stream)
	(omega~message "Removing files in ~A" (atptop~default-directory))
	))
    ;;;(sys~call-system (format nil "\\rm -rf ~A*" (atptop~default-directory)))))
    (format stream "~%~% Valid examples: ~A ~% Invalid examples: ~A ~% All examples: ~A" count*pos count*neg (+ count*pos count*neg))
    (omega~message "~%Result of case study written to file ~A ~% Valid examples: ~A ~% Invalid examples: ~A ~% All examples: ~A" outfile count*pos count*neg (+ count*pos count*neg))
    (close stream)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some Latex Pretty Printing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((transtable nil))

  (defgeneric sex=get-translation (name &optional (input nil))
    (:method ((string string) &optional (input nil))
	     (if input
		 (progn 
		   (setf transtable (acons string input transtable))
		   input)
	       (cdr (assoc string transtable :test #'string-equal))))
    (:method ((string symbol) &optional (input nil))
	     (sex=get-translation (format nil "~A" string) input))
    (:method ((name keim+name) &optional (input nil))
	     (sex=get-translation (keim~name name) input)))
)



(defun sex~formula2latex (formula)
  (let ((rest-formula formula)
	(variables nil))
    (with-output-to-string (str)
  ;;; Take care of quantifiers
			   (do* ((vars (list (logic~quantification-bound-variable rest-formula))
				       (cons (logic~quantification-bound-variable rest) vars))
				 (rest (logic~quantification-scope rest-formula)
				       (logic~quantification-scope rest)))
			       ((not (logic~universal-quantification-p rest))
				(progn (setf variables (sort (mapcar #'keim~name vars) #'string-lessp))
				       (setf rest-formula rest)
				       (format str (sex=formula-universal-prefix vars)))))
  ;;; Take care of implications
			   (unless (logic~implication-p rest-formula)
			     (error "Somethings wrong. Not an implication."))
			   (let* ((condition (car (data~appl-arguments rest-formula)))
				  (equation (cadr (data~appl-arguments rest-formula))))
  ;;; Take care of inequalities
			     (let* ((inequalities (sex=formula-inequalities-conjunction condition))
				    (sorted (sort inequalities #'(lambda (x y)
								   (or (string-lessp (car x) (car y))
								       (and (string-equal (car x) (car y))
									    (string-lessp (cadr x) (cadr y))))))))
			       (format str "[~{~A~^\\wedge~}]" (mapcar #'(lambda (x)
									 (format nil "~A\\not=~A"
										 (sex=get-translation (car x))
										 (sex=get-translation (cadr x))))
								     sorted)))
  ;;; Take care of disjointess
			     ;; not yet implemented
			     (format str "\\rightarrow")
  ;;; Take care of equation
			     (let ((args (data~appl-arguments equation)))
			       (if (logic~equality-p equation)
				   (format str "[~A = ~A]" (sex~term2latex (car args)) (sex~term2latex (cadr args)))
				 (format str "[~A \\subseteq ~A]" (sex~term2latex (car args)) (sex~term2latex (cadr args)))))
  ;;; Take care of subset
			     )
			   
			   str
			   )
))


(defun sex=formula-universal-prefix (vars)
  (let* ((var-strings (sort (mapcar #'keim~name vars) #'string-lessp)))
    (with-output-to-string (str)
			   (format str "\\all{")
			   (dolist (var var-strings)
			     (format str " ")
			     (multiple-value-bind (name number) (post2tex=split-name var)
			       (if (equal number "")
				   (format str (sex=get-translation var (format nil "~A" name)))
				 (format str (sex=get-translation var (format nil "{~A}_{~A}" name number))))))
			   (format str "}.")
			   str)))


(defun sex=formula-inequalities-conjunction (formula)
  (declare (edited  "02-APR-2007")
	   (authors Vxs)
	   (input   "A conjunction of inequalities.")
	   (effect  "None.")
	   (value   "A list of strings with the Latex representation of the inequalities."))
  (if (logic~conjunction-p formula)
      (let ((args (data~appl-arguments formula)))
	(append (sex=formula-inequalities-conjunction (first args))
		(sex=formula-inequalities-conjunction (second args))))
    (list (sex=formula-inequality formula))))

(defun sex=formula-inequality (formula)
  (if (logic~negation-p formula)
      (let* ((equality (first (data~appl-arguments formula))))
	(if (logic~equality-p equality)
	    (let* ((args (data~appl-arguments equality)))
	      (list (keim~name (first args)) (keim~name (second args))))
	  (error "Not an equality ~A!" equality)))
    (error "Not an inequality ~A!" formula)))
	
			       
					    
  

(defun sex~term2latex (formula)
  (cond ((term~appl-p formula)
	 (let* ((function (keim~name (data~appl-function formula)))
		(args (data~appl-arguments formula))
		(arg1 (car args))
		(arg2 (cadr args)))
	   (cond ((string-equal function :intersection)
		  (format nil "(~A\\cap~A)" (sex~term2latex arg1) (sex~term2latex arg2)))
		 ((string-equal function :union)
		  (format nil "(~A\\cup~A)" (sex~term2latex arg1) (sex~term2latex arg2)))
		 ((string-equal function :setminus)
		  (format nil "(~A\\setminus~A)" (sex~term2latex arg1) (sex~term2latex arg2)))
		 ((string-equal function :subset)
		  (format nil "(~A\\subseteq~A)" (sex~term2latex arg1) (sex~term2latex arg2)))
		 ((string-equal function :in)
		  (format nil "(~A\\in~A)" (sex~term2latex arg1) (sex~term2latex arg2)))
		 (t (error "~A is not a set function!" function)))))
	((string-equal (keim~name formula) :emptyset)
	 "\\emptyset")
	(t (sex=get-translation formula))))


