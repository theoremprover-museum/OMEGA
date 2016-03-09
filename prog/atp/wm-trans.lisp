;;; -*- syntax: common-lisp; package: OMEGA; base: 10; mode: LISP -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     PROVERB Project                                                      ;;
;;     AG Siekmann/FB Informatik                                            ;;
;;     Universitaet des Saarlandes                                          ;;
;;     Bau 36, 4. Stock                                                     ;;
;;     D-66041 Saarbruecken                                                 ;;
;;     Germany                                                              ;;
;;   electronic mail: proverb@cs.uni-sb.de                                  ;;
;;                                                                          ;;
;;   The author makes no representations about the suitability of this      ;;
;;   software for any purpose.  It is provided "AS IS" without express or   ;;
;;   implied warranty.  In particular, it must be understood that this      ;;
;;   software is an experimental version, and is not suitable for use in    ;;
;;   any safety-critical application, and the author denies a license for   ;;
;;   such use.                                                              ;;
;;                                                                          ;;
;;   You may use, copy, modify and distribute this software for any         ;;
;;   noncommercial and non-safety-critical purpose.  Use of this software   ;;
;;   in a commercial product is not included under this license.  You must  ;;
;;   maintain this copyright statement in all copies of this software that  ;;
;;   you modify or distribute.                                              ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;

(in-package :omega)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; translating TPTP problems to POST syntax....
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun wm~trans-directory (in-dir out-dir &optional (verbose t))
  (let* ((old-dir (directory in-dir))
	 (new-dir (mapcar #'(lambda (path)
			      (make-pathname :directory out-dir
					     :name (pathname-name path)))
			  old-dir))
	 (old-new-dir (pairlis old-dir new-dir)))
    (dolist (dir new-dir)
      (unless (probe-file dir)
	(sys~call-system (format nil "mkdir ~A" (namestring dir)))))
    (let ((file-list (apply #'append
			    (mapcar #'(lambda (dir-assoc)
					(let* ((old-files (directory (make-pathname :directory (namestring (car dir-assoc)))))
					       (new-files (mapcar #'(lambda (file)
								      (make-pathname :directory (namestring (cdr dir-assoc))
										     :name (pathname-name file)
										     :type "post"))
								  old-files)))
					  (pairlis old-files new-files)))
				    old-new-dir))))
      (with-open-file (out (make-pathname :directory out-dir
					  :name "waldmeister"
					  :type "rpy")
			   :direction :output
			   :if-exists :supersede)
	     (format out ";;;  Automatic Waldmeister Test File~%")
	     (format out "COMINT COMMENT ((%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%))~%")
	     (format out "COMINT COMMENT ((%%%%%%%     Testing some Waldmeister Problems     %%%%%%%%%%%%%%))~%")
	     (format out "COMINT COMMENT ((%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%))~%")
	     (format out "EXTERN INTERACTIVITY (NIL)")
	     (dolist (file-assoc file-list)
	       (let ((in-file (car file-assoc))
		     (out-file (cdr file-assoc)))
		 (when verbose (format t "~%Transforming Waldmeister TPTP File ~A." in-file))
		 (atptop~print-string-in-file (wm~trans in-file) out-file)
		 (format out "COMINT COMMENT ((%%%%%%  TPTP ~A  %%%%%%))~%" (pathname-name out-file))
		 (format out "OMEGA-BASIC READ-PROBLEM (\"~A\")~%" (namestring out-file))
		 (format out "EXTERN CALL-WALDMEISTER-ON-NODE default default (20) default default default default default default default default~%")))
	     (format out "COMINT COMMENT ((%%%%%%%     Testing of Waldmeister Problems finished   %%%%%%%%%))~%")))))
			    
    

(defun wm~trans (file)
  (let* ((string (atptop~read-file-as-string file))
	 (lines (remove-if #'(lambda (stringi)
			       (string= stringi ""))
			   (atptop~divide-string string #\Newline))))
    (multiple-value-bind (name sorts wmsignature variables equations conclusion)
	(wm=divide-input-line-strings lines)
      (let* ((new-sorts (remove "ANY" sorts :test #'string=))
	     (new-vars (subst "I" "ANY" variables :test #'equal))
	     (signature wmsignature)
	     (assumptions (mapcan #'(lambda (x)
			    (list (car x)
				  (wm=transform-equation (cadr x) new-vars)))
			equations)))
	(multiple-value-bind (new-sign skolem-const)
	    (wm=divide-signature (subst "I" "ANY" signature :test #'equal) assumptions new-vars)
	  (format nil "(problem ~A (in base)
                        (type-constants ~{~A~})
                        (constants ~{~A~%~})
                        ~{(assumption ~A ~A)~%~}
                        (conclusion ~A ~A))"
		  name
		  new-sorts
		  (wm=transform-constants new-sign)
		  assumptions
		  (car conclusion)
		  (wm=transform-conclusion (cadr conclusion) new-vars skolem-const)))))))

(defun wm=divide-signature (sign assumptions vars)
  (let ((ass (do* ((ass-list (cdr assumptions) (cddr ass-list))
		   (new-list (list (car ass-list)) (cons (car ass-list) new-list)))
		 ((null ass-list) (mapcar #'read-from-string (cdr new-list)))))
	(sign (mapcar #'(lambda (term) (let ((new-term (wm=rewrite-term (car term))))
					 (cons new-term (cdr term))))
		      sign))
	(vars (mapcar #'(lambda (term) (let ((new-term (wm=rewrite-term (car term))))
					 (cons new-term (cdr term))))
		      vars))
	new-sign skolems)
    (dolist (x sign)
      (if (and (not (find (car x) vars :test #'(lambda (z y) (string-equal z (car y)))))
	       (some #'(lambda (y) (wm=find-recursive (car x) y)) ass))
	  (push x new-sign)
	(push x skolems)))
    (values new-sign skolems)))

(defun wm=find-recursive (term list)
  (if (atom list)
      (cond ((and (numberp term) (numberp list)) (= term list))
	    ((numberp list)
	     (let ((new-term (read-from-string term)))
	       (and (numberp new-term) (= new-term list))))
	    (t (string-equal term list)))
    (or (wm=find-recursive term (car list))
	(wm=find-recursive term (cdr list)))))

(let ((wm*variables nil)
      (wm*skolems nil))
  
  (defun wm=transform-equation (formula vars)
    (setf wm*variables nil)
    (let ((post-formula (wm=transform-formula formula vars)))
      (format nil "~{~A~} ~A ~A"
	      (mapcar #'(lambda (x)
			  (format nil "(forall (lam ~A~%" x))
		      (reverse wm*variables))
	      post-formula
	      (make-string (* 2 (length wm*variables)) :initial-element #\)))))
  
  (defun wm=transform-formula (formula vars)
    (declare (edited  "15-OCT-1998 19:03")
	     (authors SORGE)
	     (input   "A formula-string and a list of variables.")
	     (effect  "None.")
	     (value   "A list of strings representing constants in POST syntax."))
    (if (> (atptop~number-of-char-in-string #\= formula) 0)
	(let* ((args (atptop~divide-string formula #\= :ignore-char-list (list #\space))))
	  (list '=
		(wm=transform-formula (car args) vars)
		(wm=transform-formula (cadr args) vars)))
      (multiple-value-bind
	  (functor-string rest-string)
	  (atptop~get-next-word formula #\()
	(if (string= rest-string "")
	    (wm=string2object functor-string vars)
	  (let* ((functor (wm=string2object functor-string vars))
		 (args (mapcar #'(lambda (term)
				   (wm=transform-formula term vars))
			       (wm=parse-term-list (atptop~cut-last-char rest-string)))))
	    (cons functor args))))))
  
  (defun wm=transform-conclusion (formula vars skolems)
    (setf wm*variables nil)
    (setf wm*skolems nil)
    (let ((post-formula (wm=transform-conc-formula formula vars skolems)))
      (format nil "~A ~A ~A"
	      (wm=compute-quantification wm*variables wm*skolems)
	      post-formula
	      (make-string (* 2 (+ (length wm*variables) (length wm*skolems))) :initial-element #\)))))
  
  (defun wm=transform-conc-formula (formula vars skolems)
    (declare (edited  "15-OCT-1998 19:03")
	     (authors SORGE)
	     (input   "A formula-string and a list of variables.")
	     (effect  "None.")
	     (value   "A list of strings representing constants in POST syntax."))
    (if (> (atptop~number-of-char-in-string #\= formula) 0)
	(let* ((args (atptop~divide-string formula #\= :ignore-char-list (list #\space))))
	  (list '=
		(wm=transform-conc-formula (car args) vars skolems)
		(wm=transform-conc-formula (cadr args) vars skolems)))
      (multiple-value-bind
	  (functor-string rest-string)
	  (atptop~get-next-word formula #\()
	(let ((skolem? (find functor-string skolems :test #'(lambda (x y) (string-equal x (car y))))))
	  (cond (skolem?
		 (let ((vars (wm=parse-term-list (atptop~cut-last-char rest-string))))
		   (pushnew (list functor-string vars (cadr skolem?)) wm*skolems
			    :test #'(lambda (x y) (string-equal (car x) (car y))))
		   functor-string))
		((string= rest-string "")
		 (wm=string2object functor-string vars))
		(t 
		 (let* ((functor (wm=string2object functor-string vars))
			(args (mapcar #'(lambda (term)
					  (wm=transform-conc-formula term vars skolems))
				      (wm=parse-term-list (atptop~cut-last-char rest-string)))))
		   (cons functor args))))))))
  
  (defun wm=parse-term-list (term-list-string)
    (declare (edited  "17-MAY-1996")
	     (authors Sorge)
	     (input   "A term-list-sting, this is something like s(a,b,c),p(d,e,f),r,q")
	     (effect  "None.")
	     (value   "This list is divided top-level by the commata:"
		      "returns a list of strings."
		      "( \"s(a,b,c)\"  \"p(d,e,f)\" \"r" \"q")"
		      "The function does not destroy the term-structur down in the terms."))
    (do ((rest-term-list-string term-list-string)
	 (current-word "")
	 (return-terms nil))
	((string= rest-term-list-string "") return-terms)
      (multiple-value-bind 	      
	  (next-word rest-string)
	  (atptop~get-next-word rest-term-list-string #\, :handle-break-char 'pre)
	(setq current-word (format nil "~A~A" current-word next-word))
	(setq rest-term-list-string rest-string) 
	(if (= (atptop~number-of-char-in-string #\( current-word) (atptop~number-of-char-in-string #\) current-word)) 
	    (progn
	      (if (string= rest-string "")
		  (setq return-terms (append return-terms (list current-word)))
		(setq return-terms (append return-terms (list (atptop~cut-last-char current-word))))) 
	      (setq current-word ""))))))

  (defun wm=string2object (term vars)
    (let ((var (assoc term vars :test #'string=)))
      (if var
	  (progn
	    (pushnew var wm*variables)
	    ;;            (car var))
	    ;;        term)))
           (wm=rewrite-term (car var)))
       (wm=rewrite-term term))))
  )

(defun wm=rewrite-term (term)
  (let ((symb (read-from-string term)))
    (if (or (numberp symb)
	    (env~lookup-object symb (th~env 'base)))
      (concatenate 'string term "-new")
    term)))

(defun wm=transform-constants (constants)
  (declare (edited  "15-OCT-1998 19:06")
	   (authors SORGE)
	   (input   "A signature list.")
	   (effect  "None.")
	   (value   "A list of strings representing constants in POST syntax."))
  (mapcar #'(lambda (const) (format nil "~A" const)) constants)
  )


(defun wm=divide-input-line-strings (line-strings)
  (declare (edited  "15-OCT-1998 15:52")
	   (authors SORGE Ameier)
	   (input   "A list of strings.")
	   (effect  "None.")
	   (value   "Multiple-value:"
		    "First: A list of strings, representing the axioms of the proof."
		    "Second: A list of strings, representing the theorems of the proof."
		    "Third: A list of list of strings, each list representing one lemma."
		    "Fourth: A list of strings, representing the end steps."))
  (labels ((divide-string (string char)
			  (remove ""
				  (atptop~divide-string string char)
				  :test #'string=))
	   (get-equation (string)
			 (let* ((sl (divide-string string #\SPACE))
				(name (car (last sl))))
			   (list
			    (if (atptop~string-is-prefix-of-string-p "prove_" name)
				(subseq name 6)
			      name)
			    (format nil "~A ~A ~A" (first sl) (second sl) (third sl)))))
	   (get-variables (string)
			  (let* ((sl (divide-string string #\SPACE))
				 (sort (car (last sl)))
				 (vars (divide-string (string-right-trim '(#\:) (car sl)) #\,)))
			    (mapcar #'(lambda (var)
					(list var sort))
				    vars)))
	   (get-signature (string)
			  (let* ((sl (divide-string string #\SPACE))
				 (const (string-right-trim '(#\:) (car sl)))
				 (domain (reverse (butlast (cdr sl) 2)))
				 (codomain (car (last sl))))
			    (list const
				  (if domain
				      (cons codomain domain)
				     codomain)))))
    (let* (name sorts signature variables equations conclusion)
      (do* ((rest-strings line-strings (rest rest-strings))
	    (head-string (car line-strings) (car rest-strings))
	    (mode 'start))
	  ((null rest-strings)
	   (values name sorts signature (apply #'append variables) equations conclusion))
	(cond ((atptop~string-is-prefix-of-string-p "% File" head-string)
	       (setq name (fourth (divide-string head-string #\SPACE))))
	      ((atptop~string-is-prefix-of-string-p "SORTS" head-string)
	       (setq sorts (cdr (divide-string head-string #\SPACE))))
	      ((atptop~string-is-prefix-of-string-p "CONCLUSION" head-string)
	       (setq conclusion (get-equation (subseq  head-string (length "CONCLUSION"))))
	       (setq mode 'end))
	      ((atptop~string-is-prefix-of-string-p "EQUATIONS" head-string)
	       (setq equations (list (get-equation (subseq head-string (length "EQUATIONS")))))
	       (setq mode 'equations))
	      ((string-equal mode 'equations)
	       (setq equations (append equations (list (get-equation head-string)))))
	      ((atptop~string-is-prefix-of-string-p "VARIABLES" head-string)
	       (setq variables (list (get-variables (subseq head-string (length "VARIABLES")))))
	       (setq mode 'variables))
	      ((string-equal mode 'variables)
	       (setq variables (append variables (list (get-variables head-string)))))
	      ((atptop~string-is-prefix-of-string-p "ORDERING" head-string)
	       (setq mode 'start))
	      ((atptop~string-is-prefix-of-string-p "SIGNATURE" head-string)
	       (setq signature (list (get-signature (subseq head-string (length "SIGNATURE")))))
	       (setq mode 'signature))
	      ((string-equal mode 'signature)
	       (setq signature (append signature (list (get-signature head-string)))))
	      )))))

(defun wm=compute-quantification (vars skolems)
  (declare (edited  "16-DEC-1998 10:42")
	   (authors SORGE)
	   (input   "A list of variables and skolem functions.")
	   (effect  "None.")
	   (value   "A string with the correct quantification for both existentially"
		    "and universally quantified variables."))
  (flet ((skolem-type (skolem)
		      (let ((type (third skolem)))
			(if (listp type) (car type) type)))
	 (skolem-dependence (skolem)
			    (when skolem (second skolem))))
    (labels ((devide-skolem-list (sk-list number)
				 (when (and sk-list (= (length (second (car sk-list))) number))
				   (cons (car sk-list) (devide-skolem-list (cdr sk-list) number))))
	     (get-vars (variables subset)
		       (when subset
			 (let ((var (find (car subset) variables :test #'(lambda (x y) (string-equal x (car y))))))
			   (if var
			       (cons var (get-vars variables (cdr subset)))
			     (get-vars vars (cdr subset)))))))
      (let* ((sorted-skolems (stable-sort skolems #'(lambda (x y) (< (length (cadr x)) (length (cadr y)))))))
	(multiple-value-bind (result rest-variables)
	    (do* ((count 0 (1+ count))
		  (pre-list (devide-skolem-list sorted-skolems count)
			    (devide-skolem-list rest-list count))
		  (rest-list (subseq sorted-skolems (length pre-list))
			     (subseq rest-list (length pre-list)))
		  (rest-vars vars
			     (set-difference rest-vars new-vars
					     :test #'(lambda (x y)
						       (and (not (null x))
							    (string-equal (car x) (car y))))))
		  (new-vars (get-vars rest-vars (skolem-dependence (car pre-list)))
			    (get-vars rest-vars (skolem-dependence (car pre-list))))
		  (res-str ""))
		((and (null pre-list) (null rest-list)) (values res-str rest-vars))
	      (dolist (variable new-vars)
		(setf res-str (format nil "~A (exists (lam ~A" res-str variable)))
	      (dolist (skolem pre-list)
		(setf res-str (format nil "~A (forall (lam (~A ~A)" res-str (car skolem) (skolem-type skolem)))))
	  (concatenate 'string result (format nil "~{ (exists (lam ~A~}" rest-variables)))))))

