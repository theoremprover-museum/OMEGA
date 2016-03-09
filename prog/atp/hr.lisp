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

(mod~defmod HR 
            :uses (atptop data env keim omega post sys term th)
            :documentation ""
            :exports (
		      hr~call-hr
                      hr~hr-table2table
                      hr~parse-formula
                      hr~parse-formula-new
                      hr~parse-output
                      hr~prepare-input-file
                      hr~program
                      hr~table2hr-table

		      hr*op
                      hr*out-string))

(defvar hr*op nil)

(defvar hr*out-string nil)

(defun hr~program ()
  (declare (edited  "09-DEC-2003")
	   (authors Vxs)
	   (input   "None.")
	   (effect  "None.")
	   (value   "The location of the HR program in the file system."))
  (sys~getenv 'hrhome))

(defun hr~call-hr (expr &optional batch)
  (declare (edited  "09-DEC-2003")
	   (authors Vxs)
	   (input   "A string corresponding to HR input and optionally a string containing batch commands.")
	   (effect  "Calls HR and waits for the results of its computations.")
	   (value   "A string corresponding to the result of the computation."))
  (let* ((tmp-dir (atptop~get-default-directory))
         (in-file (make-pathname :directory tmp-dir :name "hr.hrd"))
         (out-file (make-pathname :directory tmp-dir :name "hr.out"))
	 (batch-file (if batch "hr.hrb" "classify_algebra.hrb"))
	 (hrprog (hr~program))
	 (hrdir (directory-namestring  hrprog))
	 (hrname (file-namestring hrprog)))
    (omega~message "Writing HR input file.")
    (with-open-file (out in-file
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :supersede)
                    (format out "~A" expr))
    ;;; physically calling HR
    (when batch
      (omega~message "Writing HR batch file.")
      (with-open-file (out (make-pathname :directory hrdir :name batch-file)
			   :direction :output
			   :if-does-not-exist :create
			   :if-exists :supersede)
                    (format out "~A" batch)))
    (omega~message "Calling HR.")
    (sys~call-system (format nil "cp ~A ~A" (namestring in-file) hrdir))
    (sys~call-system (format nil "cd ~A; ~A batch:~A language:otter domain:~A > ~A"   ;;;; hier tptp gegen Otter austauschen, wenn Du willst
			     hrdir hrname batch-file (file-namestring in-file) (namestring out-file)))
    (omega~message "Cleaning up HR direcotry.")
    (sys~call-system (format nil "rm -f ~A~A" hrdir (file-namestring in-file)))
    (when batch (sys~call-system (format nil "rm -f ~A~A" hrdir batch-file)))
    (omega~message "Reading HR output file.")
    (setf hr*out-string (atptop~read-file-as-string out-file))
    hr*out-string
    ))


;:ld ~/omega/myomega/ameier-loader.lisp
;(setq inexpr
;      (atptop~read-file-as-string
;       (make-pathname :directory "/bham/ums/solaris/pd/packages/omega-new/atp/hr" :name "group6.hrd")))
;(hr~call-hr inexpr)


(defun hr~parse-output (expr &optional elements-alist)
  (declare (edited  "18-DEC-2003")
	   (authors Vxs)
	   (input   "A string containing HR output. Optionally an association list"
		    "assigning HR elements to real elements.")
	   (effect  "None.")
	   (value   "A list of pairs associating multiplication tables to post expressions representing the HR output."))
  (let* ((single-expressions (remove-if #'(lambda (x) (string-equal x ""))
					(atptop~divide-string expr #\newline)))
	 (structures&formulas (mapcar #'(lambda (expr)
					  (multiple-value-list 
					   (atptop~get-next-word expr #\:)))
				      single-expressions))
	 (elements (mapcar #'cdr elements-alist)))
    (mapcar #'(lambda (struct&form)
		(list (first struct&form)
		      (hr~parse-formula (second struct&form))))
	    structures&formulas)))
;;    (mapcar #'(lambda (struct&form)
;;                (list (rcl~table2operations-list
;;                       elements
;;                        (hr~hr-table2table  (first struct&form) elements-alist))
;;                      (hr~parse-formula (second struct&form))))
;;            structures&formulas)))


(defun hr=remove-leading-spaces (in-string)
  (if (equal (char in-string 0) #\space)
      (hr=remove-leading-spaces (atptop~cut-first-char in-string))
    in-string))

(defun hr=divide-unparanthesized (in-string break-char)
  (let* ((lengthi (length in-string)))
    (do* ((i 0 (incf i))
	  (open-p 0)
	  (current-string "")
	  (current-back-strings nil))
	((= i lengthi)
	 (append current-back-strings
		 (list current-string)))
      (let* ((current-char (char in-string i)))
	(setf current-string (format nil "~A~A" current-string current-char))
	(cond ((equal current-char break-char)
	       (when (= open-p 0)
		 (setf current-back-strings
		       (append current-back-strings
			       (list (atptop~cut-x-last-chars current-string 2))))
		 (setf current-string "")
		 (setf i (+ i 1))))
	      ((equal current-char #\()
	       (setf open-p (+ 1 open-p)))
	      ((equal current-char #\))
	       (setf open-p (- open-p 1)))
	      (t
	       nil))))))

(defun hr=in-term-p (obj)
  (if (stringp obj)
      (let* ((divs (atptop~divide-string obj #\space)))
	(if (string= (second divs) "in")
	    't
	  nil))
    nil))


(defun hr=string2plist (in-string)
  ;; 1. Check for 'ungeklammerte' &
  (let* ((&-divided (hr=divide-unparanthesized in-string #\&)))
    (cond ((> (length &-divided) 1)
	   (cons 'and (mapcar #'hr=string2plist &-divided)))
	  ;; 2. Check for '-'
	  ((equal (char in-string 0) #\-)
	   (list 'not (hr=string2plist (atptop~cut-x-first-chars (atptop~cut-last-char in-string) 2))))
	   ;;(list 'not (hr=string2plist (atptop~cut-x-first-chars in-string 1))))
	  ;; 3. Check for '(exists'
	  ((atptop~string-is-prefix-of-string-p "(exists" in-string)
	   (multiple-value-bind
	       (vars resti)
	       (atptop~get-next-word (atptop~cut-x-first-chars in-string 7) #\()
	     (list 'exists vars (hr=string2plist (atptop~cut-x-last-chars resti 1)))))
	  ((atptop~string-is-prefix-of-string-p "exists" in-string)
	   (multiple-value-bind
	       (vars resti)
	       (atptop~get-next-word (atptop~cut-x-first-chars in-string 6) #\()
	     (list 'exists vars (hr=string2plist (atptop~cut-x-last-chars resti 1)))))
	  (t in-string))))
	  
			       
(defun hr=plist2formula (pobj envi)
  (if (listp pobj)
      (let* ((function (first pobj))
	     (args (rest pobj)))
	(cond ((string-equal function 'exists)
	       (let* ((exists-obj (env~lookup-object 'exists envi))
		      (var-string (first args))
		      (var-strings (remove-if #'(lambda (stringi)
						  (string= stringi ""))
					      (atptop~divide-string var-string #\space)))
		      (var-symbols (mapcar #'make-symbol var-strings))
		      (pre-scope (second args))
		      (vars (mapcar #'(lambda (var-sym)
					(post~read-object `(,var-sym i) envi :variable-multiple))
				    var-symbols))
		      (inter-scope (hr=plist2formula pre-scope envi)))

		 (mapcar #'(lambda (var-sym)
			     (env~remove var-sym envi))
			 var-symbols)
		 
		 (do* ((rest-vars (reverse vars) (rest rest-vars))
		       (back-formula inter-scope))
		     ((null rest-vars)
		      back-formula)
		   (setf back-formula (term~appl-create exists-obj
							(list (term~abstr-create (list (first rest-vars))
										 back-formula)))))))
	      ((string-equal function 'not)
	       (term~appl-create (env~lookup-object 'not envi)
				 (list (hr=plist2formula (first args) envi))))
	      ((string-equal function 'and)
	       (let* ((and-obj (env~lookup-object 'and envi))
		      (rest-args (remove-if #'hr=in-term-p args))
		      (inter-rest-args (mapcar #'(lambda (argi)
						   (hr=plist2formula argi envi))
					       rest-args)))
		 (if (= (length inter-rest-args) 1)
		     (first inter-rest-args)
		   (do* ((rest-conj (reverse inter-rest-args) (rest rest-conj))
			 (back-formula nil))
		       ((null rest-conj)
			back-formula)
		     (let* ((head-conj (first rest-conj)))
		       (if (null back-formula)
			   (setf back-formula head-conj)
			 (setf back-formula (term~appl-create and-obj (list head-conj back-formula)))))))))
	      (t
	       (omega~error "NOT EXPECTED ..."))))
    
    ;; wenn wir hier sind sollte das ganze Ding folgende FOrm haben: a*b=c or (a*b=c)
    (let* ((=divs (atptop~divide-string
		   (string-right-trim '(#\)) (string-left-trim '(#\() pobj)) #\=))
		   ;;pobj #\=))
	   (*divs (atptop~divide-string (first =divs) #\*)))
      ;;(Print =divs) (print *divs) (return-from hr=plist2formula)
      (term~appl-create (env~lookup-object '= envi)
			(list (if (= (length *divs) 2)
				  (term~appl-create hr*op (list (hr=interpret-term (first *divs) envi)
								  (hr=interpret-term (second *divs) envi)))
				(hr=interpret-term (first *divs) envi))
			      (hr=interpret-term (second =divs) envi))))))

(defun hr=interpret-term (term-stringi envi)
  (cond ((string= term-stringi "id")
	 prop*unit)
	((atptop~string-is-prefix-of-string-p "inv(" term-stringi)
	 (term~appl-create prop*inv
			   (list (hr=interpret-term (atptop~cut-x-first-chars (atptop~cut-last-char term-stringi) 4)
						    envi))))
	(t
	 (let* ((env-elem (env~lookup-object (make-symbol term-stringi) envi)))
	   (if (null env-elem)
	       (omega~error "Failure in hr=interpret-term: ~A" term-stringi)
	     env-elem)))))


      
(defun hr~parse-formula (in-string)
  (declare (edited  "19-DEC-2003")
	   (authors Sorge AMeier)
	   (input   "A string representing a formula in TPTP (or Otter???) syntax.")
	   (effect  "None.")
	   (value   "A OMEGA Formula."))
  ;; we assume that OTTER output was choosen for HR!
  
  (let* ((cleared-in-string (hr=remove-leading-spaces (atptop~cut-last-char in-string)))
	 (parse-list (hr=string2plist cleared-in-string))
	 (formula (hr=plist2formula parse-list (env~create (th~env 'base))))
	 (optimized-formula (hr=optimize-formula formula)))
    optimized-formula))

(defun hr=optimize-formula (formula)
  ;; otpimization does two things:
  ;; 1. Look for the following pattern exists x. (y*z=x AND P[x])
  ;;    Replace this pattern by: P[y*z]
  ;; 2. Push all negation to the inside!

  (let* ((opti1 (hr=get-rid-of-exists formula))
	 (opti2 (hr=pushneg opti1 t)))
    opti2))

(defgeneric hr=pushneg (formula pol)
  (:method ((formula term+appl) pol)
	   (let* ((function (data~appl-function formula))
		  (args (data~appl-arguments formula)))
	     (cond ((keim~equal function (data~schema-range (env~lookup-object 'exists (th~env 'base))))
		    (if pol
			(term~appl-create function
					  (list (hr=pushneg (first args) pol)))
		      (term~appl-create (env~lookup-object 'forall (th~env 'base))
					(list (hr=pushneg (first args) pol)))))
		   ((keim~equal function (data~schema-range (env~lookup-object 'forall (th~env 'base))))
		    (if pol
			(term~appl-create function
					  (list (hr=pushneg (first args) pol)))
		      (term~appl-create (env~lookup-object 'exists (th~env 'base))
					(list (hr=pushneg (first args) pol)))))
		   ((keim~equal function (env~lookup-object 'not (th~env 'base)))
		    (hr=pushneg (first args) (not pol)))
		   ((keim~equal function (env~lookup-object 'and (th~env 'base)))
		    (if pol
			(term~appl-create function
					  (list (hr=pushneg (first args) pol)
						(hr=pushneg (second args) pol)))
		      (term~appl-create (env~lookup-object 'or (th~env 'base))
					(list (hr=pushneg (first args) pol)
					      (hr=pushneg (second args) pol)))))
		   (t
		    (if pol
			formula
		      (term~appl-create (env~lookup-object 'not (th~env 'base))
					(list formula)))))))
  (:method ((formula term+abstr) pol)
	   (let* ((vars (data~abstr-domain formula))
		  (scope (data~abstr-range formula)))
	     (term~abstr-create vars (hr=pushneg scope pol)))))


(defgeneric hr=get-rid-of-exists (formula)
  (:method ((formula term+appl))
	   (let* ((function (data~appl-function formula))
		  (args (data~appl-arguments formula)))
	     (cond ((keim~equal function (data~schema-range (env~lookup-object 'exists (th~env 'base))))
		    (let* ((scope (data~abstr-range (first args)))
			   (pattern-check (hr=check-for-and-constellation scope (first (data~abstr-domain (first args))))))
		      (if pattern-check
			  (hr=get-rid-of-exists pattern-check)
			(term~appl-create function
					  (list (hr=get-rid-of-exists (first args)))))))
		   ((keim~equal function (env~lookup-object 'not (th~env 'base)))
		    (term~appl-create function
				      (list (hr=get-rid-of-exists (first args)))))
		   ((keim~equal function (env~lookup-object 'and (th~env 'base)))
		     (term~appl-create function
				      (list (hr=get-rid-of-exists (first args))
					    (hr=get-rid-of-exists (second args)))))
		   (t
		    formula))))
  (:method ((formula term+abstr))
	   (let* ((vars (data~abstr-domain formula))
		  (scope (data~abstr-range formula)))
	     (term~abstr-create vars (hr=get-rid-of-exists scope)))))

		   
(defun hr=check-for-and-constellation (scope var)
  (if (data~appl-p scope)
      (let* ((scope-function (data~appl-function scope))
	     (scope-args (data~appl-arguments scope)))
	(if (keim~equal scope-function (env~lookup-object 'and (th~env 'base)))
	    (let* ((arg1 (first scope-args))
		   (arg2 (second scope-args)))
	      (if (and (data~appl-p arg1)
		       (keim~equal (data~appl-function arg1) (data~schema-range (env~lookup-object '= (th~env 'base)))))
		  (let* ((larg (first (data~appl-arguments arg1)))
			 (rarg (second (data~appl-arguments arg1))))
		    (if (and (keim~equal rarg var)
			     (null (data~substruct-positions rarg larg)))
			(data~replace-struct arg2 rarg larg)
		      nil))
		nil))
	  nil))
    nil))
			 
				       
	      
#|     
AMEIER: Ich glaube da ist ein Fehler drin!

(defun hr~table2hr-table (table)
  (declare (edited  "10-DEC-2003")
	   (authors Vxs)
	   (input   "A list of lists representing a multiplication table.")
	   (effect  "None.")
	   (value   "A string representing the table as input for HR and"
		    "an association list pairing input elements for HR to the original constants."))
  ;;; only <= 10 elements are currently allowed....
  (let ((elements (remove-duplicates (apply #'append table) :test #'keim~equal)))
    (if (> (length elements) 10)
	(omega~error "HR translation currently only works with structures of size at most ten.")
      (let* ((number-list (do* ((i 0 (1+ i))
				(list (cons i nil) (cons i list)))
			      ((>= i (1- (length elements))) (reverse list))))
	     (assoc-list (reverse (pairlis number-list elements))))
	(values
	 (apply #'concatenate
		(cons 'string
		      (mapcar #'(lambda (row)
				  (format nil "~{~A~}" 
					  (mapcar #'(lambda (elem) (car (rassoc elem assoc-list
										:test #'keim~equal))) row)))
			      table)))
	 assoc-list)))))
|#

(defun hr=const2number-string (const)
  (string (char (format nil "~A" (keim~name const)) 1)))
				   

(defun hr~table2hr-table (table)
  (declare (edited  "10-DEC-2003")
	   (authors Ameier Vxs)
	   (input   "A list of lists representing a multiplication table.")
	   (effect  "None.")
	   (value   "A string representing the table as input for HR and"
		    "an association list pairing input elements for HR to the original constants."))
  ;;; only <= 10 elements are currently allowed....
  (let ((elements (remove-duplicates (apply #'append table) :test #'keim~equal)))
    (if (> (length elements) 10)
	(omega~error "HR translation currently only works with structures of size at most ten.")

      (let* ((number-strings-list (mapcar #'hr=const2number-string (apply #'append table)))
	     (hr-table-string (do* ((rest-number-strings number-strings-list (rest rest-number-strings))
				    (back-string ""))
				  ((null rest-number-strings)
				   back-string)
				(setf back-string (format nil "~A~A" back-string (first rest-number-strings)))))
	     (assoc-list (mapcar #'(lambda (elem)
				     (let* ((number-string (hr=const2number-string elem))
					    (number (atptop~parse-number number-string)))
				       (cons number elem)))
				 elements)))
	(values hr-table-string
		assoc-list)))))


(defun hr~hr-table2table (table &optional elements-alist)
  (declare (edited  "10-DEC-2003")
	   (authors Vxs)
	   (input   "A string representing a table in HR format. Optionally an association list"
		    "assigning HR elements to real elements.")
	   (effect  "None.")
	   (value   "A list of lists representing a multiplication table."))
  ;;; only <= 10 elements are currently allowed....
  (let ((elem-number (isqrt (length table))))
    (if (= elem-number (sqrt (length table)))
	(flet ((chop-string (str length)
			    (do* ((str str (subseq str length))
				  (list (list (subseq str 0 length))
					(cons (subseq str 0 length) list)))
				((<= (length str) length) (reverse list)))))
	(if elements-alist
	    (mapcar #'(lambda (row)
			(mapcar #'(lambda (elem)
				    (cdr (assoc elem elements-alist :test #'equal)))
				(mapcar #'read-from-string (chop-string row 1))))
		    (chop-string table elem-number))
	    (mapcar #'(lambda (row) (mapcar #'read-from-string (chop-string row 1)))
		    (chop-string table elem-number))))
      (omega~error "The table is not of the right size. It should be square!."))))



;; (defun hr~interface (models &key (type :group))
;;   (declare (edited  "19-DEC-2003")
;; 	   (authors Vxs)
;; 	   (input   "A list of model+model objects and a keyword specifying the type of the objects."
;; 		    "This type thingy should be replaced one day by some more elaborate"
;; 		    "expressions of properties to give to HR.")
;; 	   (effect  "Calls HR and returns its results.")
;; 	   (value   "A list associating each of the models with a unique property."))
;;   (let* ((structures (mapcar #'(lambda (model)
;; 				 (multiple-value-list
;; 				  (hr~table2hr-table
;; 				   (rcl~operations-list2table (model~set-of-elements model) (model~mappings model)))))
;; 			     models))   ;;; not sure whether we ever need the translation table....
;; 	 (file-input (hr~prepare-input-file (mapcar #'car structures) type))
;; 	 (hr-output (hr~parse-output (hr~call-hr file-input)))
;; 	 )
;;     ;;; Andreas: folgendes ist ungetestet geaendert:
;;     (mapcar #'(lambda (orig hr-struct)
;; 		(list
;; 		 orig
;; 		 (cadr (assoc (car hr-struct) hr-output :test #'string-equal))))
;; 	    models
;; 	    structures)
;;     ))


(defun hr~prepare-input-file (structures type)
  (with-output-to-string (str)
    (cond ((string-equal type :group)
	   (format str "algebra001~%")
	   (format str "group(G)~%")
	   (format str "otter:\"\"~%")
           (format str "ascii:@G@ is a group of size ~A~%" (isqrt (length (car structures))))
	   (format str "~{group(~A).~%~}~%" structures)

	   (format str "~%algebra002~%element(G,E)~%ascii:@E@ in @G@~%otter:\"\"~%tptp:\"\"~%")
	   (format str "element(G,E) -> group(G)")   ;;; these rulse should be done more genericly
	   (format str "code~%")

	   (format str "~%algebra003~%*(G,A,B,C)~%ascii:@A@*@B@=@C@~%otter:@A@*@B@=@C@~%tptp:equal(multiply(@A@,@B@),@C@)")
	   (format str "*(G,A,B,C) -> group(G)~%")
	   (format str "*(G,A,B,C) -> element(G,A)~%")
	   (format str "*(G,A,B,C) -> element(G,B)~%")
	   (format str "*(G,A,B,C) -> element(G,C)~%")
	   (format str "function: 0,1,2=3~%")
	   (format str "code~%")

	   (format str "~%algebra004~%id(G,I)~%ascii:@I@=id~%otter:@I@=id~%tptp:equal(@I@,identity)~%")
	   (format str "id(G,I) -> group(G)~%")
	   (format str "id(G,I) -> element(G,I)~%")
	   (format str "function: 0=1~%")
	   (format str "code~%")

	   (format str "~%algebra005~%inv(G,A,B)~%ascii:inv(@A@)=@B@~%otter:inv(@A@)=@B@~%tptp:equal(inverse(@A@),@B@)~%")
	   (format str "inv(G,A,B) -> group(G)~%")
	   (format str "inv(G,A,B) -> element(G,A)~%")
	   (format str "inv(G,A,B) -> element(G,B)~%")
	   (format str "function: 0,1=2~%")
	   (format str "code~%")

;;	   (format str "~%algebra006~%")
;;	   (format str "commutator(G,A,B,C)~%")
;;	   (format str "ascii:commutator(@A@,@B@)=@C@~%")
;;	   (format str "otter:commutator(@A@,@B@)=@C@~%")
;;	   (format str "tptp:equal(commutator(@A@,@B@),@C@)~%")
;;	   (format str "commutator(G,A,B,C) -> group(G)~%")
;;	   (format str "commutator(G,A,B,C) -> element(G,A)~%")
;;	   (format str "commutator(G,A,B,C) -> element(G,B)~%")
;;	   (format str "commutator(G,A,B,C) -> element(G,C)~%")
;;	   (format str "function: 0,1,2=3~%")
;;	   (format str "code~%")
	   )
	  ((string-equal type :loop)
	   (format str "~%algebra001~%")
	   (format str "loop(L)~%")
	   (format str "otter:\"\"~%")
           (format str "ascii:@L@ is a loop of size ~A~%" (isqrt (length (car structures))))
	   (format str "~{loop(~A).~%~}~%" structures)

	   (format str "~%algebra002~%")
	   (format str "element(L,E)~%")
	   (format str "ascii:@E@ in @L@~%")
	   (format str "otter:""~%")
	   (format str "tptp:""~%")
	   (format str "element(L,E) -> loop(L)~%")
	   (format str "code~%")

	   (format str "~%algebra003~%")
	   (format str "*(L,A,B,C)~%")
	   (format str "ascii:@A@*@B@=@C@~%")
	   (format str "otter:@A@*@B@=@C@~%")
	   (format str "tptp:equal(multiply(@A@,@B@),@C@)~%")
	   (format str "*(L,A,B,C) -> loop(L)~%")
	   (format str "*(L,A,B,C) -> element(L,A)~%")
	   (format str "*(L,A,B,C) -> element(L,B)~%")
	   (format str "*(L,A,B,C) -> element(L,C)~%")
	   (format str "function: 0,1,2=3~%")
	   (format str "code~%")

	   (format str "~%algebra004~%")
	   (format str "id(L,I)~%")
	   (format str "otter:@I@=id~%")
	   (format str "tptp:equal(@I@,identity)~%")
	   (format str "id(L,I) -> loop(L)~%")
	   (format str "id(L,I) -> element(L,I)~%")
	   (format str "function: 0=1~%")
	   (format str "code~%")
	   )

	  ((string-equal type :quasigroup)
	   (format str "~%algebra001~%")
	   (format str "quasigroup(Q)~%")
	   (format str "otter:\"\"~%")
           (format str "ascii:@Q@ is a loop of size ~A~%" (isqrt (length (car structures))))
	   (format str "~{quasigroup(~A).~%~}~%" structures)

	   (format str "~%algebra002~%")
	   (format str "element(Q,E)~%")
	   (format str "ascii:@E@ in @Q@~%")
	   (format str "otter:""~%")
	   (format str "tptp:""~%")
	   (format str "element(Q,E) -> quasigroup(Q)~%")
	   (format str "code~%")

	   (format str "~%algebra003~%")
	   (format str "*(Q,A,B,C)~%")
	   (format str "ascii:@A@*@B@=@C@~%")
	   (format str "otter:@A@*@B@=@C@~%")
	   (format str "tptp:equal(multiply(@A@,@B@),@C@)~%")
	   (format str "*(Q,A,B,C) -> quasigroup(Q)~%")
	   (format str "*(Q,A,B,C) -> element(Q,A)~%")
	   (format str "*(Q,A,B,C) -> element(Q,B)~%")
	   (format str "*(Q,A,B,C) -> element(Q,C)~%")
	   (format str "function: 0,1,2=3~%")
	   (format str "code~%")
	   )
	  (T (omega~error "Unknown structure!")))
	  str
	 ))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A new attempt at parsing Otter formulas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hr~parse-formula-new (in-string)
  (declare (edited  "12-FEB-2004" )
	   (authors Sorge)
	   (input   "A string representing a formula in Otter syntax.")
	   (effect  "None.")
	   (value   "A OMEGA Formula."))
  (let* ((clean-string (string-trim '(#\Space #\Tab #\Newline) in-string))
	 (pre-formula (multiple-value-bind (form1 length1)
			  (read-from-string clean-string)
		       (if (= (length clean-string) length1)
			   form1
			 (read-from-string (concatenate 'string "(" clean-string ")")))))
	 (post-formula (hr=parse-formula pre-formula)))
    post-formula))

(defun hr=parse-formula (formula)
  (declare (edited  "12-FEB-2004")
	   (authors Sorge)
	   (input   "A formula in Otter list format.")
	   (effect  "None.")
	   (value   "A formula in Post syntax."))
  (if (listp formula)
      formula
    nil))


