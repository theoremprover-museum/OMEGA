;;; -*- syntax: common-lisp; package: OMEGA; base: 10; mode: KEIM -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     OMEGA Project                                                        ;;
;;     AG Siekmann/FB Informatik                                            ;;
;;     Universitaet des Saarlandes                                          ;;
;;     Bau 36, 4. Stock                                                     ;;
;;     D-66041 Saarbruecken                                                 ;;
;;     Germany                                                              ;;
;;   electronic mail: keim@cs.uni-sb.de                                     ;;
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

(mod~defmod CASEX
            :uses (maple)
            :documentation "Some helpful functions for maple calls."
            :exports (casex~find-substitution
		      casex~extract
		      casex~compute-with-maple		      

		      casex~initialize-translation
		      casex~sub-maple-terms
		      casex~pre-maple
		      ))
(in-package :omega)

(defun casex~find-substitution (antec concl)
  (let* ((sub1 (data~all-substructs antec))
	 (vars1 (remove-if-not #'term~variable-p sub1))
	 (subst (subst~create nil nil)))
    (dolist (var vars1)
      (let* ((pos (first (data~substruct-positions var antec)))
	     (appl (data~struct-at-position antec (pos~butlast pos))))
	(when (and (data~appl-p appl)
		   (= (length (data~appl-arguments appl)) 1)) ;; var occurs as the argument of a unary function f
	  (let* ((func (data~appl-function appl))
		 (appl-poss2 (data~substruct-positions func concl))) ;; positions of the function symbol f in concl
	    (when appl-poss2
	      (let* ((appls2      ;; all applications of the function f in concl
		      (mapcar #'(lambda (pos)
				  (data~struct-at-position concl (pos~butlast pos)))
			      appl-poss2))
		     (args
		      (mapcar #'(lambda (appl)
				  (first (data~appl-arguments appl)))
			      appls2))
		     (sorted-args
		      (sort args #'(lambda (arg1 arg2)
				     (if (and (typep arg1 'keim+name)
					      (typep arg2 'keim+name))
					 (string-lessp (symbol-name (keim~name arg1))
						       (symbol-name (keim~name arg2)))
				       T))
			    ))
		     (arg (first (last sorted-args)))
		     )
		(unless (find var (subst~codomain subst))
		  (subst~insert-component! var arg subst))))))))
    (omega~message "~& subst : ~S" subst)
    subst))


(defun casex~extract (antec concl subst)  ;; subst needed for determining the maple variable
  (when (and (or (notany #'term~abstr-p (data~all-substructs antec))
		 (omega~trace "Term ~A contains an abstraction and is not suitable for CASextract" antec))
	     (or (notany #'term~abstr-p (data~all-substructs concl))
		 (omega~trace "Term ~A contains an abstraction and is not suitable for CASextract" concl)))
    (setf bla antec)
;    (omega~trace "~% type of antecedent: ~S" (term~type antec))
    (if (type~num-p (term~type antec))
	(progn (casex~initialize-translation)
	       (let* ((env (pds~environment omega*current-proof-plan))
		      (a-prime (casex~pre-maple antec))
		      (a (casex~sub-maple-terms a-prime))
		      (b (casex~sub-maple-terms (casex~pre-maple concl)))
		      (var (casex=term2maple (casex=get-maple-variable a-prime subst)))
		      (k (casex~compute-with-maple "quo" b a var)))
		 (if k 
		     (let ((l (casex~compute-with-maple "rem" b a var)))
		       (if l 
			   (progn (omega~message "~% EXTRACT : ~A " concl)
				  (omega~message "~%         = ~A * ~A + ~A" k antec l)
				  (values (post~read-object k env :existing-term)
					  (post~read-object l env :existing-term)))
			 (progn (omega~message "~% EXTRACT : Cannot compute the remainder l!")
				(values nil nil))))
		   (progn (omega~message "~% EXTRACT : Cannot compute the factor k!")
			  (values nil nil)))))
      (values nil nil))))

(defun casex~pre-maple (term)
  (read-from-string
   (with-output-to-string (str)
			  (post~print term str)
			  str)))

(defun casex=subterm-list (term)
  (cond ((atom term) (list term))
	((member (car term) '(minus plus div power times))
	 (append (casex=subterm-list (second term))
		 (casex=subterm-list (third term))))
	(t (list term))))

(defun casex=get-maple-variable (term subst)
  (if (subst~empty-p subst)
      (car (casex=subterm-list term))
    (labels ((find-var-in-term (term var)
			       (if (atom term)
				   (string-equal (format nil "~A" term)
						 (format nil "~A" var))
				 (some #'(lambda (x) (find-var-in-term x var)) term)))
	     (find-term-in-list (termlist var)
				(cond ((and termlist (find-var-in-term (car termlist) var))
				       (car termlist))
				      (termlist (find-term-in-list (cdr termlist) var)))))
      (let ((term-list (casex=subterm-list term)))
	(find-term-in-list term-list (casex~pre-maple (first (subst~codomain subst))))))))

(defun casex~compute-with-maple (function &rest arguments)
  (let ((result (maple~call-maple (cons function arguments) :syntax :post2post)))
    (if (string-equal result "Error")
	nil
      (casex=transform-power
       (read-from-string result)))))

(defun casex=transform-power (term)
  (declare (edited  "03-DEC-1998")
	   (authors Sorge Pollet)
	   (input   "A term as list.")
	   (effect  "None.")
	   (value   "The same term, where all power functions are replaced by iterated multiplication."))
  (labels ((power2multip (base exp)
			 (unless (numberp exp) (omega~error "Cannot transform a power with variable exponent ~A!" exp))
			 (if (= exp 1)
			     base
			   (list 'times base (power2multip base (1- exp))))))
    (cond ((numberp term) term)
	  ((atom term) (casex=maple2term term))
	  ((equal (car term) 'power)
	   (casex=transform-power (power2multip (second term) (third term))))
	  (t (cons (casex=transform-power (car term))
		   (casex=transform-power (cdr term)))))))
  
(let ((trans-table-l2m (make-hash-table :test #'equal))
      (trans-table-p2l (make-hash-table :test #'equal)))
  
  (defun casex~initialize-translation ()
    (setf trans-table-p2l (make-hash-table :test #'equal))
    (setf trans-table-l2m (make-hash-table :test #'equal)))
  
  (defun casex=term2maple (term)
    (let* ((maple-term (casex=make-maple-term term))
           (maple-symbol (read-from-string maple-term)))
      (unless (gethash maple-symbol trans-table-l2m)
        (setf (gethash maple-symbol trans-table-l2m) term))
      maple-term))

  (defun casex=maple2term (maple-term)
    (let ((value (gethash (etypecase maple-term
			    (symbol maple-term)
			    (string (read-from-string maple-term)))
			  trans-table-l2m)))
      (if value value maple-term)))
  
  (defun casex=insert-post-term (key term)
    (setf (gethash key trans-table-p2l) term))

  (defun casex=get-post-term (lq-term)
    (gethash lq-term trans-table-p2l))
  
  )

(defgeneric casex=make-maple-term (term)
  (:method ((term symbol))
	   (casex~substitute-in-string #\_ #\- (symbol-name term)))
  (:method ((term cons))
	   (let ((maple-terms (mapcar #'casex=make-maple-term term)))
	     (format nil "~A~{_~A~}" (car maple-terms) (cdr maple-terms))))
  (:method ((num fixnum))
	   (format nil "~A" num))
  )

(defun casex~substitute-in-string (new old string)
  (declare (edited  "02-DEC-1998")
	   (authors Sorge)
	   (input   "Two characters and a string.")
	   (effect  "None.")
	   (value   "The string where OLD is replaced by NEW."))
  (do* ((old-string string (subseq old-string 1))
	(fchar (aref old-string 0) (aref old-string 0))
	(new-string  (if (string-equal fchar old)
			 (format nil "~A" new)
		       (format nil "~A" fchar))
		     (if (string-equal fchar old)
			 (concatenate 'string new-string (format nil "~A" new))
		       (concatenate 'string new-string (format nil "~A" fchar)))))
      ((<= (length old-string) 1) new-string)))


(defun casex~sub-maple-terms (term)
  (declare (edited  "02-DEC-1998")
	   (authors Sorge)
	   (input   "A term in the old, weird LINEQ syntax.")
	   (effect  "None.")
	   (value   "A term in MAPLE syntax."))
  (when term
    (if (consp term)
	(let ((head (car term)))
	  (if (find head '(minus plus div power times absval))
	      (concatenate 'string
			   "(" (string-downcase head) " "
			   (casex~sub-maple-terms (second term)) " "
			   (casex~sub-maple-terms (third term)) ")")
	    (casex=term2maple term)))
      (if (numberp term)
	  (format nil "~A" term)
	(casex=term2maple term)))))

