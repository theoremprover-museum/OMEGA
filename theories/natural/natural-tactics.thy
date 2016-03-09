;;; -*- syntax: common-lisp; package: keim; base: 10; mode: keim -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     OMEGA Project                                                        ;;
;;     AG Siekmann/FB Informatik                                            ;;
;;     Universitaet des Saarlandes                                          ;;
;;     Postfach 1150                                                        ;;
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


;;(mod~defmod omega+num :uses (mod sys inter com asi arg omega symnum)
;;            :documentation "Definition of OMEGA top level."
;;            :exports ())

(eval-when (compile)
	   (error "This file should not be compiled."))


(eval-when (load  eval)
(com~defcategory natural
  (help "Number manipulations"))

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simplify Number
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Simplify-Num

(infer~deftactic simplify-num
		 (outline-mappings (((existent existent) simplify-num-a)
				    ((existent nonexistent) simplify-num-b)
				    ((nonexistent existent) simplify-num-f)))
		 (expansion-function natac~expand-simplify-num)
		 (parameter-types position term)
		 (help "Simplify an arithmetic expression by computation."))
		 
(tac~deftactic simplify-num-b simplify-num (in natural)
   (parameters (POS pos+position "A position")
	       (TERM term+term "A term"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (natac~expand-number (formula L2) POS TERM)))
   (sideconditions (natac=formula-not-equal-with-term (formula L2) POS TERM)
		   (natac=formula-equal-with-reduced-term (formula L2) POS TERM))
   (description "Backward application of numerical simplification."))

(tac~deftactic simplify-num-f simplify-num (in natural)
   (parameters (POS pos+position "A position")
	       (TERM term+term "A term"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (natac~simplify-number (formula L1) POS)))
   (sideconditions  (natac=formula-not-equal (formula L1) POS)
		    (natac=formula-equal-with-term (formula L1) POS TERM))
   (description "Forward application of numerical simplification."))

(tac~deftactic simplify-num-a simplify-num (in natural)
   (parameters (POS pos+position "A position")
	       (TERM term+term "A term"))
   (premises L1)
   (conclusions L2)
   (sideconditions (natac=formula-equal (formula L1) (formula L2) POS)
		   (natac=formula-equal-with-term (formula L1) POS TERM))
   (description "Application of numerical simplification."))

(com~defcommand simplify-num       ;;; works   VS
  (argnames simpl-line exp-line position)
  (argtypes ndline ndline position)
  (arghelps "A line with simplified numerical function" "A line with a numerical function" "The position of the numerical function")
  (function natac=simplify-num)
  (frag-cats tactics natural)
  (defaults )
  (log-p t)
  (help "Simplification of a rational number."))

(defun natac=simplify-num (line1 line2 pos)
  (declare (edited  "12-OCT-2000")
	   (authors Sorge)
	   (input   "- A line with simplified numerical function"
		    "- A line with a numerical function"
		    "- The position of the numerical function")
	   (effect  "Applys a tactic.")
	   (value   "Undefined."))
  (if line2
      (let ((term (data~struct-at-position (node~formula line2) pos)))
	(cond ((and line1 line2 (pdsn~open-node-p line1))
	       (infer~compute-outline 'simplify-num (list line1 line2) (list pos term)))
	      ((and line1 line2 (pdsn~open-node-p line2))
	       (infer~compute-outline 'expand-num (list line2 line1) (list pos term)))
	      ((and line2 (pdsn~open-node-p line2))
	       (infer~compute-outline 'expand-num (list line2 line1) (list pos term)))
	      (line2
	       (infer~compute-outline 'simplify-num (list line1 line2) (list pos term)))))
    (omega~error "Application of Simplify-Num is not possible without a formula including a numerical function.")))

;;; Simplify-Num*

(infer~deftactic simplify-num*
		 (outline-mappings (((existent existent) simplify-num*-a)
				    ((existent nonexistent) simplify-num*-b)
				    ((nonexistent existent) simplify-num*-f)))
		 (expansion-function natac=expand-simplify-num*)
		 (parameter-types position-list term-list)
		 (help "Simplify an arithmetic expression by computation."))
		 
(tac~deftactic simplify-num*-b simplify-num* (in natural)
   (parameters (POS cons "A list of positions")
	       (TERM cons "A list of terms"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (natac~expand-number* (formula L2) POS TERM)))
   (sideconditions (natac=formula-not-equal-with-term* (formula L2) POS TERM)
		   (natac=formula-equal-with-reduced-term* (formula L2) POS TERM))
   (description "Backward application of numerical simplification in several positions."))

(tac~deftactic simplify-num*-f simplify-num* (in natural)
   (parameters (POS cons "A list of positions")
	       (TERM cons "A list of terms"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (natac~simplify-number* (formula L1) POS)))
   (sideconditions (natac=formula-not-equal* (formula L1) POS)
		   (natac=formula-equal-with-term* (formula L1) POS TERM))
   (description "Forward application of numerical simplification in several positions."))

(tac~deftactic simplify-num*-a simplify-num* (in natural)
   (parameters (POS cons "A list of positions")
	       (TERM cons "A list of terms"))
   (premises L1)
   (conclusions L2)
   (sideconditions (natac=formula-equal* (formula L1) (formula L2) POS)
		   (natac=formula-equal-with-term* (formula L1) POS TERM))
   (description "Application of numerical simplification in several positions."))

(com~defcommand simplify-num*       ;;; works   VS
  (argnames simpl-line exp-line position)
  (argtypes ndline ndline position-list)
  (arghelps "A line with simplified numerical functions" "A line with numerical functions" "The position of these numerical functions")
  (function natac=simplify-num*)
  (frag-cats tactics natural)
  (defaults )
  (log-p t)
  (help "Simplification of a rational number."))

(defun natac=simplify-num* (line1 line2 pos-list)
  (declare (edited  "12-OCT-2000")
	   (authors Sorge)
	   (input   "- A line with several simplified numerical functions"
		    "- A line with several numerical functions"
		    "- The positions of these numerical function")
	   (effect  "Applys a tactic.")
	   (value   "Undefined."))
  (if line2
      (let ((term-list (mapcar #'(lambda (pos) (data~struct-at-position (node~formula line2) pos)) pos-list)))
	(cond ((and line1 line2 (pdsn~open-node-p line1))
	       (infer~compute-outline 'simplify-num* (list line1 line2) (list pos-list term-list)))
	      ((and line1 line2 (pdsn~open-node-p line2))
	       (infer~compute-outline 'expand-num* (list line2 line1) (list pos-list term-list)))
	      ((and line2 (pdsn~open-node-p line2))
	       (infer~compute-outline 'expand-num* (list line2 line1) (list pos-list term-list)))
	      (line2
	       (infer~compute-outline 'simplify-num* (list line1 line2) (list pos-list term-list)))))
    (omega~error "Application of Simplify-Num* is not possible without a formula including numerical functions.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expand-Number
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  Expand-Num

(infer~deftactic expand-num
		 (outline-mappings (((existent existent) expand-num-a)
				    ((existent nonexistent) expand-num-b)
				    ((nonexistent existent) expand-num-f)))
		 (expansion-function natac~expand-expand-num)
		 (parameter-types position term)
		 (help "Simplify an arithmetic expression by computation."))
		 
(tac~deftactic expand-num-b expand-num (in natural)
   (parameters (POS pos+position "A position")
	       (TERM term+term "A term"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (natac~simplify-number (formula L2) POS)))
   (sideconditions (natac=formula-not-equal (formula L2) POS)
		   (natac=formula-equal-with-term (formula L2) POS TERM))
   (description "Backward application of numerical simplification."))

(tac~deftactic expand-num-f expand-num (in natural)
   (parameters (POS pos+position "A position")
	       (TERM term+term "A term"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (natac~expand-number (formula L1) POS TERM)))
   (sideconditions (natac=formula-not-equal-with-term (formula L1) POS TERM)
		   (natac=formula-equal-with-reduced-term (formula L1) POS TERM))
   (description "Forward application of numerical simplification."))

(tac~deftactic expand-num-a expand-num (in natural)
   (parameters (POS pos+position "A position")
	       (TERM term+term "A term"))
   (premises L1)
   (conclusions L2)
   (sideconditions (natac=formula-equal (formula L2) (formula L1) POS)
		   (natac=formula-equal-with-term (formula L2) POS TERM))
   (description "Application of numerical simplification."))

(com~defcommand expand-num       ;;; works   VS
  (argnames exp-line simple-line position term)
  (argtypes ndline ndline position term)
  (arghelps "A line with expanded numerical function" "A line with a simplified numerical function" "The position of the numerical function" "A numerical function term")
  (function natac=expand-num)
  (frag-cats tactics natural)
  (defaults )
  (log-p t)
  (help "Simplification of a rational number."))

(defun natac=expand-num (line1 line2 pos term)
  (declare (edited  "12-OCT-2000")
	   (authors Sorge)
	   (input   "- A line with expanded numerical function"
		    "- A line with a simplified numerical function"
		    "- The position of the numerical function"
		    "- A numerical function term")
	   (effect  "Applys a tactic.")
	   (value   "Undefined."))
  (if line2
      (cond ((and line1 line2 (pdsn~open-node-p line1))
	     (infer~compute-outline 'expand-num (list line1 line2) (list pos term)))
	    ((and line1 line2 (pdsn~open-node-p line2))
	     (infer~compute-outline 'simplify-num (list line2 line1) (list pos term)))
	    ((and line2 (pdsn~open-node-p line2))
	     (infer~compute-outline 'simplify-num (list line2 line1) (list pos term)))
	    (line2
	     (infer~compute-outline 'expand-num (list line1 line2) (list pos term))))
    (omega~error "Application of Expand-Num is not possible without a formula including a simplified numerical function.")))


;;  Expand-Num*

(infer~deftactic expand-num*
		 (outline-mappings (((existent existent) expand-num*-a)
				    ((existent nonexistent) expand-num*-b)
				    ((nonexistent existent) expand-num*-f)))
		 (expansion-function natac=expand-expand-num*)
		 (parameter-types position-list term-list)
		 (help "Simplify an arithmetic expression by computation."))
		 
(tac~deftactic expand-num*-b expand-num* (in natural)
   (parameters (POS cons "A list of positions")
	       (TERM cons "A list of terms"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (natac~simplify-number* (formula L2) POS)))
   (sideconditions (natac=formula-not-equal* (formula L2) POS)
		   (natac=formula-equal-with-term* (formula L2) POS TERM))
   (description "Backward application of numerical simplification."))

(tac~deftactic expand-num*-f expand-num* (in natural)
   (parameters (POS cons "A list of positions")
	       (TERM cons "A list of terms"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (natac~expand-number* (formula L1) POS TERM)))
   (sideconditions (natac=formula-not-equal-with-term* (formula L1) POS TERM)
		   (natac=formula-equal-with-reduced-term* (formula L1) POS TERM))
   (description "Forward application of numerical simplification."))

(tac~deftactic expand-num*-a expand-num* (in natural)
   (parameters (POS cons "A list of positions")
	       (TERM cons "A list of terms"))
   (premises L1)
   (conclusions L2)
   (sideconditions (natac=formula-equal* (formula L2) (formula L1) POS)
		   (natac=formula-equal-with-term* (formula L2) POS TERM))
   (description "Application of numerical simplification."))

(com~defcommand expand-num*       ;;; works   VS
  (argnames exp-line simple-line positions terms)
  (argtypes ndline ndline position-list term-list)
  (arghelps "A line with expanded numerical functions" "A line with simplified numerical functions" "The positions of these numerical function" "A list of numerical function terms")
  (function natac=expand-num*)
  (frag-cats tactics natural)
  (defaults )
  (log-p t)
  (help "Simplification of a rational number."))

(defun natac=expand-num* (line1 line2 pos-list term-list)
  (declare (edited  "12-OCT-2000")
	   (authors Sorge)
	   (input   "- A line with expanded numerical functions"
		    "- A line with simplified numerical functions"
		    "- A list of positions of the numerical function"
		    "- A list of numerical function terms")
	   (effect  "Applys a tactic.")
	   (value   "Undefined."))
  (if line2
      (cond ((and line1 line2 (pdsn~open-node-p line1))
	     (infer~compute-outline 'expand-num* (list line1 line2) (list pos-list term-list)))
	    ((and line1 line2 (pdsn~open-node-p line2))
	     (infer~compute-outline 'simplify-num* (list line2 line1) (list pos-list term-list)))
	    ((and line2 (pdsn~open-node-p line2))
	     (infer~compute-outline 'simplify-num* (list line2 line1) (list pos-list term-list)))
	    (line2
	     (infer~compute-outline 'expand-num* (list line1 line2) (list pos-list term-list))))
    (omega~error "Application of Expand-Num* is not possible without a formula including simplified numerical functions.")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auxiliary Functions for Numerical Simplification
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun natac=formula-equal (Phi Psi Pos)
  (or (term~alpha-equal Psi (natac~simplify-number Phi Pos 'symnum))
      (term~alpha-equal Phi (natac~simplify-number Psi Pos 'symnum))
      (term~alpha-equal Psi (natac~simplify-number Phi Pos 'setnum))
      (term~alpha-equal Phi (natac~simplify-number Psi Pos 'setnum))))
  
(defun natac=formula-equal* (Phi Psi Poslist)
  (or (natac~data-equal Psi (natac~simplify-number* Phi Poslist))
      (natac~data-equal Phi (natac~simplify-number* Psi Poslist))))
  
(defun natac=formula-not-equal (Phi Pos)
  (not (natac~data-equal Phi (natac~simplify-number Phi Pos))))

(defun natac=formula-not-equal* (Phi Poslist)
  (not (natac~data-equal Phi (natac~simplify-number* Phi Poslist))))

(defun natac=formula-not-equal-with-term (Phi Pos Term)
  (not (natac~data-equal Term (data~struct-at-position Phi Pos))))

(defun natac=formula-not-equal-with-term* (Phi Poslist Termlist)
  (every #'(lambda (term pos)
	     (not (natac~data-equal Term (data~struct-at-position Phi Pos))))
	 termlist poslist))

(defun natac=formula-equal-with-term (Phi Pos Term)
  (natac~data-equal Term (data~struct-at-position Phi Pos)))

(defun natac=formula-equal-with-term* (Phi Poslist Termlist)
  (every #'(lambda (term pos)
	     (natac~data-equal Term (data~struct-at-position Phi Pos)))
	 termlist poslist))

(defun natac=formula-equal-with-reduced-term (Phi pos term)
  (let ((red-term1 (data~struct-at-position Phi pos))
	(red-term2 (natac~reduction-cont term (pds~environment omega*current-proof-plan))))
    (natac~data-equal red-term1 red-term2)))
		    
(defun natac=formula-equal-with-reduced-term* (Phi poslist termlist)
  (let ((red-terms1 (mapcar #'(lambda (pos)
				(data~struct-at-position Phi pos))
			    poslist))
	(red-terms2 (mapcar #'(lambda (term)
			       (natac~reduction-cont term (pds~environment omega*current-proof-plan)))
			   termlist)))
    (every #'natac~data-equal red-terms1 red-terms2)))
		    
(defun natac=expand-simplify-num* (outline parameters)
  (labels ((esn*-rec (line poslist termlist)
		     (if (and (cdr poslist) (cdr termlist))
			 (esn*-rec (car (tacl~apply 'simplify-num (list nil line)
						     (list (car poslist) (car termlist))))
				   (cdr poslist) (cdr termlist))
		       (tacl~apply 'simplify-num (list (car outline) line)
				   (list (car poslist) (car termlist))))))
    (tacl~init outline)
    (esn*-rec (cadr outline) (car parameters) (cadr parameters))
    (tacl~end)))
		       
(defun natac=expand-expand-num* (outline parameters)
  (labels ((esn*-rec (line poslist termlist)
		     (if (and (cdr poslist) (cdr termlist))
			 (esn*-rec (cadr (tacl~apply 'expand-num (list line nil)
						     (list (car poslist) (car termlist))))
				   (cdr poslist) (cdr termlist))
		       (tacl~apply 'expand-num (list line (cadr outline))
				   (list (car poslist) (car termlist))))))
    (tacl~init outline)
    (esn*-rec (car outline) (car parameters) (cadr parameters))
    (tacl~end)))
		       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Additional commands to ease numerical simplification
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(com~defcommand simplify-num-step
  (argnames line)
  (argtypes ndline)
  (arghelps "A line with numerical functions")
  (function natac~simplify-step)
  (frag-cats tactics natural)
  (defaults )
  (log-p t)
  (help "Simplification of rational numbers."))

(defun natac~simplify-step (line)
  (declare (edited  "12-MAY-1996 15:59")
	   (authors SORGE)
	   (input   "A nd-line.")
	   (effect  "Simplifys all numerical expressions in that line step by step.")
	   (value   "Undefined."))
  (let* ((formula (node~formula line))
	 (pos-list (natac=sort-pos-list (natac=function-position-list formula))))
    (natac=apply-simplify-step line (apply #'append (mapcar #'reverse pos-list)))))

(defun natac=apply-simplify-step (line list)
  (if (pdsn~open-node-p line)
      (natac=apply-simplify-all-back line list)
    (natac=apply-simplify-all-forw line list)))


(com~defcommand simplify-all-num
  (argnames line)
  (argtypes ndline)
  (arghelps "A line with numerical functions")
  (function natac~simplify-all)
  (frag-cats tactics natural)
  (defaults )
  (log-p t)
  (help "Simplification of rational numbers."))


(defun natac~simplify-all (line)
  (declare (edited  "12-MAY-1996 15:59")
	   (authors SORGE)
	   (input   "A nd-line.")
	   (effect  "Simplifys all numerical expressions in that line.")
	   (value   "Undefined."))
  (let* ((formula (node~formula line))
	 (pos-list (natac=sort-pos-list (natac=function-position-list formula))))
    (natac=apply-simplify-all line
			       (mapcar #'car pos-list))))

(defun natac=apply-simplify-all (line list)
  (if (pdsn~open-node-p line)
      (natac=apply-simplify-all-back line list)
    (natac=apply-simplify-all-forw line list)))

(defun natac=apply-simplify-all-back (line list)
  (when list
    (let* ((pos (car list))
	   (outline (infer~compute-outline 'expand-num
					   (list line nil)
					   (list pos (data~struct-at-position (node~formula line) pos)))))
      (natac=apply-simplify-all-back (second outline) (cdr list)))))

(defun natac=apply-simplify-all-forw (line list)
  (when list
    (let* ((pos (car list))
	   (outline (infer~compute-outline 'simplify-num
					   (list nil line)
					   (list pos (data~struct-at-position (node~formula line) pos)))))
      (natac=apply-simplify-all-forw (first outline) (cdr list)))))

(com~defcommand suggest-simplify-num       ;;; works   VS
  (argnames line)
  (argtypes ndline)
  (arghelps "A line with numerical functions")
  (function natac~suggest-simplify-num)
  (frag-cats tactics natural)
  (defaults )
  (log-p t)
  (help "A list of positions where simplifications of numerical functions are possible."))

(defun natac~suggest-simplify-num (line)
  (declare (edited  "12-MAY-1996 17:01")
	   (authors SORGE)
	   (input   "A nd-line.")
	   (effect  "Prints all positions where simplifications of numerical functions are possible.")
	   (value   "Undefined."))
  (let ((pos-list (natac=function-position-list (node~formula line))))
      (mapcar #'(lambda (x)
		  (omega~output "~A" (pos~number-list x)))
	      pos-list)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Expansion of Numbers to Function in Set Theory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(com~defcommand num2func-line     
  (argnames line)
  (argtypes ndline)
  (arghelps "A line")
  (function natac~num2func-line)
  (frag-cats tactics natural)
  (defaults )
  (log-p t)
  (help "Changes the numbers in a line to a set theoretical function representation."))

(com~defcommand num2func-proof    
  (argnames )
  (argtypes )
  (arghelps )
  (function natac~num2func-proof)
  (frag-cats tactics natural)
  (defaults )
  (log-p t)
  (help "Changes the numbers in all proof-lines to a set theoretical function representation."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Translations of Functions in Set Theory into Numbers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(com~defcommand func2num-line     
  (argnames line)
  (argtypes ndline)
  (arghelps "A line")
  (function natac~func2num-line)
  (frag-cats tactics natural)
  (defaults )
  (log-p t)
  (help "Changes the set theoretical representation of numbers in a line to digits."))

(com~defcommand func2num-proof    
  (argnames )
  (argtypes )
  (arghelps )
  (function natac~func2num-proof)
  (frag-cats tactics natural)
  (defaults )
  (log-p t)
  (help "Changes the set theoretical representation of numbers to digits in all proof-lines."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following is a tactic for arithmetic simplification
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~deftactic arith-simplify
		 (outline-mappings (((existent) arith-simplify-b)))
		 (parameter-types symbol)
		 (expansion-function natac~expand-arith-simplify)
		 (help "Arithmetical simplification."))

(com~defcommand arith-simplify
  (argnames openline)
  (argtypes ndline)
  (arghelps "An open-line with arithmetical functions")
  (function natac=arith-simplify)
  (frag-cats tactics natural)
  (defaults )
  (log-p t)
  (help "Simplification of a arithmetical expression."))

(tac~deftactic arith-simplify-b arith-simplify (in natural)
   (parameters (Axiom symbol "A theory assumption."))
   (premises )
   (conclusions L1)
   (computations )
   (sideconditions (natac=ensure-arith-correctness (formula L1)))
   (description "Backward application of arithmetical simplification."))

;;(defun natac=formula-not-equal (Phi)
;;  (not (data~equal Phi (natac=compute-arith-simplify Phi))))

(defun natac=arith-simplify (l1)
  (let ((axiom (natac=get-corresponding-axiom (node~formula l1))))
    (infer~compute-outline 'arith-simplify (list l1) (list (keim~name Axiom)))))
   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Application of rewrite rules (i.e. a formula of the form
;; (forall (lam (x aa) (forall (lam (y bb) .....
;;        (implies (and (in x Set1) (and (in y Set2) ....
;;                 (=  (quack x y ...) 
;;                     (ruelps y x ...) ....) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic apply-rewrite
		 (outline-mappings (((existent nonexistent) apply-rewrite-b)
				    ((nonexistent existent) apply-rewrite-f)
				    ((existent existent) apply-rewrite-a)))
		 (parameter-types position symbol symbol term-list)
		 (expansion-function natac=expand-apply-rewrite)
		 (help "Apply a rewrite rule at a given position in a certain direction."))

(tac~deftactic apply-rewrite-b apply-rewrite (in natural)
   (parameters (position pos+position "A position for the application")
	       (axiom symbol "The rewrite rule")
	       (direction symbol "The direction")
	       (addargs list "A list of additional arguments."))
   (premises (L2 "A closed line to apply rewrite rule"))
   (conclusions (L1 "An open line to apply rewrite rule"))
   (computations (L2 (natac=compute-apply-rewrite (formula L1) position Axiom direction addargs)))
   (sideconditions )
   (description "Rewrite application."))

(tac~deftactic apply-rewrite-f apply-rewrite (in natural)
   (parameters (position pos+position "A position for the application")
	       (axiom symbol "The rewrite rule")
	       (direction symbol "The direction")
	       (addargs list "A list of additional arguments."))
   (premises (L2 "A closed line to apply rewrite rule"))
   (conclusions (L1 "An open line to apply rewrite rule"))
   (computations (L1 (natac=compute-apply-rewrite (formula L2) position Axiom direction addargs)))
   (sideconditions )
   (description "Rewrite application."))

(tac~deftactic apply-rewrite-a apply-rewrite (in natural)
   (parameters (position pos+position "A position for the application")
	       (axiom symbol "The rewrite rule")
	       (direction symbol "The direction")
	       (addargs list "A list of additional arguments."))
   (premises (L2 "A closed line to apply rewrite rule"))
   (conclusions (L1 "An open line to apply rewrite rule"))
   (computations )
   (sideconditions (natac=check-apply-rewrite (formula L1) (formula L2) position Axiom direction addargs))
   (description "Rewrite application."))

(defun natac=check-apply-rewrite (f1 f2 pos ax direction addargs)
  (let ((new-term1 (natac=compute-apply-rewrite f1 pos ax direction addargs))
	(new-term2 (natac=compute-apply-rewrite f2 pos ax direction addargs)))
    (or (lam~equal-p new-term1 f2)
	(lam~equal-p new-term2 f1))))

(defun natac=compute-apply-rewrite (formula pos ax direction addargs)
  (let* ((term (data~struct-at-position formula pos))
	 (newterm (if (term~appl-p term)
		      (term~appl-create (data~appl-function term)
					(butlast (data~appl-arguments term) (length addargs)))
		    term))
	 (rewrite (natac=get-rewrite-formula
		   (th~find-assumption ax
				       (prob~proof-theory omega*current-proof-plan)))))
    (multiple-value-bind (subst match-term non-match-term)
	(natac=syntactic-match newterm rewrite direction)
      (declare (ignore match-term))
      (let ((newformula (subst~apply subst non-match-term)))
	(if addargs
	    (data~replace-at-position
	     formula
	     pos
	     (term~appl-create newformula addargs))
	  (data~replace-at-position formula pos newformula))))))
      
(defun natac=get-rewrite-formula (Axiom)
  (declare (edited  "07-JUL-1997")
	   (authors Sorge)
	   (input   "An Axiom.")
	   (value   "Returns the equality line in the formula."))
  (let ((formula (th~ass-formula Axiom)))
    (data~struct-at-position formula
		      (pos~butlast
			(data~position
			 formula
			 #'(lambda (x) (and (term~primitive-p x) (string-equal (keim~name x) '=))))))))
  

(defun natac=syntactic-match (term equation direction)
  (let* ((lterm (car (data~appl-arguments equation)))
	 (rterm (cadr (data~appl-arguments equation)))
	 (lsubst (uni~syntactic-matcher lterm term))
	 (rsubst (uni~syntactic-matcher rterm term)))
    (cond ((and (or (string-equal direction :rl)
		    (string-equal direction :srl))
		rsubst)
	   (values rsubst rterm lterm))
	  (lsubst (values lsubst lterm rterm))
	  (t (values rsubst rterm lterm)))))
	
;;(defun natac=all-bound-p (subst term)
;;  (let ((vars (term~free-variables term))
;;        (domain (subst~domain subst)))
;;    (print domain)
;;    (print vars)
;;    (subsetp vars domain)))

(defun natac=match-with-term (rewterm realterm)
  (let* ((lterm (car (data~appl-arguments rewterm)))
	 (rterm (cadr (data~appl-arguments rewterm)))
	 (addargs1 (when (and (term~appl-p realterm) (term~appl-p lterm)
			      (>= (length (data~appl-arguments realterm))
				  (length (data~appl-arguments lterm))))
		     (subseq (data~appl-arguments realterm) (length (data~appl-arguments lterm)))))
	 (addargs2 (when (and (term~appl-p realterm) (term~appl-p rterm)
			      (>= (length (data~appl-arguments realterm))
				  (length (data~appl-arguments rterm))))
		     (subseq (data~appl-arguments realterm) (length (data~appl-arguments rterm)))))
	 (lconst (term~appl-create (data~appl-function realterm)
				   (butlast (data~appl-arguments realterm) (length addargs1))))
	 (rconst (term~appl-create (data~appl-function realterm)
				   (butlast (data~appl-arguments realterm) (length addargs2))))
	 (lsubst (uni~syntactic-matcher lterm lconst))
	 (rsubst (uni~syntactic-matcher rterm rconst)))
    (if lsubst
	(values lsubst addargs1)
      (values rsubst addargs2))))

(defun natac=extract-equation (formula)
  (cond ((logic~equality-p formula) formula)
	((logic~universal-quantification-p formula)
	 (natac=extract-equation (car (data~appl-arguments formula))))
	((term~abstr-p formula) (natac=extract-equation (data~abstr-scope formula)))
	((logic~implication-p formula)
	 (natac=extract-equation (cadr (data~appl-arguments formula))))))
	      
(defun natac=apply-equation (conc prem equation pos)
  (let ((cterm (data~struct-at-position (node~formula conc) pos))
	(pterm (data~struct-at-position (node~formula conc) pos))
	(lequ (car (data~appl-arguments (node~formula equation))))
	(requ (cadr (data~appl-arguments (node~formula equation)))))
   ;;(multiple-value-bind (success subst)
    (let ((subst (term~alpha-equal lequ pterm)))
      (if subst
	  (if (subst~empty-p subst)
	      (tacl~apply '=subst (list conc prem equation) (list pos))
	    (tacl~sequence
	     ((alpha-res) ('alpha-conversion (list nil equation) (list (subst~domain subst) (subst~codomain subst))))
	     (dummy ('=subst (list conc prem alpha-res) (list pos)))))
	  ;;(multiple-value-bind (success2 subst2)
	(let ((subst2 (term~alpha-equal requ pterm)))
	  (if (subst~empty-p subst2)
	      (tacl~apply '=subst (list conc prem equation) (list pos))
	    (tacl~sequence
	       ((alpha-res) ('alpha-conversion (list nil equation) (list (subst~domain subst2) (subst~codomain subst2))))
	       (dummy ('=subst (list conc prem alpha-res) (list pos))))))))))
    

(defun natac=expand-apply-rewrite (outline parameter)
  (tacl~init outline)
  (let* ((conc (first outline))         ;;; T<((lam+ab) c1...cn)>
	 (precond (second outline))     ;;; T<(#ab c1...cn)>
	 (pos (first parameter))
	 (forall-cond-equation (tacl~insert&return-assumption
				 (prob~proof-theory omega*current-proof-plan)
				 (second parameter)))
                                       ;;;  ALL (p1^p2^..^pk) => #ab=(lam+ab) (if int=1)
	                               ;;;  ALL (p1^p2^..^pk) => (lam+ab)=#ab (if int=2)
	 (hdir (third parameter))
	 (direction (if (string-equal hdir :srl) nil hdir))
	 (arglist (fourth parameter))  ;;;  the argumentlist (c1...cn)
	 (equation (natac=extract-equation (node~formula forall-cond-equation)))
                 ;;; #ab=(lam+ab) (if int=1) or (lam+ab)=#ab (if int=2)
	 (pattern (pdsj~outline-pattern (node~justification (car outline))))
	 (ext-subterm
	  (let* ((subterm (if (infer~nonexistent-pattern-p (car pattern))
			      (data~struct-at-position (node~formula precond) pos)
			    (data~struct-at-position (node~formula conc) pos))))
	    (if arglist
		(term~appl-create
		 (data~appl-function subterm)
		 (butlast (data~appl-arguments subterm) (length arglist)))
	      subterm))))
    (multiple-value-bind (subst term-of-equation non-match)
	(natac=syntactic-match ext-subterm equation direction)
      (declare (ignore non-match term-of-equation))
      (do ()
	  ((not (logic~universal-quantification-p (node~formula forall-cond-equation)))
	   forall-cond-equation)
	(setq forall-cond-equation
	      (first (tacl~apply 'foralle (list nil forall-cond-equation)
			       (list (beta~normalize
				      (subst~apply subst
						   (logic~quantification-bound-variable
						    (node~formula forall-cond-equation)))))))))
         ;;; forall-cond-equation is an implication or an equation (obtained from forall-cond-equation)
    (cond ((logic~implication-p (node~formula forall-cond-equation))
	   (let* ((result1
		   (tacl~apply 'impe (list nil nil forall-cond-equation) nil))
                         ;;; #ab=(lam+ab) (p1^p2^..^pk) ((p1^p2^..^pk)=>#ab=(lam+ab))
		  (result2
		   (if arglist
		       (dolist (arg arglist forall-cond-equation)
			 (setq forall-cond-equation
			       (first (tacl~apply 'exti (list nil (first result1)) nil)))
			 (setq forall-cond-equation
			       (first (tacl~apply 'foralle (list nil forall-cond-equation) (list arg)))))
		     (first result1)))
		  (result3                     ;;; dummy
		   (natac=apply-equation conc precond result2 pos))
		   ;;;(tacl~apply '=subst (list conc precond result2) (list pos)))
	                 ;;; T<((lam+ab) c1...cn)> T<((#ab) c1 .. cn)> (#ab=lam+ab) pos-of((lam+ab))
		  (conjunction (second result1)))
	                 ;;;  T<(#ab c1...cn)> T<((#ab) c1 .. cn)>  (evtl. braucht man auch ein lambda)
	    ;;; we still have to show the validity of the
	    ;;; conditions of the applied equation
	     (declare (ignore result3))
	     (do ()                                                          ;;;; noch auschecken fuer (and (and a b) (and c d))!!!
		 ((not (logic~conjunction-p (node~formula conjunction))))
	       (let* ((andi-res
		       (tacl~apply 'andi (list conjunction nil nil) nil))   ;;; a1 (a2^...^ak) (a1^...^ak)
		      (axiom (natac=get-corresponding-axiom (node~formula (cadr andi-res)))))
		 (tacl~apply 'arith-simplify (list (second andi-res)) (list (keim~name axiom))) ;;; a1
		 (setq conjunction (third andi-res))))  ;;; (a2^...^ak)
	     (let ((axiom (natac=get-corresponding-axiom
			   (node~formula conjunction))))
	       (tacl~apply 'arith-simplify (list conjunction) (list (keim~name axiom))))
	     (tacl~end)))
	  ((logic~equality-p (node~formula forall-cond-equation))
	   (let* ((result1
		   (dolist (arg arglist forall-cond-equation)
		     (setq forall-cond-equation
			   (first (tacl~apply 'exti (list nil forall-cond-equation) nil)))
		     (setq forall-cond-equation
			   (first (tacl~apply 'foralle (list nil forall-cond-equation) (list arg)))))))
	     (natac=apply-equation conc precond result1 pos)
	   ;;;(tacl~apply '=subst (list conc precond result1) (list pos))
	                 ;;; T<((lam+ab) c1...cn)> T<((#ab) c1 .. cn)> (#ab=lam+ab) pos-of((lam+ab))
	     (tacl~end)))))))
	     

(com~defcommand apply-rewrite
  (argnames oldline newline axiom position direction)
  (argtypes ndline ndline thy-assumption position symbol)
  (arghelps "An open line to apply rewrite rule" "A premise to apply rewrite rule"
	    "The rewrite rule" "A position for the application" "A direction")
  (function natac=apply-rewrite)
  (frag-cats tactics base)
  (defaults ((oc~default-current-planline) (com~unspecified) (com~unspecified) (com~unspecified) (com~unspecified)))
  (log-p T)
  (help "Rewriting with a given equality rule."))

(defun natac=apply-rewrite (C P Ax Pos direction)
  (let* ((rew-form (natac=get-rewrite-formula Ax)))
    (multiple-value-bind (subst addargs)
	(if C
	    (natac=match-with-term rew-form (data~struct-at-position (node~formula C) Pos))
	  (natac=match-with-term rew-form (data~struct-at-position (node~formula P) Pos)))
      (if subst
	  (infer~compute-outline 'apply-rewrite (list C P) (list Pos (keim~name Ax) direction addargs))
	(warn ";;; The tactic APPLY-REWRITE cannot be applied.")))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following tactic could go into the base-tactics file....
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~deftactic alpha-conversion
		 (outline-mappings (((existent nonexistent) alpha-conversion-b)
				    ((nonexistent existent) alpha-conversion-f)
				    ((existent existent) alpha-conversion-a)))
		 (parameter-types term-list term-list)
		 (expansion-function natac=expand-alpha-conversion)
		 (help "Apply a rewrite rule at a given position in a certain direction."))

(tac~deftactic alpha-conversion-b alpha-conversion (in natural)
   (parameters (domain list "A list of terms")
	       (codomain list "Another list of terms"))
   (premises (L2 "A closed line to apply alpha conversion"))
   (conclusions (L1 "An open line to apply alpha conversion"))
   (computations (L2 (data~replace-structs (formula L1) domain codomain)))
   (sideconditions )
   (description "Alpha conversion."))

(tac~deftactic alpha-conversion-f alpha-conversion (in natural)
   (parameters (domain list "A list of terms")
	       (codomain list "Another list of terms"))
   (premises (L2 "A closed line to apply alpha conversion"))
   (conclusions (L1 "An open line to apply alpha conversion"))
   (computations (L1 (data~replace-structs (formula L2) domain codomain)))
   (sideconditions )
   (description "Alpha conversion."))

(tac~deftactic alpha-conversion-a alpha-conversion (in natural)
   (parameters (domain list "A list of terms")
	       (codomain list "Another list of terms"))
   (premises (L2 "A closed line to apply alpha conversion"))
   (conclusions (L1 "An open line to apply alpha conversion"))
   (computations )
   (sideconditions (natac=check-alpha-conversion (formula L1) (formula L2) domain codomain))
   (description "Alpha conversion."))

(defun natac=check-alpha-conversion (f1 f2 domain codomain)
  (or (term~alpha-equal (data~replace-structs f1 domain codomain) f2)
      (term~alpha-equal (data~replace-structs f1 domain codomain) f2)))


(defun natac=expand-alpha-conversion (outline parameters)
  (omega~message "... expansion of alpha-conversion is not yet implemented!"))


;;;;;;;;;;;;;;
;;Apply the natural induction axiom
;;;;;;;;;;;;;;

(infer~deftactic apply-induct
		 (outline-mappings (((existent nonexistent nonexistent) apply-induct-b)))
		 (parameter-types term termsym)
	         (expansion-function natac=expand-apply-induct)
		 (help "Make Induction."))

(tac~deftactic apply-induct-b apply-induct (in base)
  (parameters (B term+number "An integer.") (X term+constant "A new constant."))
  (conclusions C)
  (premises Pbase Pstep)
  (hypotheses (Bsort Pbase)
	      (Hstep Pstep)
	      (Hsort Pstep))
  (sideconditions 
   (batac=forall-sort-p (formula C)))
  (computations 
   (Hstep (natac=compute-apply-induct-hyp (formula C) X))
   (Hsort (natac=compute-apply-induct-hyp-sort X))
   (Bsort (natac=compute-apply-induct-hyp-sort B))
   (Pstep (natac=compute-apply-induct-step (formula C) X))
   (Pbase (natac=compute-apply-induct-base (formula C) B)))
  (description "Induction on natural numbers with a given base case."))


(defun batac=forall-sort-p (formula)       ;;; Put this here as it is the first time it is used (was missing beforehand!). VS
  (and
   (term~appl-p formula)
   (data~schema-equal (data~appl-function formula)
               (env~lookup-object :forall-sort (pds~environment omega*current-proof-plan)))))


(defun natac=get-induct-predicate (term)
  (first (data~appl-arguments term)))

(defun natac=compute-apply-induct-base (term b)
  (beta~normalize
   (data~appl-create (natac=get-induct-predicate term) (list b))))

(defun natac=compute-apply-induct-step (term n)
  (beta~normalize 
   (data~appl-create (natac=get-induct-predicate term)
		     (list (data~appl-create
			    (env~lookup-object :s (pds~environment omega*current-proof-plan))
			    (list n))))))

(defun natac=compute-apply-induct-hyp (term n)
  (beta~normalize
   (data~appl-create (natac=get-induct-predicate term)
		    (list n))))

(defun natac=compute-apply-induct-hyp-sort (n)
  (data~appl-create (env~lookup-object :nat (pds~environment omega*current-proof-plan))
		    (list n)))

;(defun natac=expand-apply-induct (outline param)
;  (let* ((axiom  (tacl~insert&return-assumption 'natbural 'nat-induct))
;         (conc (car outline))
;         (Pbase  (cadr outline))
;         (Pbase (caddr outline))
;         (old-hyp (car (set-difference (pdsn~hyps prem) (pdsn~hyps conc))))
;         (n (car param))
;         (pred (natac=get-induct-predicate (node~formula (pds~label2node conc))))
;         (para ((data~abstr-domain pred))
;    (tacl~init outline)
;    (let ((result 
;           (tacl~sequence
;            (foralli ('foralli-sort (list conc nil) (list para)))
;            (foralle ('foralle (list axiom nil) (list      
;       (tac~forget&destroy-hyp (list (third result)) old-hyp (fourth result) :test 'term~alpha-equal)
;       (tacl~apply 'weaken (list (third result) prem) nil))
;    (tacl~end)))
    

(com~defcommand apply-induct
  (argnames line basis)
  (argtypes ndline term)
  (arghelps "An line to prove" "A natural number for the base case.")
  (function natac=apply-induct)
  (defaults natac=apply-induct-defaults)
  (frag-cats tactics base) 
  (log-p T)
  (help "Apply the indcution axiom on a line."))

(defun natac=apply-induct (line basis)
  (let* ((env (pds~environment omega*current-proof-plan))
	(const (term~generate-term-primitive-with-new-name
		'n
		(env~lookup-object :num env)
		'term+constant
		env)))
    (infer~compute-outline 'apply-induct (list line nil nil)
			   (list basis const))))


(defun natac=apply-induct-defaults (line basis)
  (cond ((not (com~specified-arg-p line))
	 (list (oc~default-current-planline) (com~unspecified)))
	((not (com~specified-arg-p basis))
	 (list line (post~read-object 0 (pds~environment omega*current-proof-plan) :existing-term)))
	(t (list line basis))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Two tactics to derive explicit sets from first-n-nats predicate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic set2nats
		 (outline-mappings (((existent existent) set2nats-a)
				    ((existent nonexistent) set2nats-b)
				    ((nonexistent existent) set2nats-f)))
		 (expansion-function natac=expand-set2nats)
		 (parameter-types position)
		 (help "Convert an explicitly given set of natural numbers into a representation involving the FIRST-N-NATS predicate."))


(tac~deftactic set2nats-a set2nats (in base)
   (parameters (position pos+position "A position."))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (natac=set2nats-a-p (formula L1) (formula L2) position))
   (description "Closing set2nats substitution."))

(defun natac=set2nats-a-p (formula1 formula2 pos)
  (declare (edited  "01-MAR-2000")
	   (authors Sorge)
	   (input   "Two formulas and a position.")
	   (effect  "None.")
	   (value   "T if set2nats is applicable at the position with both formulas."))
  (let ((abstraction (data~struct-at-position formula1 pos))
	(first-n (data~struct-at-position formula2 pos)))
    (multiple-value-bind (success order elements)
	(natac=disect-nat-set abstraction)
      (and
       success
       (data~equal (data~appl-function first-n)
		   (env~lookup-object 'first-n-nats (pds~environment omega*current-proof-plan)))
       (= (keim~name (car (data~appl-arguments first-n))) order)
       (let ((number-list (mapcar #'keim~name elements)))
	 (natac=n2zero-list-p (reverse (sort number-list #'<)) (1- order)))))))



(tac~deftactic set2nats-f set2nats (in base)
   (parameters (position pos+position "A position."))
   (premises L1)
   (conclusions L2)
   (computations (L2 (natac=set2nats-create-f (formula L1) position)))
   (sideconditions (natac=set2nats-f-p (formula L1) position))
   (description "Forward application of set2nats substitution."))


(let (success order elements)
  
  (defun natac=set2nats-f-p (formula pos)
    (declare (edited  "01-MAR-2000")
	     (authors Sorge)
	     (input   "A formula and a position.")
	     (effect  "None.")
	     (value   "T if the sub-formula at position is a set of natural numbers in successive order, starting from 0."))
    (multiple-value-setq (success order elements)
      (natac=disect-nat-set (data~struct-at-position formula pos)))
    (when success
      (let ((number-list (mapcar #'keim~name elements)))
	(natac=n2zero-list-p (reverse (sort number-list #'<)) (1- order)))))

  (defun natac=set2nats-create-f (formula pos)
    (when success
      (data~replace-at-position formula
				pos
				(term~appl-create 
				 (env~lookup-object 'first-n-nats (pds~environment omega*current-proof-plan))
				 (list (post~read-object order (pds~environment omega*current-proof-plan) :existing-term))))))
  )
  

(tac~deftactic set2nats-b set2nats (in base)
   (parameters (position pos+position "A position."))
   (premises L1)
   (conclusions L2)
   (computations (L1 (natac=set2nats-create-b (formula L2) position))) 
   (sideconditions (natac=set2nats-b-p (formula L2) position))
   (description "Backward application of set2nats substitution."))

(defun natac=set2nats-b-p (formula pos)
  (data~equal (data~appl-function (data~struct-at-position formula pos))
	      (env~lookup-object 'first-n-nats (pds~environment omega*current-proof-plan))))

(defun natac=set2nats-create-b (formula pos)
  (let* ((term (data~struct-at-position formula pos))
	 (number (keim~name (car (data~appl-arguments term)))))
    (data~replace-at-position formula
			      pos
			      (zmztac=produce-number-set (zmztac=count-first-n-nats number)))))


(infer~deftactic nats2set
		 (outline-mappings (((existent existent) nats2set-a)
				    ((existent nonexistent) nats2set-b)
				    ((nonexistent existent) nats2set-f)))
		 (expansion-function natac=expand-nats2set)
		 (parameter-types position)
		 (help "Convert an explicitly given set of natural numbers into a representation involving the FIRST-N-NATS predicate."))

(com~defcommand nats2set
  (argnames expl-line set-line position)
  (argtypes ndline ndline position)
  (arghelps "A line with a representation involving FIRST-N-NATS"
	    "A line containing a set representation of natural numbers"
	    "The position of the FIRST-N-NATS function or the set")
  (function natac=nats2set)
  (frag-cats tactics natural)
  (defaults natac=nats2set-defaults)
  (log-p t)
  (help "Convert a representation involving the FIRST-N-NATS predicate into an explicitly given set of natural numbers."))

(defun natac=nats2set (expl-line set-line position)
  (infer~compute-outline 'nats2set (list set-line expl-line) (list position)))

(defun natac=nats2set-defaults (expl-line set-line position)
  (cond ((not (com~specified-arg-p expl-line))
	 (list (pds~find-support
		#'(lambda (p)
		    (data~substruct-positions (env~lookup-object 'first-n-nats (pds~environment omega*current-proof-plan))
					      p :test #'data~equal)))
	       (com~unspecified) (com~unspecified)))
	((not (com~specified-arg-p set-line))
	 (list expl-line nil  (com~unspecified)))
	(t (list expl-line set-line
		 (let ((pos (when (pdsn~p expl-line)
			      (car (data~substruct-positions
				    (env~lookup-object 'first-n-nats (pds~environment omega*current-proof-plan))
				    (node~formula expl-line)
				    :test #'data~equal)))))
		   (when pos (pos~butlast pos)))))))


(tac~deftactic nats2set-a nats2set (in base)
   (parameters (position pos+position "A position."))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (natac=set2nats-a-p (formula L2) (formula L1) position))
   (description "Closing nats2set substitution."))

(tac~deftactic nats2set-f nats2set (in base)
   (parameters (position pos+position "A position."))
   (premises L1)
   (conclusions L2)
   (computations (L2 (natac=set2nats-create-b (formula L1) position)))
   (sideconditions (natac=set2nats-b-p (formula L1) position))
   (description "Forward application of nats2set substitution."))

(tac~deftactic nats2set-b nats2set (in base)
   (parameters (position pos+position "A position."))
   (premises L1)
   (conclusions L2)
   (computations (L1 (natac=set2nats-create-f (formula L2) position))) 
   (sideconditions (natac=set2nats-f-p (formula L2) position))
   (description "Backward application of nats2set substitution."))

(defun natac=n2zero-list-p (elements n)
  (declare (edited  "01-MAR-2000")
	   (authors Sorge)
	   (input   "A list of numbers and a number n.")
	   (effect  "None.")
	   (value   "T if the list is of the form '(n n-1 .... 0)."))
  (cond ((and (null elements) (< n 0)) t)
	((null elements) nil)
	((and elements (< n 0)) nil)
	((= (car elements) n) (natac=n2zero-list-p (cdr elements) (1- n)))))
    

(defun natac=disect-nat-set (abstraction)
  (declare (edited  "29-FEB-2000")
	   (authors Sorge)
	   (input   "An abstraction.")
	   (effect  "None.")
	   (value   "Returns three values:"
		    "First: T if the formula has the form '(lam (x (o num) (or (= x m1) (or (= x m2) .... (= x mn)))))'."
		    "Second: If first is t then n."
		    "Third: If first is t then list m1, m2, ..."))
  (when (and (data~abstr-p abstraction)
	     (= (length (data~abstr-binder abstraction)) 1))
    (let ((variable (data~abstr-bound-var abstraction))
	  (disjunct-list (natac=disect-disjunction (data~abstr-scope abstraction))))
      (when (every #'logic~equality-p disjunct-list)
	(let ((numbers (remove-duplicates (natac=disect-equations disjunct-list variable) :test #'data~equal)))
	  (when (every #'term~number-p numbers)
	    (values t (length numbers) numbers)))))))
  
(defun natac=disect-equations (equations variable)
  (declare (edited  "01-MAR-2000")
	   (authors Sorge)
	   (input   "A list of equations and a variable.")
	   (effect  "None.")
	   (value   "If the list of equations is of the form x = ci, where x is the variable, the function"
		    "returns a list containing the ci. In all other case it returns NIL."))
  (let* ((feq (car equations))
	 (farg (first (data~appl-arguments feq)))
	 (sarg (second (data~appl-arguments feq)))
	 (rest? (cdr equations))
	 (result (when rest? (natac=disect-equations rest? variable))))
    (when (or (and rest? result) (null rest?))
      (let ((var1? (data~equal variable farg))
	    (var2? (data~equal variable sarg)))
	(cond ((and var1? (not var2?)) (cons sarg result))
	      ((and var2? (not var1?)) (cons farg result)))))))
      
(defun natac=disect-disjunction (disjunction)
  (declare (edited  "01-MAR-2000")
	   (authors Sorge)
	   (input   "A disjuntion.")
	   (effect  "None.")
	   (value   "A list of the single disjuncts."))
  (if (and (term~appl-p disjunction) (logic~disjunction-p disjunction))
      (let* ((arguments (data~appl-arguments disjunction))
	     (ldisj (first arguments))
	     (rdisj (second arguments)))
	(append (natac=disect-disjunction ldisj)
		(natac=disect-disjunction rdisj)))
    (list disjunction)))


;;; Expansion: 
;;; Use successively the first-n-nats axiom until step case
;;; Then do a beta-normalization
;;; Check out differences in disjunctions with assoc + comm of or
;;; When doing forall-sort eliminations (on the instantiated step axiom) use well-sortedness.


(infer~deftactic defn-expand-nums
		 (outline-mappings (((existent existent) defn-expand-nums-a)
				    ((nonexistent existent) defn-expand-nums-f)
				    ))
		 (expansion-function natac=expand-defn-expand-nums)
		 (help "Expansion of numbers so successor terms."))

(tac~deftactic defn-expand-nums-f defn-expand-nums (in natural)
  (premises L1)
  (conclusions L2)
  (computations (L2 (natac=numbers-2-function (:formula L1))))
  (description "Forward application number expansion"))

(tac~deftactic defn-expand-nums-a defn-expand-nums (in natural)
  (premises L1)
  (conclusions L2)
  (sideconditions (natac=numbers-2-function-check (:formula L1)(:formula L2)))
  (description "Closed application number expansion"))

(defun natac=numbers-2-function-check (num suc)
  (data~equal suc (natac=numbers-2-function num)))

(defun natac=expand-defn-expand-nums (outline parameters) 
  (let ((L1 (second outline))
	(L2 (first outline)))
    (tacl~init outline)
    (let ((last (potac=expand-numbers L1 :forward t)))
      (tacl~apply 'weaken (list L2 last) nil))
    (tacl~end)))

(com~defcommand defn-expand-nums
  (argnames linenum linesuc)
  (argtypes ndline ndline)
  (arghelps "A premise with numbers" "A conclusion with s-terms")
  (function natac=defn-expand-num)
  (frag-cats tactics natural)
  (log-p T)
  (help "Expand number occurrences in a line."))



(defun natac=defn-expand-num (lnum lsuc)
  (infer~compute-outline 'defn-expand-nums (list lsuc lnum) nil))

(infer~deftactic defn-contract-nums
		 (outline-mappings (((existent existent) defn-contract-nums-a)
				    ((existent nonexistent) defn-contract-nums-f)
				    ))
		 (expansion-function natac=expand-defn-contract-nums)
		 (help "Expansion of numbers so successor terms."))

(tac~deftactic defn-contract-nums-f defn-contract-nums (in natural)
  (premises L1)
  (conclusions L2)
  (computations (L1 (natac=numbers-2-function (:formula L2))))
  (description "Forward application number expansion"))

(tac~deftactic defn-contract-nums-a defn-contract-nums (in natural)
  (premises L1)
  (conclusions L2)
  (sideconditions (natac=numbers-2-function-check  (:formula L2)(:formula L1)))
  (description "Closed application number expansion"))

(defun natac=expand-defn-contract-nums (outline parameters) 
  (let ((L1 (second outline))
	(L2 (first outline)))
    (tacl~init outline)
    (let ((last (potac=expand-numbers L2 :forward nil)))
      (tacl~apply 'weaken (list last l1) nil))
    (tacl~end)))

(com~defcommand defn-contract-nums
  (argnames linenum linesuc)
  (argtypes ndline ndline)
  (arghelps "A conclusion with numbers" "A premise with s-terms")
  (function natac=defn-contract-num)
  (frag-cats tactics natural)
  (log-p T)
  (help "Contract number occurrences in a line."))

(defun natac=defn-contract-num (lnum lsuc)
  (infer~compute-outline 'defn-contract-nums (list lnum lsuc) nil))

