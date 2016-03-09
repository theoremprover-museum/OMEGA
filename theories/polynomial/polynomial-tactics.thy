;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: KEIM -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1993 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     KEIM Project                                                         ;;
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

(eval-when (load compile eval)
  (unless (com~find-category 'polynomial)
    (com~defcategory polynomial
		     (help "Tactics of the theory  polynomial ."))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some of the tactics defined in here migth as well be defined in
;; a theory earlier, like natural ....
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some general stuff...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Queries
;;;;;;;;;;;
;;; Query for functions

(defun polynom=plus-function-p (term)
  (data~equal term
	      (env~lookup-object
	       natac*plus
	       (pds~environment omega*current-proof-plan))))

(defun polynom=times-function-p (term)
  (data~equal term
	      (env~lookup-object
	       natac*times
	       (pds~environment omega*current-proof-plan))))

(defun polynom=sum-p (term)
  (and (term~appl-p term)
       (polynom=plus-function-p (data~appl-function term))))

(defun polynom=product-p (term)
  (and (term~appl-p term)
       (polynom=times-function-p (data~appl-function term))))

;;; Query for objects

(defun polynom=polynomial-p (term Pos)
  (cacom~polynomial-p (data~struct-at-position term pos)))

;;; Query for applicability

(defun polynom=applicable-assoc-plus-p (Term)
  (and (term~appl-p Term)
       (= (length (data~appl-arguments Term)) 2)
       (term~appl-p (first (data~appl-arguments Term)))
       (data~equal (data~appl-function term)
                   (data~appl-function (first (data~appl-arguments term))))
       (polynom=plus-function-p (data~appl-function Term))))
  
(defun polynom=applicable-assoc-plus-backwards-p (Term)
  (and (term~appl-p Term)
       (= (length (data~appl-arguments Term)) 2)
       (term~appl-p (second (data~appl-arguments Term)))
       (data~equal (data~appl-function term)
                   (data~appl-function (second (data~appl-arguments term))))
       (polynom=plus-function-p (data~appl-function Term))))
  
(defun polynom=applicable-commu-plus-p (Term)
  (and (term~appl-p Term)
       (= (length (data~appl-arguments Term)) 2)
       (polynom=plus-function-p (data~appl-function Term))))
  
;;; Building objects

(defun polynom=create-number (number)
  (term~constant-create number
		       (env~lookup-object
                        natac*number-type
                        (pds~environment omega*current-proof-plan))))

;;; Applications
;;;;;;;;;;;;;;;;
;;; Applying associativity to Plus

(defun polynom=sum-create (arg1 arg2)
  (term~appl-create (env~lookup-object natac*plus
				  (pds~environment omega*current-proof-plan))
	       (list arg1 arg2)))

(defun polynom=product-create (arg1 arg2)
  (term~appl-create (env~lookup-object natac*times
				  (pds~environment omega*current-proof-plan))
	       (list arg1 arg2)))

(defun polynom=substraction-create (arg1 arg2)
  (term~appl-create (env~lookup-object natac*minus
				  (pds~environment omega*current-proof-plan))
	       (list arg1 arg2)))

(defun polynom=exponent-create (arg1 arg2)
  (term~appl-create (env~lookup-object natac*power
				  (pds~environment omega*current-proof-plan))
	       (list arg1 arg2)))

(defun polynom=apply-assoc (Term)
  (let ((Aterm (first (data~appl-arguments (first (data~appl-arguments Term)))))
        (Bterm (second (data~appl-arguments (first (data~appl-arguments Term)))))
        (Cterm (second (data~appl-arguments Term)))
        (Opp (data~appl-function Term)))
    (term~appl-create Opp
                 (list Aterm
                       (term~appl-create
                        Opp
                        (list Bterm Cterm))))))

(defun polynom=apply-assoc-backwards (Term)
  (let ((Aterm (first (data~appl-arguments Term)))
        (Bterm (first (data~appl-arguments (second (data~appl-arguments Term)))))
        (Cterm (second (data~appl-arguments (second (data~appl-arguments Term)))))
        (Opp (data~appl-function Term)))
    (term~appl-create Opp
                 (list (term~appl-create Opp (list Aterm Bterm))
                   Cterm))))

;;; Applying commutativity to Plus

(defun polynom=apply-commu (Term)
  (let ((Aterm (first (data~appl-arguments Term)))
        (Bterm (second (data~appl-arguments Term)))
        (Opp (data~appl-function Term)))
    (term~appl-create Opp
                 (list Bterm Aterm))))
		       
(defun polynom=simplify-num-application (node pos &optional (node2 nil))
  (let ((term (data~struct-at-position (node~formula node) pos)))
    (if (pdsn~open-node-p node)
	(tacl~apply 'expand-num (list node node2) (list pos term))
      (tacl~apply 'simplify-num (list node2 node) (list pos term)))))
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The beginning of the General Polynomial Tactics....
;; They are a bit tricky as they require some special
;; logical function constants. Therefore not all direction can be
;; supplied so far.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; First query stuff and auxiliary functions for special polynomial
;; constants. This might need some adjusting one day.....
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun polynom=create-functions (list)
  (remove-if #'null
	     (mapcar #'(lambda (x)
			 (env~lookup-object x
					    (if omega*current-proof-plan
						(pds~environment omega*current-proof-plan)
					      (th~env (th~find-theory 'polynomial)))))
		     list)))


(defun polynom=get-addargs (term length)
  (when (term~appl-p term)
    (subseq (data~appl-arguments term) length)))

(defun polynom=append-addargs (function addargs)
  (if (and addargs (term~appl-p function))
     (term~appl-create function addargs)
    function))

(defun polynom=new-number-vars (name number)
  (when (> number 0)
    (cons (term~generate-term-primitive-with-new-name
	   name
	   (env~lookup-object natac*number-type
			      (pds~environment omega*current-proof-plan))
	   'term+variable
	   (pds~environment omega*current-proof-plan))
	  (polynom=new-number-vars name (1- number)))))

(defun polynom=pplus-function-p (function)
  (find function (polynom=create-functions '(p-plus-r1 p-plus-q1 p-plus-z1 p-plus-n1
					     p-plus-r2 p-plus-q2 p-plus-z2 p-plus-n2
					     p-plus-r3 p-plus-q3 p-plus-z3 p-plus-n3
					     p-plus-r4 p-plus-q4 p-plus-z4 p-plus-n4))))

(defun polynom=ptimes-function-p (function)
  (find function (polynom=create-functions '(p-times-r1 p-times-q1 p-times-z1 p-times-n1
					     p-times-r2 p-times-q2 p-times-z2 p-times-n2
					     p-times-r3 p-times-q3 p-times-z3 p-times-n3
					     p-times-r4 p-times-q4 p-times-z4 p-times-n4))))

(defun polynom=pderiv-function-p (function)
  (find function (polynom=create-functions '(p-deriv-r1 p-deriv-q1 p-deriv-z1 p-deriv-n1
					     p-deriv-r2 p-deriv-q2 p-deriv-z2 p-deriv-n2
					     p-deriv-r3 p-deriv-q3 p-deriv-z3 p-deriv-n3
					     p-deriv-r4 p-deriv-q4 p-deriv-z4 p-deriv-n4))))
		  
(defun polynom=stimes-function-p (function)
  (find function (polynom=create-functions '(s-times-r1 s-times-q1 s-times-z1 s-times-n1
					     s-times-r2 s-times-q2 s-times-z2 s-times-n2
					     s-times-r3 s-times-q3 s-times-z3 s-times-n3
					     s-times-r4 s-times-q4 s-times-z4 s-times-n4))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tactics for polynomial addition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; POLY-PLUS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic poly-plus
                 (outline-mappings (((existent existent) poly-plus-a)
                                    ((nonexistent existent) poly-plus-f)
;;;				    ((existent nonexistent) poly-plus-b)
				    )
				   )
                 (expansion-function polynom=expand-poly-plus)
                 (parameter-types position)
                 (help "Polynomial addition (p (+) q) = lam z. (p z + q z)."))

(com~defcommand poly-plus
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A line containing the sum of two polynomials"
            "A line containing the sum of their monomials"
	    "The position of this polynomial")
  (function polynom=poly-plus)
  (frag-cats tactics polynomial)
  (defaults)
  (log-p t)
  (help "Polynomial addition (p (+) q) = lam z. (p z + q z)."))

(defun polynom=poly-plus (l1 l2 pos)
  (if (pdsn~open-node-p l1)
      (infer~compute-outline 'poly-split-plus (list l1 l2) (list pos))
    (infer~compute-outline 'poly-plus (list l2 l1) (list pos))))



;;
;; Poly PLUS directions
;;


(tac~deftactic poly-plus-f poly-plus (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=poly-plus-f (formula L1) Pos)))
   (sideconditions (polynom=addition-p (formula L1) Pos))
   (description "Forward application of Poly-Plus."))

(defun polynom=addition-p (formula Pos)
  (let ((term (data~struct-at-position formula pos)))
    (and (term~appl-p Term)
	 (polynom=pplus-function-p (data~appl-function Term)))))

					
(defun polynom=apply-polynomial-addition (term)
  (let* ((addargs (polynom=get-addargs term 2))
	 (arg1 (first (data~appl-arguments term)))
	 (arg2 (second (data~appl-arguments term)))
	 (oldvarlist (data~abstr-binder arg1))
	 (newvars (polynom=new-number-vars "Z" (length oldvarlist)))
	 (abstr (beta~normalize
		 (term~abstr-create newvars
				    (polynom=sum-create (term~appl-create arg1 newvars)
							(term~appl-create arg2 newvars))))))
    (if addargs
	(term~appl-create abstr addargs)
      abstr)))

 
(defun polynom=poly-plus-f (L1 Pos)
  (let* ((Subterm (data~struct-at-position L1 Pos))
         (Sum (polynom=apply-polynomial-addition Subterm)))
    (data~replace-at-position L1 Pos Sum)))

;;
;; The B direction
;; Doesn't make much sense so far, as we so not have sorted term and thus do not know, wich p-plus symbol is needed.
;
;;(tac~deftactic poly-plus-b (in polynomial)
;;   (arguments L2 POS)
;;   (argumenttypes pdsn+node pos+position)
;;   (arghelps "A line" "A position")
;;   (premises L1)
;;   (conclusions L2)
;;   (computations (L1 (polynom=poly-plus-b (formula L2) Pos)))
;;   (sideconditions (polynom=polynomial-sum-p (formula L2) Pos))
;;   (description "Forward application of Poly-Plus."))
;;                                        
;;(defun polynom=polynomial-sum-p (formula Pos)
;;  (let* ((term (data~struct-at-position formula pos))
;;         (vars (polynom=get-abstr-var term))
;;         (scope (polynom=innermost-scope term)))
;;    (and vars
;;         (term~appl-p scope)
;;         (polynom=plus-function-p (data~appl-function scope))
;;         (cacom=polynomial-p (first (data~appl-arguments scope)))
;;         (cacom=polynomial-p (second (data~appl-arguments scope)))
;;         ()))
;;
;;
;;(defun polynom=poly-plus-b (formula pos)
;;  (let* ((term (data~struct-at-position formula pos))
;;         (scope (polynom=innermost-scope term))
;;         (arg1 (car (data~appl-arguments scope)))
;;         (arg2 (cadr (data~appl-arguments scope)))
;;         (varnum (length 

;;
;; A-direction
;;


(tac~deftactic poly-plus-a poly-plus (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations )
   (sideconditions (polynom=addition-p (formula L1) Pos)
		   (polynom=poly-plus-ap (formula L1) (formula L2) Pos))
   (description "Application of Poly-Plus."))


(defun polynom=poly-plus-ap (L1 L2 Pos)
  (lam~equal-p (polynom=poly-plus-f L1 Pos) L2))



(defun polynom=expand-poly-plus (outline parameters)
  (let* ((conc (car outline))
	 (precond (cadr outline))
	 (pos (car parameters))
	 (addargs (polynom=get-addargs (data~struct-at-position (node~formula precond) pos) 2)))
    (tacl~init outline)
    (tacl~sequence
     (rewr-res ('apply-rewrite (list nil precond) (list pos 'P-Add-Real1 nil addargs)))
     (lambda-res ('lambda (list conc (car rewr-res)) nil)))
    (tacl~end)))


;;; Poly-Split-Plus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic poly-split-plus
                 (outline-mappings (((existent existent) poly-split-plus-a)
                                    ((existent nonexistent) poly-split-plus-b)))
                 (expansion-function polynom=expand-poly-split-plus)
                 (parameter-types position)
                 (help "Polynomial addition backward (e+(d+c))=((a+d)+(b+c))."))

;;(com~defcommand poly-split-plus
;;  (argnames line1 line2 position)
;;  (argtypes ndline ndline position)
;;  (arghelps "An open line containing the sum of two polynomials"
;;            "A closed line containing the sum of their monomials"
;;            "The position of this sum")
;;  (function polynom=poly-split-plus)
;;  (frag-cats tactics polynomial)
;;  (defaults)
;;  (log-p t)
;;  (help "Polynomial addition backward  (p (+) q) = lam z. (p z + q z)."))
;;
;;(defun polynom=poly-split-plus (l1 l2 pos)
;;  (infer~compute-outline 'poly-split-plus (list l1 l2) (list pos)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;  decided to put it in one command, as long as there are no backward and forward directions


;;
;; Poly Split PLUS directions
;;


(tac~deftactic poly-split-plus-b poly-split-plus (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=poly-plus-f (formula L2) Pos)))
   (sideconditions (polynom=addition-p (formula L2) Pos))
   (description "Forward application of Poly-Plus."))

;;
;; A-direction
;;


(tac~deftactic poly-split-plus-a poly-split-plus (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=addition-p (formula L2) Pos)
		   (polynom=poly-plus-ap (formula L2) (formula L1) Pos))
   (description "Application of Poly-Plus."))

(defun polynom=expand-poly-split-plus (outline parameters)
  (let* ((conc (car outline))
	 (precond (cadr outline))
	 (pos (car parameters))
	 (addargs (polynom=get-addargs (data~struct-at-position (node~formula conc) pos) 2)))
    (tacl~init outline)
    (tacl~sequence
     (rewr-res ('apply-rewrite (list conc nil) (list pos 'P-Add-Real1 nil addargs)))
     (lambda-res ('lambda (list (cadr rewr-res) precond) nil)))
    (tacl~end)))


;;  POP-FIRST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic pop-first
		 (outline-mappings (((existent existent) pop-first-a)
				    ((nonexistent existent) pop-first-f)
				    ((existent nonexistent) pop-first-b)))
		 (expansion-function polynom=expand-pop-first)
		 (parameter-types position)
		 (help "Associativity ((a+b)+c)=(a+(b+c))."))

(com~defcommand pop-first
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A line containing (a+b)+c"
            "A line containing a+(b+c)"
            "The position of the subterm")
  (function polynom=pop-first)
  (frag-cats tactics polynomial)
  (defaults)
  (log-p t)
  (help "Parameter swap within term ((a+b)+c)=(a+(b+c))."))

(defun polynom=pop-first (l1 l2 pos)
  (infer~compute-outline 'pop-first (list l1 l2) (list pos)))

;;
;; Pop-First Directions
;;

(tac~deftactic pop-first-f pop-first (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=pop-first-f (formula L1) POS)))
   (sideconditions (polynom=pop-first-fp (formula L1) Pos))
   (description "Forward application of Pop-First."))

(defun polynom=pop-first-fp (L1 Pos)
  (polynom=applicable-assoc-plus-p
   (data~struct-at-position L1 Pos)))

(defun polynom=pop-first-f (L1 Pos)
  (let* ((Subterm (data~struct-at-position L1 Pos))
         (Shift (polynom=apply-assoc Subterm)))
    (data~replace-at-position L1 Pos Shift)))

(tac~deftactic pop-first-a pop-first (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=pop-first-ap
                    (formula L1) (formula L2) Pos)) 
   (description "Test application of Pop-First."))


(defun polynom=pop-first-ap (L1 L2 Pos)
  (lam~equal-p (polynom=pop-first-f L1 Pos) L2))


;;
;; Backward
;;

(tac~deftactic pop-first-b pop-first (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=pop-first-b (formula L2) POS)))
   (sideconditions (polynom=pop-first-bp (formula L2) Pos))
   (description "Backward application of Pop-First."))

(defun polynom=pop-first-bp (L2 Pos)
  (polynom=applicable-assoc-plus-backwards-p
   (data~struct-at-position L2 Pos)))

(defun polynom=pop-first-b (L2 Pos)
  (let* ((Subterm (data~struct-at-position L2 Pos))
         (Shift (polynom=apply-assoc-backwards Subterm)))
    (data~replace-at-position L2 Pos Shift)))

(defun polynom=expand-pop-first (outline parameters)
  (let ((conc (car outline))
	(precond (cadr outline))
	(pos (car parameters)))
    (tacl~init outline)
    (tacl~apply 'apply-rewrite (list conc precond) (list pos 'a-plus-real nil nil))
    (tacl~end)))


;; pop-second
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic pop-second
		 (outline-mappings (((existent existent) pop-second-a)
				    ((nonexistent existent) pop-second-f)
				    ((existent nonexistent) pop-second-b)))
		 (expansion-function polynom=expand-pop-second)
		 (parameter-types position)
		 (help "Parameter swap within term (a+(b+c))=(b+(a+c))."))

(com~defcommand pop-second
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A line containing a+(b+c)"
            "A line containing b+(a+c)"
            "The position of the subterm")
  (function polynom=pop-second)
  (frag-cats tactics polynomial)
  (defaults)
  (log-p t)
  (help "Parameter swap within term ((a+b)+c)=(a+(b+c))."))

(defun polynom=pop-second (l1 l2 pos)
  (infer~compute-outline 'pop-second (list l1 l2) (list pos)))



;;
;; Pop second directions
;;


(tac~deftactic pop-second-f pop-second (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=pop-second-f (formula L1) Pos)))
   (sideconditions (polynom=pop-second-fp (formula L1) Pos))
   (description "Forward application of Pop-Second."))


(defun polynom=pop-second-fp (L1 Pos)
  (polynom=applicable-commu-plus-p
   (data~struct-at-position L1 Pos)))


(defun polynom=apply-swap (Term)
  (if (polynom=applicable-assoc-plus-backwards-p Term)
      (let ((Aterm (first (data~appl-arguments Term)))
	    (Bterm (first (data~appl-arguments (second (data~appl-arguments Term)))))
	    (Cterm (second (data~appl-arguments (second (data~appl-arguments Term)))))
	    (Opp (data~appl-function Term)))
	  (term~appl-create Opp
		       (list Bterm
			     (term~appl-create
			      Opp
			      (list Aterm Cterm)))))
    (polynom=apply-commu Term)))

(defun polynom=pop-second-f (L1 Pos)
  (let* ((Subterm (data~struct-at-position L1 Pos))
         (Shift (polynom=apply-swap Subterm)))
    (data~replace-at-position L1 Pos Shift)))


;;
;; A-direction
;;


(tac~deftactic pop-second-a pop-second (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=pop-second-fp (formula L1) Pos)
		   (polynom=pop-second-ap (formula L1) (formula L2) Pos))
   (description "Test application of Pop-Second."))


(defun polynom=pop-second-ap (L1 L2 Pos)
  (lam~equal-p (polynom=pop-second-f L1 Pos) L2))

;;
;; B-Direction
;;

(tac~deftactic pop-second-b pop-second (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=pop-second-f (formula L2) Pos)))
   (sideconditions (polynom=pop-second-fp (formula L2) Pos))
   (description "Backward application of Pop-Second."))


(defun polynom=expand-pop-second (outline parameters)
  (let* ((conc (car outline))
	 (precond (cadr outline))
	 (pos (car parameters))
	 (term (data~struct-at-position (node~formula precond) pos))
	 (pattern (pdsj~outline-pattern (node~justification conc)))
	 (rl-switch1 (if (and (infer~nonexistent-pattern-p (car pattern))
			      (infer~existent-pattern-p (cadr pattern)))
			 'srl nil))
	 (rl-switch2 (if rl-switch1 nil 'rl)))
    (tacl~init outline)
    (if (not (polynom=sum-p (cadr (data~appl-arguments term))))
	(tacl~apply 'apply-rewrite (list conc precond) (list pos 'c-plus-real nil nil))
      (tacl~sequence
       (rewr1-res ('apply-rewrite (list nil precond) (list pos 'a-plus-real rl-switch1 nil)))
       (rewr2-res ('apply-rewrite (list nil (car rewr1-res)) (list (pos~add-end 1 pos) 'c-plus-real nil nil)))
       (rewr3-res ('apply-rewrite (list conc (car rewr2-res)) (list pos 'a-plus-real rl-switch2 nil)))))
    (tacl~end)))

;;;
;; pushing: backward application of pop-rules
;;;

;; PUSH-FIRST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~deftactic push-first
		 (outline-mappings (((existent existent) push-first-a)
				    ((nonexistent existent) push-first-f)
				    ((existent nonexistent) push-first-b)))
		 (expansion-function polynom=expand-push-first)
		 (parameter-types position)
		 (help "Associativity (a+(b+c))=((a+b)+c)."))

(com~defcommand push-first
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A line containing (a+b)+c"
            "A line containing a+(b+c)"
            "The position of the subterm")
  (function polynom=push-first)
  (frag-cats tactics polynomial)
  (defaults)
  (log-p t)
  (help "Parameter swap within term ((a+b)+c)=(a+(b+c))."))

(defun polynom=push-first (l1 l2 pos)
  (infer~compute-outline 'push-first (list l1 l2) (list pos)))

;;
;; Push-First Directions
;;

(tac~deftactic push-first-f push-first (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=pop-first-b (formula L1) POS)))
   (sideconditions (polynom=pop-first-bp (formula L1) Pos))
   (description "Forward application of Push-First."))

(tac~deftactic push-first-a push-first (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=pop-first-ap
                    (formula L2) (formula L1) Pos)) 
   (description "Test application of Push-First."))

;;
;; Backward
;;

(tac~deftactic push-first-b push-first (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=pop-first-f (formula L2) POS)))
   (sideconditions (polynom=pop-first-fp (formula L2) Pos))
   (description "Backward application of Push-First."))


(defun polynom=expand-push-first (outline parameters)
  (let ((conc (car outline))
	(precond (cadr outline))
	(pos (car parameters)))
    (tacl~init outline)
    (tacl~apply 'apply-rewrite (list conc precond) (list pos 'a-plus-real nil nil))
    (tacl~end)))


;;; push-second
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;(infer~deftactic push-second
;;                 (outline-mappings (((existent existent) pop-second-a)
;;                                    ((existent nonexistent) pop-second-f)
;;                                    ((nonexistent existent) pop-second-b)))
;;                 (expansion-function polynom=expand-push-second)
;;                 (parameter-types position)
;;                 (help "Parameter swap within term (a+(b+c))=(b+(a+c))."))
;;
;;
;;(defun polynom=expand-push-second (outline parameters)
;;  (let* ((conc (car outline))
;;         (precond (cadr outline))
;;         (pos (car parameters))
;;         (term (data~struct-at-position (node~formula precond) pos)))
;;    (tacl~init outline)
;;    (if (not (polynom=sum-p (cadr (data~appl-arguments term))))
;;        (tacl~apply 'apply-rewrite (list conc precond) (list pos 'c-plus-real nil nil))
;;      (tacl~sequence
;;       (rewr1-res ('apply-rewrite (list nil precond) (list pos 'a-plus-real nil nil)))
;;       (rewr2-res ('apply-rewrite (list nil (car rewr1-res)) (list (pos~add-end 1 pos) 'c-plus-real nil nil)))
;;       (rewr3-res ('apply-rewrite (list conc (car rewr2-res)) (list pos 'a-plus-real nil nil)))))
;;    (tacl~end)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Monomial addition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic mon-plus
                 (outline-mappings (((existent existent) mon-plus-a)
                                    ((nonexistent existent) mon-plus-f)))
                 (expansion-function polynom=expand-mon-plus)
                 (parameter-types position)
                 (help "Monomial addition ((a+d)+(b+c))=(e+(d+c))."))

(com~defcommand mon-plus
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A line containing a polynomial of the form: (a+d)+(b+c)"
            "A line containing a polynomial of the form: e+(d+c)"
            "The position of this polynomial")
  (function polynom=mon-plus)
  (frag-cats tactics polynomial)
  (defaults)
  (log-p t)
  (help "Monomial addition ((a+d)+(b+c))=(e+(d+c))."))

(defun polynom=mon-plus (l1 l2 pos)
  (infer~compute-outline 'mon-plus (list l2 l1) (list pos)))



;;
;; Mon PLUS directions
;;


(tac~deftactic mon-plus-f mon-plus (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=mon-plus-f (formula L1) Pos)))
   (sideconditions (polynom=applicable-mon-plus-p (formula L1) Pos))
   (description "Forward application of Mon-Plus."))

(defun polynom=applicable-mon-plus-p (formula Pos)
  (let ((term (data~struct-at-position formula pos)))
    (and (polynom=sum-p Term)
	 (= (length (data~appl-arguments Term)) 2)
	 (let ((arg1 (first (data~appl-arguments term)))
	       (arg2 (second (data~appl-arguments term))))
	   (or (and (cacom~coeff&monomial-p arg1)
		    (cacom~coeff&monomial-p arg2)
		    (cacom~exponent-equal arg1 arg2))
	       (and (cacom~coeff&monomial-p arg1)
		    (cacom~exponent-equal arg1 (first (data~appl-arguments arg2))))
	       (and (cacom~coeff&monomial-p arg2)
		    (cacom~exponent-equal arg2 (first (data~appl-arguments arg1))))
	       (cacom~exponent-equal (first (data~appl-arguments arg1))
				     (first (data~appl-arguments arg2))))))))

	   
					
(defun polynom=polynomial-sum-p (formula Pos)
  (let ((term (data~struct-at-position formula pos)))
    (and (polynom=sum-p Term)
	 (= (length (data~appl-arguments Term)) 2)
	 (cacom~polynomial-p (first (data~appl-arguments Term)))
	 (cacom~polynomial-p (second (data~appl-arguments Term))))))

(defun polynom=exponent-equal-two-mon-p (formula Pos)
  (let* ((term (data~struct-at-position formula pos))
	 (arg1 (first (data~appl-arguments term)))
	 (arg2 (second (data~appl-arguments term))))
    (or (and (cacom~coeff&monomial-p arg1)
	     (cacom~coeff&monomial-p arg2)
	     (cacom~exponent-equal arg1 arg2))
	(and (cacom~coeff&monomial-p arg1)
	     (cacom~exponent-equal arg1 (first (data~appl-arguments arg2))))
	(and (cacom~coeff&monomial-p arg2)
	     (cacom~exponent-equal arg2 (first (data~appl-arguments arg1))))
	(cacom~exponent-equal (first (data~appl-arguments arg1))
			      (first (data~appl-arguments arg2))))))

(defun polynom=apply-monomial-addition (term)
  (let* ((arg1 (first (data~appl-arguments term)))
	 (arg2 (second (data~appl-arguments term))))
     (cond ((and (cacom~coeff&monomial-p arg1)
		 (cacom~coeff&monomial-p arg2))
	    (polynom=product-create (polynom=create-number
				     (+ (cacom~coefficient arg1)
					(cacom~coefficient arg2)))
				    (cacom~monomial arg1)))
	   ((cacom~coeff&monomial-p arg1)
	    (polynom=sum-create
	     (polynom=product-create (polynom=create-number
				      (+ (cacom~coefficient arg1)
					 (cacom~coefficient (first (data~appl-arguments arg2)))))
				     (cacom~monomial arg1))
	     (second (data~appl-arguments arg2))))
	   ((cacom~coeff&monomial-p arg2)
	    (polynom=sum-create
	     (polynom=product-create (polynom=create-number
				  (+ (cacom~coefficient arg2)
				    (cacom~coefficient (first (data~appl-arguments arg1)))))
				 (cacom~monomial arg2))
	     (second (data~appl-arguments arg1))))
	   (t 
	    (polynom=sum-create
	       (polynom=product-create (polynom=create-number
				    (+ (cacom~coefficient (first (data~appl-arguments arg2)))
				      (cacom~coefficient (first (data~appl-arguments arg1)))))
				   (cacom~monomial (first (data~appl-arguments arg1))))
	       (polynom=sum-create (second (data~appl-arguments arg1))
				   (second (data~appl-arguments arg2)))))
	   )))

 
(defun polynom=mon-plus-f (L1 Pos)
  (let* ((Subterm (data~struct-at-position L1 Pos))
         (Sum (polynom=apply-monomial-addition Subterm)))
    (data~replace-at-position L1 Pos Sum)))


;;
;; A-direction
;;


(tac~deftactic mon-plus-a mon-plus (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=applicable-mon-plus-p (formula L1) Pos)
		   (polynom=mon-plus-ap (formula L1) (formula L2) Pos))
   (description "Application of Mon-Plus."))


(defun polynom=mon-plus-ap (L1 L2 Pos)
  (let ((term  (polynom=mon-plus-f L1 Pos)))
    (lam~equal-p term L2)))


(defun polynom=expand-mon-plus (outline parameters)
  (let* ((conc (car outline))
	 (precond (cadr outline))
	 (pos (car parameters))
	 (arg1 (car (data~appl-arguments (data~struct-at-position (node~formula precond) pos))))
	 (arg2 (cadr (data~appl-arguments (data~struct-at-position (node~formula precond) pos)))))
    (tacl~init outline)
    (cond ((and (polynom=sum-p arg1)
	        (polynom=sum-p arg2))  
	   (let* ((rewr4-res 
		   (tacl~sequence
		    (rewr1-res ('apply-rewrite (list nil precond) (list pos 'a-plus-real 'rl nil)))
		    (rewr2-res ('apply-rewrite (list nil (car rewr1-res))
					       (list (pos~add-end 1 pos) 'c-plus-real nil nil)))
		    (rewr3-res ('apply-rewrite (list nil (car rewr2-res))
					       (list (pos~add-end 1 pos) 'a-plus-real nil nil)))
		    (rewr4-res ('apply-rewrite (list nil (car rewr3-res))
					       (list (pos~add-end 1 (pos~add-end 1 pos)) 'Mon-Add-Real1 nil nil)))))
		  (simpl-res (polynom=simplify-num-application (car rewr4-res) (pos~add-end 1 (pos~add-end 1 (pos~add-end 1 pos))))))
	     (tacl~apply 'apply-rewrite (list conc (car simpl-res)) (list pos 'a-plus-real nil nil))))
	  ((and (polynom=sum-p arg1)
	        (not (polynom=sum-p arg2))) 
	   (let ((rewr3-res
		  (tacl~sequence
		   (rewr1-res ('apply-rewrite (list nil precond) (list pos 'c-plus-real nil nil)))
		   (rewr2-res ('apply-rewrite (list nil (car rewr1-res)) (list pos 'a-plus-real nil nil)))
		   (rewr3-res ('apply-rewrite (list nil (car rewr2-res))
					      (list (pos~add-end 1 pos) 'Mon-Add-Real1 nil nil))))))
	     (polynom=simplify-num-application  (car rewr3-res) (pos~add-end 1 (pos~add-end 1 pos)) conc)))
	  ((and (not (polynom=sum-p arg1))
	        (polynom=sum-p arg2)) 
	   (let ((rewr2-res
		  (tacl~sequence
		   (rewr1-res ('apply-rewrite (list nil precond) (list pos 'a-plus-real nil nil)))
		   (rewr2-res ('apply-rewrite (list nil (car rewr1-res))
					      (list (pos~add-end 1 pos) 'Mon-Add-Real1 nil nil))))))
	     (polynom=simplify-num-application (car rewr2-res) (pos~add-end 1 (pos~add-end 1 pos)) conc)))
	  ((and (not (polynom=sum-p arg1))
	        (not (polynom=sum-p arg2))) 
	   (let ((rewr-res (tacl~apply 'apply-rewrite (list nil precond) (list pos 'Mon-Add-Real1 nil nil))))
	     (polynom=simplify-num-application (car rewr-res) (pos~add-end 1 pos) conc))))
    (tacl~end)))

;;; Mon-Split-Plus

(infer~deftactic mon-split-plus
                 (outline-mappings (((existent existent) mon-split-plus-a)
                                    ((existent nonexistent) mon-split-plus-b)))
                 (expansion-function polynom=expand-mon-split-plus)
                 (parameter-types position)
                 (help "Monomial addition backward (e+(d+c))=((a+d)+(b+c))."))

(com~defcommand mon-split-plus
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A line containing a polynomial of the form: (a+d)+(b+c)"
            "A line containing a polynomial of the form: e+(d+c)"
            "The position of this polynomial")
  (function polynom=mon-split-plus)
  (frag-cats tactics polynomial)
  (defaults)
  (log-p t)
  (help "Monomial addition backward (e+(d+c))=((a+d)+(b+c))."))

(defun polynom=mon-split-plus (l1 l2 pos)
  (infer~compute-outline 'mon-split-plus (list l1 l2) (list pos)))



;;
;; Mon Split PLUS directions
;;


(tac~deftactic mon-split-plus-b mon-split-plus (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=mon-plus-f (formula L2) Pos)))
   (sideconditions (polynom=applicable-mon-plus-p (formula L2) Pos))
   (description "Forward application of Mon-Plus."))

;;
;; A-direction
;;


(tac~deftactic mon-split-plus-a mon-split-plus (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=applicable-mon-plus-p (formula L2) Pos)
		   (polynom=mon-plus-ap (formula L2) (formula L1) Pos))
   (description "Application of Mon-Plus."))


(defun polynom=expand-mon-split-plus (outline parameters)
  (let* ((conc (car outline))
	 (precond (cadr outline))
	 (pos (car parameters))
	 (arg1 (car (data~appl-arguments (data~struct-at-position (node~formula conc) pos))))
	 (arg2 (cadr (data~appl-arguments (data~struct-at-position (node~formula conc) pos)))))
    (tacl~init outline)
    (cond ((and (polynom=sum-p arg1)
	        (polynom=sum-p arg2))  
	   (let* ((rewr4-res
		   (tacl~sequence
		    (rewr1-res ('apply-rewrite (list conc nil) (list pos 'a-plus-real 'rl nil)))
		    (rewr2-res ('apply-rewrite (list (cadr rewr1-res) nil)
					       (list (pos~add-end 1 pos) 'c-plus-real nil nil)))
		    (rewr3-res ('apply-rewrite (list (cadr rewr2-res) nil)
					       (list (pos~add-end 1 pos) 'a-plus-real nil nil)))
		    (rewr4-res ('apply-rewrite (list (cadr rewr3-res) nil)
					       (list (pos~add-end 1 (pos~add-end 1 pos)) 'Mon-Add-Real1 nil nil)))))
		  (simpl-res (polynom=simplify-num-application (cadr rewr4-res) (pos~add-end 1 (pos~add-end 1 (pos~add-end 1 pos))))))
	     (tacl~apply 'apply-rewrite (list (cadr simpl-res) precond) (list pos 'a-plus-real nil nil))))
	  ((and (polynom=sum-p arg1)
	        (not (polynom=sum-p arg2))) 
	   (let ((rewr3-res
		  (tacl~sequence
		   (rewr1-res ('apply-rewrite (list conc nil)
					      (list pos 'c-plus-real nil nil)))
		   (rewr2-res ('apply-rewrite (list (cadr rewr1-res) nil) (list pos 'a-plus-real 'rl nil)))
		   (rewr3-res ('apply-rewrite (list (cadr rewr2-res) nil)
					      (list (pos~add-end 1 pos) 'Mon-Add-Real1 nil nil))))))
	     (polynom=simplify-num-application  (cadr rewr3-res) (pos~add-end 1 (pos~add-end 1 pos)) precond)))
	  ((and (not (polynom=sum-p arg1))
	        (polynom=sum-p arg2)) 
	   (let ((rewr2-res
		  (tacl~sequence
		   (rewr1-res ('apply-rewrite (list conc nil) (list pos 'a-plus-real nil nil)))
		   (rewr2-res ('apply-rewrite (list (cadr rewr1-res) nil)
					      (list (pos~add-end 1 pos) 'Mon-Add-Real1 nil nil))))))
	     (polynom=simplify-num-application (cadr rewr2-res) (pos~add-end 1 (pos~add-end 1 pos)) precond)))
	  ((and (not (polynom=sum-p arg1))
	        (not (polynom=sum-p arg2))) 
	   (let ((rewr-res (tacl~apply 'apply-rewrite (list conc nil) (list pos 'Mon-Add-Real1 nil nil))))
	     (polynom=simplify-num-application (cadr rewr-res) (pos~add-end 1 pos) precond))))
    (tacl~end)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tactics for Polynomial Multiplication
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; POLY-TIMES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic poly-times
                 (outline-mappings (((existent existent) poly-times-a)
                                    ((nonexistent existent) poly-times-f)))
                 (expansion-function polynom=expand-poly-times)
                 (parameter-types position)
                 (help "Polynomial multiplication (p (*) q) = lam z. (p z * q z)."))

(com~defcommand poly-times
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A line containing the product of two polynomials"
            "A line containing the product of their monomials"
	    "The position of this polynomial")
  (function polynom=poly-times)
  (frag-cats tactics polynomial)
  (defaults)
  (log-p t)
  (help "Polynomial muliplication (p (*) q) = lam z. (p z * q z)."))

(defun polynom=poly-times (l1 l2 pos)
  (if (pdsn~open-node-p l1)
      (infer~compute-outline 'poly-split-times (list l1 l2) (list pos))
    (infer~compute-outline 'poly-times (list l2 l1) (list pos))))



;;
;; Poly TIMES directions
;;


(tac~deftactic poly-times-f poly-times (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=poly-times-f (formula L1) Pos)))
   (sideconditions (polynom=multiplication-p (formula L1) Pos))
   (description "Forward application of Poly-Times."))

(defun polynom=multiplication-p (formula Pos)
  (let ((term (data~struct-at-position formula pos)))
    (and (term~appl-p Term)
	 (polynom=ptimes-function-p (data~appl-function Term)))))

					
(defun polynom=apply-polynomial-multiplication (term)
  (let* ((addargs (polynom=get-addargs term 2))
	 (arg1 (first (data~appl-arguments term)))
	 (arg2 (second (data~appl-arguments term)))
	 (oldvarlist (data~abstr-binder arg1))
	 (newvars (polynom=new-number-vars "Z" (length oldvarlist)))
	 (abstr (beta~normalize
		 (term~abstr-create newvars
				    (polynom=product-create (term~appl-create arg1 newvars)
							    (term~appl-create arg2 newvars))))))
    (if addargs
	(term~appl-create abstr addargs)
      abstr)))

 
(defun polynom=poly-times-f (L1 Pos)
  (let* ((Subterm (data~struct-at-position L1 Pos))
         (Sum (polynom=apply-polynomial-multiplication Subterm)))
    (data~replace-at-position L1 Pos Sum)))

;;
;; A-direction
;;


(tac~deftactic poly-times-a poly-times (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations )
   (sideconditions (polynom=multiplication-p (formula L1) Pos)
		   (polynom=poly-times-ap (formula L1) (formula L2) Pos))
   (description "Application of Poly-Times."))


(defun polynom=poly-times-ap (L1 L2 Pos)
  (lam~equal-p (polynom=poly-times-f L1 Pos) L2))



(defun polynom=expand-poly-times (outline parameters)
  (let* ((conc (car outline))
	 (precond (cadr outline))
	 (pos (car parameters))
	 (addargs (polynom=get-addargs (data~struct-at-position (node~formula precond) pos) 2)))
    (tacl~init outline)
    (tacl~sequence
     (rewr-res ('apply-rewrite (list nil precond) (list pos 'P-Times-Real1 nil addargs)))
     (lambda-res ('lambda (list conc (car rewr-res)) nil)))
    (tacl~end)))


;;; Poly-Split-Times
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic poly-split-times
                 (outline-mappings (((existent existent) poly-split-times-a)
                                    ((existent nonexistent) poly-split-times-b)))
                 (expansion-function polynom=expand-poly-split-times)
                 (parameter-types position)
                 (help "Polynomial multiplication backward (e+(d+c))=((a+d)+(b+c))."))

;;
;; Poly Split TIMES directions
;;


(tac~deftactic poly-split-times-b poly-split-times (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=poly-times-f (formula L2) Pos)))
   (sideconditions (polynom=multiplication-p (formula L2) Pos))
   (description "Forward application of Poly-Times."))

;;
;; A-direction
;;


(tac~deftactic poly-split-times-a poly-split-times (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=multiplication-p (formula L2) Pos)
		   (polynom=poly-times-ap (formula L2) (formula L1) Pos))
   (description "Application of Poly-Times."))

(defun polynom=expand-poly-split-times (outline parameters)
  (let* ((conc (car outline))
	 (precond (cadr outline))
	 (pos (car parameters))
	 (addargs (polynom=get-addargs (data~struct-at-position (node~formula conc) pos) 2)))
    (tacl~init outline)
    (tacl~sequence
     (rewr-res ('apply-rewrite (list conc nil) (list pos 'P-Times-Real1 nil addargs)))
     (lambda-res ('lambda (list (cadr rewr-res) precond) nil)))
    (tacl~end)))


;;
;; Monomial multiplication
;;

(infer~deftactic mon-times
                 (outline-mappings (((existent existent) mon-times-a)
                                    ((nonexistent existent) mon-times-f)))
                 (expansion-function polynom=expand-mon-times)
                 (parameter-types position)
                 (help "Monomial multiplication (a*(b+c))=(e+(a*c))."))

(com~defcommand mon-times
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A line containing a polynomial of the form: (e+(a*c))"
            "A line containing a polynomial of the form: (a*(b+c))"
            "The position of this polynomial")
  (function polynom=mon-times)
  (frag-cats tactics polynomial)
  (defaults)
  (log-p t)
  (help "Monomial multiplication (a*(b+c))=(e+(a*c))."))

(defun polynom=mon-times (l1 l2 pos)
  (infer~compute-outline 'mon-times (list l2 l1) (list pos)))



;;
;; Mon TIMES directions
;;


(tac~deftactic mon-times-f mon-times (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=mon-times-f (formula L1) Pos)))
   (sideconditions (polynom=mon-times-fp (formula L1) Pos))
   (description "Forward application of Mon-Times."))

(defun polynom=mon-times-fp (formula Pos)
  (let ((term (data~struct-at-position formula pos)))
    (and (polynom=product-p Term)
	 (= (length (data~appl-arguments Term)) 2)
	 (let ((arg1 (first (data~appl-arguments term)))
	       (arg2 (second (data~appl-arguments term))))
	   (or (and (cacom~coeff&monomial-p arg1)
		    (cacom~coeff&monomial-p arg2))
	       (and (cacom~coeff&monomial-p arg1)
		    (polynom=sum-p arg2)
		    (cacom~coeff&monomial-p (first (data~appl-arguments arg2)))))))))
					
(defun polynom=multiply-monomials (mon1 mon2)
  (let* ((fmon1 (first (data~appl-arguments mon1)))
	 (fmon2 (first (data~appl-arguments mon2)))
	 (mon1? (cacom~monomial-p fmon1)))
    (if mon1?
	(polynom=product-create
	 (polynom=exponent-create
	  (first (data~appl-arguments fmon1))
	  (polynom=create-number
	   (+ (cacom~exponent fmon1)
	      (cacom~exponent fmon2))))
	 (polynom=multiply-monomials (second (data~appl-arguments mon1))
				     (second (data~appl-arguments mon2))))
      (polynom=exponent-create
       (first (data~appl-arguments mon1))
       (polynom=create-number
	(+ (cacom~exponent mon1)
	   (cacom~exponent mon2)))))))
	 
(defun polynom=apply-monomial-multiplication (term)
  (let* ((arg1 (first (data~appl-arguments term)))
	 (arg2 (second (data~appl-arguments term))))
    (if (and (cacom~coeff&monomial-p arg1)
	     (cacom~coeff&monomial-p arg2))
	(polynom=product-create (polynom=create-number
				 (* (cacom~coefficient arg1)
				    (cacom~coefficient arg2)))
				(polynom=multiply-monomials (cacom~monomial arg1)
							    (cacom~monomial arg2)))
      (polynom=sum-create
       (polynom=product-create (polynom=create-number
				(* (cacom~coefficient arg1)
				   (cacom~coefficient (first (data~appl-arguments arg2)))))
			       (polynom=multiply-monomials (cacom~monomial arg1)
							   (cacom~monomial (first (data~appl-arguments arg2)))))
       (polynom=product-create arg1 (second (data~appl-arguments arg2)))))))
 
(defun polynom=mon-times-f (L1 Pos)
  (let* ((Subterm (data~struct-at-position L1 Pos))
         (Prod (polynom=apply-monomial-multiplication Subterm)))
    (data~replace-at-position L1 Pos Prod)))


;;
;; A-direction
;;


(tac~deftactic mon-times-a mon-times (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=mon-times-fp (formula L1) Pos)
		   (polynom=mon-times-ap (formula L1) (formula L2) Pos))
   (description "Application of Mon-Times."))


(defun polynom=mon-times-ap (L1 L2 Pos)
  (lam~equal-p (polynom=mon-times-f L1 Pos) L2))



(defun polynom=expand-mon-times (outline parameters)
  (let* ((conc (car outline))
	 (precond (cadr outline))
	 (pos (car parameters))
	 (arg1 (car (data~appl-arguments (data~struct-at-position (node~formula precond) pos))))
	 (arg2 (cadr (data~appl-arguments (data~struct-at-position (node~formula precond) pos)))))
    (tacl~init outline)
    (if (polynom=sum-p arg2)
	(let* ((rewr2-res
		(tacl~sequence
		 (rewr1-res ('apply-rewrite (list nil precond) (list pos 'dist-left-real nil nil)))
		 (rewr2-res ('apply-rewrite (list nil (car rewr1-res))
					    (list (pos~add-end 1 pos) 'mon-multip-real1 nil nil)))))
	       (simpl1-res (polynom=simplify-num-application (car rewr2-res) (pos~add-end 1 (pos~add-end 1 pos)))))
	  (polynom=simplify-num-application (car simpl1-res) (pos~add-end 2 (pos~add-end 2 (pos~add-end 1 pos))) conc))
      (let* ((rewr1-res (tacl~apply 'apply-rewrite (list nil precond) (list pos 'mon-multip-real1 nil nil)))
	     (simpl1-res (polynom=simplify-num-application (car rewr1-res) (pos~add-end 1 pos))))
	(polynom=simplify-num-application (car simpl1-res) (pos~add-end 2 (pos~add-end 2 pos)) conc)))
    (tacl~end)))

;;; Mon-Split-Times
(infer~deftactic mon-split-times
                 (outline-mappings (((existent existent) mon-split-times-a)
                                    ((existent nonexistent) mon-split-times-b)))
                 (expansion-function polynom=expand-mon-split-times)
                 (parameter-types position)
                 (help "Monomial multiplication backward (e+(a*c))=(a*(b+c))."))

(com~defcommand mon-split-times
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A line containing a polynomial of the form: (a*(b+c))"
            "A line containing a polynomial of the form: (e+(a*c))"
            "The position of this polynomial")
  (function polynom=mon-split-times)
  (frag-cats tactics polynomial)
  (defaults)
  (log-p t)
  (help "Monomial multiplication backward (e+(d+c))=((a+d)+(b+c))."))

(defun polynom=mon-split-times (l1 l2 pos)
  (infer~compute-outline 'mon-split-times (list l1 l2) (list pos)))



;;
;; Mon Split TIMES directions
;;


(tac~deftactic mon-split-times-b mon-split-times (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=mon-times-f (formula L2) Pos)))
   (sideconditions (polynom=mon-times-fp (formula L2) Pos))
   (description "Forward application of Mon-Times."))

;;
;; A-direction
;;


(tac~deftactic mon-split-times-a mon-split-times (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=mon-times-fp (formula L2) Pos)
		   (polynom=mon-times-ap (formula L2) (formula L1) Pos))
   (description "Application of Mon-Times."))


(defun polynom=expand-mon-split-times (outline parameters)
  (let* ((conc (car outline))
	 (precond (cadr outline))
	 (pos (car parameters))
	 (arg1 (car (data~appl-arguments (data~struct-at-position (node~formula conc) pos))))
	 (arg2 (cadr (data~appl-arguments (data~struct-at-position (node~formula conc) pos)))))
    (tacl~init outline)
    (if (polynom=sum-p arg2)
	(let* ((rewr2-res
		(tacl~sequence
		 (rewr1-res ('apply-rewrite (list conc nil) (list pos 'dist-left-real nil nil)))
		 (rewr2-res ('apply-rewrite (list (cadr rewr1-res) nil)
				    (list (pos~add-end 1 pos) 'mon-multip-real1 nil nil)))))
	       (simpl1-res (polynom=simplify-num-application (cadr rewr2-res) (pos~add-end 1 (pos~add-end 1 pos)))))
	  (polynom=simplify-num-application (cadr simpl1-res) (pos~add-end 2 (pos~add-end 2 (pos~add-end 1 pos))) precond))
      (let* ((rewr1-res (tacl~apply 'apply-rewrite (list conc nil) (list pos 'mon-multip-real1 nil nil)))
	     (simpl1-res (polynom=simplify-num-application (cadr rewr1-res) (pos~add-end 1 pos))))
	(polynom=simplify-num-application (cadr simpl1-res) (pos~add-end 2 (pos~add-end 2 pos)) precond)))
    (tacl~end)))



;;
;; Distributivity Right
;;



(infer~deftactic distribute-right
                 (outline-mappings (((existent existent)
                                     distribute-right-a)
                                    ((existent nonexistent)
                                     distribute-right-b)
                                    ((nonexistent existent)
                                     distribute-right-f)))
                 (expansion-function polynom=expand-distribute-right)
                 (parameter-types position)
                 (help "Monomial distributivity ((a+b)*c)=(a*c+b*c)."))

(com~defcommand distribute-right
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A line containing a polynomial of the form: (a*c+b*c)"
            "A line containing a polynomial of the form: (a+b)*c"
            "A position")
  (function polynom=distribute-right)
  (frag-cats tactics polynomial)
  (defaults)
  (log-p t)
  (help "Monomial distributivity ((a+b)*c)=(a*c+b*c)."))

(defun polynom=distribute-right (l1 l2 pos)
  (infer~compute-outline 'distribute-right (list l1 l2) (list pos)))

;;
;; f
;;

(tac~deftactic distribute-right-f distribute-right (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=right-distrib-f (formula L1) Pos)))
   (sideconditions (polynom=right-distrib-fp (formula L1) Pos))
   (description "Forward application monomial distributivity."))

(defun polynom=right-distrib-fp (l1 pos)
  (let ((S1 (data~struct-at-position l1 pos)))
    (and (polynom=product-p S1)
         (polynom=sum-p (first (data~appl-arguments S1))))))

(defun polynom=right-distrib-f (l1 pos)
  (let* ((S1 (data~struct-at-position l1 pos))
         (DTerm
          (term~appl-create (data~appl-function (first (data~appl-arguments S1))) ; +
                       (list (term~appl-create (data~appl-function S1)
                                          (list
                                           (first (data~appl-arguments
                                                   (first (data~appl-arguments S1))))
                                           (second (data~appl-arguments S1))))
                             (term~appl-create (data~appl-function S1)
                                         (list
                                          (second (data~appl-arguments
                                                   (first (data~appl-arguments S1))))
                                          (second (data~appl-arguments S1))))))))
    (data~replace-at-position L1 Pos Dterm)))
;;
;; b
;;


(tac~deftactic distribute-right-b distribute-right (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=right-distrib-b (formula L2) Pos)))
   (sideconditions (polynom=right-distrib-bp (formula L2) Pos))
   (description "Forward application monomial distributivity."))


(defun polynom=right-distrib-bp (l1 pos)
  (let ((S1 (data~struct-at-position l1 pos)))
    (and (polynom=sum-p S1)
         (term~appl-p (first (data~appl-arguments S1)))
         (term~appl-p (second (data~appl-arguments S1)))
         (data~equal (data~appl-function (first (data~appl-arguments S1)))
                     (data~appl-function (second (data~appl-arguments S1))))
         (polynom=times-function-p (data~appl-function (first (data~appl-arguments S1))))
	 (data~equal (second (data~appl-arguments (first (data~appl-arguments S1))))
		     (second (data~appl-arguments (second (data~appl-arguments S1))))))))
    
           
(defun polynom=right-distrib-b (l1 pos)
  (let* ((S1 (data~struct-at-position l1 pos))
         (Dterm (term~appl-create (data~appl-function (first (data~appl-arguments S1))) ; *
                             (list (term~appl-create
                                    (data~appl-function S1)
                                    (list
                                     (first (data~appl-arguments
                                             (first (data~appl-arguments S1))))
                                    (first (data~appl-arguments
                                            (second (data~appl-arguments S1))))))
                                   (second (data~appl-arguments
                                            (second (data~appl-arguments S1))))))))
    (data~replace-at-position L1 Pos Dterm)))

                                  
;;
;; a
;;


(tac~deftactic distribute-right-a distribute-right (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=right-distrib-ap (formula L2) (formula L1) Pos))
   (description "Test application monomial distributivity."))

                                  
(defun polynom=right-distrib-ap (L2 L1 pos)
  (lam~equal-p (polynom=right-distrib-f l1 pos) L2))
  

(defun polynom=expand-distribute-right (outline parameters)
  (let* ((conc (car outline))
	 (precond (cadr outline))
	 (pos (car parameters)))
    (tacl~init outline)
    (tacl~apply 'apply-rewrite outline (list pos 'dist-right-real nil nil))
    (tacl~end)))


;;
;; backward direction distribute -> cummulate
;;


(infer~deftactic cummulate-right
                 (outline-mappings (((existent existent)
                                     cummulate-right-a)
                                    ((existent nonexistent)
                                     cummulate-right-b)
                                    ((nonexistent existent)
                                     cummulate-right-f)))
                 (expansion-function polynom=expand-distribute-right)
                 (parameter-types position)
                 (help "Monomial commulation (a*c+b*c)=((a+b)*c)."))

(com~defcommand cummulate-right
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps
   "A line containing a polynomial of the form: (a+b)*c"
   "A line containing a polynomial of the form: (a*c+b*c)"
   "A position")
  (function polynom=cummulate-right)
  (frag-cats tactics polynomial)
  (defaults)
  (log-p t)
  (help "Monomial distributivity (a*c+b*c)=((a+b)*c)."))

(defun polynom=cummulate-right (l1 l2 pos)
  (infer~compute-outline 'cummulate-right (list l1 l2) (list pos)))

;;
;; f
;;

(tac~deftactic cummulate-right-f cummulate-right (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=right-distrib-b (formula L1) Pos)))
   (sideconditions (polynom=right-distrib-bp (formula L1) Pos))
   (description "Forward application monomial distributivity."))

;;
;; b
;;


(tac~deftactic cummulate-right-b cummulate-right (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=right-distrib-f (formula L2) Pos)))
   (sideconditions (polynom=right-distrib-fp (formula L2) Pos))
   (description "Forward application monomial distributivity backwards."))

;;
;; a
;;


(tac~deftactic cummulate-right-a cummulate-right (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=right-distrib-ap (formula L1) (formula L2) Pos))
   (description "Test application monomial distributivity backwards."))

;;
;; Distributivity left
;;

(infer~deftactic distribute-left
                 (outline-mappings (((existent existent) distribute-left-a)
                                    ((existent nonexistent) distribute-left-b)
                                    ((nonexistent existent) distribute-left-f)))
                 (expansion-function polynom=expand-distribute-left)
                 (parameter-types position)
                 (help "Monomial distributivity (a*(b+c))=(a*b+a*c)."))

(com~defcommand distribute-left
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A line containing a polynomial of the form: (a*(b+c))"
            "A line containing a polynomial of the form: (a*b+a*c)"
            "A position")
  (function polynom=distribute-left)
  (frag-cats tactics polynomial)
  (defaults)
  (log-p t)
  (help "Monomial distributivity ((a+b)*c)=(a*c+b*c)."))

(defun polynom=distribute-left (l1 l2 pos)
  (infer~compute-outline 'distribute-left (list l2 l1) (list pos)))

;;
;; f
;;

(tac~deftactic distribute-left-f distribute-left (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=left-distrib-f (formula L1) Pos)))
   (sideconditions (polynom=left-distrib-fp (formula L1) Pos))
   (description "Forward application monomial distributivity."))

(defun polynom=left-distrib-fp (l1 pos)
  (let ((S1 (data~struct-at-position l1 pos)))
    (and (polynom=product-p S1)
         (polynom=sum-p (second (data~appl-arguments S1))))))

(defun polynom=left-distrib-f (l1 pos)
  (let* ((S1 (data~struct-at-position l1 pos))
         (DTerm
          (term~appl-create (data~appl-function (second (data~appl-arguments S1))) ; +
                       (list (term~appl-create (data~appl-function S1)
                                          (list
                                           (first (data~appl-arguments S1))
                                           (first (data~appl-arguments
                                                   (second (data~appl-arguments S1))))))
                             (term~appl-create (data~appl-function S1)
                                         (list
                                          (first (data~appl-arguments S1))
                                          (second (data~appl-arguments
                                                   (second (data~appl-arguments S1))))))))))
    (data~replace-at-position L1 Pos Dterm)))
;;
;; b
;;


(tac~deftactic distribute-left-b distribute-left (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=left-distrib-b (formula L2) Pos)))
   (sideconditions (polynom=left-distrib-bp (formula L2) Pos))
   (description "Forward application monomial distributivity."))


(defun polynom=left-distrib-bp (l1 pos)
  (let ((S1 (data~struct-at-position l1 pos)))
    (and (polynom=sum-p S1)
         (term~appl-p (first (data~appl-arguments S1)))
         (term~appl-p (second (data~appl-arguments S1)))
         (data~equal (data~appl-function (first (data~appl-arguments S1)))
                     (data~appl-function (second (data~appl-arguments S1))))
         (polynom=times-function-p (data~appl-function (first (data~appl-arguments S1))))
	 (data~equal (first (data~appl-arguments (first (data~appl-arguments S1))))
		     (first (data~appl-arguments (second (data~appl-arguments S1))))))))

           
(defun polynom=left-distrib-b (l1 pos)
  (let* ((S1 (data~struct-at-position l1 pos))
         (Dterm (term~appl-create (data~appl-function (first (data~appl-arguments S1))) ; *
                             (list (first (data~appl-arguments
                                            (first (data~appl-arguments S1))))
                                   (term~appl-create
                                    (data~appl-function S1)
                                    (list
                                     (second (data~appl-arguments
                                             (first (data~appl-arguments S1))))
                                    (second (data~appl-arguments
                                            (second (data~appl-arguments S1))))))))))
    (data~replace-at-position L1 Pos Dterm)))

                                  
;;
;; a
;;


(tac~deftactic distribute-left-a distribute-left (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=left-distrib-ap (formula L2) (formula L1) Pos))
   (description "Test application monomial distributivity."))

                                  
(defun polynom=left-distrib-ap (L2 L1 pos)
  (lam~equal-p (polynom=left-distrib-f l1 pos) L2))
  
(defun polynom=expand-distribute-left (outline parameters)
  (let ((conc (car outline))
	(precond (cadr outline))
	(pos (car parameters)))
    (tacl~init outline)
    (tacl~apply 'apply-rewrite (list conc precond) (list pos 'dist-left-real nil nil))
    (tacl~end)))



;;
;; backward direction distribute -> cummulate
;;


(infer~deftactic cummulate-left
                 (outline-mappings (((existent existent)
                                     cummulate-left-a)
                                    ((existent nonexistent)
                                     cummulate-left-b)
                                    ((nonexistent existent)
                                     cummulate-left-f)))
                 (expansion-function polynom=expand-cummulate-left)
                 (parameter-types position)
                 (help "Monomial commulation (a*c+b*c)=((a+b)*c)."))

(com~defcommand cummulate-left
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps
   "A line containing a polynomial of the form: (a*c+b*c)"
   "A line containing a polynomial of the form: (a+b)*c"
   "A position")
  (function polynom=cummulate-left)
  (frag-cats tactics polynomial)
  (defaults)
  (log-p t)
  (help "Monomial distributivity (a*c+b*c)=((a+b)*c)."))

(defun polynom=cummulate-left (l1 l2 pos)
  (infer~compute-outline 'cummulate-left (list l2 l1) (list pos)))

;;
;; f
;;

(tac~deftactic cummulate-left-f cummulate-left (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=left-distrib-b (formula L1) Pos)))
   (sideconditions (polynom=left-distrib-bp (formula L1) Pos))
   (description "Forward application monomial distributivity."))

;;
;; b
;;


(tac~deftactic cummulate-left-b cummulate-left (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=left-distrib-f (formula L2) Pos)))
   (sideconditions (polynom=left-distrib-fp (formula L2) Pos))
   (description "Forward application monomial distributivity backwards."))

;;
;; a
;;


(tac~deftactic cummulate-left-a cummulate-left (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=left-distrib-ap (formula L1) (formula L2) Pos))
   (description "Test application monomial distributivity backwards."))

(defun polynom=expand-cummulate-left (outline parameters)
  (let ((conc (car outline))
	(precond (cadr outline))
	(pos (car parameters)))
    (tacl~init outline)
    (tacl~apply 'apply-rewrite (list conc precond) (list pos 'dist-left-real nil nil))
    (tacl~end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Monomial scalar multiplication
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic mon-stimes
                 (outline-mappings (((existent existent) mon-stimes-a)
                                    ((nonexistent existent) mon-stimes-f)))
                 (expansion-function polynom=expand-mon-stimes)
                 (parameter-types position)
                 (help "Monomial scalar multiplication s * m = sm."))

(com~defcommand mon-stimes
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A line containing a monomial of the form: (s * m)"
            "A line containing a monomial of the form: sm"
            "The position of this monomial")
  (function polynom=mon-stimes)
  (frag-cats tactics polynomial)
  (defaults)
  (log-p t)
  (help "Monomial scalar multiplication s * m = sm."))

(defun polynom=mon-stimes (l1 l2 pos)
  (if (pdsn~open-node-p l1)  
      (infer~compute-outline 'mon-split-stimes (list l1 l2) (list pos))
    (infer~compute-outline 'mon-stimes (list l2 l1) (list pos))))




;;
;; Mon STIMES directions
;;


(tac~deftactic mon-stimes-f mon-stimes (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=mon-stimes-f (formula L1) Pos)))
   (sideconditions (polynom=applicable-mon-stimes-p (formula L1) Pos))
   (description "Forward application of Mon-Stimes."))

(defun polynom=applicable-mon-stimes-p (formula Pos)
  (let ((term (data~struct-at-position formula pos)))
    (and (polynom=product-p Term)
	 (= (length (data~appl-arguments Term)) 2)
	 (cacom~coeff&monomial-p (second (data~appl-arguments Term)))
	 )))  ;;; Gefahr bei konstanten Koeffizienten !!!!   VS.
	   
(defun polynom=apply-mon-stimes (term)
  (let* ((arg1 (first (data~appl-arguments term)))
	 (arg2 (second (data~appl-arguments term)))
	 (arg21 (first (data~appl-arguments arg2)))
	 (arg22 (second (data~appl-arguments arg2))))
    (if (and (term~number-p arg1) (term~number-p arg21))
	(polynom=product-create
	 (polynom=create-number (* (keim~name arg1) (keim~name arg21)))
	 arg22)
      (polynom=product-create
       (polynom=product-create arg1 arg21)
       arg22))))
 
(defun polynom=mon-stimes-f (L1 Pos)
  (let* ((Subterm (data~struct-at-position L1 Pos))
         (Sum (polynom=apply-mon-stimes Subterm)))
    (data~replace-at-position L1 Pos Sum)))

(defun polynom=expand-mon-stimes (outline parameters)
  (let ((conc (car outline))
	(prem (cadr outline))
	(pos (car parameters)))
    (tacl~init outline)
    (let ((rewr-res (tacl~apply 'apply-rewrite (list nil prem) (list pos 'a-times-real nil nil)))) ;;; conc1 prem
      (polynom=simplify-num-application (car rewr-res) (pos~add-end 1 pos) conc))  ;;; conc conc1
    (tacl~end)))


;;
;; A-direction
;;


(tac~deftactic mon-stimes-a mon-stimes (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=applicable-mon-stimes-p (formula L1) Pos)
		   (polynom=mon-stimes-ap (formula L1) (formula L2) Pos))
   (description "Application of Mon-Stimes."))


(defun polynom=mon-stimes-ap (L1 L2 Pos)
  (lam~equal-p (polynom=mon-stimes-f L1 Pos) L2))


;;; Mon-Split-Stimes

(infer~deftactic mon-split-stimes
                 (outline-mappings (((existent existent) mon-split-stimes-a)
                                    ((existent nonexistent) mon-split-stimes-b)))
                 (expansion-function polynom=expand-mon-split-stimes)
                 (parameter-types position)
                 (help "Monomial scalar multiplication backward (e+(d+c))=((a+d)+(b+c))."))


;;
;; Mon Split STIMES directions
;;


(tac~deftactic mon-split-stimes-b mon-split-stimes (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=mon-stimes-f (formula L2) Pos)))
   (sideconditions (polynom=applicable-mon-stimes-p (formula L2) Pos))
   (description "Forward application of Mon-Plus."))

;;
;; A-direction
;;


(tac~deftactic mon-split-stimes-a mon-split-stimes (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=applicable-mon-stimes-p (formula L2) Pos)
		   (polynom=mon-stimes-ap (formula L2) (formula L1) Pos))
   (description "Application of Mon-Plus."))

(defun polynom=expand-mon-split-stimes (outline parameters)
  (let ((conc (car outline))
	(prem (cadr outline))
	(pos (car parameters)))
    (tacl~init outline)
    (let ((rewr-res (tacl~apply 'apply-rewrite (list conc nil) (list pos 'a-times-real nil nil)))) ;;; conc1 prem
      (polynom=simplify-num-application (cadr rewr-res) (pos~add-end 1 pos) prem))  ;;; conc conc1
    (tacl~end)))




;;
;; Multiply Zero Left
;;


(infer~deftactic 0*e
                 (outline-mappings (((existent existent)
                                     0*e-a)
                                    ((nonexistent existent)
                                     0*e-f)))
                 (expansion-function polynom=expand-distributivity)
                 (parameter-types position)
                 (help "(0*a)=0."))

(com~defcommand 0*e
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A line containing the form 0"
            "A line containing the form (0*a)"
            "A position")
  (function polynom=0*e)
  (frag-cats tactics polynomial)
  (defaults)
  (log-p t)
  (help "(0*a)=0."))

(defun polynom=0*e (l1 l2 pos)
  (infer~compute-outline '0*e (list l1 l2) (list pos)))

;;
;; f
;;


(tac~deftactic 0*e-f 0*e (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=0*e-f (formula L1) Pos)))
   (sideconditions (polynom=0*e-fp (formula L1) Pos))
   (description "(0*a) -> 0"))

(defun polynom=0*e-fp (L1 Pos)
  (let ((S1 (data~struct-at-position l1 pos)))
    (and (polynom=product-p S1)
	 (lam~equal-p (polynom=create-number 0) (first (data~appl-arguments S1))))))
      
(defun polynom=0*e-f (L1 Pos)
  (let ((DTerm (polynom=create-number 0)))
    (data~replace-at-position L1 Pos Dterm)))




;;
;; Multiply Zero Right
;;


(infer~deftactic *0e
                 (outline-mappings (((existent existent)
                                     *0e-a)
                                    ((nonexistent existent)
                                     *0e-f)))
                 (expansion-function polynom=expand-*0e)
                 (parameter-types position)
                 (help "(a*0)=0."))

(com~defcommand *0e
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A line containing the form 0"
            "A line containing the form (a*0)"
            "A position")
  (function polynom=*0e)
  (frag-cats tactics polynomial)
  (defaults)
  (log-p t)
  (help "(a*0)=0."))

(defun polynom=*0e (l1 l2 pos)
  (infer~compute-outline '*0e (list l1 l2) (list pos)))


;;
;; f
;;


(tac~deftactic *0e-f *0e (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=*0e-f (formula L1) Pos)))
   (sideconditions (polynom=*0e-fp (formula L1) Pos))
   (description "(a*0) -> 0"))

(defun polynom=*0e-fp (L1 Pos)
  (let ((S1 (data~struct-at-position l1 pos)))
    (and (polynom=product-p S1)
	 (lam~equal-p (polynom=create-number 0) (second (data~appl-arguments S1))))))
      
(defun polynom=*0e-f (L1 Pos)
   (let ((S1 (data~struct-at-position l1 pos))
         (DTerm (polynom=create-number 0)))
     (data~replace-at-position L1 Pos Dterm)))

;;
;; a
;;


(tac~deftactic *0e-a *0e (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=*0e-ap (formula L1) (formula L2) Pos))
   (description "(a*0) -> 0"))


(tac~deftactic 0*e-a 0*e (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=*0e-ap (formula L1) (formula L2) Pos))
   (description "(a*0) -> 0"))

(defun polynom=*0e-ap (L1 L2 Pos)
  (lam~equal-p (polynom=*0e-f l1 pos) (data~struct-at-position L2 pos)))

(defun polynom=0*e-ap (L1 L2 Pos)
  (lam~equal-p (polynom=0*e-f l1 pos) (data~struct-at-position L2 pos)))

;;
;; Introduction
;;

(infer~deftactic 0*i
                 (outline-mappings (((existent existent)
                                     0*i-a)
                                    ((existent nonexistent)
                                     0*i-b)))
                 (expansion-function polynom=expand-0*i)
                 (parameter-types position)
                 (help "(0*a)=0."))

(com~defcommand 0*i
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A line containing the form (0*a)"
            "A line containing the form 0"
            "A position")
  (function polynom=0*i)
  (frag-cats tactics polynomial)
  (defaults)
  (log-p t)
  (help "(0*a)=0."))

(defun polynom=0*i (l1 l2 pos)
  (infer~compute-outline '0*i (list l1 l2) (list pos)))

;;
;; f
;;


(tac~deftactic 0*i-b 0*i (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=0*e-f (formula L2) Pos)))
   (sideconditions (polynom=0*e-fp (formula L2) Pos))
   (description "(0*a) -> 0"))

;;
;; Multiply Zero Right
;;


(infer~deftactic *0i
                 (outline-mappings (((existent existent)
                                     *0i-a)
                                    ((existent nonexistent)
                                     *0i-b)))
                 (expansion-function polynom=expand-distributivity)
                 (parameter-types position)
                 (help "(a*0)=0."))

(com~defcommand *0i
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A line containing the form (0*a)"
            "A line containing the form 0"
            "A position")
  (function polynom=*0i)
  (frag-cats tactics polynomial)
  (defaults)
  (log-p t)
  (help "(a*0)=0."))

(defun polynom=*0i (l1 l2 pos)
  (infer~compute-outline '*0i (list l1 l2) (list pos)))


;;
;; b
;;

(tac~deftactic *0i-b *0i (in polynomial) 
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=*0e-f (formula L2) Pos)))
   (sideconditions (polynom=*0e-fp (formula L2) Pos))
   (description "(0*a) -> 0"))


;;
;; a
;;

(tac~deftactic *0i-a *0i (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=*0i-ap (formula L1) (formula L2) Pos))
   (description "(a*0) -> 0"))


(tac~deftactic 0*i-a 0*i (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=0*i-ap (formula L1) (formula L2) Pos))
   (description "(a*0) -> 0"))

(defun polynom=*0i-ap (L2 L1 Pos)
  (lam~equal-p (polynom=*0e-f l1 pos) L2))

(defun polynom=0*i-ap (L2 L1 Pos)
  (lam~equal-p (polynom=0*e-f l1 pos) L2))


;;
;; Push-last
;;


(infer~deftactic push-last
		 (outline-mappings (((existent existent) push-last-a)
				    ((nonexistent existent) push-last-f)))
		 (expansion-function polynom=expand-push-last)
		 (parameter-types position)
		 (help "Iterated Associativity ((a+c)+d) = (a+(e+(b+d)))."))

;;
;; Push-Last Directions
;;

(tac~deftactic push-last-f push-last (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=push-last-f (formula L1) POS)))
   (sideconditions (polynom=push-last-fp (formula L1) Pos))
   (description "Forward application of Push-Last."))

(tac~deftactic push-last-a push-last (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=push-last-fp (formula L1) Pos)
		   (polynom=push-last-ap (formula L1) (formula L2) Pos))
   (description "Test application of Push-Last."))


(defun polynom=push-last-f (Formula Pos)
  (let* ((term (data~struct-at-position formula pos))
	 (args (data~appl-arguments term))
	 (new-term (polynom=shift-within (car args) (cadr args))))
    (data~replace-at-position formula pos new-term)))

(defun polynom=shift-within (term add)
  (if (polynom=sum-p term)
      (polynom=sum-create
       (first (data~appl-arguments term))
       (polynom=shift-within
	(second (data~appl-arguments term))
	add))
    (polynom=sum-create term add)))

(defun polynom=push-last-ap (F1 F2 Pos)
  (lam~equal-p (polynom=push-last F1 Pos) F2))

(defun polynom=push-last-fp (Formula Pos)
  (polynom=applicable-assoc-plus-p
   (data~struct-at-position Formula Pos)))

(defun polynom=expand-push-last (outline parameters)
  (let ((conc (car outline))
	(precond (cadr outline))
	(position (car parameters)))
    (tacl~init outline)
    (do ((new-outline (if (polynom=sum-p (data~struct-at-position (node~formula precond)
								  (pos~add-end 2 (pos~add-end 1 position))))
			  (tacl~apply 'apply-rewrite (list nil precond)
				  (list position 'a-plus-real nil nil)) 
			(tacl~apply 'apply-rewrite (list conc precond)
				  (list position 'a-plus-real nil nil)))
		      (if (polynom=sum-p (data~struct-at-position (node~formula (car new-outline))
								  (pos~add-end 2 (pos~add-end 1 pos))))
			  (tacl~apply 'apply-rewrite (list nil (car new-outline))
				      (list pos 'a-plus-real nil nil)) 
			(tacl~apply 'apply-rewrite (list conc (car new-outline))
				    (list pos 'a-plus-real nil nil))))
	 (pos (pos~add-end 2 position) (pos~add-end 2 pos)))
	((not (polynom=sum-p (data~struct-at-position (node~formula (car new-outline))
						      (pos~add-end 1 pos))))
	 new-outline))
    (tacl~end)))

(com~defcommand push-last
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A line containing ((a+c)+d)"
            "A line containing (a+(e+(b+d)))"
            "The position of the subterm")
  (function polynom=push-last)
  (frag-cats tactics polynomial)
  (defaults)
  (log-p t)
  (help "Iterated Associativity ((a+c)+d) = (a+(e+(b+d)))."))

(defun polynom=push-last (l1 l2 pos)
  (if (pdsn~open-node-p l1)
      (infer~compute-outline 'push-last (list l1 l2) (list pos))
    (infer~compute-outline 'pop-last (list l2 l1) (list pos))))


;;
;; Backward: Pop-last
;;

(infer~deftactic pop-last
		 (outline-mappings (((existent existent) pop-last-a)
				    ((existent nonexistent) pop-last-b)))
		 (expansion-function polynom=expand-pop-last)
		 (parameter-types position)
		 (help "Iterated Associativity ((a+c)+d) = (a+(e+(b+d)))."))

(tac~deftactic pop-last-b pop-last (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=push-last-f (formula L2) POS)))
   (sideconditions (polynom=push-last-fp (formula L2) Pos))
   (description "Backward application of Pop-Last."))


(tac~deftactic pop-last-a pop-last (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=push-last-fp (formula L2) Pos)
		   (polynom=push-last-ap (formula L2) (formula L1) Pos))
   (description "Test application of Pop-Last."))

(defun polynom=pop-last-bp (Formula Pos)
  (polynom=applicable-assoc-plus-backwards-p
   (data~struct-at-position Formula Pos)))

(defun polynom=expand-pop-last (outline parameters)
  (let ((conc (car outline))
	(precond (cadr outline))
	(position (car parameters)))
    (tacl~init outline)
    (do ((new-outline (if (polynom=sum-p (data~struct-at-position (node~formula conc)
								  (pos~add-end 2 (pos~add-end 1 position))))
			  (tacl~apply 'apply-rewrite (list conc nil)
				  (list position 'a-plus-real nil nil)) 
			(tacl~apply 'apply-rewrite (list conc precond)
				  (list position 'a-plus-real nil nil)))
		      (if (polynom=sum-p (data~struct-at-position (node~formula (cadr new-outline))
								  (pos~add-end 2 (pos~add-end 1 pos))))
			  (tacl~apply 'apply-rewrite (list (cadr new-outline) nil)
				      (list pos 'a-plus-real nil nil)) 
			(tacl~apply 'apply-rewrite (list (cadr new-outline) precond)
				    (list pos 'a-plus-real nil nil))))
	 (pos (pos~add-end 2 position) (pos~add-end 2 pos)))
	((not (polynom=sum-p (data~struct-at-position (node~formula (cadr new-outline))
						      (pos~add-end 1 pos))))
	 new-outline))
    (tacl~end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Scalar multiplication 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic stimes-poly
                 (outline-mappings (((existent existent) stimes-poly-a)
                                    ((nonexistent existent) stimes-poly-f)
;;;				    ((existent nonexistent) poly-plus-b)
				    )
				   )
                 (expansion-function polynom=expand-stimes-poly)
                 (parameter-types position)
                 (help "Polynomial scalarmultiplication (p (s*) c) = lam z. (c * (p z))."))

(com~defcommand stimes-poly
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A line containing a scalarmultiplication"
            "A line containing the result of the multiplication"
	    "The position of this scalarmultiplication")
  (function polynom=stimes-poly)
  (frag-cats tactics polynomial)
  (defaults)
  (log-p t)
  (help "Polynomial scalarmultiplication (p (s*) c) = lam z. (c * (p z))."))

(defun polynom=stimes-poly (l1 l2 pos)
  (if (pdsn~open-node-p l1)
      (infer~compute-outline 'split-stimes-poly (list l1 l2) (list pos))
    (infer~compute-outline 'stimes-poly (list l2 l1) (list pos))))



;;
;; Scalar Mult directions
;;


(tac~deftactic stimes-poly-f stimes-poly (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=stimes-poly-f (formula L1) Pos)))
   (sideconditions (polynom=stimes-p (formula L1) Pos))
   (description "Forward application of Stimes-Poly."))

(defun polynom=stimes-p (formula Pos)
  (let ((term (data~struct-at-position formula pos)))
    (and (term~appl-p Term)
	 (polynom=stimes-function-p (data~appl-function Term)))))

					
(defun polynom=apply-polynomial-scalar-multiplication (term)
  (let* ((addargs (polynom=get-addargs term 2))
	 (arg1 (first (data~appl-arguments term)))
	 (arg2 (second (data~appl-arguments term)))
	 (oldvarlist (data~abstr-binder arg1))
	 (newvars (polynom=new-number-vars "Z" (length oldvarlist)))
	 (abstr (beta~normalize
		 (term~abstr-create newvars
				    (polynom=product-create arg2
							    (term~appl-create arg1 newvars))))))
    (if addargs
	(term~appl-create abstr addargs)
      abstr)))

 
(defun polynom=stimes-poly-f (L1 Pos)
  (let* ((Subterm (data~struct-at-position L1 Pos))
         (Sum (polynom=apply-polynomial-scalar-multiplication Subterm)))
    (data~replace-at-position L1 Pos Sum)))

;;
;; A-direction
;;


(tac~deftactic stimes-poly-a stimes-poly (in polynomial) 
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations )
   (sideconditions (polynom=stimes-p (formula L1) Pos)
		   (polynom=stimes-poly-ap (formula L1) (formula L2) Pos))
   (description "Application of Stimes-Poly."))


(defun polynom=stimes-poly-ap (L1 L2 Pos)
  (lam~equal-p (polynom=stimes-poly-f L1 Pos) L2))


;;; Split-Stimes-Poly

(infer~deftactic split-stimes-poly
                 (outline-mappings (((existent existent) split-stimes-poly-a)
                                    ((existent nonexistent) split-stimes-poly-b)))
                 (expansion-function polynom=expand-split-stimes-poly)
                 (parameter-types position)
                 (help "Polynomial scalarmultiplication backward lam z. (c * (p z)) = (p (s*) c)."))

;;(com~defcommand split-stimes-poly
;;  (argnames line1 line2 position)
;;  (argtypes ndline ndline position)
;;  (arghelps "An open line containing the sum of two polynomials"
;;            "A closed line containing the sum of their monomials"
;;            "The position of this sum")
;;  (function polynom=split-stimes-poly)
;;  (frag-cats tactics polynomial)
;;  (defaults)
;;  (log-p t)
;;  (help "Polynomial addition backward  (p (+) q) = lam z. (p z + q z)."))
;;
;;(defun polynom=split-stimes-poly (l1 l2 pos)
;;  (infer~compute-outline 'split-stimes-poly (list l1 l2) (list pos)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;  decided to put it in one command, as long as there are no backward and forward directions


;;
;; Poly Split PLUS directions
;;


(tac~deftactic split-stimes-poly-b split-stimes-poly (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=stimes-poly-f (formula L2) Pos)))
   (sideconditions (polynom=stimes-p (formula L2) Pos))
   (description "Forward application of Stimes-Poly."))

;;
;; A-direction
;;


(tac~deftactic split-stimes-poly-a split-stimes-poly (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=stimes-p (formula L2) Pos)
		   (polynom=stimes-poly-ap (formula L2) (formula L1) Pos))
   (description "Application of Stimes-Poly."))


(defun polynom=expand-stimes-poly (outline parameters)
  (let* ((conc (car outline))
	 (precond (cadr outline))
	 (pos (car parameters))
	 (addargs (polynom=get-addargs (data~struct-at-position (node~formula precond) pos) 2)))
    (tacl~init outline)
    (tacl~sequence
     (rewr-res ('apply-rewrite (list nil precond) (list pos 's-times-real1 nil addargs)))
     (lambda-res ('lambda (list conc (car rewr-res)) nil)))
    (tacl~end)))

(defun polynom=expand-split-stimes-poly (outline parameters)
  (let* ((conc (car outline))
	 (precond (cadr outline))
	 (pos (car parameters))
	 (addargs (polynom=get-addargs (data~struct-at-position (node~formula conc) pos) 2)))
    (tacl~init outline)
    (tacl~sequence
     (rewr-res ('apply-rewrite (list conc nil) (list pos 's-times-real1 nil addargs)))
     (lambda-res ('lambda (list (cadr rewr-res) precond) nil)))
    (tacl~end)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Polynomial Sorting  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; for no sorting tactics exists a command. They should not be applied manually!

;;
;;  Basic Sorting
;; 

(infer~deftactic poly-sort-forw
		 (outline-mappings (((existent existent) poly-sort-forw-a)
				    ((nonexistent existent) poly-sort-forw-f)))
		 (expansion-function polynom=expand-poly-sort-forw)
		 (parameter-types position integer integer integer)
		 (help "Iterated Associativity ((a+c)+d) = (a+(e+(b+d)))."))

(tac~deftactic poly-sort-forw-f poly-sort-forw (in polynomial)
   (parameters (POS pos+position "A position")
	       (Pos1 integer)
	       (Pos2 integer)
	       (Length integer))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=poly-sort-forw-f (formula L1) POS Pos1 Pos2)))
   (sideconditions )
   (description "Forward application of Poly-Sort."))


(tac~deftactic poly-sort-forw-a poly-sort-forw (in polynomial)
   (parameters (POS pos+position "A position")
	       (Pos1 integer)
	       (Pos2 integer)
	       (Length integer))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=poly-sort-ap (formula L1) (formula L2) Pos Pos1 Pos2))
   (description "Test application of Poly-Sort-Forw."))

(defun polynom=expand-poly-sort-forw (outline parameters)
  (let* ((conc (car outline))
	 (pos (first parameters))
	 (count (- (second parameters) (third parameters)))
	 (pos2 (pos~concatenate pos
				(pos~list-position (make-list (1- (second parameters))
							      :initial-element 2))))
	 (new-outline (cdr outline))
	 (dummy))
    (tacl~init outline)
    (if (= (second parameters) (fourth parameters))
	(tacl~apply 'apply-rewrite outline (list pos2 'c-plus-real nil))
      (dotimes (x count)
	(when (= (1+ x) count) (setf dummy conc))
	(setf new-outline
	      (tacl~sequence
	       (rewr1-res ('apply-rewrite (list nil (car new-outline)) (list pos2 'a-plus-real nil nil)))
	       (rewr2-res ('apply-rewrite (list nil (car rewr1-res)) (list (pos~add-end 1 pos2) 'c-plus-real nil nil)))
	       (rewr3-res ('apply-rewrite (list dummy (car rewr2-res)) (list pos2 'a-plus-real nil nil)))))
	(setf pos2 (pos~butlast pos2))
      ))
    (tacl~end)))

(infer~deftactic poly-sort-back
		 (outline-mappings (((existent existent) poly-sort-back-a)
				    ((existent nonexistent) poly-sort-back-b)))
		 (expansion-function polynom=expand-poly-sort-back)
		 (parameter-types position integer integer integer)
		 (help "Iterated Associativity ((a+c)+d) = (a+(e+(b+d)))."))

(tac~deftactic poly-sort-back-b poly-sort-back (in polynomial)
   (parameters (POS pos+position "A position")
	       (Pos1 integer)
	       (Pos2 integer)
	       (Length integer))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=poly-sort-forw-f (formula L2) POS Pos1 Pos2)))
   (sideconditions )
   (description "Backward application of Poly-Sort."))


(tac~deftactic poly-sort-back-a poly-sort-back (in polynomial)
   (parameters (POS pos+position "A position")
	       (Pos1 integer)
	       (Pos2 integer)
	       (Length integer))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=poly-sort-ap (formula L2) (formula L1) Pos Pos1 Pos2))
   (description "Test application of Poly-Sort-Back."))


(defun polynom=expand-poly-sort-back (outline parameters)
  (let* ((precond (cadr outline))
	 (pos (first parameters))
	 (count (- (second parameters) (third parameters)))
	 (pos2 (pos~concatenate pos
				(pos~list-position (make-list (1- (second parameters))
							      :initial-element 2))))
	 (new-outline (list nil (car outline)))
	 (dummy))
    (tacl~init outline)
    (if (= (second parameters) (fourth parameters))
	(tacl~apply 'apply-rewrite outline (list pos2 'c-plus-real nil))
      (dotimes (x count)
	(when (= (1+ x) count) (setf dummy precond))
	(setf new-outline
	      (tacl~sequence
	       (rewr1-res ('apply-rewrite (list (cadr new-outline) nil) (list pos2 'a-plus-real nil nil)))
	       (rewr2-res ('apply-rewrite (list (cadr rewr1-res) nil) (list (pos~add-end 1 pos2) 'c-plus-real nil nil)))
	       (rewr3-res ('apply-rewrite (list (cadr rewr2-res) dummy) (list pos2 'a-plus-real nil nil)))))
	(setf pos2 (pos~butlast pos2))
	))
    (tacl~end)))

(defun polynom=poly-sort-ap (f1 f2 pos Pos1 Pos2)
  (lam~equal-p (polynom=poly-sort-forw-f f1 pos Pos1 Pos2) f2))

(defun polynom=poly2monoms (term)
  (if (polynom=sum-p term)
      (cons (first (data~appl-arguments term))
	    (polynom=poly2monoms (second (data~appl-arguments term))))
    (list term)))

(defun polynom=monoms2poly (term-list)
  (if (null (cdr term-list))
      (car term-list)
    (polynom=sum-create (car term-list)
			(polynom=monoms2poly (cdr term-list)))))

(defun polynom=poly-sort-it (term pos1 pos2)
  (unless (<= pos1 pos2)
    (let* ((monoms (polynom=poly2monoms term))
	   (move-term (nth pos1 monoms))
           (new-monoms (append (subseq monoms 0 pos2)
                               (list move-term)
                               (subseq monoms pos2 pos1)
                               (subseq monoms (1+ pos1)))))
      (polynom=monoms2poly new-monoms))))
      
(defun polynom=poly-sort-forw-f (formula pos pos1 pos2)
  (let* ((term (data~struct-at-position formula pos))
	 (new-term (polynom=poly-sort-it term pos1 pos2)))
    (data~replace-at-position formula pos new-term)))
			      

;;
;;  Reduction on Sum
;;


(infer~deftactic Reduce-Sum
		 (outline-mappings (((existent existent) Reduce-Sum-a)
				    ((nonexistent existent) Reduce-Sum-f)))
		 (expansion-function polynom=expand-mon-plus)
		 (parameter-types position)
		 (help "Backward summing of monomials (a + (b + c)) = (e + c)."))

(tac~deftactic Reduce-Sum-f Reduce-Sum (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=Reduce-Sum-f (formula L1) POS)))
   (sideconditions )
   (description "Forward application of Reduce-Sum."))

(tac~deftactic Reduce-Sum-a Reduce-Sum (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=reduce-sum-ap (formula L1) (formula L2) Pos))
   (description "Test application of Reduce-Sum."))

(infer~deftactic Expand-Sum
		 (outline-mappings (((existent existent) Expand-Sum-a)
				    ((existent nonexistent) Expand-Sum-b)))
		 (expansion-function polynom=expand-mon-split-plus)
		 (parameter-types position)
		 (help "Backward summing of monomials (e + c) = (a + (b + c))."))

(tac~deftactic Expand-Sum-b Expand-Sum (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=Reduce-Sum-f (formula L2) POS)))
   (sideconditions )
   (description "Backward application of Expand-Sum."))


(tac~deftactic Expand-Sum-a Expand-Sum (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=reduce-sum-ap (formula L2) (formula L1) Pos))
   (description "Test application of Expand-Sum."))


(defun polynom=reduce-sum-ap (f1 f2 pos)
  (lam~equal-p (polynom=Reduce-Sum-f f1 pos) f2))

(defun polynom=Reduce-Sum-f (formula pos)
  (let* ((term (data~struct-at-position formula pos))
	 (new-term (polynom=apply-monomial-addition term)))
    (data~replace-at-position formula pos new-term)))
			      

;;
;;  Reduction on Zero
;;


(infer~deftactic Reduce-0
		 (outline-mappings (((existent existent) Reduce-0-a)
				    ((nonexistent existent) Reduce-0-f)))
		 (expansion-function polynom=expand-Reduce-0)
		 (parameter-types position)
		 (help "Backward summing of monomials (a + (b + c)) = (e + c)."))

(tac~deftactic Reduce-0-f Reduce-0 (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=Reduce-0-f (formula L1) POS)))
   (sideconditions )
   (description "Forward application of Reduce-0."))

(tac~deftactic Reduce-0-a Reduce-0 (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=reduce-0-ap (formula L1) (formula L2) Pos))
   (description "Test application of Reduce-0."))

(defun polynom=expand-reduce-0 (outline parameters)
  (let ((conc (car outline))
	(precond (cadr outline))
	(pos (car parameters)))
    (tacl~init outline)
    (tacl~sequence
     (rew-res1 ('apply-rewrite (list nil precond) (list (pos~add-end 1 pos) '0-times-real nil nil)))
     (rew-res2 ('apply-rewrite (list conc (car rew-res1)) (list pos '0-plus-real nil nil))))
    (tacl~end)))


(infer~deftactic Expand-0
		 (outline-mappings (((existent existent) Expand-0-a)
				    ((existent nonexistent) Expand-0-b)))
		 (expansion-function polynom=expand-Expand-0)
		 (parameter-types position)
		 (help "Backward summing of monomials (e + c) = (a + (b + c))."))

(tac~deftactic Expand-0-b Expand-0 (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=Reduce-0-f (formula L2) POS)))
   (sideconditions )
   (description "Backward application of Expand-0."))


(tac~deftactic Expand-0-a Expand-0 (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=reduce-0-ap (formula L2) (formula L1) Pos))
   (description "Test application of Expand-0."))


(defun polynom=reduce-0-ap (f1 f2 pos)
  (lam~equal-p (polynom=Reduce-0-f f1 pos) f2))

(defun polynom=Reduce-0-f (formula pos)
  (let* ((term (data~struct-at-position formula pos))
	 (new-term (when (polynom=sum-p term)
		     (second (data~appl-arguments term)))))
    (data~replace-at-position formula pos new-term)))
			      
(defun polynom=expand-expand-0 (outline parameters)
  (let ((conc (car outline))
	(precond (cadr outline))
	(pos (car parameters)))
    (tacl~init outline)
    (tacl~sequence
     (rew-res1 ('apply-rewrite (list conc nil) (list (pos~add-end 1 pos) '0-times-real nil nil)))
     (rew-res2 ('apply-rewrite (list (cadr rew-res1) precond) (list pos '0-plus-real nil nil))))
    (tacl~end)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Polynomial Differentiation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; MON-DIFF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic mon-diff
                 (outline-mappings (((existent existent) mon-diff-a)
                                    ((nonexistent existent) mon-diff-f)))
                 (expansion-function polynom=expand-mon-diff)
                 (parameter-types position)
                 (help "Monomial differentiation: d(m + p, i) = (m' (+) d(p, i))."))

(com~defcommand mon-diff
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A line containing a differential operator for polynomials"
            "A line containing a differentiated monomial and differential operator with the rest polynomial"
	    "The position of this operator")
  (function polynom=mon-diff)
  (frag-cats tactics polynomial)
  (defaults)
  (log-p t)
  (help "Monomial differentiation: d(m + p, i) = (m' (+) d(p, i))."))

(defun polynom=mon-diff (l1 l2 pos)
;;  (if (pdsn~open-node-p l1)
;;      (infer~compute-outline 'poly-split-times (list l1 l2) (list pos))
    (infer~compute-outline 'mon-diff (list l2 l1) (list pos)))



;;
;; Mon Diff directions
;;


(tac~deftactic mon-diff-f mon-diff (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=mon-diff-f (formula L1) Pos)))
   (sideconditions (polynom=differentiation-p (formula L1) Pos)
		   (polynom=monom-not-constant-p (formula L1) Pos))
   (description "Forward application of Mon-Diff."))

(defun polynom=monom-not-constant-p (formula Pos)
  (let* ((deriv-term (data~struct-at-position formula pos))
	 (poly (data~abstr-range
		(car (data~appl-arguments deriv-term))))
	 (term (if (polynom=sum-p poly)
		   (car (data~appl-arguments poly))
		 poly))
	 (delta (keim~name (cadr (data~appl-arguments deriv-term))))
	 (monoms (polynom=monom2vars&exps (cacom~monomial term)))
	 (diff-monom (nth (1- delta) monoms)))
    (or (not (numberp (cacom~exponent diff-monom)))
	(not (= 0 (cacom~exponent diff-monom))))))
    

(defun polynom=differentiation-p (formula Pos)
  (let ((term (data~struct-at-position formula pos)))
    (and (term~appl-p Term)
	 (polynom=pderiv-function-p (data~appl-function Term)))))

					
(defun polynom=pplus-function (pderiv)
  (let* ((func-name (symbol-name (keim~name pderiv)))
	 (pos (position #\- func-name :from-end t))
	 (suffix (subseq func-name pos))
	 (pplus (read-from-string
		 (concatenate 'string
			      "p-plus"
			      suffix))))
    (env~lookup-object pplus
		       (pds~environment omega*current-proof-plan))))
    
(defun polynom=monom2vars&exps (term)
  (if (polynom=product-p term)
      (cons (first (data~appl-arguments term))
	    (polynom=monom2vars&exps (second (data~appl-arguments term))))
    (list term)))

(defun polynom=vars&exps2monom (term-list)
  (if (null (cdr term-list))
      (car term-list)
    (polynom=product-create (car term-list)
			(polynom=vars&exps2monom (cdr term-list)))))

(defun polynom=differentiate-monomial (monom number)
  (let* ((coeff (cacom~coefficient monom))
	 (exp-list (polynom=monom2vars&exps (cacom~monomial monom)))
	 (diff-monom (nth (1- number) exp-list))
	 (exp (cacom~exponent diff-monom))
	 (new-exp (1- exp))
	 (new-coeff (* exp coeff))
	 (new-monom (polynom=exponent-create
		     (car (data~appl-arguments diff-monom))
		     (polynom=create-number new-exp))))
    (polynom=product-create (polynom=create-number new-coeff)
			    (polynom=vars&exps2monom
			     (substitute new-monom diff-monom exp-list
					 :test #'data~equal
					 :count 1)))))
     

(defun polynom=apply-monomial-differentiation (term)
  (let* ((addargs (polynom=get-addargs term 2))
	 (func (data~appl-function term))
	 (poly (first (data~appl-arguments term)))
	 (in-poly (data~abstr-range poly))
	 (delta (second (data~appl-arguments term)))
	 (oldvars (data~abstr-binder poly)))
    (if (polynom=sum-p in-poly)
	(let* ((monom (first (data~appl-arguments in-poly)))
	       (rest-poly (second (data~appl-arguments in-poly)))
	       (pplus (polynom=pplus-function func))
	       (new-monom (term~abstr-create oldvars
					     (polynom=differentiate-monomial monom (keim~name delta))))
	       (new-poly (term~appl-create func
					   (list
					    (term~abstr-create oldvars rest-poly)
					    delta)))
	       (new-term (term~appl-create pplus (list new-monom new-poly))))
	  (if addargs
	      (term~appl-create new-term addargs)
	    new-term))
      (let* ((new-monom (term~abstr-create oldvars
					   (polynom=differentiate-monomial in-poly (keim~name delta)))))
	(if addargs
	    (term~appl-create new-monom addargs)
	  new-monom)))))
 
(defun polynom=mon-diff-f (L1 Pos)
  (let* ((Subterm (data~struct-at-position L1 Pos))
         (Sum (polynom=apply-monomial-differentiation Subterm)))
    (data~replace-at-position L1 Pos Sum)))

;;
;; A-direction
;;


(tac~deftactic mon-diff-a mon-diff (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations )
   (sideconditions (polynom=differentiation-p (formula L1) Pos)
		   (polynom=monomial-not-constant-p (formula L1) Pos)
		   (polynom=mon-diff-ap (formula L1) (formula L2) Pos))
   (description "Application of Mon-Diff."))


(defun polynom=mon-diff-ap (L1 L2 Pos)
  (lam~equal-p (polynom=mon-diff-f L1 Pos) L2))

(defun polynom=compute-number-split (prem-term pos)
  (let* ((hterm (data~struct-at-position prem-term pos))
	 (hterm1 (data~abstr-range (car (data~appl-arguments hterm))))
	 (deriv-pos (keim~name (cadr (data~appl-arguments hterm))))
	 (rterm1 (if (polynom=sum-p hterm1)
		     (car (data~appl-arguments hterm1))
		   hterm1))
	 (coeff1 (cacom~coefficient rterm1))
         (exp1 (cacom~exponent
                (nth (1- deriv-pos)
                     (polynom=monom2vars&exps (cacom~monomial rterm1))))))
    (values
     (polynom=product-create
      (polynom=create-number coeff1)
      (polynom=create-number exp1))
     (polynom=substraction-create
      (polynom=create-number exp1)
      (polynom=create-number 1)))))
     
(defun polynom=expand-mon-diff (outline parameters)
  (let* ((conc (car outline))
	 (precond (cadr outline))
	 (pos (car parameters))
	 (term (data~struct-at-position (node~formula precond) (pos~add-end 0 (pos~add-end 1 pos))))
	 (addargs (polynom=get-addargs (data~struct-at-position (node~formula precond) pos) 2)))
    (multiple-value-bind (product exp)
	(polynom=compute-number-split (node~formula precond) pos)
      (tacl~init outline)
      (if (polynom=sum-p term)
	  (tacl~sequence
	   (simp-res1 ('simplify-num (list conc nil) (list (pos~concatenate pos (pos~list-position '(1 0 1))) product)))
	   (simp-res2 ('simplify-num (list (cadr simp-res1) nil) (list (pos~concatenate pos (pos~list-position '(1 0 2 2))) exp)))
	   (rew-res1 ('apply-rewrite (list (cadr simp-res2) nil) (list (pos~add-end 1 pos) 'mon-diff-real1-1 nil nil)))
	   (rew-res2 ('apply-rewrite (list (cadr rew-res1) nil) (list pos 'p-diff-real1 nil nil)))
	   (rew-res3 ('apply-rewrite (list (cadr rew-res2) nil) (list (pos~add-end 1 pos) 'p-add-real1 nil addargs)))
	   (lam-res ('lambda (list (cadr rew-res3) precond) nil)))
	(tacl~sequence
       (simp-res1 ('simplify-num (list conc nil) (list (pos~concatenate pos (pos~list-position '(0 1))) product)))
       (simp-res2 ('simplify-num (list (cadr simp-res1) nil) (list (pos~concatenate pos (pos~list-position '(0 2 2))) exp)))
       (rew-res1 ('apply-rewrite (list (cadr simp-res2) precond) (list pos 'mon-diff-real1-1 nil nil)))
       ))
    (tacl~end))))

(infer~deftactic mon-integ
                 (outline-mappings (((existent existent) mon-integ-a)
                                    ((existent nonexistent) mon-integ-b)))
                 (expansion-function polynom=expand-mon-integ)
                 (parameter-types position)
                 (help "Monomial differentiation: d(m' (+) d(p, i)) = (m + p, i)."))

(com~defcommand mon-integ
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A line containing a differentiated monomial and differential operator with the rest polynomial"
            "A line containing a differential operator for polynomials"
	    "The position of this operator")
  (function polynom=mon-integ)
  (frag-cats tactics polynomial)
  (defaults)
  (log-p t)
  (help "Monomial differentiation: d(m' (+) d(p, i)) = (m + p, i)."))

(defun polynom=mon-integ (l1 l2 pos)
    (infer~compute-outline 'mon-integ (list l1 l2) (list pos)))

(tac~deftactic mon-integ-b mon-integ (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=mon-diff-f (formula L2) Pos)))
   (sideconditions (polynom=differentiation-p (formula L2) Pos)
		   (polynom=monom-not-constant-p (formula L2) Pos))
   (description "Backward application of Mon-Integ."))


(tac~deftactic mon-integ-a mon-integ (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations )
   (sideconditions (polynom=differentiation-p (formula L2) Pos)
		   (polynom=monomial-not-constant-p (formula L2) Pos)
		   (polynom=mon-diff-ap (formula L2) (formula L1) Pos))
   (description "Application of Mon-Integ."))

(defun polynom=expand-mon-integ (outline parameters)
  (let* ((conc (car outline))
	 (precond (cadr outline))
	 (pos (car parameters))
	 (term (data~struct-at-position (node~formula conc) (pos~add-end 0 (pos~add-end 1 pos)))))
    (multiple-value-bind (product exp)
	(polynom=compute-number-split (node~formula conc) pos)
    (tacl~init outline)
    (if (polynom=sum-p term)
	(tacl~sequence
	 (simp-res1 ('expand-num (list nil precond) (list (pos~concatenate pos (pos~list-position '(1 0 1))) product)))
	 (simp-res2 ('expand-num (list nil (car simp-res1)) (list (pos~concatenate pos (pos~list-position '(1 0 2 2))) exp)))
	 (rew-res1 ('apply-rewrite (list nil (car simp-res2)) (list (pos~add-end 1 pos) 'mon-diff-real1-1 nil nil)))
	 (rew-res2 ('apply-rewrite (list nil (car rew-res1)) (list pos 'p-diff-real1 nil nil)))
	 (rew-res3 ('apply-rewrite (list nil (car rew-res2)) (list (pos~add-end 1 pos) 'p-add-real1 nil nil)))
	 (lam-res ('lambda (list conc (car rew-res3)) nil)))
      (tacl~sequence
       (simp-res1 ('expand-num (list nil precond) (list (pos~concatenate pos (pos~list-position '(0 1))) product)))
       (simp-res2 ('expand-num (list nil (car simp-res1)) (list (pos~concatenate pos (pos~list-position '(0 2 2))) exp)))
       (rew-res1 ('apply-rewrite (list conc (car simp-res2)) (list pos 'mon-diff-real1-1 nil nil)))
       ))
    (tacl~end))))


;; CONST-DIFF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic const-diff
                 (outline-mappings (((existent existent) const-diff-a)
                                    ((nonexistent existent) const-diff-f)))
                 (expansion-function polynom=expand-const-diff)
                 (parameter-types position)
                 (help "Monomial differentiation: d(c + p, i) = (0 (+) d(p, i))."))

(com~defcommand const-diff
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A line containing a differential operator for polynomials"
            "A line containing a differentiated constant monomial and differential operator with the rest polynomial"
	    "The position of this operator")
  (function polynom=const-diff)
  (frag-cats tactics polynomial)
  (defaults)
  (log-p t)
  (help "Monomial differentiation: d(c + p, i) = (0 (+) d(p, i))."))

(defun polynom=const-diff (l1 l2 pos)
    (infer~compute-outline 'const-diff (list l2 l1) (list pos)))


(tac~deftactic const-diff-f const-diff (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=const-diff-f (formula L1) Pos)))
   (sideconditions (polynom=differentiation-p (formula L1) Pos)
		   (polynom=constant-p (formula L1) Pos))
   (description "Forward application of Const-Diff."))

(defun polynom=constant-p (formula Pos)
  (let* ((deriv-term (data~struct-at-position formula pos))
	 (poly (data~abstr-range
		(car (data~appl-arguments deriv-term))))
	 (term (if (polynom=sum-p poly)
		   (car (data~appl-arguments poly))
		 poly))
	 (delta (keim~name (cadr (data~appl-arguments deriv-term))))
	 (monoms (polynom=monom2vars&exps (cacom~monomial term)))
	 (diff-monom (nth (1- delta) monoms)))
    (and (numberp (cacom~exponent diff-monom))
	 (= 0 (cacom~exponent diff-monom)))))
    

(defun polynom=apply-constant-differentiation (term)
  (let* ((addargs (polynom=get-addargs term 2))
	 (func (data~appl-function term))
	 (poly (first (data~appl-arguments term)))
	 (in-poly (data~abstr-range poly))
	 (delta (second (data~appl-arguments term)))
	 (oldvars (data~abstr-binder poly)))
    (if (polynom=sum-p in-poly)
	(let* ((monom (first (data~appl-arguments in-poly)))
	       (rest-poly (second (data~appl-arguments in-poly)))
	       (pplus (polynom=pplus-function func))
	       (new-monom (term~abstr-create oldvars (polynom=create-number 0)))
	       (new-poly (term~appl-create func
					   (list
					    (term~abstr-create oldvars rest-poly)
					    delta)))
	       (new-term (term~appl-create pplus (list new-monom new-poly))))
	  (if addargs
	      (term~appl-create new-term addargs)
	    new-term))
      (let* ((new-monom (term~abstr-create oldvars (polynom=create-number 0))))
	(if addargs
	    (term~appl-create new-monom addargs)
	  new-monom)))))
 
(defun polynom=const-diff-f (L1 Pos)
  (let* ((Subterm (data~struct-at-position L1 Pos))
         (Sum (polynom=apply-constant-differentiation Subterm)))
    (data~replace-at-position L1 Pos Sum)))


(tac~deftactic const-diff-a const-diff (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations )
   (sideconditions (polynom=differentiation-p (formula L1) Pos)
		   (polynom=constant-p (formula L1) Pos)
		   (polynom=const-diff-ap (formula L1) (formula L2) Pos))
   (description "Application of Const-Diff."))


(defun polynom=const-diff-ap (L1 L2 Pos)
  (lam~equal-p (polynom=const-diff-f L1 Pos) L2))

(defun polynom=expand-const-diff (outline parameters)
  (let* ((conc (car outline))
	 (precond (cadr outline))
	 (pos (car parameters))
	 (term (data~struct-at-position (node~formula precond) (pos~add-end 0 (pos~add-end 1 pos)))))
      (tacl~init outline)
      (if (polynom=sum-p term)
	  (tacl~sequence
	   (rew-res1 ('apply-rewrite (list conc nil) (list (pos~add-end 1 pos) 'const-diff-real1-1 nil nil)))
	   (rew-res2 ('apply-rewrite (list (cadr rew-res1) nil) (list pos 'p-diff-real1 nil nil)))
	   (rew-res3 ('apply-rewrite (list (cadr rew-res2) nil) (list (pos~add-end 1 pos) 'p-add-real1 nil nil)))
	   (lam-res ('lambda (list (cadr rew-res3) precond) nil)))
	(tacl~apply 'apply-rewrite outline (list pos 'const-diff-real1-1 nil nil))
	)
      (tacl~end)))


(infer~deftactic const-integ
                 (outline-mappings (((existent existent) const-integ-a)
                                    ((existent nonexistent) const-integ-b)))
                 (expansion-function polynom=expand-const-integ)
                 (parameter-types position)
                 (help "Constant differentiation: (0 (+) d(p, i)) = (c + p, i)."))

(com~defcommand const-integ
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A line containing a differentiated monomial and differential operator with the rest polynomial"
            "A line containing a differential operator for polynomials"
	    "The position of this operator")
  (function polynom=const-integ)
  (frag-cats tactics polynomial)
  (defaults)
  (log-p t)
  (help "Constant differentiation: (0 (+) d(p, i)) = (c + p, i)."))

(defun polynom=const-integ (l1 l2 pos)
    (infer~compute-outline 'const-integ (list l1 l2) (list pos)))

(tac~deftactic const-integ-b const-integ (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=const-diff-f (formula L2) Pos)))
   (sideconditions (polynom=differentiation-p (formula L2) Pos)
		   (polynom=constant-p (formula L2) Pos))
   (description "Backward application of Const-Integ."))


(tac~deftactic const-integ-a const-integ (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations )
   (sideconditions (polynom=differentiation-p (formula L2) Pos)
		   (polynom=constant-p (formula L2) Pos)
		   (polynom=const-diff-ap (formula L2) (formula L1) Pos))
   (description "Application of Const-Integ."))


(defun polynom=expand-const-integ (outline parameters)
  (let* ((conc (car outline))
	 (precond (cadr outline))
	 (pos (car parameters))
	 (term (data~struct-at-position (node~formula conc) (pos~add-end 0 (pos~add-end 1 pos)))))
    (tacl~init outline)
    (if (polynom=sum-p term)
	(tacl~sequence
	 (rew-res1 ('apply-rewrite (list nil precond) (list (pos~add-end 1 pos) 'const-diff-real1-1 nil nil)))
	 (rew-res2 ('apply-rewrite (list nil (car rew-res1)) (list pos 'p-diff-real1 nil nil)))
	 (rew-res3 ('apply-rewrite (list nil (car rew-res2)) (list (pos~add-end 1 pos) 'p-add-real1 nil nil)))
	 (lam-res ('lambda (list conc (car rew-res3)) nil)))
      (tacl~apply 'apply-rewrite outline (list pos 'const-diff-real1-1 nil nil))
      )
    (tacl~end)))


;; ADD-MON
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic add-mon
                 (outline-mappings (((existent existent) add-mon-a)
                                    ((nonexistent existent) add-mon-f)))
                 (expansion-function polynom=expand-add-mon)
                 (parameter-types position)
                 (help "Adding a monomial to a poiynomial: (m (+) p) = (m + p)."))

(com~defcommand add-mon
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A line containing a sum of a monomial and polynomial"
            "A line containing one polynomial that is the some of the above terms"
	    "The position of sum")
  (function polynom=add-mon)
  (frag-cats tactics polynomial)
  (defaults)
  (log-p t)
  (help "Adding a monomial to a poiynomial: (m (+) p) = (m + p)."))

(defun polynom=add-mon (l1 l2 pos)
    (infer~compute-outline 'add-mon (list l2 l1) (list pos)))


(tac~deftactic add-mon-f add-mon (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=add-mon-f (formula L1) Pos)))
   (sideconditions (polynom=addition-p (formula L1) Pos))
   (description "Forward application of Add-Mon."))

(defun polynom=0-monomial-p (monom)
  (if (term~abstr-p monom)
      (let ((term (data~abstr-range monom)))
	(and (term~number-p term)
	     (= 0 (keim~name term))))
    (and (term~number-p monom)
	 (= 0 (keim~name monom)))))

(defun polynom=append-monomial (term)
  (let* ((addargs (polynom=get-addargs term 2))
	 (arg1 (first (data~appl-arguments term)))
	 (arg2 (second (data~appl-arguments term)))
	 (oldvarlist (data~abstr-binder arg1))
	 (newvars (polynom=new-number-vars "Z" (length oldvarlist)))
	 (abstr
	  (cond ((polynom=0-monomial-p arg1) arg2)
		((polynom=0-monomial-p arg2) arg1)
		(t (beta~normalize
		    (term~abstr-create newvars
				       (polynom=sum-create (term~appl-create arg1 newvars)
							   (term~appl-create arg2 newvars))))))))
    (if addargs
	(term~appl-create abstr addargs)
      abstr)))

 
(defun polynom=add-mon-f (L1 Pos)
  (let* ((Subterm (data~struct-at-position L1 Pos))
         (Sum (polynom=append-monomial Subterm)))
    (data~replace-at-position L1 Pos Sum)))


(tac~deftactic add-mon-a add-mon (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations )
   (sideconditions (polynom=addition-p (formula L1) Pos)
		   (polynom=add-mon-ap (formula L1) (formula L2) Pos))
   (description "Application of Add-Mon."))


(defun polynom=add-mon-ap (L1 L2 Pos)
  (lam~equal-p (polynom=add-mon-f L1 Pos) L2))



(defun polynom=expand-add-mon (outline parameters)
  (let* ((conc (car outline))
	 (precond (cadr outline))
	 (pos (car parameters))
	 (term1 (data~struct-at-position (node~formula precond) (pos~add-end 1 pos)))
	 (term2 (data~struct-at-position (node~formula precond) (pos~add-end 2 pos))))
    (tacl~init outline)
    (cond ((polynom=0-monomial-p term1)
	   (tacl~sequence
	    (rew-res0 ('apply-rewrite (list nil precond) (list pos 'p-add-real1 nil nil)))
	    (lam-res ('beta-normalize (list nil (car rew-res0)) nil))
	    (rew-res1 ('apply-rewrite (list conc (car lam-res)) (list (pos~add-end 0 pos) '0-plus-real nil nil)))))
	  ((polynom=0-monomial-p term2)
	   (tacl~sequence 
	    (rew-res0 ('apply-rewrite (list nil precond) (list pos 'p-add-real1 nil nil)))
	    (lam-res ('beta-normalize (list nil (car rew-res0)) nil))
	    (rew-res1 ('apply-rewrite (list nil (car lam-res)) (list (pos~add-end 0 pos) 'c-plus-real nil nil)))
	    (rew-res2 ('apply-rewrite (list conc (car rew-res1)) (list (pos~add-end 0 pos) '0-plus-real nil nil)))))
	  (t (tacl~sequence
	      (rew-res ('apply-rewrite (list nil precond) (list pos 'p-add-real1 nil nil)))
	      (lam-res ('lambda (list conc (car rew-res)) nil)))))
    (tacl~end)))
	


;; SEPARATE-MON
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic separate-mon
                 (outline-mappings (((existent existent) separate-mon-a)
                                    ((existent nonexistent) separate-mon-b)))
                 (expansion-function polynom=expand-separate-mon)
                 (parameter-types position)
                 (help "Adding a monomial to a poiynomial: (m + p) = (m (+) p)."))

(com~defcommand separate-mon
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A line containing a sum of a monomial and polynomial"
            "A line containing one polynomial that is the some of the above terms"
	    "The position of sum")
  (function polynom=separate-mon)
  (frag-cats tactics polynomial)
  (defaults)
  (log-p t)
  (help "Adding a monomial to a poiynomial: (m + p) = (m (+) p)."))

(defun polynom=separate-mon (l1 l2 pos)
    (infer~compute-outline 'separate-mon (list l1 l2) (list pos)))


(tac~deftactic separate-mon-b separate-mon (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=add-mon-f (formula L2) Pos)))
   (sideconditions (polynom=addition-p (formula L2) Pos))
   (description "Forward application of Separate-Mon."))

(tac~deftactic separate-mon-a separate-mon (in polynomial)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations )
   (sideconditions (polynom=addition-p (formula L2) Pos)
		   (polynom=add-mon-ap (formula L2) (formula L1) Pos))
   (description "Application of Separate-Mon."))


(defun polynom=expand-separate-mon (outline parameters)
  (let* ((conc (car outline))
	 (precond (cadr outline))
	 (pos (car parameters))
	 (term1 (data~struct-at-position (node~formula conc) (pos~add-end 1 pos)))
	 (term2 (data~struct-at-position (node~formula conc) (pos~add-end 2 pos))))
    (tacl~init outline)
    (cond ((polynom=0-monomial-p term1)
	   (tacl~sequence
	    (rew-res0 ('apply-rewrite (list conc nil) (list pos 'p-add-real1 nil nil)))
	    (lam-res ('beta-normalize (list (cadr rew-res0) nil) nil))
	    (rew-res1 ('apply-rewrite (list (cadr lam-res) precond) (list (pos~add-end 0 pos) '0-plus-real nil nil)))))
	  ((polynom=0-monomial-p term2)
	   (tacl~sequence 
	    (rew-res0 ('apply-rewrite (list conc nil) (list pos 'p-add-real1 nil nil)))
	    (lam-res ('beta-normalize (list (cadr rew-res0) nil) nil))
	    (rew-res1 ('apply-rewrite (list (cadr lam-res) nil) (list (pos~add-end 0 pos) 'c-plus-real nil nil)))
	    (rew-res2 ('apply-rewrite (list (cadr rew-res1) precond) (list (pos~add-end 0 pos) '0-plus-real nil nil)))))
	  (t (tacl~sequence
	      (rew-res ('apply-rewrite (list conc nil) (list pos 'p-add-real1 nil nil)))
	      (lam-res ('lambda (list (cadr rew-res) precond) nil)))))
    (tacl~end)))
	
