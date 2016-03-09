;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: Theory -*-
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

(th~deftheory MATRIX
	      (uses field real)
	      (help "Matrix theory based on double indexed functions."))

;; basics

(th~defdef matrix
	   (in matrix)
	   (type-variables aa)
           (definition
	     (lam (aij (aa num num))
		  (lam (m num)
		       (lam (n num)
			    (lam (F (o aa))
				 (forall-sort (lam (ii num)
						   (forall-sort (lam (jj num)
								     (in (aij ii jj) F))
								(integer-intervall 1 n)))
					      (integer-intervall 1 m)))))))
	   (help "An m x n matrix with elements from F defined by double inedexed function."))

(th~defdef vector
	   (in matrix)
	   (type-variables aa)
           (definition
	     (lam (vi (aa num))
		  (lam (n num)
		       (lam (F (o aa))
			    (forall-sort (lam (ii num)
					      (in (vi ii) F))
					 (integer-intervall 1 n))))))
	   (help "A vector containing n elements from F as indexed function."))


(th~defdef diag-matrix
	   (in matrix)
	   (type-variables aa)
           (definition
	     (lam (diag aa)
		  (lam (other aa)
		       (lam (ii num)
			    (lam (jj num)
				 (ifthen (= ii jj) diag  other))))))
	   (help "The matrix having `diag' on the diagonal, `other' everywhere else."))

(th~defdef vector-n
	   (in matrix)
	   (type-variables aa)
           (definition
	     (lam (elem aa)
		  (lam (other aa)
		       (lam (n num)
			    (lam (ii num)
				     (ifthen (= ii n) elem  other))))))
	     (help "The vector having elem at position n, other everywhere else."))

(th~defdef matrixfromvector
	   (in matrix)
	   (type-variables aa)
           (definition
	     (lam (v (aa num))
		  (lam (ii num)
		       (lam (jj num)
			    (v jj)))))
	   (help "A vector as n x 1 matrix."))

(th~defdef nthrowvector
	   (in matrix)
	   (type-variables aa)
           (definition
	     (lam (aij (aa num num))
		  (lam (n num)
		       (lam (ii num) (aij n ii)))))
	     (help "The nth row vector of a matrix."))

(th~defdef nthcolvector
	   (in matrix)
	   (type-variables aa)
           (definition
	     (lam (aij (aa num num))
		  (lam (n num)
		       (lam (ii num) (aij ii n)))))
	     (help "The nth column vector of a matrix."))

;; operations on integer-intervalls

(th~defdef foldl
	   (in matrix)
	   (type-variables aa)
           (definition
	     (lam (op (aa aa aa))
		  (lam (start num)
		       (lam (end num)
			    (lam  (ai (aa num))
				  (recursion-poly
				   (lam (ii num) (op (ai (s (plus ii end)))))
				   (ai end)
				   (minus start end)))))))
	   (help "Application of operation op over elements a_start to a_end."))

(th~deftheorem foldl-base
             (in matrix)
	     (conclusion
	     (all-types aa
		        (forall-sort (lam (m num)
			(forall (lam (ai (aa num))
		        (forall (lam (op (aa aa aa))
				     (= (foldl op m m ai) (ai m)))))))
				     nat)))
	   (help "Base case of foldl."))


(th~deftheorem foldl-step
             (in matrix)
	     (conclusion
	     (all-types aa
		        (forall-sort (lam (m num)
		        (forall-sort (lam (n num)
			(forall (lam (ai (aa num))
		        (forall (lam (op (aa aa aa))
				     (implies (leq n m)
					      (= (foldl op m (s n) ai)
						 (op (ai (plus (s n) m)) (foldl op m n ai)))))))))
				     nat))
				     nat)))
	   (help "Step case of foldl."))


;; Scalar-multiplication of matrices

(th~defdef scalartimes
	   (in matrix)
	   (type-variables aa)
           (definition
	     (lam (op (aa aa aa))
		  (lam  (scalar aa)
			(lam  (aij (aa num num))
			      (lam (ii num)(jj num)(op scalar (aij ii jj)))))))
	   (help "Apply operation op on all elements of two matrices."))


(th~deftheorem scalartimes-is-closed
             (in matrix)
	     (conclusion
	     (all-types aa
			(forall (lam (aij (aa num num))
		        (forall-sort (lam (m num)
		        (forall-sort (lam (n num)
		        (forall (lam (F (o aa))
		        (forall-sort (lam (scalar aa)
		        (forall (lam (fadd (aa aa aa))
		        (forall (lam (fmul (aa aa aa))
				     (implies (and (ring f fadd fmul)
						   (matrix aij m n F))
					      (matrix (scalartimes fmul scalar aij) m n F))
				     )))))F))))nat))nat)))))
	   (help "Multiplication with scalar and m x n matrices is closed."))



;; Addition of matrices

(th~defdef matplus
	   (in matrix)
	   (type-variables aa)
           (definition
	     (lam (op (aa aa aa))
		  (lam  (aij (aa num num))
			(lam  (bij (aa num num))
			      (lam (ii num)(jj num)(op (aij ii jj)(bij ii jj)))))))
	   (help "Apply operation op on all elements of two matrices."))


(th~deftheorem matplus-is-closed
             (in matrix)
	     (conclusion
	     (all-types aa
			(forall (lam (aij (aa num num))
		        (forall (lam (bij (aa num num))
		        (forall-sort (lam (m num)
		        (forall-sort (lam (n num)
		        (forall (lam (F (o aa))
		        (forall (lam (fadd (aa aa aa))
		        (forall (lam (fmul (aa aa aa))
				     (implies (and (ring f fadd fmul)
						   (and (matrix aij m n F)
							(matrix bij m n F)))
					      (matrix (matplus fadd aij bij) m n F))
				     )))))))nat))nat)))))))
	   (help "Addition of m x n matrices is closed."))


;; Multiplication of matrices

(th~defconstant mattimes
		(in matrix)
		(type  (all-types aa ((aa num num) (aa num num)(aa num num)(aa aa aa)(aa aa aa))))
		(help "Multiplication of matrices."))

(th~defaxiom mattimes-def
             (in matrix)
	     (formula
	     (all-types aa
			(forall (lam (aij (aa num num))
		        (forall (lam (bij (aa num num))
		        (forall-sort (lam (m num)
		        (forall-sort (lam (k num)
		        (forall-sort (lam (n num)
		        (forall (lam (F (o aa))
		        (forall (lam (fadd (aa aa aa))
		        (forall (lam (fmul (aa aa aa))
				     (implies (and (ring f fadd fmul)
						   (and (matrix aij m k f)
							(matrix bij k n f)))
					      (= (mattimes fmul fadd aij bij)
						 (lam (ii num)(jj num)
						      (foldl fadd 1 k (lam (ff num)(fmul (aij ii ff) (bij ff jj)))))))
				     )))))))nat))nat))nat)))))))
	   (help "Definition of Multiplication of matrices."))

	   
(th~deftheorem mattimes-closed
             (in matrix)
	     (conclusion
		      (all-types aa
			(forall (lam (aij (aa num num))
		        (forall (lam (bij (aa num num))
		        (forall-sort (lam (m num)
		        (forall-sort (lam (k num)
		        (forall-sort (lam (n num)
		        (forall (lam (F (o aa))
		        (forall (lam (fadd (aa aa aa))
		        (forall (lam (fmul (aa aa aa))
				     (implies (and (ring f fadd fmul)
						   (and (matrix aij m k F)
							(matrix bij k n F)))
					      (matrix (mattimes fmul fadd aij bij) m n F))
				     )))))))nat))nat))nat)))))))
	   (help "Multiplication  of m x k matrix with k x n matrix is a m x n matrix."))

;; Transposed matrix

(th~defdef transpose
	   (in matrix)
	   (type-variables aa)
           (definition
	     (lam (aij (aa num num)) (lam (ii num)(jj num) (aij jj ii))))
	   (help "Change the indexes."))

(th~deftheorem transposed-matrix
             (in matrix)
	     (conclusion
	      (all-types aa
			(forall (lam (aij (aa num num))
		        (forall-sort (lam (m num)
		        (forall-sort (lam (n num)
		        (forall (lam (F (o aa))
				     (implies
				      (matrix aij m n F)
				      (matrix (transpose aij) n m F))
				     )))nat))nat)))))
	     (help "Change the indexes."))

(th~defconstant determinant
		(in matrix)
		(type  (all-types aa (num (aa num num))))
		(help "Determinant of a matrix."))
