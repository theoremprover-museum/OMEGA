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

(th~deftheory BLOCK
	      (uses matrix)
	      (help "Data structure for blocks of blocks"))

(th~deftype bloc
	    (in block)
	    (arguments 1)
	    (help "The type of blocks containing elements of the same type."))


;; Some block constructors

(th~defconstant block2
 	    (in block)
	    (type (all-types aa ((bloc aa) (bloc aa) (bloc aa)(bloc aa)(bloc aa))))
	    (help "The 2x2 block constructor."))

(th~defconstant block1
 	    (in block)
	    (type (all-types aa ((bloc aa) aa)))
	    (help "The 1x1 block constructor."))

(th~defconstant zeroblock
 	    (in block)
	    (type (all-types aa ((bloc aa) num num)))
	    (help "The zero block of size m x n constructor."))

;; Predicate Matrick

(th~defconstant  matrick
	     (in block)
	     (type (all-types aa (o (bloc aa) num num )))
	     (help "The predicate on blocks for being a m x n matrix."))

(th~defaxiom matrick-step
	     (in block)
	     (formula
	      (all-types aa
			 (forall-sort (lam (m num)
			 (forall-sort (lam (n num)
			 (forall (lam (B1 (bloc aa))
			 (forall (lam (B2 (bloc aa))
			 (forall (lam (B3 (bloc aa))
			 (forall (lam (B4 (bloc aa))
				      (equiv (matrick m n (block2 B1 B2 B3 B4))
					     (exists-sort (lam (k num)
				             (exists-sort (lam (l num)
							       (and (and
								     (matrick k l B1)
								     (matrick (minus m k)  l B2))
								    (and
								     (matrick k (minus n l) B3)
								     (matrick (minus m k)(minus n l) B4))))
							  nat)) nat))
				      ))))))))) nat))nat)))
	     (help "Step case for being a matrick."))

(th~defaxiom matrick-base1
	     (in block)
	     (formula
	      (all-types aa
			 (forall-sort (lam (m num)
			 (forall-sort (lam (n num)
			 (forall (lam (elem aa)
				      (matrick 1 1 (block1 elem))
				      )))nat))nat)))
	     (help "Base case for being a matrick."))

(th~defaxiom matrick-zero
	     (in block)
	     (formula
	      (all-types aa
			 (forall-sort (lam (m num)
			 (forall-sort (lam (n num)
				      (matrick m n  (zeroblock m n))
				      )nat))nat)))
	     (help "Base case for being a matrick."))


;; Operation plus

(th~defconstant block-plus
 	    (in block)
	    (type (all-types aa ((bloc aa)(bloc aa)(bloc aa)(aa aa aa))))
	    (help "Times for Blocks."))

(th~defaxiom block-plus-step
	     (in block)
	     (formula
	      (all-types aa
			 (forall-sort (lam (m1 num)
			 (forall-sort (lam (m2 num)
			 (forall-sort (lam (n1 num)
			 (forall-sort (lam (n2 num)
			 (forall (lam (A1 (bloc aa))
			 (forall (lam (A2 (bloc aa))
			 (forall (lam (A3 (bloc aa))
			 (forall (lam (A4 (bloc aa))
			 (forall (lam (B1 (bloc aa))
			 (forall (lam (B2 (bloc aa))
			 (forall (lam (B3 (bloc aa))
			 (forall (lam (B4 (bloc aa))
			 (forall (lam (someplus (aa aa aa))
				      (implies (and (and (and (matrick m1 n1 A1)
							      (matrick m1 n1 B1))
							 (and (matrick m1 n2 A2)
							      (matrick m1 n2 B2)))
						    (and (and (matrick m2 n1 A3)
							      (matrick m2 n1 B3))
							 (and (matrick m2 n2 A4)
							      (matrick m2 n2 B4))))
					       (= (block-plus someplus
							      (block2 A1 A2 A3 A4)
							      (block2 B1 B2 B3 B4))
						  (block2 (block-plus someplus A1 B1)
							  (block-plus someplus A2 B2)
							  (block-plus someplus A3 B3)
							  (block-plus someplus A4 B4))))
				      ))))))))))))))))))) nat))nat)) nat)) nat)))
	     (help "to come."))


(th~defaxiom block-plus-base1
	     (in block)
	     (formula
	      (all-types aa
			 (forall (lam (A1 aa)
			 (forall (lam (B1 aa)
			 (forall (lam (someplus (aa aa aa))
					       (= (block-plus someplus
							      (block1 a1)
							      (block1 b1))
						  (block1 (someplus a1 b1)))
				      ))))))))
	     (help "to come."))

(th~defaxiom block-plus-zero
	     (in block)
	     (formula
	      (all-types aa
			 (forall-sort (lam (m num)
		         (forall-sort (lam (n num)
			 (forall (lam (op (aa aa aa))
				      (unit (matrick m n) (block-plus op) (zeroblock m n))))) nat)) nat)))
	      (help "to come."))

;; operation times

(th~defconstant block-times
 	    (in block)
	    (type (all-types aa ((bloc aa)(bloc aa)(bloc aa)(aa aa aa)(aa aa aa))))
	    (help "Times for Blocks."))

(th~defaxiom block-times-step
	     (in block)
	     (formula
	      (all-types aa
			 (forall-sort (lam (m1 num)
			 (forall-sort (lam (n1 num)
			 (forall-sort (lam (k1 num)
			 (forall-sort (lam (m2 num)
			 (forall-sort (lam (n2 num)
			 (forall-sort (lam (k2 num)
			 (forall (lam (A1 (bloc aa))
			 (forall (lam (A2 (bloc aa))
			 (forall (lam (A3 (bloc aa))
			 (forall (lam (A4 (bloc aa))
			 (forall (lam (B1 (bloc aa))
			 (forall (lam (B2 (bloc aa))
			 (forall (lam (B3 (bloc aa))
			 (forall (lam (B4 (bloc aa))
			 (forall (lam (someplus (aa aa aa))
			 (forall (lam (sometimes (aa aa aa))
				      (implies (and (and (and (matrick m1 k1 A1)
							      (matrick k1 n1 B1))
							 (and (matrick m1 k2 A2)
							      (matrick k1 n2 B2)))
						    (and (and (matrick m2 k1 A3)
							      (matrick k2 n1 B3))
							 (and (matrick m2 k2 A4)
							      (matrick k2 n2 B4))))
					       (= (block-times someplus sometimes
							       (block2 A1 A2 A3 A4)
							       (block2 B1 B2 B3 B4))
						  (block2 (block-plus someplus
								      (block-times someplus sometimes A1 B1)
								      (block-times someplus sometimes A2 B3))
							  (block-plus someplus
								      (block-times someplus sometimes A1 B2)
								      (block-times someplus sometimes A2 B4))
							  (block-plus someplus
								      (block-times someplus sometimes A3 B1)
								      (block-times someplus sometimes A4 B3))
							  (block-plus someplus
								      (block-times someplus sometimes A3 B2)
								      (block-times someplus sometimes A4 B4)))))
				      ))))))))))))))))))))) nat)) nat)) nat)) nat)) nat)) nat)))
	     (help "to come."))


(th~defaxiom block-times-base1
	     (in block)
	     (formula
	      (all-types aa
			 (forall (lam (A1 aa)
			 (forall (lam (B1 aa)
			 (forall (lam (someplus (aa aa aa))
			 (forall (lam (sometimes (aa aa aa))
				      (= (block-times someplus sometimes
						     (block1 a1)
						     (block1 b1))
						  (block1 (sometimes a1 b1)))
				      ))))))))))
	     (help "to come."))

(th~defaxiom block-times-zero
	     (in block)
	     (formula
	      (all-types aa
			 (forall-sort (lam (m num)
		         (forall-sort (lam (n num)
			 (forall (lam (op1 (aa aa aa))
			 (forall (lam (op2 (aa aa aa))
			 (forall (lam (A1 (bloc aa))
				(implies  (matrick m n A1)
					  (and 
					   (= (block-times op1 op2 A1 (zeroblock m n)) (zeroblock m n))
					   (= (block-times op1 op2 (zeroblock m n) A1) (zeroblock m n))))
				)))))))
				      nat)) nat)))
	     (help "to come."))


;; Square triangle matrick of size m x n

(th~defconstant triangle-block
 	    (in block)
	    (type (all-types aa ((bloc aa) num num)))
	    (help "Triangle Blocks."))


(th~defaxiom block-times-zero
	     (in block)
	     (formula
	      (all-types aa
			 (forall-sort (lam (m num)
		         (forall-sort (lam (n num)
			 (exists  (lam (a aa)
			 (exists-sort (lam (v1 (bloc aa))
					   (= (triangle-block m n)
					      (= (block a  
				)))))))
				      nat)) nat)))
	     (help "to come."))
