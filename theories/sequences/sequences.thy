;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: THEORY -*-
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

(th~deftheory sequences
	      (uses natural metric)
	      (help "Theory of sequences and limits."))

(th~defdef sequence-in
	   (in sequences)
	   (type-variables bb)
           (definition
             (lam (Seq (bb num))
		    (lam (G (o bb))
			 (forall (lam (x num)
				      (implies (in x nat)
					       (in (seq x) G)))))))
	   (help "sequence-in G S is true, iff all elements of S are in G."))

(th~defdef sequence-lift-2
	   (in sequences)
	   (type-variables bb cc dd)
           (definition	    
             (lam (op (bb cc dd))
		       (lam (seq1 (dd num))
			    (lam (seq2 (cc num))
				 (lam (x num)
				      (op (seq1 x) (seq2 x)))))))
	   (help "Lifted a binary operation to sequences."))


(th~defdef epsilon-n-quant
	   (in sequences)
	   (type-variables aa bb)
           (definition
             (lam (S1 (bistruct aa bb))
		(lam (F (aa num num))
		 (forall (lam (epsilon aa)
		  (implies (in epsilon 
			       (struct-set (struct-measure-field S1)))
			   (exists (lam (n num)
			    (and (in n Nat)
		                 (forall (lam (m num)
			          (implies (and (in m Nat) (geq m n))
					   (struct-ordering
					    (struct-measure-field S1)
					    (F n m)
					    epsilon)))))))))))))
	   (help "The epsilon-n quantifier for a metric space, it takes as
  	          arguments the metric space S, a binary function F from natural numbers
	          to the measure-space M of S, and tests whether for all for all
                  epsilon in M, there is a natural number n, such that for all m>n
                  F(n,m) is less than epsilon. This quantifier is  very convenient
                  for defining notions of linits of sequences."))


(th~defdef is-limit-of
	   (in sequences)
	   (type-variables aa bb)
           (definition
             (lam (S1 (bistruct aa bb))
		(lam (Seq (bb num))
		 (lam (x bb)
		  (epsilon-n-quant S1
		   (lam (n num) (lam (m num) (struct-metric S1 x (seq m)))))))))
	   (help "The predicate that checks whether the third argument is
                  the limit of the sequence (arg 2) in a metric space (arg 1)."))


(th~defdef limit
	   (in sequences)
	   (type-variables aa bb)
           (definition
             (lam (S1 (bistruct aa bb))
		    (lam (Seq (bb num))
			 (that (lam (x bb)
				    (is-limit-of S1 seq x))))))
	   (help "The limit of a sequence in a metric space."))



(th~defdef convergent
	   (in sequences)
	   (type-variables aa bb)
           (definition
             (lam (S1 (bistruct aa bb))
		    (lam (Seq (bb num))
			 (defined (limit S1 seq)))))
	   (help "The convergence predicate for sequences: 
                  A sequence is convergent, iff it has a limit."))


(th~defdef null-sequence
	   (in sequences)
	   (type-variables aa bb)
           (definition
             (lam (S1 (bistruct aa bb))
                  (lam (Seq (bb num))
                       (= (limit S1 seq)
                          (struct-neut (struct-base S1))))))
	   (help "A sequence is a null-sequence, iff its limit is zero."))

(th~defdef tail-sequence
	   (in sequences)
	   (type-variables bb)
           (definition
             (lam (n num)
                  (lam (seq (bb num))
                       (lam (m num)
                            (seq (plus m n))))))
           (help "The n-tail of a sequence, i.e. (tail-sequence n seq) (m) 
                  is seq n+m."))


(th~defdef cauchy-sequence
	   (in sequences)
	   (type-variables aa bb)
           (definition
             (lam (S1 (bistruct aa bb))
                  (lam (seq (bb num))
		       (epsilon-n-quant S1
 		        (lam (n num) (lam (m num) (struct-metric S1 (seq n) (seq m))))))))
	   (help "The definition of a Cauchy sequence."))


(th~defdef sumton
	   (in sequences)
	   (type-variables aa)
	   (definition
	     (lam (S (bistruct aa aa))
		  (lam (Seq (aa num))
		       (lam (n num)
			    ((iterate-warg
			      (lam (i num)
				   (struct-op (struct-measure-field S) (Seq i)))
			      n
			     (struct-neut (struct-measure-field S))))))))
	   (help "In a given structure St, (sumton St G n) is the sum
                  (where (struct-op St) takes the place of the addition
                   operation) of the first n elements of the sequence G.
                  (sumton St G) is the sequence of sums."))

(th~defdef sumofsequence
           (in sequences)
           (type-variables aa)
           (definition
             (lam (S1 (bistruct aa aa))
                    (lam (Seq (aa num))
                         (limit S1 (sumton S1 Seq)))))
           (help "The sum of the sequence"))

							     	
