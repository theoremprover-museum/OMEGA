;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: LISP -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     OMEGA Project                                                         ;;
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

(th~deftheory NEUMANN-Bernays-Goedel
	      (uses post)
	      (credit "http://mbase.ags.uni-sb.de/~authors/Neumann.html"
	              "http://mbase.ags.uni-sb.de/~authors/Bernays.html"
	              "http://mbase.ags.uni-sb.de/~authors/Goedel.html")
              (author "http://www.ags.uni-sb.de/~kohlhase")
	      (date 16091998)
	      (type-constants class)
	      (constants (in (o class class)))
	      (help "von Neumann Bernays Goedel set theory."))

(th~defdef nbg-set
	   (in Neumann-Bernays-Goedel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	   (date 16091998)
	   (definition
	    (lam (Z class)
		 (exists (lam (S class)
		  (in Z S)))))
	   (help "The property of being a set in ZF set theory"))
	     
	   
(th~defdef nbg-pairset
	   (in Neumann-Bernays-Goedel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	   (date 17091998)
           (credit "http://mbase.ags.uni-sb.de/~authors/zermelo.html")
	   (definition
	     (lam (A class)
		  (lam (B class)
		       (that (lam (P class) ;;; do we need to require nbg-set here?
			(forall (lam (Y class)
			 (equiv (in Y P)
				(or (= Y A) (= Y B))))))))))
	   (help "The operator for creating sets of two elements in NBG set theory."))


(th~defaxiom nbg-pairset
	   (in Neumann-Bernays-Goedel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	   (date 16091998)
           (formula
             (forall (lam (A class)
              (forall (lam (B class)
	       (nbg-set (nbg-pairset A B)))))))
	   (help "The Axiom for pairs in  NBG set theory."))


(th~defaxiom nbg-extensionality
	   (in Neumann-Bernays-Goedel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	   (date 16091998)
           (formula
             (forall (lam (S class)
              (forall (lam (T class)
               (implies (forall-sort nbg-set (lam (z class)
			 (equiv (in z S) (in z T))))
			(= S T)))))))
	   (help "The Extensionality Axiom of NBG set theory: two sets are equal,
	   if they have the same elements."))

(th~defdef nbg-singleton
	   (in Neumann-Bernays-Goedel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	   (date 17091998)
           (definition
             (lam (X class) (nbg-pairset X X)))
	   (help "The singleton set operator of NBG set theory, defined by as the pair set of identical elements."))

(th~defdef nbg-opair
	   (in Neumann-Bernays-Goedel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	   (date 17091998)
           (credit "http://mbase.ags.uni-sb.de/~authors/zermelo.html")
	   (definition
	     (lam (x class)
		  (lam (y class)
		       (nbg-pairset (nbg-singleton x)
				   (nbg-pairset x y)))))
	   (help "The constructor for ordered pairs in NBG set theory.
                  An ordered pair (x,y) is defined as {{x},{x,y}}."))



(th~defdef nbg-first
	   (in Neumann-Bernays-Goedel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	   (date 17091998)
	   (definition
	     (lam (P class)
		  (that (lam (Z class)
     		    (in (nbg-singleton Z) P)))))
	   (help "The projection onto the first element of an orderd pair in NBG set theory."))

(th~defdef nbg-second
	   (in Neumann-Bernays-Goedel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	   (date 17091998)
           (credit "http://mbase.ags.uni-sb.de/~authors/zermelo.html")
	   (definition
	     (lam (P class)
		  (that (lam (Z class)
     		    (in (nbg-pairset (nbg-first P) Z) P)))))
	   (help "The projection onto the second element of an orderd pair in NBG set theory."))		 


(th~defaxiom nbg-membership-class
	   (in Neumann-Bernays-Goedel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	   (date 16091998)
           (formula
             (exists (lam (M class)
              (forall (lam (X class)
               (forall (lam (Y class)
               (equiv (in (nbg-opair X Y) M)
		      (in X Y)))))))))
	   (help "The Axiom of NBG set theory that guarantees the existence of the membership relation as a class."))


(th~defdef nbg-intersection
	   (in Neumann-Bernays-Goedel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	   (date 17091998)
	   (definition
	     (lam (A class)
		  (lam (B class)
		       (that (lam (Z class) 
			(forall-sort nbg-set (lam (Y class)
			 (equiv (in Y Z) (and (in Y A) (in Y B))))))))))
	   (help "The intersection operator from NBG set theory."))

(th~defaxiom nbg-intersection
	   (in Neumann-Bernays-Goedel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	   (date 16091998)
           (formula
             (forall (lam (A class)
              (forall (lam (B class)
	       (defined (nbg-intersection A B)))))))
	   (help "The Axiom for intersections in  NBG set theory."))

(th~defdef nbg-union
	   (in Neumann-Bernays-Goedel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	   (date 16091998)
           (credit "http://mbase.ags.uni-sb.de/~authors/zermelo.html")
	   (definition
	    (lam (S class)
             (lam (T class)
  	      (that (lam (U class)
	       (forall (lam (Z class)
	        (implies (or (in Z S) (in Z T))
			 (in Z U)))))))))
	   (help "The union operator from NBG set theory."))



(th~defdef nbg-complement
	   (in Neumann-Bernays-Goedel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	   (date 16091998)
	   (definition
	    (lam (S class)
		 (that (lam (C class)
		  (forall-sort nbg-set (lam (z class)
		   (equiv (in z C) (not (in z S)))))))))
	   (help "The comlement operator from NBG set theory:
                  (nbg-complement T) is the set of all Z that are
                  not in T."))

(th~defaxiom nbg-complement
	   (in Neumann-Bernays-Goedel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	   (date 16091998)
           (formula
             (forall (lam (A class)
	       (defined (nbg-complement A)))))
	   (help "The Axiom for intersections in  NBG set theory."))

(th~defdef nbg-domain
	   (in Neumann-Bernays-Goedel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	   (date 17091998)
	   (definition
	     (lam (R class)
		  (that (lam (D class)
		   (forall-sort nbg-set (lam (X class)
		    (equiv (in X D)
			   (exists-sort nbg-set (lam (Y class)
			    (in (nbg-opair X Y) R))))))))))
	   (help "The domain of a relation from NBG set theory."))

(th~defaxiom nbg-domain
	   (in Neumann-Bernays-Goedel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	   (date 16091998)
           (formula
             (forall (lam (A class)
	       (defined (nbg-domain A)))))
	   (help "The Axiom for domains in  NBG set theory."))

(th~defaxiom nbg-direct-product
	   (in Neumann-Bernays-Goedel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	   (date 16091998)
           (formula
             (forall (lam (A class)
              (exists (lam (B class)
               (forall-sort nbg-set (lam (X class)
                (forall-sort nbg-set (lam (Y class)
		 (equiv (in (nbg-opair X Y) B)
			(in X A)))))))))))
	   (help "The Axiom for direct products in  NBG set theory."))

(th~defaxiom nbg-permutation-1
	   (in Neumann-Bernays-Goedel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	   (date 16091998)
           (formula
             (forall (lam (A class)
              (exists (lam (B class)
               (forall-sort nbg-set(lam (X class)
                (forall-sort nbg-set (lam (Y class)
		 (equiv (in (nbg-opair X Y) B)
			(in (nbg-opair Y X) A)))))))))))
	   (help "The first permutation Axiom in  NBG set theory."))

(th~defdef nbg-otriple
	   (in Neumann-Bernays-Goedel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	   (date 17091998)
	   (definition
	     (lam (x class)
		  (lam (y class)
		       (lam (z class)
		       (nbg-opair  x (nbg-opair y z))))))
	   (help "The constructor for ordered triples in NBG set theory."))


(th~defaxiom nbg-permutation-2
	   (in Neumann-Bernays-Goedel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	   (date 16091998)
           (formula
             (forall (lam (A class)
              (exists (lam (B class)
               (forall-sort nbg-set (lam (X class)
                (forall-sort nbg-set (lam (Y class)
                 (forall-sort nbg-set (lam (Z class)
		  (equiv (in (nbg-otriple X Y Z) B)
			 (in (nbg-otriple Y Z X) A)))))))))))))
	   (help "The second permutation Axiom in  NBG set theory."))


(th~defaxiom nbg-permutation-3
	   (in Neumann-Bernays-Goedel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	   (date 16091998)
           (formula
             (forall (lam (A class)
              (exists (lam (B class)
               (forall-sort nbg-set (lam (X class)
                (forall-sort nbg-set (lam (Y class)
                 (forall-sort nbg-set (lam (Z class)
		  (equiv (in (nbg-otriple X Y Z) B)
			 (in (nbg-otriple X Z Y) A)))))))))))))
	   (help "The third permutation Axiom in  NBG set theory."))


(th~defdef nbg-empty
	   (in Neumann-Bernays-Goedel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	   (date 17091998)
           (credit "http://mbase.ags.uni-sb.de/~authors/zermelo.html")
           (definition
             (lam (A class)
		  (forall (lam (Z class)
		   (not (in Z A))))))
	   (help "The emptyness predicate in NBG set theory."))

(th~defdef nbg-subset
	   (in Neumann-Bernays-Goedel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	   (date 16091998)
           (definition
             (lam (U class)
                  (lam (V class)
                       (forall-sort nbg-set (lam (x class)
			(implies (in x U) (in x V)))))))
	   (help "Subset for NBG set theory, defined by implication"))

(th~defaxiom nbg-infinity
	   (in Neumann-Bernays-Goedel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	   (date 16091998)
           (formula
             (exists (lam (S class)
	      (and (and (nbg-set S) (not (nbg-empty S)))
		   (forall (lam (Y class)
		    (implies (and (nbg-set Y) (in Y S))
			     (exists (lam (Z class)
			      (and (and (nbg-set Z)
					(in Z S))
				   (nbg-subset Y Z)))))))))))
	   (help "The Axiom of infinity in  NBG set theory."))

(th~defdef nbg-sum-class
	   (in Neumann-Bernays-Goedel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	   (date 17091998)
	   (date 16091998)
           (definition
             (lam (S class)
	      (that (lam (P class)
	       (forall (lam (T class)
	        (implies (nbg-set T)
			 (equiv (in T P)
				(exists (lam (Y class)
				 (and (nbg-set Y)
				      (and (in T Y)
					   (in Y S)))))))))))))
	   (help "The sum class operator of NBG set theory."))


(th~defaxiom nbg-sum-class
	   (in Neumann-Bernays-Goedel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	   (date 16091998)
           (formula
             (forall (lam (A class)
	       (nbg-set (nbg-sum-class A)))))
	   (help "The Axiom for domains in  NBG set theory."))

(th~defdef nbg-power-class
	   (in Neumann-Bernays-Goedel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	   (date 17091998)
           (definition
             (lam (S class)
	      (that (lam (P class)
	       (forall (lam (T class)
	        (implies (nbg-set T)
			 (equiv (in T P) (nbg-subset T S)))))))))
	   (help "The power set operator of NBG set theory, defined by subset."))


(th~defaxiom nbg-power-class
	   (in Neumann-Bernays-Goedel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	   (date 16091998)
           (formula
             (forall (lam (A class)
	       (nbg-set (nbg-power-class A)))))
	   (help "The Axiom for the power class in  NBG set theory."))

(th~defdef nbg-univocal
	   (in Neumann-Bernays-Goedel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	   (date 16091998)
           (definition
            (lam (A class)
		 (forall (lam (X class)
                  (forall (lam (Y class)
                   (forall (lam (Z class)
		    (implies (and (and (nbg-set X) (nbg-set Y))  (nbg-set Y))
			     (implies (and (in (nbg-opair X Y) A)
					   (in (nbg-opair X Z) A))
				      (= Y Z)))))))))))
	   (help "The property of being univocal from  NBG set theory."))

(th~defaxiom nbg-replacement
	   (in Neumann-Bernays-Goedel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	   (date 16091998)
           (formula
            (forall (lam (A class)
	     (implies (nbg-univocal A)
		      (exists (lam (X class)
                       (exists (lam (Y class)
			(exists (lam (Z class)
		         (implies (and (and (nbg-set X) (nbg-set Y))  (nbg-set Y))
			     (equiv (in Z Y)
				    (exists (lam (W class)
				     (implies (nbg-set W)
					      (and (in W X)
						   (in (nbg-opair W Z) A)))))))))))))))))
			      (help "The replacement Axiom NBG set theory."))

(th~defdef nbg-function
	   (in Neumann-Bernays-Goedel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	   (date 16091998)
           (definition
            (lam (F class)
		 (and (nbg-univocal F)
		      (forall (lam (X class)
		         (implies (nbg-set X)
				  (equiv (in X F)
					 (exists (lam (Y class)
					  (exists (lam (Z class)
					   (and (and (nbg-set Y) (nbg-set Z))
						(= Z (nbg-opair Y Z))))))))))))))
	   (help "The property of being a funciton from NBG set theory."))

(th~defaxiom nbg-foundation
	     (in Neumann-Bernays-Goedel)
	     (author "http://www.ags.uni-sb.de/~kohlhase")
	     (formula
	      (forall (lam (S class)
	       (implies (not (nbg-empty S))
			(exists (lam (X class)
			 (and (and (nbg-set X) (in X S))
			      (forall (lam (Y class)
			       (implies (nbg-set Y)
					(not (and (in Y X) (in Y S)))))))))))))
	     (help "The foundation axiom of NBG set theory."))

(th~defdef nbg-apply
	   (in Neumann-Bernays-Goedel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	   (date 18091998)
	   (definition
	     (lam (F class)
	      (lam (arg class)
	       (that (lam (z class)
		(in (nbg-opair arg z) F))))))
	   (help "Function application in NBG set theory."))
	
(th~defaxiom nbg-global-choice
	     (in Neumann-Bernays-Goedel)
	     (author "http://www.ags.uni-sb.de/~kohlhase")
	     (formula
	      (exists (lam (F class)
	       (and (nbg-function F)
		    (forall-sort nbg-set (lam (x class)
		     (implies (not (nbg-empty X))
			      (in (nbg-apply F X) X))))))))
	     (help "The axiom of global choice in NBG set theory."))

#|complete nonsense
(th~defdef nbg-lambda
	     (in Neumann-Bernays-Goedel)
	     (author "http://www.ags.uni-sb.de/~kohlhase")
	     (definition
	      (lam (x class)
		   (lam (A class)
			(that (lam (F class)
			 (forall (lam (a class)
			  (forall (lam (b class)
			   (equiv (in (nbg-opair a b) F)
				  (= b (substitute ))))))))))))
	     (help "The function constructor in NBG set theory."))
		


|#
