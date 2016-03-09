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

(th~deftheory ZERMELO-FRAENKEL
	      (uses post)
	      (credit "http://mbase.ags.uni-sb.de/~authors/zermelo.html"
	              "http://mbase.ags.uni-sb.de/~authors/fraenkel.html")
              (author "http://www.ags.uni-sb.de/~kohlhase")
	      (date 16091998)
	      (help "Zermelo Fraenkel set theory."))

(th~deftype zf
	    (in zermelo-fraenkel)
	    (arguments 0)
	    (help "The type of sets (all objects are sets in ZF)"))

(th~defconstant in
		(in zermelo-fraenkel)
		(type (o zf zf))
		(help "The elementary membership predicate of Zermelo Fraenkel set theory."))

(th~defaxiom zf-extensionality
	   (in zermelo-fraenkel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	   (date 16091998)
           (credit "http://mbase.ags.uni-sb.de/~authors/zermelo.html")
           (formula
             (forall (lam (S zf)
              (forall (lam (T zf)
               (implies (forall (lam (z zf)
                         (equiv (in z S) (in z T))))
			(= S T)))))))
	   (help "The Extensionality Axiom of ZF set theory: two sets are equal,
	   if they have the same elements."))

(th~defdef zf-select
	   (in zermelo-fraenkel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	   (date 16091998)
           (credit "http://mbase.ags.uni-sb.de/~authors/zermelo.html")
	   (definition
	    (lam (S zf)
	         (lam (P (o zf))
		      (that (lam (T zf)
	                    (forall (lam (z zf) 
                            (equiv (in z T)
		                   (and (in z S) (P z))))))))))
	   (help "The selection operator from ZF set theory. (zf-select S P) is the
	   set of those Z in S, such that PZ holds."))

(th~defaxiom zf-selection
	   (in zermelo-fraenkel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	   (date 16091998)
           (credit "http://mbase.ags.uni-sb.de/~authors/zermelo.html")
           (formula 
            (forall (lam (P (o zf))
	     (forall (lam (S zf)
	      (defined (zf-select S P)))))))
           (help "The selection axiom of ZF set theory. For any set S and any
     	   property P, there is a set T={z in S|Pz} of all Z in S, such that P
	   holds on Z."))

	    
(th~defdef zf-union
	   (in zermelo-fraenkel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	   (date 16091998)
           (credit "http://mbase.ags.uni-sb.de/~authors/zermelo.html")
	   (definition
	    (lam (S zf)
             (lam (T zf)
  	      (that (lam (U zf)
	       (forall (lam (Z zf)
	        (implies (or (in Z S) (in Z T))
			 (in Z U)))))))))
	   (help "The union operator from ZF set theory."))

(th~defaxiom zf-union
	   (in zermelo-fraenkel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	   (date 16091998)
           (credit "http://mbase.ags.uni-sb.de/~authors/zermelo.html")
           (formula
	    (forall (lam (S zf)
	     (forall (lam (T zf)
	      (defined (zf-union S T)))))))
	   (help "The axiom on unions of ZF set theory that guarantees the
	   union of two sets."))


(th~defdef zf-intersection
	   (in zermelo-fraenkel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	   (date 16091998)
           (credit "http://mbase.ags.uni-sb.de/~authors/zermelo.html")
	   (definition
	    (lam (S zf)
             (lam (T zf)
	      (zf-select S (lam (Z zf) (in Z T))))))
	   (help "The intersection operator from ZF set theory."))

(th~defdef zf-complement
	   (in zermelo-fraenkel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	   (date 16091998)
           (credit "http://mbase.ags.uni-sb.de/~authors/zermelo.html")
	   (definition
	    (lam (S zf)
             (lam (T zf)
	      (zf-select S (lam (Z zf) (not (in Z T)))))))
	   (help "The comlement operator from ZF set theory:
                  (zf-complement S T) is the set of all Z, in S that are
                  not in T."))

(th~defdef zf-urset
	   (in zermelo-fraenkel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	(date 17091998)
           (credit "http://mbase.ags.uni-sb.de/~authors/zermelo.html")
           (definition
             (that (lam (X zf)
	      (= X X))))
	   (help "A UR-set in ZF set theory, its existence is guaranteed by the
	   infinity axiom, uniqueness by the extensionality axiom."))


(th~defdef zf-emptyset
	   (in zermelo-fraenkel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	(date 17091998)
           (credit "http://mbase.ags.uni-sb.de/~authors/zermelo.html")
           (definition
             (zf-select zf-urset (lam (Z zf) (not (= Z Z)))))
	   (help "The empty set in ZF set theory defined by by selection from the
	   ZF ur-set."))


(th~defdef zf-union-collection
	   (in zermelo-fraenkel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	   (date 16091998)
           (credit "http://mbase.ags.uni-sb.de/~authors/zermelo.html")
	   (definition
            (lam (C zf)
	     (that (lam (U zf)
	       (forall (lam (S zf)
	        (forall (lam (Z zf)
	         (implies (and (in S C) (in Z S))
		          (in Z U))))))))))
	   (help "The operator for the union over a collection of sets
                  from ZF set theory."))

(th~defdef zf-intersection-collection
	   (in zermelo-fraenkel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	   (date 16091998)
           (credit "http://mbase.ags.uni-sb.de/~authors/zermelo.html")
	   (definition
            (lam (C zf)
	     (ifthen (= C zf-emptyset)
		     zf-emptyset
		     (zf-select
		      (zf-union-collection C)
		      (lam (Z zf)
			   (forall (lam (S zf)
			    (implies (in S C) (in Z S)))))))))
	   (help "The intersection operator for collections of sets
                  from ZF set theory."))

(th~defdef zf-subset
	   (in zermelo-fraenkel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	   (date 16091998)
           (credit "http://mbase.ags.uni-sb.de/~authors/zermelo.html")
           (definition
             (lam (U zf)
                  (lam (V zf)
                       (forall (lam (x zf) (implies (in x U) (in x V)))))))
	   (help "Subset for ZF set theory, defined by implication"))

(th~defdef zf-powerset
	   (in zermelo-fraenkel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	   (date 17091998)
	   (date 16091998)
           (credit "http://mbase.ags.uni-sb.de/~authors/zermelo.html")
           (definition
             (lam (S zf)
	      (that (lam (P zf)
	       (forall (lam (T zf)
	        (implies (zf-subset T S) (in T P))))))))
	   (help "The power set operator of ZF set theory, defined by subset."))

(th~defaxiom zf-powerset
	   (in zermelo-fraenkel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	   (date 16091998)
	   (credit "http://mbase.ags.uni-sb.de/~authors/zermelo.html")
           (formula
	    (forall (lam (S zf)
	     (defined (zf-powerset S)))))
	   (help "The power set axiom of ZF set theory that guarantees the
	   powerset of a set."))


(th~defdef zf-singleton
	   (in zermelo-fraenkel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	(date 17091998)
           (credit "http://mbase.ags.uni-sb.de/~authors/zermelo.html")
           (definition
             (lam (X zf)
	      (zf-select (zf-powerset X) 
		      (lam (Z zf) (= X Z)))))
	   (help "The singleton set operator of ZF set theory, defined by
	   selection from the power set."))

(th~defaxiom zf-infinity
	   (in zermelo-fraenkel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	   (date 17091998)
           (credit "http://mbase.ags.uni-sb.de/~authors/zermelo.html")
           (formula
	    (exists (lam (S zf)
	     (and (exists (lam (Y zf) 
                   (and
		    (forall (lam (Z zf) (not (in Z Y))))
		    (in Y S))))
		  (forall (lam (Z zf)
		   (implies (in Z S)
		            (in (zf-union Z (zf-singleton Z)) S))))))))
	   (help "The axiom of infinity from ZF set theory that guarantees the
	   existence of an infinite set."))

 

(th~defdef zf-pairset
	   (in zermelo-fraenkel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	(date 17091998)
           (credit "http://mbase.ags.uni-sb.de/~authors/zermelo.html")
	   (definition
	     (lam (x zf)
		  (lam (y zf)
		       (zf-union (zf-singleton x) (zf-singleton y)))))
	   (help "The operator for creating sets of two elements in ZF set theory."))

(th~defdef zf-opair
	   (in zermelo-fraenkel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	(date 17091998)
           (credit "http://mbase.ags.uni-sb.de/~authors/zermelo.html")
	   (definition
	     (lam (x zf)
		  (lam (y zf)
		       (zf-pairset (zf-singleton x)
				   (zf-pairset x y)))))
	   (help "The constructor for ordered pairs in ZF set theory.
                  An ordered pair (x,y) is defined as {{x},{x,y}}."))

(th~defdef zf-first
	   (in zermelo-fraenkel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	(date 17091998)
	   (definition
	     (lam (P zf)
		  (that (lam (Z zf)
     		    (in (zf-singleton Z) P)))))
	   (help "The projection onto the first element of an orderd pair in ZF set theory."))

(th~defdef zf-second
	   (in zermelo-fraenkel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	(date 17091998)
           (credit "http://mbase.ags.uni-sb.de/~authors/zermelo.html")
	   (definition
	     (lam (P zf)
		  (that (lam (Z zf)
     		    (in (zf-pairset (zf-first P) Z) P)))))
	   (help "The projection onto the second element of an orderd pair in ZF set theory."))		 

(th~defdef zf-product
	   (in zermelo-fraenkel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	(date 17091998)
	   (definition
	     (lam (S zf)
	      (lam (T zf)
	       (zf-select (zf-powerset (zf-powerset (zf-union S T)))
			  (lam (Z zf)
			       (and (in (zf-first Z) S)
				    (IN (zf-second Z) T)))))))
	   (help "The constructor for Cartesian products from ZF set theory."))

(th~defdef zf-domain
	   (in zermelo-fraenkel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	(date 17091998)
	   (definition
	     (lam (R zf)
		  (ifthen
		   (= R zf-emptyset)
		   zf-emptyset
		   (zf-select (zf-union-collection (zf-union-collection R))
			      (lam (Z zf)
				   (exists (lam (W zf)
						(in (zf-opair Z W) R))))))))
	   (help "The domain of a relation from ZF set theory."))

(th~defdef zf-codomain
	   (in zermelo-fraenkel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	   (date 17091998)
	   (definition
	     (lam (R zf)
		  (ifthen
		   (= R zf-emptyset)
		   zf-emptyset
		   (zf-select (zf-union-collection (zf-union-collection R))
			      (lam (Z zf)
				   (exists (lam (W zf)
						(in (zf-opair W Z) R))))))))
	   (help "The codomain of a relation from ZF set theory."))

(th~defdef zf-function
	   (in zermelo-fraenkel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	   (date 18091998)
	   (definition
	     (lam (F zf)
		  (forall (lam (x zf)
		   (forall (lam (y zf)
		    (forall (lam (z zf)
		     (implies (and (in (zf-opair x y) F)
				   (in (zf-opair x z) F))
			      (= y z))))))))))
	   (help "The property of being a function in ZF set theory."))


(th~defdef zf-apply
	   (in zermelo-fraenkel)
           (author "http://www.ags.uni-sb.de/~kohlhase")
	   (date 18091998)
	   (definition
	     (lam (F zf)
	      (lam (arg zf)
	       (that (lam (z zf)
		(in (zf-opair arg z) F))))))
	   (help "Function application in ZF set theory."))
	
	   
;;; Need to code Fraenkel's Substitution Axioms
;;; Need to code Fraenkel's axiom of choice

(th~defaxiom zf-foundation
	     (in zermelo-fraenkel)
	     (author "http://www.ags.uni-sb.de/~kohlhase")
	     (credit "http://mbase.ags.uni-sb.de/~authors/fraenkel.html")
	     (formula
	      (forall (lam (S zf)
	       (implies (not (= S zf-emptyset))
			(exists (lam (z zf)
			 (and (in Z S)
			      (= zf-emptyset
				 (zf-intersection Z S)))))))))
	     (help "The foundation axiom of ZF set theory."))


	     
