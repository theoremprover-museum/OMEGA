;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: THEORY -*-
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

(th~deftheory FUNCTION
              (uses typed-set)
	      (help "Properties of functions on sets."))

(th~defdef injective
	   (in function)
           (type-variables bb cc)
	   (definition
             (lam (G (o bb))
                  (lam (op (cc bb))
                       (forall-sort (lam (x bb)
                        (forall-sort (lam (y bb)
			 (implies
			  (= (op x) (op y))
			  (= x y)))
				     G))
				    G))))
	   (help "Definition of the predicate for injectivity.
                  (injective G f) is true, iff fx=fy implies x=y on G."))

(th~defdef total
	   (in function)
           (type-variables bb cc)
           (definition
             (lam (D (o bb))
		  (lam (op (cc bb))
		       (forall-sort (lam (x bb)
					 (exists (lam (y cc)
							   (= y (op x)))))
				      D))))
	   (help "Definition of the predicate for totality.
                  (total G f) is true, iff for all x in G f(x) exists."))

(th~defdef total-2
	   (in function)
           (type-variables bb cc dd)
           (definition
             (lam (G1 (o bb))
		  (lam (G2 (o cc))
		       (lam (op (dd cc bb))
			    (forall (lam (x bb)
					 (forall (lam (y cc)
						      (implies (and (in x G1)
								    (in y G2))
							       (exists (lam (z dd)
									    (= z (op x y)))))))))))))
	   (help "Definition of the predicate for totality.
                  (total G1 G2 f) is true, iff for all x in G1, y in G2 f(x,y) exists."))

(th~defdef surjective
	   (in function)
           (type-variables bb cc)
           (definition 
             (lam (F (o bb))
                  (lam (G (o cc))
                       (lam (op (cc bb))
                            (forall-sort (lam (x cc)
			     (exists-sort (lam (y bb)
			      (= x (op y)))
					  F))
					 G)))))
	   (help "Definition of the predicate for surjectivity.
                  (surjective G H f) is true, iff for all x in
                  H there is a pre-image y of x under f in G."))

; soemthing goes wong -kk-

(th~defdef bijective
	   (in function)
           (type-variables bb cc)
	   (definition 
             (lam (F (o bb))
                  (lam (G (o cc))
                       (lam (op (cc bb))
                            (and
                             (surjective F G op)
                             (injective F op))))))
	   (help "Definition of the predicate for bijectivity.
                  (bijective G F f) is true, iff f is one-to one from G to F."))


(th~defdef closed-under-1
	   (in function)
           (type-variables bb)
           (definition 
             (lam (G (o bb))
                  (lam (op (bb bb))
                       (forall-sort (lam (x bb)
					 (in (op x) G))
				    G))))
	   (help "Definition of the predicate for closure under a function.
                  (closed-under G f) is true, iff the set G is closed under
                  the operation f."))

(th~defdef closed-under-2
	   (in function)
           (type-variables bb)
           (definition 
	     (lam (G (o bb))
		  (lam (dot (bb bb bb))
		       (forall-sort
			(lam (x bb)
			     (forall-sort
			      (lam (y bb)
				   (in (dot x y) G))
			      G))
			G))))
	   (help "Definition of the predicate for closure under an operation. (closed-under G *) is true, iff the set G is closed under the operation *."))

(th~defdef function-domain-range
	   (in function)
           (type-variables aa bb)
           (definition 
	    (lam (F (aa bb))
	     (lam (G (o bb))
	      (lam (H (o aa))
	       (forall-sort (lam (x bb)
		(in (F x) H))
		G)))))
	   (help "The predicate for domain and range of a function.
                  It is true, iff its first arguemtn is a function
                  that when applied to members of its second arg yields
                  members of the third"))

(th~defdef function-domain2-range
	   (in function)
           (type-variables aa bb cc)
           (definition 
	    (lam (F (aa bb cc))
	     (lam (G (o cc))
	      (lam (H (o bb))
	       (lam (K (o aa))
	        (forall-sort (lam (x cc)
		 (forall-sort (lam (y bb)
		  (in (F x y) K))
			      H))
			     G))))))
	   (help "The predicate for domain and range of a function.
                  It is true, iff its first arguemtn is a function
                  that when applied to members of its second arg yields
                  members of the third"))

(th~defdef idempotent
	   (in function)
           (type-variables bb)
           (definition 
	     (lam (G (o bb))
		  (lam (op (bb bb))
		       (forall-sort
			(lam (x bb)
			     (= (op (op x)) (op x)))
			G))))
	   (help "Definition of the predicate for idempotence. (idempotent G f) is true, iff on G f(f(x))= f(x)."))


(th~defdef nilpotent
	   (in function)
           (type-variables bb)
           (definition 
		       (lam (G (o bb))
			    (lam (op (bb bb))
				 (forall-sort
				  (lam (x bb)
				       (= (op (op x)) x))
				  G))))
	   (help "Definition of the predicate for nilpotence. (nilpotent G f) is true, iff on G f(f(x))=x."))

(th~defdef compose-functions
	   (in function)
           (type-variables bb cc dd)
           (definition
             (lam (F (cc dd))
                  (lam (G (bb cc))
                       (lam (x dd) (G (F x))))))
	   (help "Function composition."))


(th~defdef domain
	   (in function)
           (type-variables bb cc)
           (definition 
             (lam (fun (cc bb))
                  (lam (x bb)
                       (defined (fun x)))))
	   (help "Definition of the domain of a function."))

;; we only need apply-point-wise for more than one argument, since the unary one 
;; can be obtained by function composition.
(th~defdef apply-pointwise-2
	   (in function)
	   (type-variables bb cc dd ee ff)
           (definition 
             (lam (op (bb dd ee))
                  (lam (F (ee cc))
                       (lam (G (dd ff))
                            (lam (X cc)
                                 (lam (Y ff)
                                      (op (F X) (G Y))))))))
	   (help "A new function is obtained by applying a binary operator pointwise to two functions."))

(th~defdef image
	   (in function)
	   (type-variables bb cc)
           (definition 
             (lam (F (bb cc))
                  (lam (x bb)
                       (exists (lam (y cc)
                                    (= x (f y)))))))
	   (help "The image of a function."))

(th~defdef image-of-domain
	   (in function)
	   (type-variables bb cc)
           (definition 
             (lam (F (bb cc))
		  (lam (dom (o cc))
		       (lam (x bb)
			    (exists-sort (lam (y cc)
					      (= x (f y)))
					 dom)))))
	   (help "The image of a function."))

(th~defdef functions
	   (in function)
	   (type-variables bb cc)
           (definition 
             (lam (Dom (o cc))
		  (lam (Im (o bb))
		       (lam (F (bb cc))
                            (and (total Dom F)
                                 (subset (image-of-domain F dom) Im))))))
	   (help "The set of total functions G with image in H."))



(th~defdef smaller-cardinality
	   (in function)
	   (type-variables aa bb)
	   (definition
	     (lam (G (o aa))
		  (lam (H (o bb))
		       (not (exists (lam (F (bb aa))
					 (surjective G H F)))))))
	   (help "(smaller-cardinality G H) is true, iff there is no surjective function F:G->H"))
	     

(th~defdef iteratep+
	   (in function)
	   (type-variables aa)
	   (definition
	     (lam (f (aa aa))
		  (lam (g (aa aa))
		       (forall (lam (p (o (aa aa)))
				    (implies
				     (and (p f)
					  (forall (lam (j (aa aa))
						       (implies
							(p j)
							(p (compose-functions j f))))))
				     (p g)))))))
	   (help "g is a composition of one or more copies of f. (Compare 
compose-function with the TPS definition -- it is the other way round.)"))




(th~defdef iteratep
           (in function)
           (type-variables aa)
           (definition
             (lam (f (aa aa))
                  (lam (g (aa aa))
                       (forall (lam (p (o (aa aa)))
                                    (implies
                                     (and (p (lam (u aa) u))
                                          (forall (lam (j (aa aa))
                                                       (implies
                                                        (p j)
                                                        (p (compose-functions j f))))))
                                     (p g)))))))
           (help "g is a composition of zero or more copies of f. (Compare 
compose-function with the TPS definition -- it is the other way round.)"))



(th~defdef injectivep
	   (in function)
           (type-variables bb cc)
	   (definition
	     (lam (op (cc bb))
		  (forall (lam (x bb)
			       (forall (lam (y bb)
					    (implies (= (op x)
							(op y))
						     (= x y)))
					     ))
			        )))
	   (help "Definition of an alternative predicate for injectivity.
                  (injective f) is true, iff fx=fy implies x=y."))

(th~defdef symmetric-function
	   (in function)
           (type-variables aa bb)
           (definition
             (lam (fun (aa bb bb))
                  (forall (lam (x bb)
                               (forall (lam (y bb)
						 (= (fun x y) (fun y x)))
					     ))
			        )))
	   (help "Definition of the predicate for symmetric function
                  (symmetric-function F) is true, iff Fxy =Fyx."))


(th~defdef identity-function-on-set
	   (in function)
	   (type-variables aa)
	   (definition
	     (lam (theset (o aa))
		  (that (lam (f (aa aa))
			     (and (= (domain f) theset)
				  (and (= (image f) theset)
				       (forall (lam (x aa)
						    (implies (theset x)
							     (= (f x) x))))))))))
	   (help "Definition of the identity-function on a set."))


(th~defaxiom identity-function
	     (in function)
	     (type-variables aa)
	     (formula (forall (lam (theset (o aa))
				   (forall (lam (x aa)
						(implies (theset x)
							 (= (identity-function-on-set theset x)
							    x)))))))
	     (help "The property of the identity-function on a set."))

(th~defdef restriction  
	   (in function)
	   (type-variables aa bb)
           (definition
	     (lam (op (aa aa aa))
		  (lam (G (o aa))
			 (that (lam (op1 (aa aa aa))
			       (forall-sort (lam (elem1 aa)
						 (forall-sort (lam (elem2 aa)
								   (= (op elem1 elem2) (op1 elem1 elem2)))
							      G))
						 G))))))
	   (help "Restricted operation of one operation to a subset"))

(th~defdef invariant  
	   (in function)
	   (type-variables aa bb)
           (definition
		  (lam (G (o aa))
		       (lam (ops (o (aa aa)))
			    (forall-sort (lam (op (aa aa))
					      (closed-under-1 G op))
					 ops)))))

(th~defdef invert-function
	   (in function)
	   (type-variables aa bb)
           (definition
		  (lam (f (bb aa))
		       (lam (y bb)
			    (that (lam (x aa) (= y (f x)))))))) 



(th~defdef increasing-wrt-ord
	   (in function)
	   (type-variables aa bb)
           (definition
	     (lam (f (bb aa))
		  (lam (ord1 (o aa aa))
		       (lam (ord2 (o bb bb))
			    (forall (lam (x aa)
					 (forall (lam (y aa)
						      (implies (ord1 x y) (ord2 (f x) (f y))))))))))))

(th~defdef decreasing-wrt-ord
	   (in function)
	   (type-variables aa bb)
           (definition
	     (lam (f (bb aa))
		  (lam (ord1 (o aa aa))
		       (lam (ord2 (o bb bb))
			    (forall (lam (x aa)
					 (forall (lam (y aa)
						      (implies (ord1 x y) (ord2 (f y) (f x))))))))))))


(th~defdef function-domain-to-range
	   (in function)
	   (type-variables aa bb)
           (definition
	     (lam (f (bb aa))
		  (lam (X (o aa))
		       (lam (Y bb)
			    (exists (lam (Z aa)
					 (and (X Z) (= Y (f Z))))))))))



(th~defdef surjectivep-mateja-chris
	   (in function)
	   (type-variables aa bb)
           (definition
	       (lam (op (bb aa))	
		    (forall (lam (Y bb)
				 (exists (lam (X aa)
					      (= Y (op X)))))))))


(th~defdef bijectivep-mateja-chris
	   (in function)
	   (type-variables aa bb)
           (definition
	     (lam (op (bb aa))
		  (and (injectivep op)
		       (surjectivep-mateja-chris op)))))
