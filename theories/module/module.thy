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

(th~deftheory module
              (uses ring)
	      (help "Module theory"))


(th~defdef module-distributivity1
	   (in module)
	   (type-variables aa bb)
	   (definition
	   (lam (M (o bb))
	     (lam (madd (bb bb bb))
	       (lam (R (o aa))
		(lam (rmmul (bb bb aa))
		     (forall-sort (lam (r1 aa)
		       (forall-sort (lam (m1 bb)
			 (forall-sort (lam (m2 bb)
					   (= (rmmul r1 (madd m1 m2)) (madd (rmmul r1 m1) (rmmul r1 m2))))
				      M))
				    M))
				  R))))))
	   (help "Module-Distributivity1: r1*(m1+m2)=r1*m1+r1*m2 (r1 in ring, m1 and m2 in module)."))

(th~defdef module-distributivity2
	   (in module)
	   (type-variables aa bb)
	   (definition
	   (lam (M (o bb))
	     (lam (madd (bb bb bb))
	       (lam (R (o aa))
		 (lam (radd (aa aa aa))
		  (lam (rmmul (bb bb aa))
		       (forall-sort (lam (r1 aa)
			(forall-sort (lam (r2 aa)
			 (forall-sort (lam (m1 bb)
					   (= (rmmul (radd r1 r2) m1) (madd (rmmul r1 m1) (rmmul r2 m1))))
				      M))
				     R))
				    R)))))))
	   (help "Module-Distributivity2: (r1+r2)*m1=r1*m1+r2*m1 (r1,r2 in ring, m1 in module)."))

(th~defdef module-associativity
	   (in module)
	   (type-variables aa bb)
	   (definition
	   (lam (M (o bb))
	    (lam (R (o aa))
	     (lam (rmul (aa aa aa))
	      (lam (rmmul (bb bb aa))
	       (forall-sort (lam (r1 aa)
		(forall-sort (lam (r2 aa)
		 (forall-sort (lam (m1 bb)
				   (= (rmmul (rmul r1 r2) m1) (rmmul r1 (rmmul r2 m1))))
			      M))
			     R))
			    R))))))
	   (help "Module-Associativity: (r1*r2)*m1=r1*(r2*m1) (r1,r2 in ring, m1 in module)."))


(th~defdef module-unitary
	   (in module)
	   (type-variables aa bb)
	   (definition
	   (lam (M (o bb))
	     (lam (rmmul (bb bb aa))
	      (lam (one aa)
		   (forall-sort (lam (m1 bb)
				     (= (rmmul one m1) m1))
				M)))))
	   (help "Module-Unitary: 1*m1=m1 (1= *-unit in ring, m1 in module)."))


(th~defdef module
	   (in module)
	   (type-variables aa bb)
           (definition
             (lam (M (o bb))
		  (lam (madd (bb bb bb))
		       (lam (R (o aa))
			    (lam (radd (aa aa aa))
				 (lam (rmul (aa aa aa))
				      (lam (rmmul (bb bb aa))
					   (and (and (abelian-group M madd)
						     (abelian-ring-with-one R radd rmul))
						(and (and (module-distributivity1 M madd R rmmul)
							  (module-distributivity2 M madd R radd rmmul))
						     (and (module-associativity M R rmul rmmul)
							  (module-unitary M rmmul (struct-unit R rmul))))))))))))
	   (help "Module predicate for structures."))

(th~defdef submodule
	   (in module)
	   (type-variables aa bb)
           (definition
             (lam (A (o bb))
	      (lam (M (o bb))
	       (lam (madd (bb bb bb))
		(lam (R (o aa))
		 (lam (radd (aa aa aa))
		  (lam (rmul (aa aa aa))
		   (lam (rmmul (bb bb aa))
			(and (subset A M)
			     (and (module M madd R radd rmul rmmul)
				  (module A madd R radd rmul rmmul)))))))))))
	   (help "A is submodule of M, if A subset M and both a modules wrt. the same operations and R."))


(th~defdef prime-submodule
	   (in module)
	   (type-variables aa bb)
           (definition
             (lam (P (o bb))
	      (lam (M (o bb))
	       (lam (madd (bb bb bb))
		(lam (R (o aa))
		 (lam (radd (aa aa aa))
		  (lam (rmul (aa aa aa))
		   (lam (rmmul (bb bb aa))
			(and (submodule P M madd R radd rmul rmmul)
			     (and (not (= P M))
				  (forall-sort (lam (r1 aa)
				    (forall-sort (lam (m1 bb)
						      (implies (in (rmmul r1 m1) P)
							       (or (in m1 P)
								   (forall-sort (lam (x bb)
										     (in (rmmul r1 x) P))
										M))))
						 M))
					       R)))))))))))
	   (help "When is P a prime-module of a module M."))


(th~defdef radical-of-submodule
	   (in module)
	   (type-variables aa bb)
           (definition
             (lam (A (o bb))
	      (lam (M (o bb))
	       (lam (madd (bb bb bb))
		(lam (R (o aa))
		 (lam (radd (aa aa aa))
		  (lam (rmul (aa aa aa))
		   (lam (rmmul (bb bb aa))
			(lam (x bb)
			     (and (in x M)
				  (forall (lam (P (o bb))
					       (implies (and (submodule A P madd R radd rmul rmmul)
							     (prime-submodule P M madd R radd rmul rmmul))
							(in x P))))))))))))))
	   (help "The radical of a submodule."))




(th~defdef module-group
	   (in module)
	   (type-variables aa bb)
	   (definition
	     (lam (M (bistruct aa bb))
		  (bistruct-struct1 M))))

(th~defdef module-ring
	   (in module)
	   (type-variables aa bb)
	   (definition
	     (lam (M (bistruct aa bb))
		  (bistruct-struct2 M))))

(th~defdef module-operation
	   (in module)
	   (type-variables aa bb)
	   (definition
	     (lam (M (bistruct aa bb))
		  (bistruct-ops2xs1=s1 M))))

(th~defdef module-isomorphic
	   (in module)
	   (type-variables aa bb1 bb2)
	   (definition
	     (lam (M1 (o bb1))
		  (lam (madd1 (bb1 bb1 bb1))
		       (lam (M2 (o bb2))
			    (lam (madd2 (bb2 bb2 bb2))
				 (lam (R (o aa))
				      (lam (rmmul1 (bb1 bb1 aa))
					   (lam (rmmul2 (bb2 bb2 aa))
						(exists-sort (lam (h (bb2 bb1))
						    (and (and (homomorphism M1 madd1 M2 madd2 h) 
							      (injective M1 h))
							 (and (surjective M1 M2 h)
							      (forall-sort (lam (rr aa)
								 (forall-sort (lam (mm bb1)
										   (= (h (rmmul1 rr mm))
										      (rmmul2 rr (h mm))))
									      M1))
									   R))))
							     (functions M1 M2))))))))))
	   (help "Isomorphism Between Moduln."))
