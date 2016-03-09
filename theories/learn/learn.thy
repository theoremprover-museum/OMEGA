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

(th~deftheory learn
              (uses post) ; (uses relatiion)
	      (help "Group theory"))


(th~defdef not-empty
	   (in learn)
	   (definition
	     (lam (G (o i))
		  (exists (lam (a i) (G a))))))
				

(th~defdef closed-under
	   (in learn)
	   (definition
		  (lam (G (o i))
		       (lam (op (i i i))
			    (forall-sort (lam (a i)
				    (forall-sort (lam (b i)
							(G (op a b))) G)) G)))))

(th~defdef associative 
	   (in learn)
	   (definition
		  (lam (G (o i))
		       (lam (op (i i i))
			    (forall-sort (lam (a i)
				    (forall-sort (lam (b i)
					    (forall-sort (lam (c i)
						      (= (op a (op b c)) (op (op a b) c))) G)) G)) G)))))


(th~defdef unit
	   (in learn)
	   (definition
		  (lam (G (o i))
		       (lam (op (i i i))
			    (lam (e i)
				 (forall-sort (lam (a i)
						     (and (= (op a e) a)
							  (= (op e a) a))) G))))))

(th~defdef inverse-exist
	   (in learn)
	   (definition
		  (lam (G (o i))
		       (lam (op (i i i))
			    (lam (e i)
				 (forall-sort (lam (a i)
						     (exists-sort (lam (x i)
								    (and (= (op a x) e)
									 (= (op x a) e))) G)) G))))))

(th~defdef closed-inverse
	   (in learn)
	   (help "G5")
	   (definition
		  (lam (G (o i))
		       (lam (inv (i i))
			    (forall-sort (lam (a i)
					      (G (inv a))) G)))))

(th~defdef inverse 
	   (in learn)
	   (help "G5")
	   (definition
		  (lam (G (o i))
		       (lam (op (i i i))
			    (lam (e i)
				 (lam (inv (i i))
				      (forall-sort (lam (a i)
							(and (= (op a (inv a)) e)
							     (= (op (inv a) a) e))) G)))))))

(th~defdef left-inverse 
	   (in learn)
	   (help "G5")
	   (definition
		  (lam (G (o i))
		       (lam (op (i i i))
			    (lam (e i)
				 (lam (inv (i i))
				      (forall-sort (lam (a i)
							(= (op (inv a) a) e)) G)))))))

(th~defdef right-inverse 
	   (in learn)
	   (help "G5")
	   (definition
		  (lam (G (o i))
		       (lam (op (i i i))
			    (lam (e i)
				 (lam (inv (i i))
				      (forall-sort (lam (a i)
							(= (op a (inv a)) e)) G)))))))

(th~defdef left-unit
	   (in learn)
	   (definition
		  (lam (G (o i))
		       (lam (op (i i i))
			    (lam (e i)
				 (forall-sort (lam (a i)
						     (= (op e a) a)) G))))))

(th~defdef left-inverse-exist
	   (in learn)
	   (definition
		  (lam (G (o i))
		       (lam (op (i i i))
			    (lam (e i)
				 (forall-sort (lam (a i)
						     (exists-sort (lam (x i)
								    (= (op x a) e)) G)) G))))))

(th~defdef right-unit
	   (in learn)
	   (definition
		  (lam (G (o i))
		       (lam (op (i i i))
			    (lam (e i)
				 (forall-sort (lam (a i)
						     (= (op a e) a)) G))))))

(th~defdef right-inverse-exist
	   (in learn)
	   (definition
		  (lam (G (o i))
		       (lam (op (i i i))
			    (lam (e i)
				 (forall-sort (lam (a i)
						     (exists-sort (lam (x i)
								       (= (op a x) e)) G)) G))))))


(th~defdef divisors-exist
	   (in learn)
	   (definition
		  (lam (G (o i))
		       (lam (op (i i i))
			    (forall-sort (lam (a i)
				    (forall-sort (lam (b i)
							(and (exists-sort (lam (x i)
										 (= (op a x) b)) G)
							     (exists-sort (lam (y i)
										 (= (op y a) b)) G))) G)) G)))))

;;;; Important Note: We assume that groups are defined such that the operator op is always
;;;; defined after set G, i.e. as the second argument given to the "group"-predicate


(th~defdef group01
	   (in learn)
	   (definition
		  (lam (G (o i))
		       (lam (op (i i i))
			    (and (not-empty G)
				 (and (closed-under G op)
				      (and (associative G op)
					   (exists (lam (e i)
						   (and (G e)
							(and (unit G op e)
							     (inverse-exist G op e))))))))))))
						   
(th~defdef group02
	   (in learn)
	   (definition
		  (lam (G (o i))
		       (lam (op (i i i))
			    (and (not-empty G)
				 (and (closed-under G op)
				      (and (associative G op)
					   (exists (lam (e i)
						   (and (G e)
							(and (left-unit G op e)
							     (left-inverse-exist G op e))))))))))))

(th~defdef group03
	   (in learn)
	   (definition
		  (lam (G (o i))
		       (lam (op (i i i))
			    (and (not-empty G)
				 (and (closed-under G op)
				      (and (associative G op)
					   (exists (lam (e i)
							(and (G e)
							(and (right-unit G op e)
							     (right-inverse-exist G op e))))))))))))

(th~defdef group04
	   (in learn)
	   (definition
		  (lam (G (o i))
		       (lam (op (i i i))
			    (and (not-empty G)
				 (and (closed-under G op)
				      (and (associative G op)
					   (divisors-exist G op))))))))

(th~defdef group05
	   (in learn)
	   (definition
	     (lam (G (o i))
		  (lam (op (i i i))
		       (lam (e i)
			    (lam (inv (i i))
				 (and (not-empty G)
				      (and (closed-under G op)
					   (and (closed-inverse G inv)
						(and (associative G op)
						     (and (G e)
							  (and (unit G op e)
							       (inverse G op e inv)))))))))))))

(th~defdef group
 (in learn)
 (definition
   (lam (G (o i))
	(lam (op (i i i))
	     (lam (e i)
		  (lam (inv (i i))
		       (and (not-empty G)
			    (and (closed-under G op)
				 (and (closed-inverse G inv)
				      (and (associative G op)
					   (and (G e)
						(and (left-unit G op e)
						     (and (right-unit G op e)
							  (and (left-inverse G op e inv)
							       (right-inverse G op e inv)))))))))))))))
      








