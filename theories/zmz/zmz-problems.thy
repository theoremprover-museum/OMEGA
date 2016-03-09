;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: LISP -*-
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

(th~defproblem notiso-z2z+-z2z*
	       (in zmz)
	       (conclusion notiso-z2z+-z2z*
		(not (isomorphic (resclass-set 2) plus-resclass (resclass-set 2) times-resclass))))


(th~defproblem notiso-z5z--z5z2x+y
	       (in zmz)
	       (conclusion notiso-z5z--z5z2x+y
		(not (isomorphic (resclass-set 5) minus-resclass (resclass-set 5) (lam (x (o num)) (y (o num))
										       (plus-resclass (times-resclass (resclass 5 2) x)
												      y))))))

(th~defproblem notiso-z5z--z5z3x+y
	       (in zmz)
	       (conclusion notiso-z5z--z5z3x+y
		(not (isomorphic (resclass-set 5) minus-resclass (resclass-set 5) (lam (x (o num)) (y (o num))
										       (plus-resclass (times-resclass (resclass 5 3) x)
												      y))))))

(th~defproblem notiso-z5z2x+y-z5z3x+y
	       (in zmz)
	       (conclusion notiso-z5z2x+y-z5z3x+y
		(not (isomorphic (resclass-set 5)
				 (lam (x (o num)) (y (o num))
				      (plus-resclass (times-resclass (resclass 5 2) x)
						     y))
				 (resclass-set 5)
				 (lam (x (o num)) (y (o num))
				      (plus-resclass (times-resclass (resclass 5 3) x)
						     y))))))

(th~defproblem iso-z2z+-z2z+1
	       (in zmz)
	       (conclusion iso-z2z+-z2z+1
			   (isomorphic (resclass-set 2) plus-resclass
				       (resclass-set 2) (lam (x (o num)) (y (o num))
							     (plus-resclass (plus-resclass x y)
									    (resclass 2 1))))))

(th~defproblem iso-z2zXz2z+X+-z2zXz2z+1X+1
	       (in zmz)
	       (conclusion iso-z2zXz2z+X+-z2zXz2z+1X+1
			   (isomorphic (cartesian-product (resclass-set 2)
							  (resclass-set 2))
				       (pair-operation plus-resclass
						       plus-resclass)
				       (cartesian-product (resclass-set 2)
							  (resclass-set 2))
				       (pair-operation (lam (x (o num)) (y (o num))
							    (plus-resclass (plus-resclass x y)
									   (resclass 2 1)))
						       (lam (x (o num)) (y (o num))
							    (plus-resclass (plus-resclass x y)
									   (resclass 2 1)))))))


(th~defproblem modiso-z2zXz2z+X+-z2zXz2z+1X+1-z2z++
	       (in zmz)
	       (conclusion modiso-z2zXz2z+X+-z2zXz2z+1X+1-z2z++
			   (module-isomorphic (cartesian-product (resclass-set 2)
								 (resclass-set 2))
					      (pair-operation plus-resclass
							      plus-resclass)
					      (cartesian-product (resclass-set 2)
								 (resclass-set 2))
					      (pair-operation (lam (x (o num)) (y (o num))
								   (plus-resclass (plus-resclass x y)
										  (resclass 2 1)))
							      (lam (x (o num)) (y (o num))
								   (plus-resclass (plus-resclass x y)
										  (resclass 2 1))))
					      (resclass-set 2)
					      (scalar-operation plusgen-resclass
								plusgen-resclass)
					      (scalar-operation plusgen-resclass
								plusgen-resclass))))
						       




(th~defproblem module-z2zoz3z+o+-z6z+*-*go*g
	       (in zmz)
	       (assumption ass1 (abelian-group (cartesian-product (resclass-set 2)
								  (resclass-set 3))
					       (lam (x (tuple (o num) (o num)))
						    (lam (y (tuple (o num) (o num)))
							 (pair (plus-resclass (first-of-pair x)
									      (first-of-pair y))
							       (plus-resclass (second-of-pair x)
									      (second-of-pair y)))))))
	       (assumption ass2
			   (abelian-ring-with-one (resclass-set 6) plus-resclass times-resclass))
	       (conclusion module-z6z+*-z2zoz3z+o+-+go+g
			   (module (cartesian-product (resclass-set 2)
						      (resclass-set 3))
				   (pair-operation plus-resclass
						   plus-resclass)
				   (resclass-set 6)
				   plus-resclass
				   times-resclass
				   (scalar-operation timesgen-resclass
						     timesgen-resclass))))



(th~defproblem submodule-z2zo0-z2zoz3z-+o+-z6z+*-*go*g
	       (in zmz)
	       (assumption ass1 (module (cartesian-product (resclass-set 2)
							   (resclass-set 3))
					(pair-operation plus-resclass
							plus-resclass)
					(resclass-set 6)
					plus-resclass
					times-resclass
					(scalar-operation timesgen-resclass
							  timesgen-resclass)))
	       (assumption ass2 (unit (cartesian-product (resclass-set 2)
							 (resclass-set 3))
				      (pair-operation plus-resclass
						      plus-resclass)
				      (pair (resclass 2 0) (resclass 3 0))))
	       (assumption ass3 (inverse-function (cartesian-product (resclass-set 2)
								     (resclass-set 3))
						  (pair-operation plus-resclass
								  plus-resclass)
						  (pair (resclass 2 0) (resclass 3 0))
						  (lam (x (tuple (o num) (o num)))
						       (pair (minus-resclass (resclass 2 0)
									     (first-of-pair x))
							     (minus-resclass (resclass 3 0)
									     (second-of-pair x))))))
	       (conclusion submodule-1
			   (submodule (cartesian-product (resclass-set 2)
							 (lam (x (o num))
							      (= x (resclass 3 0))))
				      (cartesian-product (resclass-set 2)
							 (resclass-set 3))
				      (pair-operation plus-resclass
						      plus-resclass)
				      (resclass-set 6)
				      plus-resclass
				      times-resclass
				      (scalar-operation timesgen-resclass
							timesgen-resclass))))


(th~defproblem submodule-z2zo0-z2zoz3z-+o+-z6z+*-*go*g-FREE
	       (in zmz)
	       (assumption ass1 (module (cartesian-product (resclass-set 2)
							   (resclass-set 3))
					(pair-operation plus-resclass
							plus-resclass)
					(resclass-set 6)
					plus-resclass
					times-resclass
					(scalar-operation timesgen-resclass
							  timesgen-resclass)))
	       (conclusion submodule-1
			   (submodule (cartesian-product (resclass-set 2)
							 (lam (x (o num))
							      (= x (resclass 3 0))))
				      (cartesian-product (resclass-set 2)
							 (resclass-set 3))
				      (pair-operation plus-resclass
						      plus-resclass)
				      (resclass-set 6)
				      plus-resclass
				      times-resclass
				      (scalar-operation timesgen-resclass
							timesgen-resclass))))


;; SPECIAL STUFF FROM VOLKER NEEDED TO DO THIS TUPLE STUFF!
;; (th~defproblem closed-z2z0011-plusxplus-a
;; 	       (in zmz)
;; 	       (conclusion
;; 		(closed-under (set (tuple (resclass 2 0) (resclass 2 0))
;; 				   (tuple (resclass 2 1) (resclass 2 1)))
;; 			      (pair-operation plus-resclass plus-resclass))))


(th~defproblem closed-z2z0011-plusxplus-b
	       (in zmz)
	       (conclusion
		(closed-under (lam (x (tuple (o num) (o num)))
				   (or (= x (pair (resclass 2 0) (resclass 2 0)))
				       (= x (pair (resclass 2 1) (resclass 2 1)))))
			      (pair-operation plus-resclass plus-resclass))))


;; SPECIAL STUFF FROM VOLKER NEEDED TO DO THIS TUPLE STUFF!
;;(th~defproblem closed-z2z0011-plus
;;	       (in zmz)
;;	       (conclusion
;;		(closed-under (set (tuple (resclass 2 0) (resclass 2 0))
;;				   (tuple (resclass 2 1) (resclass 2 1)))
;;			      (pair-operation plus-resclass plus-resclass))))

;; SPECIAL STUFF FROM VOLKER NEEDED TO DO THIS TUPLE STUFF!
;;(th~defproblem closed-z2z001+111-plusxplusxtimes-b
;;	       (in zmz)
;;	       (conclusion
;;		(closed-under (set (tuple (resclass 2 0) (resclass 2 0) (resclass 2 1))
;;				   (tuple (resclass 2 1) (resclass 2 1) (resclass 2 1)))
;;			      (pair-operation plus-resclass (pair-operation plus-resclass times-resclass)))))

(th~defproblem closed-z2z001+111-plusxplusxtimes-a
	       (in zmz)
	       (conclusion
		(closed-under (lam (x (tuple (o num) (tuple (o num) (o num))))
				   (or (= x (pair (resclass 2 0) (pair (resclass 2 0) (resclass 2 1))))
				       (= x (pair (resclass 2 1) (pair (resclass 2 1) (resclass 2 1))))))
			      (pair-operation plus-resclass (pair-operation plus-resclass times-resclass)))))

(th~defproblem closed-z2zxz2z-plusxtimes
	 (in zmz)
	 (conclusion
	  (closed-under (cartesian-product (resclass-set 2)
					   (resclass-set 2))
			(pair-operation plus-resclass
					times-resclass))))

(th~defproblem closed-z4zo13-plus
	 (in zmz)
	 (conclusion
	  (closed-under (lam (x (o num)) (or (= x (resclass 4 0))
					     (= x (resclass 4 2))))
			plus-resclass)))

(th~defproblem closed-z4zo13-plus-b
	 (in zmz)
	 (conclusion
	  (closed-under (set (resclass 4 0) (resclass 4 2))
			plus-resclass)))

(th~defproblem iso-z2z+-z4zo13+
	       (in zmz)
	       (conclusion iso-z2z+-z2z+1
			   (isomorphic (resclass-set 2)
				       plus-resclass
				       (set (resclass 4 0) (resclass 4 2))
				       plus-resclass)))

(th~defproblem iso-z2zxz4zo13+x+-z4zo13xz2z+x+
	       (in zmz)
	       (conclusion iso-z2z+-z2z+1
			   (isomorphic (cartesian-product (resclass-set 2) 
							  (set (resclass 4 0) (resclass 4 2)))
				       (pair-operation plus-resclass plus-resclass)
				       (cartesian-product (lam (x (o num)) (or (= x (resclass 4 2))
									       (= x (resclass 4 0))))
							  (set (resclass 2 0) (resclass 2 1)))
				       (pair-operation plus-resclass plus-resclass))))

(th~defproblem iso-z2zxz4zo13+x+-z4zo13xz2z+x+-b
	       (in zmz)
	       (conclusion iso-z2z+-z2z+1
			   (isomorphic (cartesian-product (resclass-set 2) 
							  (lam (x (o num)) (or (= x (resclass 4 2))
									       (= x (resclass 4 0)))))
				       (pair-operation plus-resclass plus-resclass)
				       (cartesian-product (lam (x (o num)) (or (= x (resclass 4 2))
									       (= x (resclass 4 0))))
							  (resclass-set 2))
				       (pair-operation plus-resclass plus-resclass))))

(th~defproblem iso-z2zxz4zo13xz3zo02+x+x*-z4zo13xz2zxz3zo12+x+x+
	       (in zmz)
	       (conclusion iso-z2z+-z2z+1
			   (isomorphic (cartesian-product (resclass-set 2) 
							  (cartesian-product
							   (set (resclass 4 0) (resclass 4 2))
							   (set (resclass 3 1))))
				       (pair-operation plus-resclass (pair-operation plus-resclass times-resclass))
				       (cartesian-product (lam (x (o num)) (or (= x (resclass 4 2))
									       (= x (resclass 4 0))))
							  (cartesian-product (set (resclass 2 0) (resclass 2 1))
									     (set (resclass 3 0))))
				       (pair-operation plus-resclass (pair-operation plus-resclass times-resclass)))))

(th~defproblem iso-z2zxz4zo13xz3zo02+x+x*-z4zo13xz2zxz3zo12+x+x+
	       (in zmz)
	       (conclusion iso-z2z+-z2z+1
			   (isomorphic (cartesian-product (resclass-set 2) 
							  (cartesian-product
							   (set (resclass 3 1))
							   (set (resclass 4 0) (resclass 4 2))))
				       (pair-operation plus-resclass (pair-operation plus-resclass times-resclass))
				       (cartesian-product (lam (x (o num)) (or (= x (resclass 4 2))
									       (= x (resclass 4 0))))
							  (cartesian-product (set (resclass 2 0) (resclass 2 1))
									     (set (resclass 3 0))))
				       (pair-operation plus-resclass (pair-operation plus-resclass times-resclass)))))
