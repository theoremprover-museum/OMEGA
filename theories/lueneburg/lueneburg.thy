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

#-keim31-new(in-package :omega)

(th~deftheory lueneburg
	      (uses post)
	      (type-constants ff)
	      (constants
	       (* (ff ff ff))
	       (e ff)
	       (+ (ff ff ff))
	       (- (ff ff))
	       (+inv (ff ff))
	       (*inv (ff ff))
	       (zero ff)
	       (null ff)
	       (eins ff)
	       (ring (o ff))
	       (inv (ff ff))
	       (< (o ff ff))
	       (n ff)	  
	       (fx ff)  
	       (gx ff)  
	       (l1 ff)  
	       (l2 ff))
	      (help "The home theory for the lueneburg examples."))


(th~defaxiom associativity-plus
	     (in lueneburg)
	     (formula
		      (forall (lam (r ff)
				   (forall (lam (s ff)
						(forall (lam (t ff)
							     (= (+ (+ r s) t) (+ r
										 (+ s t)))))))))))
(th~defaxiom neutral-element-and-inverse-plus
	     (in lueneburg)
	     (formula
                      (and (forall (lam (r ff) (= (+ r null) r)))
                           (forall (lam (r ff) (= (+ r (- r)) null))))))
(th~defaxiom commutativity-plus
	     (in lueneburg)
	     (formula
		      (forall (lam (r ff)
				   (forall (lam (s ff)
						(= (+ r s) (+ s r))))))))
(th~defaxiom Ass
	     (in lueneburg)
	     (formula
		      (forall (lam (x ff)
				   (forall (lam (y ff)
						(forall (lam (z ff)
							     (= (* (* x y) z) (* x
										 (* y z)))))))))))
(th~defaxiom R-Id
	     (in lueneburg)
	     (formula
		      (forall (lam (x ff)
				   (= (* x e) x)))))
(th~defaxiom L-Id
	     (in lueneburg)
	     (formula
		      (forall (lam (x ff)
				   (= (* e x) x)))))
(th~defaxiom Idem
	     (in lueneburg)
	     (formula
		      (forall (lam (x ff)
				   (= (* x x) e)))))
(th~defaxiom associativity-times
	     (in lueneburg)
	     (formula
		      (forall (lam (r ff)
				   (forall (lam (s ff)
						(forall (lam (t ff)
							     (= (* (* r s) t) (* r
										 (* s t)))))))))))
(th~defaxiom neutral-element-times
	     (in lueneburg)
	     (formula
		      (and (not (= eins null))
			   (forall (lam (r ff)
					(and (= (* r eins) r)
					     (= (* eins r) r)))))))
(th~defaxiom distributivity
	     (in lueneburg)
	     (formula
		      (forall (lam (r ff)
				   (forall (lam (s ff)
						(forall (lam (t ff)
							     (and (= (* r (+ s t)) (+ (* r s) (* r t)))
								  (= (* (+ r s) t)
								     (+ (* r t) (*
										 s
										 t))))))))))))
(th~defaxiom closedness
	     (in lueneburg)
	     (formula
		      (forall (lam (r ff)
				   (forall (lam (s ff)
						(implies (and (ring r)
							      (ring s))
							 (and (ring (+ r s))
							      (ring (* r
								       s))))))))))
(th~defaxiom +-com
	     (in lueneburg)
	     (formula
		     (forall (lam (x ff)
				  (forall (lam (y ff)
					       (= (+ x y) (+ y x))))))))
(th~defaxiom +-assoz
	     (in lueneburg)
	     (formula
		     (forall (lam (x ff)
				  (forall (lam (y ff)
					       (forall (lam (z ff)
							    (= (+ (+ x y) z) (+ x
										(+
										 y
										 z)))))))))))
(th~defaxiom zero-element
	     (in lueneburg)
	     (formula
		     (forall (lam (x ff)
				  (and (= (+ x zero) x)
				       (= (+ zero X) x))))))
(th~defaxiom +inv-element
	     (in lueneburg)
	     (formula
		     (forall (lam (x ff)
				  (and (= (+ x (+inv x)) zero)
				       (= (+ (+inv x) x) zero))))))
(th~defaxiom *-com
	     (in lueneburg)
	     (formula
		     (forall (lam (x ff)
				  (forall (lam (y ff)
					       (= (* x y) (* y x))))))))
(th~defaxiom *-assoz
	     (in lueneburg)
	     (formula
		     (forall (lam (x ff)
				  (forall (lam (y ff)
					       (forall (lam (z ff)
							    (= (* (* x y) z) (* x
										(*
										 y
										 z)))))))))))
(th~defaxiom e-element
	     (in lueneburg)
	     (formula
		     (forall (lam (x ff)
				  (and (and (= (* x e) x)
					    (= (* e X) x))
				       (not (= e zero)))))))
(th~defaxiom *inv-element
	     (in lueneburg)
	     (formula
		     (forall (lam (x ff)
				  (implies (not (= x zero))
					   (and (= (* x (*inv x)) e)
						(= (* (*inv x) x) e)))))))
(th~defaxiom distrib-1
	     (in lueneburg)
	     (formula
		     (forall (lam (x ff)
				  (forall (lam (y ff)
					       (forall (lam (z ff)
							    (= (* x (+ y z))
							       (+ (* x y) (* x
									     z)))))))))))
(th~defaxiom distrib-2
	     (in lueneburg)
	     (formula
		     (forall (lam (x ff)
				  (forall (lam (y ff)
					       (forall (lam (z ff)
							    (= (* (+ y z) x)
							       (+ (* y x) (* z
									     x)))))))))))

 
