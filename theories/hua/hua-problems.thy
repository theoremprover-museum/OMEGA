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

(th~defproblem deussen-4.11.1 (in hua)
	 (type-constants ss uu vv)
	 (constants
	  (phi_1 (uu ss))
	  (phi_2 (vv ss))
	  (rho_1 (o ss ss))
	  (rho_2 (o ss ss))
	  (big_phi (vv uu))
	  )
	 (assumption def-rho1 (induzierte_eqrel_abb rho_1 phi_1))

	 (assumption def-rho2 (induzierte_eqrel_abb rho_2 phi_2))

	 (assumption def-big-phi (forall (lam (x ss)
					      (= (big_phi (phi_1 x))
						 (phi_2 x)))))
	 
	 (conclusion thm (sub-relation rho_1 rho_2))
	 )
	 
;(th~defproblem deussen-4.11.2 (in hua)
;         (type-constants ss uu vv)
;         (constants
;          (phi_1 (uu ss))
;          (phi_2 (vv ss))
;          (rho_1 (o ss ss))
;          (rho_2 (o ss ss))
;          (big_phi (vv uu))
;          )
;         (assumption def-rho1 (induzierte_eqrel_abb rho_1 phi_1))
;
;         (assumption def-rho2 (induzierte_eqrel_abb rho_2 phi_2))
;
;         (assumption sub-relation (sub-relation rho_1 rho_2))
;
;         (assumption surjectiv_phi_1 (surjective phi_1))
;;??????????????????
;         (conclusion thm (exists (lam (big_phi (vv uu))         ; big_phi ist Abbildung
;                                      (and
;                                       (forall (lam (x ss)
;                                                    (forall (lam (y ss))
;                                                            (implies (= (phi_1 x)
;                                                                        (phi_1 y))))))
;                                      (forall (lam (x ss)
;                                                   (= (big_phi (phi_1 x))
;                                                      (phi_2 x))))))))
;         )
	 
(th~defproblem deussen-4.8 (in hua)
	 (type-constants bb)
	 (constants
	  (Rho (o bb bb))
	  (Sigma (o bb bb)))
	  

(assumption symm-rho (symmetric Rho))

(assumption symm-sigma (symmetric Sigma))

(conclusion thm (symmetric (relation-intersection Sigma Rho)))
)


(th~defproblem deussen-5.3 (in hua)
	 (type-constants bb)
	 (constants
	  (sg (struct bb))
	  (rho (o bb bb))
	  (sigma (o bb bb)))
	 
(assumption leftcongruence-rho (leftcongruence sg rho))

(assumption leftcongruence-sigma (leftcongruence sg sigma))

(conclusion thm (leftcongruence sg (relation-intersection sigma rho)))
)


(th~defproblem deussen-5.7.1 (in hua)
	 (type-constants ss uu vv)
	 (constants
	  (phi_1 (morphism ss ss))
	  (phi_2 (morphism ss ss))
	  (rho_1 (o ss ss))
	  (rho_2 (o ss ss))
	  (big_phi (morphism ss ss))
	  )
	 (assumption def-rho1 (induzierte_eqrel_hom rho_1 phi_1))

	 (assumption def-phi1 (homomorphism
			       (struct-set (morphism-domain phi_1))
			       (struct-op (morphism-domain phi_1))
			       (struct-set (morphism-codomain phi_1))
			       (struct-op (morphism-codomain phi_1))
			       (morphism-function phi_1)))


	 (assumption def-rho2 (induzierte_eqrel_hom rho_2 phi_2))
	 
	 (assumption def-phi2 (homomorphism
			       (struct-set (morphism-domain phi_2))
			       (struct-op (morphism-domain phi_2))
			       (struct-set (morphism-codomain phi_2))
			       (struct-op (morphism-codomain phi_2))
			       (morphism-function phi_2)))
	 (assumption def-big-phi (forall (lam (x ss)
					      (= (morphism-function big_phi
								    (morphism-function phi_1 x))
						 (morphism-function phi_2 x)))))

	 (assumption def-hom-big_phi (homomorphism 
			       (struct-set (morphism-domain big_phi))
			       (struct-op (morphism-domain big_phi))
			       (struct-set (morphism-codomain big_phi))
			       (struct-op (morphism-codomain big_phi))
			       (morphism-function big_phi)))

	 (conclusion thm (sub-relation rho_1 rho_2))
	 )
	 
;(th~defproblem deussen-5.7.2c (in hua)
;         (type-variables ss h1h1 h2h2 ff)
;         (constants
;          (big_phi morphism)
;          (phi_1 morphism)
;          (phi_2 morphism)
;          (H1 (o h1h1))
;          (H2 (o h2h2))
;          (F (o ff))
;          )
;
;         (assumption def-phi1 (homomorphism phi_1))
;
;         (assumption def-phi2 (homomorphism phi_2))
;
;         (assumption sur-phi1 (surjective (struct-set (morphism-domain phi_1))
;                                          (struct-set (morphism-codomain phi_1))
;                                          (morphism-function phi_1)
;                                          ))
;         
;         (assumption def-big-phi (forall (lam (x ss)
;                                              (implies (F x)
;                                                       (= (morphism-function big_phi
;                                                                             (morphism-function phi_1 x))
;                                                          (morphism-function phi_2 x))))))
;
;         (assumption lemma-phi1 (forall (lam (x ss)
;                                             (implies (F x)
;                                                      (H1 (morphism-function phi_1 x))))))
;
;         (assumption lemma-phi2 (forall (lam (x ss)
;                                             (implies (F x)
;                                                      (H1 (morphism-function phi_2 x))))))
;
;         (assumption lemma-big_phi (forall (lam (x ss)
;                                                (implies (H1 x)
;                                                         (H2 (morphism-function big_phi
;                                                                                x))))))
;         
;         (conclusion thm (homomorphism big_phi))
;)










