;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: keim -*-
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


(infer~defmethod Mult-by-Price
		 (outline-mappings (((existent nonexistent existent) Mult-by-Price)))
		 (help "Rewriting with the sf-factor-axiom theorem."))

(meth~defmethod Mult-by-Price Mult-by-Price
                (in size)
                (rating 5)
                (declarations
                 (sorted-meta-variables
		  (K num) (F (num num))
		  (v num) (u num) (m num)
		  (phi o) (phi2 o) (phi3 o)))
		(premises (+ l2) p1)
		(application-condition
		 (mand (mbind F-var (type-newvar (:type (num num))))
		       (mbind u-var (type-newvar (:type num)))
		       (match-and-bind-subterm (:term (size-function F-var v u-var)) Phi subst pos)))
		(conclusions (- conc))
		(outline-computations
		 (F (subst-apply subst F-var))
		 (u (subst-apply subst u-var))
		 (phi2 (termrplatpos phi pos (:term (size-function (s-times-r1 F K) m u))))
		 (ppos (posappend pos (:position (1))))
		 (phi3 (termrplatpos phi2 ppos (compute-with-cas (termatpos phi2 ppos)))))
		(expansion-computations
		 (conv (th-ass (:string "sf-factor-axiom")))
		 (the-cas (:symbol mycas)))
		(decl-content
		 (p1 () (sizeunit-factor K m v))
		 (E1 (conv) (= (size-function F v u) (size-function (s-times-r1 F K) m u)) ("Assertion" () (conv p1)))
		 (l2 () phi3 ("open" () ()))
		 (l1 (conv) phi2 ("cas" (ppos the-cas) (l2)))
                 (conc () phi ("=subst" (pos) (l1 E1)))
		 )
		(proc-content schema-interpreter)
		(remark "The method checks whether the conclusion term contains a size function. It then tries to multiply this size function with an appropriate price.")
		)

(infer~defmethod add-by-denom
		 (outline-mappings (((existent nonexistent) add-by-denom)))
		 (help "Add size functions with equal denomination."))

(meth~defmethod add-by-denom add-by-denom
                (in size)
                (rating 3)
                (declarations
                 (sorted-meta-variables
		  (F (num num)) (G (num num))
		  (u num term) (m num term)
		  (phi o) (phi2 o) (phi3 o)))
		(premises (+ l2))
		(application-condition
		 (mand (mbind F-var (type-newvar (:type (num num)))) ;;; C1
		       (mbind G-var (type-newvar (:type (num num)))) ;;; C2
		       (mbind u-var (type-newvar (:type num))) ;;; C3
		       (mbind m-var (type-newvar (:type num))) ;;; C4 
		       (match-and-bind-subterm (:term (p-plus-r1 (size-function F-var m-var u-var)
								 (size-function G-var m-var u-var)))
					       Phi subst pos)))  ;;; C5
		(conclusions (- conc))
		(outline-computations
		 (F (subst-apply subst F-var))
		 (G (subst-apply subst G-var))
		 (m (subst-apply subst m-var))
		 (u (subst-apply subst u-var))
		 (phi2 (termrplatpos phi pos (:term (size-function (p-plus-r1 F G) m u))))
		 (ppos (posappend pos (:position (1))))
		 (phi3 (termrplatpos phi2 ppos (compute-with-cas (termatpos phi2 ppos)))))
		(expansion-computations
		 (sfadd (th-ass (:string "sf-addition")))
		 (the-cas (:symbol mycas)))
		(conclusions (- conc))
		(decl-content
		 (E1 (sfadd) (= (p-plus-r1 (size-function F m u)
					   (size-function G m u))
				(size-function (p-plus-r1 F G) m u)) ("Assertion" () (sfadd)))
		 (l2 () phi3 ("open" () ()))
		 (l1 (sfadd) phi2 ("cas" (ppos the-cas) (l2)))
                 (conc () phi ("=subst" (pos) (l1 E1)))
		 )
		(proc-content schema-interpreter)
		(remark "The method checks whether the conclusion term contains a sum of two size functions. It then computes a singel sizefunction by addition if the denominators are eqaual.")
		)


#|  just as a comparison to the old formalism.....
(infer~defmethod pplus-size-function
		 (outline-mappings (((existent nonexistent existent existent) pplus-size-function)))
		 (help "Rewriting with the pplus-size-function theorem."))

(meth~defmethod pplus-size-function pplus-size-function
                (in size)
                (rating 3)
                (declarations
                 (sorted-meta-variables
                  (p1 o prln) (p2 o prln) (E1 o prln) (E2 o prln) (E3 o prln)
                  (l1 o prln) (l2 o prln) (l3 o prln) (l4 o prln)
                  (l5 o prln) (l6 o prln) (conc o prln)
                  (conv o prln) (sfadd o prln)
		  (the-cas o sym)
                  (h o prlnlist)
                  (h2 o prlnlist)
                  (h1 o prlnlist)
                  (F (num num) term) (G (num num) term)
		  (F-var (num num) term) (G-var (num num) term)
                  (K num term) (L num term)
                  (V num term)
                  (U num term) (U-var num term)
		  (W num term)
		  (m num term)
		  (sfGWU (num num) term)
                  (sfFVU-pat (num num) term) (sfGWU-pat (num num) term) (specterm-pat (num num) term)
		  (timesFK (num num) term) (timesGL (num num) term) (plus-timesFK-timesGL (num num) term)
		  (specterm3 (num num) term) (sf-timesFK-MU (num num) term) (sf-timesGL-MU (num num) term)
		  (specterm1 (num num) term) (specterm2 (num num) term)
                  (Phi o term) (Phi1 o term) (Phi2 o term) (Phi3 o term) (Phi4 o term) (Phi5 o term) (Phi6 o term) 
		  (substpos o pos) (substpos1 o pos) (substpos2 o pos) (subst o sub)
		  (ppos o pos) (fspos o pos) (sspos o pos)
		  )
                 (sorted-meta-constants
		  (mnot (ee ee) (o o))
		  (mequal (o ee ee) (o prln prln))
                  (mand (ee ee ee) (o o o))
                  (mbind (o ff ee) (o term var))
		  (subtermpos (ff ee o) (pos term term))
		  (termmgm (ff ee ee) (sub term term))
		  (termatpos (ff ee o) (term pos term))
		  (termrplatpos (o ff o o) (term term pos term))
		  (application (gg ff ee) (appl termlist term))
		  (listcons (dd dd ff) (list list o))
		  (termemptylist gg termlist)
                  (substapply (ee ee o) (term term sub))
		  (convax o prln)
		  (sfaddax o prln)
		  (mycas o sym)
		  (newvar (ff ee) (term type))
		  (termtype (ff ee) (type term))
		  (typeabstr (ff dd ee) (type type list))
		  (typeemptylist ff typelist)
		  (compute-with-cas (ee ee) (term term))
		  (posappend (o o o) (pos pos pos))
		  (emptypos o pos)
		  (poscons (o o num) (pos pos o))
		  ))
		(premises (+ l6) p1 p2)
		(outline-constraint
		 (mand
		  (mand                 
		   (mand
		    (mand
		     (mand (mnot (mequal p2 p1))              ;;C1
			   (mbind F-var (newvar (typeabstr (listcons (termtype K) typeemptylist)
							   (termtype K)))))   ;;C2
		     (mand (mbind u-var (newvar (termtype v)))    ;;C3
			   (mbind G-var (newvar (typeabstr (listcons (termtype K) typeemptylist)
							   (termtype K))))))  ;;C4
		    (mand
		     (mand (mbind sfFVU-pat (application size-function
							 (listcons F-var
								   (listcons v (listcons u-var termemptylist))))) ;;C5
			   (mbind sfGWU-pat (application size-function
							 (listcons G-var
								   (listcons w (listcons u-var termemptylist)))))) ;;C6
		     (mand (mbind specterm-pat (application p-plus-r1
							    (listcons sfFVU-pat
								      (listcons sfGWU-pat termemptylist)))) ;;C7
			   (mbind substpos (subtermpos phi specterm-pat))))) ;;C8
		   (mand
		    (mand
		     (mand (mbind subst (termmgm specterm-pat (termatpos phi substpos))) ;;C9
			   (mbind F (substapply subst F-var))) ;;C10
		     (mand (mbind G (substapply subst G-var)) ;;C11
			   (mbind u (substapply subst u-var)))) ;;C12
		    (mand
		     (mand (mbind timesFK (application s-times-r1
						       (listcons F (listcons K termemptylist))));;C13
			   (mbind timesGL (application s-times-r1
						       (listcons G (listcons L termemptylist))))) ;;C14
		     (mand (mbind plus-timesFK-timesGL
				  (application p-plus-r1 (listcons timesFK (listcons timesGL termemptylist)))) ;;C15
			   (mbind specterm3
				  (application size-function
					       (listcons plus-timesFK-timesGL
							 (listcons m (listcons u termemptylist))))))))) ;;C16
		  (mand
		   (mand
		    (mand (mbind phi3 (termrplatpos phi substpos specterm3)) ;;C17
			  (mbind ppos (posappend substpos (poscons 1 emptypos)))) ;;C18
		    (mand (mbind fspos (posappend ppos (poscons 1 emptypos)))   ;;C19
			  (mbind sspos (posappend ppos (poscons 2 emptypos))))) ;;C20
		   (mand
		    (mand (mbind phi4 (termrplatpos phi3 fspos
						    (compute-with-cas (termatpos phi3 fspos)))) ;;C21
			  (mbind phi5 (termrplatpos phi4 sspos
						    (compute-with-cas (termatpos phi4 sspos))))) ;;C22
		    (mbind phi6 (termrplatpos phi5 ppos
					      (compute-with-cas (termatpos phi5 ppos)))))) ;;C23
		  )
		 )
		(expansion-constraint
		 (mand 
		  (mand
		   (mand
		    (mand (mbind sf-timesFK-MU
				 (application size-function
					      (listcons timesFK (listcons m (listcons u termemptylist))))) ;;C1
			  (mbind sfGWU (application size-function
						    (listcons G
							      (listcons w (listcons u termemptylist)))))) ;;C2
		    (mand (mbind specterm1 (application p-plus-r1
							(listcons sf-timesFK-MU (listcons sfGWU termemptylist)))) ;;C3
			  (mbind phi1 (termrplatpos phi substpos specterm1)))) ;;C4
		   (mand
		    (mand (mbind sf-timesGL-MU (application size-function
							    (listcons timesGL
								      (listcons m (listcons u termemptylist))))) ;;C5
			  (mbind specterm2 (application p-plus-r1
							(listcons sf-timesFK-MU
								  (listcons sf-timesGL-MU termemptylist))))) ;;C6
		    (mand (mbind phi2 (termrplatpos phi1 substpos specterm2)) ;;C7
			  (mbind conv convax)))) ;;C8
		  (mand
		   (mand (mbind sfadd sfaddax) ;;C9
			 (mbind substpos1 (posappend substpos (poscons 1 emptypos)))) ;;C10
		   (mand (mbind substpos2 (posappend substpos (poscons 2 emptypos))) ;;C11
			 (mbind the-cas mycas))) ;;C12
		  )
		 )
		(conclusions (- conc))
		(decl-content
		 (p1 h1 (sizeunit-factor K m v))
		 (p2 h2 (sizeunit-factor L m w))
		 (E1 (h1 conv) (= (size-function F v u) (size-function (s-times-r1 F K) m u)) ("Assertion" () (conv p1)))
  		 (E2 (h2 conv) (= (size-function G w u) (size-function (s-times-r1 G L) m u)) ("Assertion" () (conv p2)))
		 (E3 (sfadd) (= (p-plus-r1 (size-function (s-times-r1 F K) m u)
					   (size-function (s-times-r1 G L) m u))
				(size-function (p-plus-r1 (s-times-r1 F K)
							  (s-times-r1 G L)) m u)) ("Assertion" () (sfadd)))
		 (l6 h phi6 ("open" () ()))
		 (l5 (h conv sfadd) phi5 ("cas" (ppos the-cas) (l6)))
		 (l4 (h conv sfadd) phi4 ("cas" (sspos the-cas) (l5)))
		 (l3 (h conv sfadd) phi3 ("cas" (fspos the-cas) (l4)))
		 (l2 (h conv sfadd) phi2 ("=subst" (substpos) (l3 E3)))
		 (l1 (h conv sfadd) phi1 ("=subst" (substpos2) (l2 E2)))
                 (conc h phi ("=subst" (substpos1) (l1 E1)))
		 )
		(proc-content schema-interpreter)
		(remark "The method checks whether the conclusion term contains a sum of two size functions. It then computes a singel sizefunction by multiplication with appropriate prizes and addition if the denominators are eqaual.")
		)
|#

