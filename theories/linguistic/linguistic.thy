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

(th~deftheory LINGUISTIC
	      (uses function)
	      (constants (frank i)
			 (henry i)
			 (andrew i)
			 (john i)
			 (marvin i))
	      (help "The linguistic theory."))

(th~defdef is-subset
	   (in linguistic)
	   (type-variables bb)
           (definition
             (lam  (U (o bb))
		   (lam (V (o bb))
			(forall (lam (x bb) (implies (U x) (V x)))))))
	   (help "The predicate for subset."))


(th~defdef most
	   (in linguistic)
	   (type-variables bb)
           (definition
             (lam  (P (o bb))
		   (lam (Q (o bb))
			    (smaller-cardinality (setminus P Q) (intersection P Q)))))
	   (help "The most quantifier."))

	        
(th~defdef no
	   (in linguistic)
	   (type-variables bb)
           (definition
             (lam  (P (o bb))
		   (lam (Q (o bb))
			    (not (exists (lam (x bb) (and (P x)
							  (Q x)))))))))
	  


(th~defdef exactly
	   (in linguistic)
	   (type-variables bb cc)
           (definition
             (lam  (I (o cc))
		   (lam (P (o bb))
		
			    (exists (lam (f (bb cc))
				  (exists (lam (g (cc bb))  
					    (and 
					             (surjective  P  I g)
						     (surjective I  P  f))))))))))
					  
							
(th~defdef atmost
	   (in linguistic)
	   (type-variables bb cc)
           (definition
	     (lam (I (o cc))
                (lam  (P (o bb))
		 
			    (exists (lam (f (bb cc)) (surjective I P  f)))))))
							 

(th~defdef atleast
	   (in linguistic)
	   (type-variables bb cc)
           (definition
	     (lam (I (o cc))
                (lam  (P (o bb))
		  
			    (exists (lam (f (cc bb)) (surjective  P  I f)))))))


(th~defdef one-thing
	   (in linguistic)
	   (definition
	     (lam (x i) (= x frank))))

(th~defdef two-things
	   (in linguistic)
	   (definition
	     (lam (x i) (and (not (= frank henry))
		             (or (= x henry)
			         (= x frank))))))

(th~defdef three-things
	   (in linguistic)
	   (definition
	     (lam (x i)
		  (and (not (= frank andrew))
		       (and (not (= andrew henry))
		            (and (not (= frank henry))
			         (or (= x andrew)   
			             (or (= x henry)
			                 (= x frank)))))))))

(th~defdef four-things
	   (in linguistic)
	   (definition
	     (lam (x i)
                (and (not (= john frank))
		     (and (not (= john andrew))
			  (and (not (= john henry))
			       (and (not (= frank andrew))
				    (and (not (= andrew henry))
					 (and (not (= frank henry))
					      (or (= x john)
						  (or (= x andrew)   
						      (or (= x henry)
							  (= x frank)))))))))))))

(th~defdef five-things
	   (in linguistic)
	   (definition
	     (lam (x i)
                (and (not (= john frank))
		     (and (not (= john andrew))
			  (and (not (= john henry))
			       (and (not (= frank andrew))
				    (and (not (= andrew henry))
					 (and (not (= frank henry))
					      (and (not (= marvin john))
						   (and (not (= marvin frank))
							(and (not (= marvin andrew))
							     (and (not (= marvin henry))
					(or (= x marvin)			  
					      (or (= x john)
						  (or (= x andrew)   
						      (or (= x henry)
							  (= x frank))))))))))))))))))



(th~defdef all
	   (in linguistic)
	   (type-variables bb)
           (definition
             (lam  (P (o bb))
		   (lam (Q (o bb))
			    (forall (lam (x bb) (implies (P x)
							 (Q x))))))))

(th~defdef a
	   (in linguistic)
	   (type-variables bb)
           (definition
             (lam  (P (o bb))
		   (lam (Q (o bb))
			    (exists (lam (x bb) (and  (P x)
						      (Q x))))))))

;;(th~defdef one
;;	   (in linguistic)
;;	   (type-variables bb)
;;          (definition
;;             (lam  (P (o bb))
;;		   (lam (Q (o bb))
;;			    (exists-unique (lam (x bb) (implies (P x)
;;						       	        (Q x))))))))


(th~defdef some
	   (in linguistic)
	   (type-variables bb)
           (definition
             (lam  (P (o bb))
		   (lam (Q (o bb))
			    (exists (lam (x bb) (and  (P x)
						      (Q x))))))))




(th~defdef downwardmonotonic
	   (in linguistic)
	   (type-variables bb)
           (definition
               (lam (Q (o (o bb) (o bb)))
			    (forall (lam (X (o bb))
				 (forall (lam (Y (o bb))
				       (forall (lam (Z (o bb))
					         (implies (and (Q x y)
							       (is-subset z y))
							  (Q x z)))))))))))


(th~defdef upwardmonotonic
	   (in linguistic)
	   (type-variables bb)
           (definition
               (lam (Q (o (o bb) (o bb)))
			    (forall (lam (X (o bb))
				 (forall (lam (Y (o bb))
				       (forall (lam (Z (o bb))
					         (implies (and (Q x y)
							       (is-subset y z))
							  (Q x z)))))))))))


(th~defdef persistency
	   (in linguistic)
	   (type-variables bb)
           (definition
               (lam (Q (o (o bb) (o bb)))
			    (forall (lam (X (o bb))
				 (forall (lam (Y (o bb))
				       (forall (lam (Z (o bb))
					         (implies (and (Q x y)
							       (is-subset x z))
							  (Q z y)))))))))))


(th~defdef antipersistency
	   (in linguistic)
	   (type-variables bb)
           (definition
               (lam (Q (o (o bb) (o bb)))
			    (forall (lam (X (o bb))
				 (forall (lam (Y (o bb))
				       (forall (lam (Z (o bb))
					         (implies (and (Q x y)
							       (is-subset z x))
							  (Q z y)))))))))))





