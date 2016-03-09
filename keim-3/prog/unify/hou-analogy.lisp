;;; -*- Syntax: Common-lisp; package: OMEGA; base: 10; mode: keim -*-
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; hou-analogy.lisp; This file is part of the OMEGA system
;;
;; major updates: 4.10.1999
;; 
;;
;; Authors: Carsten Ullrich
;; email: cullrich@ags.uni-sb.de 
;;
;; For information about this program, write to:                          
;;   OMEGA Project                                                        
;;   AG Siekmann/FB Informatik                                            
;;   Universitaet des Saarlandes                                          
;;   Bau 36, 4. Stock                                                     
;;   D-66041 Saarbruecken                                                 
;;   Germany    
;;
;; For information about the newest version of Omega, see 
;;   http://www.ags.uni-sb.de/~omega/
;;
;; This program is free software; it can be used under the terms of the GNU General
;; Public License as published by the Free Software Foundation; either version 2 of
;; the License, or any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; merchantibility or fitness for a particular purpose. 
;; See the GNU General Public License (http://www.fsf.org/copyleft/gpl.html)
;; for more details.
;;
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;; this file contains the modifications for higher order matching for analogy

(in-package :keim)


(mod~defmod hou-analogy
            :uses (bind data hou keim term uni-main)
            :documentation "Higher Order Matching Modifactions for analogy."
            :exports (uni+analogy

		      hou~trivial-p
		      uni~bind
		      hou~alpha-eta
                      ))

(defclass uni+analogy (uni+match)
  ()
  (:documentation
   "Structure for analogy matching."))

(defmethod hou~trivial-p ((term1 term+constant) (term2 term+appl) (uni uni+analogy))
  nil)

(defmethod hou~trivial-p ((term1 term+constant) (term2 term+constant) (uni uni+analogy))
  (or (and (logic~connective-p term1)(logic~connective-p term2)
	   (eq (keim~name term1)(keim~name term2)))
      (and (eq (keim~name term1) (keim~name term2))
	   (data~equal (term~type term1) (term~type term2)))
      ))

(defmethod hou~trivial-p ((term1 term+constant) (term2 term+abstr) (uni uni+analogy))
  nil)

(defmethod uni~bind ((variable term+variable)(term data+object)(uni uni+analogy))
  (declare (edited  "11.12.96")
           (authors kk cu)
           (input   "variable and term")
           (effect  "Sets binding of variable to term.")
           (value   "nil when failure or, term if success."))
  (unless (or (hou~nullvariable-p variable)
              (hou~contains-nullvariable-p term uni))
    (bind~bind variable term)
    term))

(defmethod hou~alpha-eta ((term1 term+term) (term2 term+term) (uni uni+analogy))
  (declare (edited  "16.4.96")
           (authors kk)
           (input   "term1 term2 (one of them an abstraction)
                     and unfictation structure uni.")
           (effect  "Executes ALPHA-eta-reduction to term1 and term2.")
           (value "two reduced terms"))
  (let* ((bind1 (and (data~abstr-p term1) (data~abstr-domain term1)))
         (bind2 (and (data~abstr-p term2) (data~abstr-domain term2)))        
         (lb1 (length (the list bind1)))
         (lb2 (length (the list bind2))))
    (if (> lb2 lb1)
	(let* ((shared-store
		(mapcar #'(lambda (x)
			    (pop bind2) ;; throw away excess binder
			    (hou~generate-nullvariable x)) bind1)) 
	       (xtra-binder (mapcar #'hou~generate-nullvariable bind2)))
	  ;; extra parameters of term2
	  (if (and xtra-binder shared-store)
	      (list (term~appl-create term2 (append shared-store xtra-binder))
		    (term~appl-create term1 (append shared-store xtra-binder)))
	    ;; else
	    (if xtra-binder
		(list (term~appl-create term2 xtra-binder)
		      (term~appl-create term1 xtra-binder))
	      ;; else
	      (list (term~appl-create term2 shared-store)
		    (term~appl-create term1 shared-store)))))
      ;; else
      (let* ((shared-store
              (mapcar #'(lambda (x)
                          (pop bind1) ;; throw away excess binder
                          (hou~generate-nullvariable x)) bind2)) 
             (xtra-binder (mapcar #'hou~generate-nullvariable bind1)))
        ;; extra parameters of term1
        (if (and xtra-binder shared-store)
            (list (term~appl-create term1 (append shared-store xtra-binder))
                  (term~appl-create term2 (append shared-store xtra-binder)))
          ;; else
          (if xtra-binder
              (list (term~appl-create term1 xtra-binder)
                    (term~appl-create term2 xtra-binder))
            ;; else
            (list (term~appl-create term1 shared-store)
                  (term~appl-create term2 shared-store))))))))
