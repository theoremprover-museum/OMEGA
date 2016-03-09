;;; -*- Package: KEIM; Syntax: Common-lisp; Mode: LISP -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1997 by AG Siekmann, Fachbereich Informatik,             ;;
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


(in-package "KEIM")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AdHoc Rewrite Systems for Keim (kk)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(mod~defmod rewrite :uses (data)
	    :documentation "Rewriting on terms."
	    :exports (
                      rewrite~preorder
		      )
	    )

(defun rewrite=preorder-aux (term pred trans)
  (let ((position (data~position term pred)))
    (if position 
        (values
         (data~replace-at-position
          term position (funcall trans (data~struct-at-position
                                        term position))
          :replacers-downto '(term+constant type+primitive))
         
         position)
      ;; else
      term)))
  
(defun rewrite~preorder (term predicates rulenames transformations
                              &optional tlist)
  (declare (edited  "01-DEC-1997")
	   (authors Konrad)
	   (input "A term, a list of test predicates and transformations.")
	   (effect "Postorder transformation with multiple rules.")
	   (value  "a term and a list of rules and positions."))
  (let ((dropp predicates)(dropn rulenames))
    (dolist (i transformations)
      (multiple-value-bind (nterm pos)
          (rewrite=preorder-aux term (pop dropp) i)
        (if pos
            ;; restart process with new term
            (return-from rewrite~preorder
              (rewrite~preorder
               nterm predicates rulenames transformations
               (cons (cons (pop dropn) pos) tlist)))
          ;; else (get next rulename)
          (pop dropn))))
    ;; no transformation left
    (values term (nreverse tlist))))
           
    




