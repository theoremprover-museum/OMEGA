;;; -*- syntax: common-lisp; package: keim; base: 10; mode: keim -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     OMEGA Project                                                        ;;
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

;;
;; Cond-Funcs + Deffuns for existse-sort*
;;

(meth~defcond mexsort-formula-p (args cmapp)
	      (declare (edited  "22-FEB-2001")
		       (authors Ameier)
		       (input   "A list of arguments (a formula) and a cmapping <c,mapp>.")
		       (effect  "None.")
		       (value   "If the formula is a exists-sort formula, then cmapp, otherwise cmapp is mapped"
				"to nil."))
	      (let* ((formula (first args)))
		(if (potac=exists-sort-formula? formula)
		    cmapp
		  (meth~mapp-new-constraint cmapp nil))))

(meth~deffun mcompute-hyps-for-exsort (exsort-formula new-consts)
  (declare (edited  "05-MAR-2001")
	   (authors Ameier)
	   (input   "A formula starting with exists-sort and a set of new constants.")
	   (effect  "None.")
	   (value   "A list of ndlines with the existse-sort hyps corresponding to the formula."
		    "The lines are justified by HYP."))
  (let* ((hyp-formulas (potac=get-hyps-recursive exsort-formula new-consts)))
    (mapcar #'(lambda (hyp-formula)
		(pdsn~make-hypothesis hyp-formula (pds~new-node-name)))
	    hyp-formulas)))

(meth~deffun mcompute-andes-for-exsort (hyps-lines)
  (declare (edited  "05-MAR-2001")
	   (authors Ameier)
	   (input   "A list of conjunctive hyp-lines.")
	   (effect  "None.")
	   (value   "A list of ndlines that consist of the left- and the right-conjunct of the input lines"
		    "respectively. The lines are justified with 'andel' and 'ander' respectively."))
  (apply #'append (mapcar #'(lambda (hyp-line)
			      (let* ((left-formula (first (data~appl-arguments (node~formula hyp-line))))
				     (right-formula (second (data~appl-arguments (node~formula hyp-line))))
				     (andel-just (pdsj~closed-just-create (infer~find-method 'andel) (list hyp-line) nil
									  "grounded"))
				     (ander-just (pdsj~closed-just-create (infer~find-method 'ander) (list hyp-line) nil
									  "grounded")))
				(list (pdsn~create (pds~new-node-name) (list hyp-line) left-formula andel-just)
				      (pdsn~create (pds~new-node-name) (list hyp-line) right-formula ander-just))))
			  hyps-lines)))

(meth~deffun mcompute-sponsors-for-exsort (ande-lines)
  (declare (edited  "05-MAR-2001")
	   (authors Ameier)
	   (input   "The list of the ande-lines.")
	   (effect  "None.")
	   (value   "A list containing only the odd-elements of the input list (the first, the third etc.) and the"
		    "last item."))
  (let* ((counter 0))
    (append (apply #'append (mapcar #'(lambda (item)
					(if (oddp (incf counter))
					    (list item)
					  nil))
				    ande-lines))
	    (last ande-lines))))
