;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     PROVERB Project                                                      ;;
;;     AG Siekmann/FB Informatik                                            ;;
;;     Universitaet des Saarlandes                                          ;;
;;     Bau 36, 4. Stock                                                     ;;
;;     D-66041 Saarbruecken                                                 ;;
;;     Germany                                                              ;;
;;   electronic mail: proverb@cs.uni-sb.de                                  ;;
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



(mod~defmod ALX 
            :uses (alb data env infer just keim logic meta node ot pds pdsn post sys term) 
            :documentation "Auxiliary functions for the assertion level."
            :exports (
                      alx~add-non-hyp-assertion!
                      alx~break
                      alx~break-after-debug-lines
                      alx~copy-proof
                      alx~create-short-name
                      alx~debug
                      alx~debug-lines
		      alx~defn-e-line-p
                      alx~deletable-hypothesis-p
                      alx~double-negation-p
                      alx~fall
                      alx~formula-without-double-negation
		      alx~get-best-same-line
                      alx~line-not-to-match-p
                      alx~match-p
                      alx~negate-formula
                      alx~negation-normal
                      alx~negation-scope
                      alx~non-hyp-assertions
		      alx~reset-non-hyp-assertions
                      alx~rule-symbol
                      alx~same-line-p
                      alx~set-premise-tree-debug!
                      alx~skip-same-line
                      alx~sort-premises
                      alx~structural-rule-p
                      
                      alx*show-premise-tree
                      alx*show-rule-tree
                      alx*tracer
                      alx*verbose))




;#########################################
;##                                     ##
;##              Variablen              ##
;##                                     ##
;#########################################

(defvar alx*tracer nil
  "If this variable is T, some trace will be shown, else it won't.")

(defvar alx*show-rule-tree nil
  "If this variable is T, a call of {\\vb altr~show-cond} will show rule trees, else
   it won't.")

(defvar alx*show-premise-tree nil
  "If this variable is T, a call of {\\vb altr~show-cond} will show premise trees,
   else it won't.")

(defvar alx*verbose nil
  "If this variable is T, more comments are given about what is done.")

(defvar alx*debug-lines nil
  "A variable containing the labels of the lines to be debugged.")

(defvar alx*break-lines nil
  "A variable containing the labels of the lines to break.")

(defvar alx*structural-rules '(:hyp :impi :existse :ore :indirect)
  "A list of symboles denoting structural gentzen rules.")

(defvar alx*defn-e-e-assertions nil
  "A list of defn-e lines, that could be assertions.")



;#########################################
;##                                     ##
;##           Hilfsfunktionen           ##
;##                                     ##
;#########################################

(defun alx~same-line-p (line)
  (declare (edited  "30-JUN-1997" "04-OCT-1993 16:14")
	   (authors Afiedler AFIEDLER)
	   (input   "A proof line.")
	   (value   "T, iff the justification of LINE is the nd-rule `Weaken'."))
  (string= (keim~name (just~method (node~justification line)))
	   :weaken))

(defun alx~skip-same-line (line)	;kann das so bleiben, oder Abfrage nach selben Assumptions?
  (declare (edited  "09-JUN-1993 10:41")
	   (authors AFIEDLER)
	   (input   "A proof line.")
	   (value   "If LINE is a same-line, the preceeding line, that is"
		    "not a same-line, else LINE."))
  (if (alx~same-line-p line)
      (alx~skip-same-line (car (pdsn~just-premises line)))
    line))

(defun alx~get-best-same-line (line orig)
  (declare (edited  "25-SEP-1998")
	   (authors Afiedler)
	   (input   "Two pds nodes.")
	   (effect  )
	   (value   "The pds node with the lowest value in the subproof with LINE as root"
		    "that is equal to ORIG in its hypotheses and formula."))
  (let ((prems (append (apply #'append
			      (mapcar #'(lambda (pre)
					  (let ((same (alx~get-best-same-line pre orig)))
					    (if same (list same))))
				      (pdsn~just-premises line)))
		       (let ((line-hyps (pdsn~hyps line))
			     (orig-hyps (pdsn~hyps orig)))
			 (if (and (not (eq line orig))
				  (data~equal (node~formula line) (node~formula orig))
				  (subsetp line-hyps orig-hyps)
				  (subsetp orig-hyps line-hyps))
			     (list line))))))
    (if prems
	(let* ((vals (mapcar #'alb~line-value prems))
	       (pos (position (apply #'min vals) vals :test #'=)))
	  (nth pos prems)))))


(defun alx~structural-rule-p (obj)
  (declare (edited  "02-OCT-1996")
	   (authors Afiedler)
	   (input   "A proof line or a justification.")
	   (value   "T, if its rule is a structural one."))
  (member (alx~rule-symbol obj) alx*structural-rules :test #'string=))





(defun alx~copy-proof (proof newname)
  (declare (edited  "10-JUL-1997" "26-FEB-1993 13:40")
	   (authors Afiedler nesmith afiedler)
	   (input   "A proof and a name.")
	   (effect  "Creates a copy of PROOF with the name NEWNAME.")
	   (value   "The new proof."))
;;; eigentlich muss hier ein echtes pds~copy hin.
  (let ((old-name (keim~name proof))
	(string-stream (make-string-output-stream))
	(newproof nil))
    (keim~set-name! proof newname)
    (let ((*standard-output* string-stream))
      (pds~pprint-proof-plan proof 'pds-post))
    (with-input-from-string (in (get-output-stream-string string-stream))
			    (let ((read-in (read in)))
			      (setq newproof
				    (omega::ot~read-proof-plan
				     (post~read-object (append (list (car read-in) nil)
							       (cdr read-in))
						       (env~create)
						       nil)))))
    (keim~set-name! proof old-name)
    newproof))




(defun alx~negation-scope (term)      
  (declare (edited  "07-OCT-1992 08:46")
	   (authors AFIEDLER)
	   (input   "A term.")
	   (value   "If TERM is a negation, the scope of the negation, else an error is"
		    "signalled."))
  (if (logic~negation-p term)
      (cadr (data~substructs term))
    (error (sys~make-condition 'alb+term-error :term term :type 'negation))))

(defun alx~double-negation-p (term)
  (declare (edited "13-SEPT-1993 15:54")
	   (input "A formula.")
	   (value "T, if FORMULA is a double negation."))
  (and (logic~negation-p term) (logic~negation-p (alx~negation-scope term))))

(defun alx~negate-formula (formula)
  (declare (edited  "25-JUN-1997" "08-FEB-1993 15:41")
	   (authors Afiedler AFIEDLER)
	   (input   "A formula.")
	   (value   "The negated formula."))
  (data~appl-create (env~lookup-object 'not (alb~rule-environment pds*current-proof-plan))
		    (list formula)))

(defun alx~formula-without-double-negation (formula)
  (declare (edited  "15-NOV-1993 11:51")
	   (authors AFIEDLER)
	   (input   "A formula.")
	   (value   "If, FORMULA is a double negation the not double negated formula,"
		    "otherwise FORMULA."))
  (if (alx~double-negation-p formula)
      (alx~negation-scope (alx~negation-scope formula))
    formula))

(defun alx~negation-normal (node)
  (declare (edited  "23-JUN-1997" "27-NOV-1992 09:10")
	   (authors Afiedler AFIEDLER)
	   (input   "A rule node with a negation as formula.")
	   (effect  "Tries to push the negation in the formula of NODE.")
	   (value   "The new formula, if pushing is possible, otherwise NIL."))
  (let* ((formula (node~formula node))
	 (neg (alx~negation-scope formula)))
    (if (or (logic~universal-quantification-p neg)
	    (logic~existential-quantification-p neg)
	    (logic~disjunction-p neg)
	    (logic~conjunction-p neg)
	    (logic~implication-p neg)
	    (logic~equivalence-p neg))
	(pds~pushneg formula (pds~environment pds*current-proof-plan)))))



(defgeneric alx~sort-premises (prems hyp)
  (declare (edited  "10-NOV-1993 11:04")
	   (authors AFIEDLER)
	   (input   "A list of the necessary preconditions to apply a rule tree, i.e."
		    "labels of premise lines containing the label of that hypothesis"
		    "line, from which that rule tree is created, and that label.")
	   (value   "If HYP is not a member of PREMS, PREMS, otherwise a list of the"
		    "necessary preconditions to apply a rule tree, where HYP is the"
		    "first element of the list."
		    "If HYP is an axiom, it appears twice in that list."))
  (:method (prems (hyp symbol))
	   (alx~sort-premises prems (pds~label2node hyp)))
  (:method (prems (hyp pdsn+node))
	   (cons hyp (remove hyp prems))
	   #+old(if (and (member hyp prems) (not (eq hyp (car prems))))
	       (cons hyp (remove hyp prems))
	     prems)))


(defun alx~line-not-to-match-p (line)
  (declare (edited  "23-MAR-1993 09:44")
	   (authors AFIEDLER)
	   (input   "A line of the current proof.")
	   (effect  "Decides whether LINE should be matched. Hypothesis lines and lines"
		    "with formula `false' need no matching, as they will be marked"
		    "anyway. Exists-e lines should remain in the abstracted proof, as"
		    "well as case analyses.")
	   (value   "T, iff LINE should not be matched."))
  (let ((false (env~lookup-object 'false (alb~rule-environment pds*current-proof-plan))))
    (or (pdsn~hypothesis-node-p line)
	(if false (data~equal-p (node~formula line) false))
	(string= (alx~rule-symbol line) :existse))))



(defgeneric alx=all-bound-variables (wff) 
  (declare (edited  "29-MAY-1996")
	   (authors Afiedler)
	   (input   "A formula.")
	   (value   "A list of all bound variables in WFF."))
  (:method ((wff data+abstr))
	   (append (data~abstr-domain wff)
		   (alx=all-bound-variables (data~abstr-range wff))))
  (:method ((wff term+term))
	   (apply #'append (mapcar #'alx=all-bound-variables (data~substructs wff)))))


(defun alx~match-p (meta-wff real-wff &optional (bindings nil) (meta-vars nil))
  (declare (edited  "16-APR-1998" "24-FEB-1993 19:23")
	   (authors Afiedler)
	   (input   "A formula possibly containing meta-variables and a formula which is"
		    "treated as constant and optional a binding and the meta-variables"
		    "of the first."
                    "BINDINGS is an a-list associating meta-variables to terms and"
		    "type-variables to types. It is assumed that it represents an"
		    "idempotent substitution.")
	   (value   "A substitution, if the formulae match (the substitution is empty, if"
		    "they are equal), NIL, if they don't match."))
  (if (term~alpha-equal meta-wff real-wff)
      (progn (when alx*tracer
	       (omega~output "@"))
	     (subst~create nil nil))
    (let* ((subst
             (term~alpha-match meta-wff real-wff :subst bindings))
;	    (uni~unify-two-terms meta-wff real-wff :subst bindings :bindables meta-vars))
	    ;MP an AF: mit bindables werden nur die Substs f"ur diese Vars ausgegeben
	   )
	    

      ;; :additional-bind (append (alx=all-bound-variables meta-wff) meta-vars))))
      ;; ADDITIONAL BIND GIBTS NICHT MEHR !! HALLO ARMIN !! GRUSS AMEIER !!

      (if subst
	  (progn (unless subst (setq subst (subst~create nil nil)))
		 (if (subst~empty-p subst)
		     (when alx*tracer
		       (omega~output "@"))
		   (when alx*tracer
		     (omega~output "#")))
		 subst)
	(progn (when alx*tracer
		 (omega~output "."))
	       nil)))))


(defun alx~create-short-name (proof)
  (declare (edited  "28-MAY-1993 11:13")
	   (authors AFIEDLER)
	   (input   "A proof")
	   (value   "The name of PROOF with suffix `-short'"))
  (make-symbol (concatenate 'string (symbol-name (keim~name proof)) "-short")))

(defgeneric alx~rule-symbol (obj)
  (declare (edited  "17-JAN-1994 12:16")
	   (authors AFIEDLER)
	   (input   "A proof line, a justification or a string.")
	   (value   "LINE's rule name as an upcase string."))
  (:method ((line pdsn+node))
	   (alx~rule-symbol (node~justification line)))
  (:method ((just just+justification))
	   (alx~rule-symbol (just~method just)))
  #+old(:method ((just just+justification))
	   (string-upcase (keim~name (just~method just))))
  (:method ((just infer+inference)) ;;; boeser Hack fuer TPS
	   (let ((just-name (string-upcase (keim~name just))))
	     (cond ((string= just-name "TPS*HYP") "HYP")
		   ((string= just-name "TPS*SAME-AS") "WEAKEN")
		   ((string= just-name "TPS*CASES") "ORE")
		   ((string= just-name "TPS*DEDUCT") "IMPI")
		   ((string= just-name "TPS*DISJ-IMP") "OR2IMP")
		   ((string= just-name "TPS*CONJ") "ANDE")
		   ((string= just-name "TPS*EQUIVIMP") "EQUIVE")
		   ((string= just-name "TPS*IDISJ-L") "ORIL")
		   ((string= just-name "TPS*IDISJ-R") "ORIR")
		   ((string= just-name "TPS*IMP-DISJ") "IMP2OR")
		   ((string= just-name "TPS*IMPEQUIV") "EQUIVI")
		   ((string= just-name "TPS*INDIRECT") "INDIRECT")
		   ((string= just-name "TPS*MP") "IMPE")
		   ((string= just-name "TPS*ABSURD") "FALSEE")
		   ((string= just-name "TPS*NEGELIM") "NOTE")
		   ((string= just-name "TPS*NEGINTRO") "NOTI")
		   ((string= just-name "TPS*AB") "LAMBDA")
		   ((string= just-name "TPS*EGEN") "EXISTSI")
		   ((string= just-name "TPS*RULEC") "EXISTSE")
		   ((string= just-name "TPS*CHOOSE") "HYP")
		   ((string= just-name "TPS*UGEN") "FORALLI")
		   ((string= just-name "TPS*UI") "FORALLE")
		   (t just-name))))
  #+original(:method ((just infer+inference))
	   (string-upcase (keim~name just)))
  (:method ((just string))
	   (string-upcase just))
  (:method ((just symbol))
	   (string-upcase just)))



(defgeneric alx~deletable-hypothesis-p (hyp)
  (declare (edited  "16-NOV-1993 09:37")
	   (authors AFIEDLER)
	   (input   "A hypothesis line or its label.")
	   (value   "T, if HYP can be deleted."))
  (:method ((hyp pdsn+node))
	   (not (alx=not-deletable-hypothesis-p (keim~name hyp))))
  (:method ((hyp symbol))
	   (not (alx=not-deletable-hypothesis-p hyp)))
  (:method ((hyp string))
	   (not (alx=not-deletable-hypothesis-p hyp))))

(defun alx=not-deletable-hypothesis-p (l)
  (declare (edited  "25-OCT-1993 09:42")
	   (authors AFIEDLER)
	   (input   "A label of a proof line.")
	   (value   "T, iff the label begins with `L?', where ? is a number."))
  (or (string-equal "L1" l :end2 2)
      (string-equal "L2" l :end2 2)
      (string-equal "L3" l :end2 2)
      (string-equal "L4" l :end2 2)
      (string-equal "L5" l :end2 2)
      (string-equal "L6" l :end2 2)
      (string-equal "L7" l :end2 2)
      (string-equal "L8" l :end2 2)
      (string-equal "L9" l :end2 2)
      (string-equal "L0" l :end2 2)))


(defun alx=non-hyp-assertions ()
  (declare (edited  "23-OCT-1997")
	   (authors Afiedler)
	   (input   "None.")
	   (value   "A list of all non hyp assertions."))
  alx*defn-e-e-assertions)

(defun alx~add-non-hyp-assertion! (line)
  (declare (edited  "23-OCT-1997")
	   (authors Afiedler)
	   (input   "A proof line.")
	   (effect  "Adds LINE to the list of non-hyp assertions.")
	   (value   "The altered list of non-hyp assertions."))
  (setq alx*defn-e-e-assertions (cons line alx*defn-e-e-assertions)))

(defun alx=remove-non-hyp-assertion! (line)
  (declare (edited  "23-OCT-1997")
	   (authors Afiedler)
	   (input   "A proof line.")
	   (effect  "Removes LINE from the list of non-hyp assertions.")
	   (value   "The altered list of non-hyp assertions."))
  (setq alx*defn-e-e-assertions (remove line alx*defn-e-e-assertions)))

(defun alx~reset-non-hyp-assertions ()
  (declare (edited  "23-OCT-1997")
	   (authors Afiedler)
	   (input   "None.")
	   (effect  "Empties the list of non-hyp assertions.")
	   (value   "Undefined."))
  (setq alx*defn-e-e-assertions nil))

(defun alx~non-hyp-assertions (&optional line)
  (declare (edited  "23-OCT-1997")
	   (authors Afiedler)
	   (input   "An optional proof line.")
	   (value   "If LINE is given, a list of its possible non-hyp assertions,"
		    "otherwise a list of all possible non-hyp assertions."))
  (let ((all (alx=non-hyp-assertions))
	(hyps (pdsn~hyps line)))
    (if line
	(remove-if-not #'(lambda (el) (subsetp (pdsn~hyps el) hyps)) all)
      all))
  #+nein(alx=remove-non-hyp-assertion! line))



(defun alx~defn-e-line-p (line)
  (declare (edited  "23-OCT-1997")
	   (authors Afiedler)
	   (input   "A proof line.")
	   (value   "T iff LINE is a defn-e line, otherwise NIL."))
  (string= (alx~rule-symbol line) :DefnE))





;################################################
;##                                            ##
;##              Debugging Tools               ##
;##                                            ##
;################################################

(defun alx~debug (y)
  (declare (edited  "27-SEP-1993 09:36")
	   (authors AFIEDLER)
	   (input   "T or NIL.")
	   (effect  "Forces debugging output, if T.")
	   (value   "Undefined."))
  (setq alx*verbose y)
  (setq alx*tracer y)
  (setq alx*show-rule-tree y)
  (setq alx*show-premise-tree y))

(defun alx~debug-lines (&rest lines)
  (declare (edited  "06-JAN-1994 16:28")
	   (authors AFIEDLER)
	   (input   "Arbitrary many lines or their labels.")
	   (effect  "Lets LINES be debugged, i. e. the according trees are shown.")
	   (value   "A list of the labels of the lines to be debugged."))
  (alx~debug (if lines t))
  (setq alx*debug-lines (mapcar #'(lambda (line)
				     (if (pdsn~p line)
					 line
				       (pds~label2node line)))
				 lines)))

(defun alx~fall (x)
  (when alx*tracer
    (omega~output "~%~%Fall: ~A~%" x)))

(defgeneric alx~set-premise-tree-debug! (line)
  (declare (edited  "06-JAN-1994 16:24")
	   (authors AFIEDLER)
	   (input   "A proof line.")
	   (effect  "Lets LINE be debugged, i. e. the according trees are shown.")
	   (value   "Undefined."))
  (:method ((line pdsn+node))
	   (setq alx*show-premise-tree (member line alx*debug-lines)))
  (:method ((line symbol))
	   (setq alx*show-premise-tree (member (pds~label2node line) alx*debug-lines))))

(defgeneric alx~break (line)
  (declare (edited  "10-JAN-1994 17:14")
	   (authors AFIEDLER)
	   (input   "A proof line or its label.")
	   (effect  "A break, if LINE is marked accordingly.")
	   (value   "Undefined."))
  (:method ((line pdsn+node))
	   (if (member line alx*break-lines) (break)))
  (:method ((line symbol))
	   (if (member (pds~label2node line) alx*break-lines) (break))))

(defun alx=set-break-lines! (&rest lines)
  (declare (edited  "10-JAN-1994 17:15")
	   (authors AFIEDLER)
	   (input   "A proof lines or their labels.")
	   (effect  "The lines are marked as breaking lines.")
	   (value   "The list of the labels of the breaking lines."))
  (setq alx*break-lines (mapcar #'(lambda (line)
				     (if (pdsn~p line)
					 line
				       (pds~label2node line)))
				 lines)))

(defun alx~break-after-debug-lines (&rest lines)
  (declare (edited  "11-JAN-1994 09:00")
	   (authors AFIEDLER)
	   (input   "Proof lines or their labels.")
	   (effect  "The lines are marked to be debugged and to break.")
	   (value   "A list of the labels of the marked lines."))
  (apply #'alx=set-break-lines! lines)
  (apply #'alx~debug-lines lines))

