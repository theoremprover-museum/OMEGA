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



(mod~defmod ALIFT 
            :uses (al alb alj altr alx infer just keim misc mod node pds pdsj pdsn pp
		      prob sys term) 
            :documentation "Lifting to the assertion level."
            :exports (alift+apply-warning
                      alift+apply-warning-assertion
                      
                      alift~apply!
                      alift~apply-assertion-tactic
                      alift~apply-command!
                      alift~clear-proof-hash-tables
                      alift~initialize
                      alift~proof
                      alift~proof!
                      alift~subproof!
                      
                      alift*show-proof))





;################################################
;##                                            ##
;##               Conditions                   ##
;##                                            ##
;################################################

(sys~define-condition
 alift+apply-warning (sys+warning)
 ((assertion))
 (lambda (condition stream)
   (format stream "Can't apply assertion ~A."
	   (alift+apply-warning-assertion condition))))


;#########################################
;##                                     ##
;##   Anwendung auf fertigen Beweis     ##
;##                                     ##
;#########################################

;;; Bookkeeping: welche Anwendungen sind moeglich

(defvar alift*possibilities-hash-table (misc~make-hash-table 'alift*possibilities-hash-table)
  "A hash table that holds the preconditions that lead to the conclusion by applying the
   hypoyhesis's rule tree.
   The keys are references to possibilities of applying a rule (i. e. strings with the
   prefix \"POSS-\"), and the data are lists of labels of proof lines, the
preconditions.")

(defun alift=add-possibility! (preconditions)
  (declare (edited  "14-JAN-1993 11:34")
	   (authors AFIEDLER)
	   (input   "A list of the necessary preconditions to apply a rule tree, i. e."
		    "labels of premise lines.")
	   (effect  #{Stores PRECONDITIONS in {\vb alift*possibilities-hash-table} with a#}
		    #{new name with prefix 'POSS-' as key.#})
	   (value   "The key, i. e. a reference to a possibility of applying a rule tree."))
  (let ((name (gensym "POSS-")))
    (setf (misc~gethash name alift*possibilities-hash-table) preconditions)
    name))

(defun alift=delete-possibilities! ()
  (declare (edited  "18-FEB-1993 13:24")
	   (authors AFIEDLER)
	   (input   "NONE")
	   (effect  #{Deletes all possibilities of applying rules by clearing#}
		    #{{\vb alift*possibilities-hash-table}.#})
	   (value   "Undefined."))
  (misc~clrhash alift*possibilities-hash-table))

(defun alift=all-possibilities-premise-lines ()
  (declare (edited  "12-JAN-1994 10:16")
	   (authors AFIEDLER)
	   (input   "None.")
	   (value   "A list of all possible premises."))
  (let ((poss-list nil))
    (misc~maphash #'(lambda (key value)
		      (declare (ignore key))
		      (setq poss-list (append poss-list
					      (list value))))
		  alift*possibilities-hash-table)
    poss-list))

(defun alift=best-possibility (poss-list)
  (declare (edited  "12-JAN-1994 10:03")
	   (authors AFIEDLER)
	   (input   "A list of lists proof lines.")
	   (value   "That list of proof lines, that have the best average values."))
  (let* ((average-list (mapcar #'(lambda (poss)
				   (/ (apply #'+ (mapcar #'alb~line-value poss))
				      (length poss)))
			       poss-list))
	 (minimum (apply #'min average-list))
	 (pos (position minimum average-list)))
    (nth pos poss-list)))

(defun alift=show-possibilities (poss-list)
  (declare (edited  "18-MAR-1993 09:50")
	   (authors AFIEDLER)
	   (input   "A list of proof lines.")
	   (effect  "Shows the proof lines.")
	   (value   "Undefined."))
  (dolist (poss poss-list)
    (omega~output "~1&   ~A" poss))
  (terpri))



;;; Bookkeeping: welche proof lines gehoeren zu assertion level

(defvar alift*marked-proof-lines-hash-table
  (misc~make-hash-table 'alift*marked-proof-lines-hash-table) 
  "A hash table that holds marked proof lines.
   The keys are the labels of marked proof lines, and the data are the according lines.")

(defun alift=marked-line-p (line)
  (declare (edited  "16-DEC-1992 14:12")
	   (authors AFIEDLER)
	   (input   "A proof line.")
	   (value   "The line, if it is marked, otherwise NIL. Marked lines are those"
		    "that won't be deleted while shortening the proof."))
  (let ((label (keim~name line)))
    (misc~gethash label alift*marked-proof-lines-hash-table)))

(defun alift=mark-line! (line)
  (declare (edited  "16-DEC-1992 14:18")
	   (authors AFIEDLER)
	   (input   "A proof line")
	   (effect  #{Marks LINE by storing it in#}
		    #{{\vb alift*marked-proof-lines-hash-table}#} with the label of LINE#}
		    #{as the key. Only those lines should be marked that shall not be#}
		    #{deleted while shortening the proof.#})
	   (value   "LINE."))
  (let ((label (keim~name line)))
    (omega~output "~%Mark ~S" label)
    (omega~message ".")
    (setf (misc~gethash label alift*marked-proof-lines-hash-table) line)))

(defun alift=all-marked-lines (proof)
  (declare (edited  "02-MAR-1993 15:24")
	   (authors AFIEDLER)
	   (input   "A natural deduction proof.")
	   (value   "A list of all marked lines in PROOF, i. e. those lines that shall"
		    "not be deleted while shortening the proof."))
  (apply #'append
	 (mapcar #'(lambda (line) (if (alift=marked-line-p line) (list line)))
		 (prob~proof-steps proof))))

(defun alift=unmark-all-lines! ()
  (declare (edited  "17-FEB-1993 15:57")
	   (authors AFIEDLER)
	   (input   "None.")
	   (effect  #{Unmarks all marked lines by clearing#}
		    #{{\vb alift*marked-proof-lines-hash-table}.#})
	   (value   "Undefined."))
  (misc~clrhash alift*marked-proof-lines-hash-table))


(defun alift~clear-proof-hash-tables ()
  (declare (edited  "01-MAR-1993 16:08")
	   (authors AFIEDLER)
	   (input   "None.")
	   (effect  #{Clears the hash tables of the pres module, that are filled during
		    the runtime of {\vb alift~proof}.#})
	   (value   "Undefined."))
  (misc~clear-hash-tables (list alift*marked-proof-lines-hash-table
				alift*possibilities-hash-table)))



;;; Anwendung einer Assertion

(defun alift=trees-to-try (line)
  (declare (edited  "23-OCT-1997")
	   (authors Afiedler)
	   (input   "A proof line.")
	   (value   "A list of all lines, which could be applicable assertions to LINE."))
  (append (pdsn~hyps line) (alx~non-hyp-assertions line))
  )
  

(defun alift=try-assertions-to-line (line ass-s)
  (declare (edited  "23-OCT-1997")
	   (authors Afiedler)
	   (input   "A proof line.")
	   (effect  #{Tries to apply the assertions ASS-S to LINE. The different#}
		    #{possibilities are stored in the {\vb alift*possibilities-hash-table}.#})
	   (value   "Undefined."))
  (dolist (ass ass-s)
    (alift=add-possible-hyp (keim~name ass) line)))

(defun alift=actualize! (line poss)
  (declare (edited  "30-JUN-1997" "12-JAN-1994 15:21")
           (authors Afiedler AFIEDLER)
           (input   "A proof line and a list of proof lines, that are premises of LINE,"
		    "where the first premise is a rule tree.")
           (effect  "Actualizes the justification of LINE, namely sets it to the rule"
		    "tree.")
           (value   "Undefined."))
  (let ((just (alj~find line)))
    (alj~add-or-replace line (alj~create poss
					 (if just
					     (pdsj~status just)
					   "expanded"))))
  #+old(alj~add! line
	    (pdsj~closed-just-create (infer~find-method 'assertion) poss nil "expanded"))
  )

(defun alift=add-possible-hyp-help (hyp line root)
  (declare (edited  "10-JAN-1994 09:25")
	   (authors AFIEDLER)
	   (input   "A label of a hypothesis line, a proof line and a rule node.")
	   (effect  "Every rule node above ROOT is tried to match against LINE, until"
		    "the first matches. Then a premise tree is built up and tried to match"
		    "against the subproof with root LINE. If the formula in the matching"
		    "rule node is a meta-variable or the bindings to its meta-variables"
		    "are compound, then the search is repeated in the rule nodes above"
		    "it. (`Above' means `precondition nodes'.) The different possibilities"
		    #{are stored in the {\vb alift*possibilities-hash-table}.#}
		    "Possible applications of the rule tree that do not need any further"
		    "premises are discarded.")
	   (value   "Undefined."))
  (dolist (con-res (al~match-conclusion (node~formula line) root))
    (let ((prems (al~get-best-premises (altr~create-premise-tree (car con-res)
								 :line line
								 :pos (cadr con-res))
				       line (caddr con-res))))
      (when prems
	(if (= 1 (length prems))
	    (when alx*tracer
	      (omega~output ";;; Discarding this one: too small...~%"))
	  (alift=add-possibility! (alx~sort-premises prems hyp)))))))





(defun alift=add-possible-hyp (hyp line)
  (declare (edited  "10-JAN-1994 12:26")
	   (authors AFIEDLER)
	   (input   "A label of a hypothesis line and a proof line.")
	   (effect  #{Tries to apply the rule tree with label HYP to LINE. The different#}
		    #{possibilities are stored in the {\vb alift*possibilities-hash-table}.#})
	   (value   "Undefined."))
  (let ((rule-tree (sys~handler-case
		    (altr~get-rule-tree hyp)
		    (altr+tree-error () nil))))
    (when rule-tree
      (when alx*verbose
	(omega~output "~1&Rule tree ~S" hyp))
      (dolist (root (altr~find-roots rule-tree))
	(alift=add-possible-hyp-help hyp line root)))))

(defun alift~subproof! (line)
  (declare (edited  "12-JAN-1994 17:45")
	   (authors AFIEDLER)
	   (input   "A proof line.")
	   (effect  "Applies appropriate rule to LINE. Recursive for the premise lines.")
	   (value   "Undefined."))
  (alift=delete-possibilities!)
  (when line
    (alx~set-premise-tree-debug! line)
    (unless (alift=marked-line-p line)
      (if (alx~line-not-to-match-p line)
	  (mapcar #'alift~subproof!
		  (pdsn~just-premises (alift=mark-line! line)))
	(let ((trees (alift=trees-to-try line))
	      #+old(hyps (pdsn~hyps line)))
	  (when alx*verbose
	    (omega~output "~%~%Testing ~S~%" line))
	  (if trees ;hyps 
	      (progn
              ;;; versuche Regelbaum anzuwenden
		(alift=try-assertions-to-line line trees)
		#+old(dolist (hyp hyps)
		  (alift=add-possible-hyp (keim~name hyp) line))
		(let ((poss-list (alift=all-possibilities-premise-lines)))
		  (when alx*verbose
		    (omega~output "~% ~A possible rule application~:P~A"
				  (length poss-list)
				  (if (= (length poss-list) 0) " " ":"))
		    (alift=show-possibilities poss-list))
		  (alift=mark-line! line)
		  (alx~break line)
		  (if poss-list
		      (let ((best (alift=best-possibility poss-list)))
			(omega~output ", choosing ~A" best)
              ;;; wenn anwendbar, weiter mit Vorbedingungen
			(mapcar #'alift~subproof! best)
			(alift=actualize! line best))
              ;;; sonst zu den Vorgaengern
		    (mapcar #'alift~subproof! (pdsn~just-premises line)))))
            ;;; wenn keine Hypothesen, dann zum Vorgaenger
	    (mapcar #'alift~subproof!
		    (pdsn~just-premises (alift=mark-line! line)))))))))


(defun alift=proof! (proof)
  (declare (edited  "16-DEC-1992 14:06")
	   (authors AFIEDLER)
	   (input   "A natural deduction proof.")
	   (effect  "Shortens PROOF by applying rules.")
	   (value   "PROOF."))
  (alift~subproof! (prob~proof-root proof))
  proof)




;;; Prae- und Postprocessing

(defun alift=preprocess-help (line)
  (declare (edited  "02-DEC-1996"  "25-OCT-1993 10:08")
	   (authors Afiedler AFIEDLER)
	   (input   "A line.")
	   (effect  "Skips the SAME-lines and creates the rule trees of the subproof"
		    "with LINE as a root.")
	   (value   "LINE."))
  (if (pdsn~hypothesis-node-p line)
      (sys~handler-case
       (altr~get-rule-tree (keim~name line))
       (altr+tree-error () (altr~create-rule-tree line)))
    (progn (setf (just~premises (node~justification line))
		 (mapcar #'(lambda (l)
			     (alift=preprocess-help (alx~skip-same-line l)))
			 (pdsn~just-premises line)))
	   (when (alx~defn-e-line-p line)
	     (sys~handler-case
	      (altr~get-rule-tree (keim~name line))
	      (altr+tree-error ()
			       (altr~create-rule-tree line)
			       (alx~add-non-hyp-assertion! line))))
	   ))
  line)



(defun alift=preprocess (proof)
  (declare (edited  "02-DEC-1996" "25-OCT-1993 09:58")
	   (authors Afiedler AFIEDLER)
	   (input   "A natural deduction proof.")
	   (effect  "Valuates the lines, skips the same-lines and creates the rule trees"
		    "of PROOF.")
	   (value   "PROOF."))
  (let ((conc (prob~proof-root proof)))
    (when (alx~same-line-p conc)
      (setf (just~premises (node~justification conc))
	    (list (alx~skip-same-line conc))))
    (alift=preprocess-help conc))
  (dolist (line (prob~proof-steps proof))
    (if (pdsn~hypothesis-node-p line)
	(sys~handler-case
	 (altr~get-rule-tree (keim~name line))
	 (altr+tree-error ()
			  (setf (alb~line-value line) 0)
			  (altr~create-rule-tree line)))))
  (altr~delete-useless-rule-trees)
  proof)

(defun alift=postprocess! (proof)
  (declare (edited  "02-DEC-1996" "26-OCT-1993 09:27")
	   (authors Afiedler AFIEDLER)
	   (input   "A proof.")
	   (effect  "The marked lines are shown and lines to delete are deleted.")
	   (value   "The short proof."))
  (omega~output "~%~%Marked lines: ~%~A~%~%The abstracted proof has ~A lines.~%~%"
		(alift=all-marked-lines proof)
		(length (alift=all-marked-lines proof))))

(defun alift~initialize (&optional (proof pds*current-proof-plan))
  (declare (edited  "27-JAN-1994 11:49")
	   (authors AFIEDLER)
	   (input   "NONE.")
	   (effect  "Clears the hash tables of the pres module, that are filled during"
		    #{the runtime of {\vb alift~proof}.#})
	   (value   "Undefined."))
  (unless proof
    (setq pds*current-proof-plan proof))
  (alb~make-proof-hash-tables proof)
  (alift~clear-proof-hash-tables)
  (alx~reset-non-hyp-assertions))



;;; Hauptfunktion

(defvar alift*show-proof nil
  "If this variable is T, the proof will be shown in the beginning and the end of a call to
   {\\vb alift~proof}, else it won't.")

(defun alift=proof (proof copy)
  (declare (edited  "27-JAN-1994 14:03")
	   (authors AFIEDLER)
	   (input   "A natural deduction proof and a boolean")
	   (effect  #{The proof is at first copied, if COPY is T, and its lines are#}
		    #{valued and it becomes the current proof. Then all possible#}
		    #{rule trees are created and shown, if#}
		    #{{\vb alx*show-rule-tree} is T. After that, the rule trees are#}
		    #{applied to the proof. Created premise trees are shown, if#}
		    #{{\vb alx*show-premise-tree} is T. Finally the shorter proof is#}
		    #{shown.#})
	   (value   "The shorter proof."))
  (if copy
      (progn (omega~message "~&Copying proof ...")
	     (setq pds*current-proof-plan
		   (alx~copy-proof proof (alx~create-short-name proof)))
	     (omega~message " done"))
    (setq pds*current-proof-plan proof))
  (let ((pds*avoid-node-breaks t))
    (let ((nodes (pds~linearize-plan (prob~proof-root pds*current-proof-plan))))
      (omega~output "~%~%The original proof has ~A lines.~%~%" (length nodes))
      (when alift*show-proof
	(omega~output "~%The proof:~%==========~%~%")
	(mapc #'(lambda (node) (pp~pprint node 'pds-pretty)) nodes)))
    (alift~initialize)
    (alift=preprocess pds*current-proof-plan)
    (omega~output "~%")
    (if (= (alb~rule-tree-count) 0)
	(omega~warn "~1&Cannot abstract proof: no applicable rule trees.~%")
      (progn
	(omega~message "~1&Lifting to assertion level")
	(alift=proof! pds*current-proof-plan)
	(alift=postprocess! pds*current-proof-plan)
	(let ((nodes (pds~linearize-plan (prob~proof-root pds*current-proof-plan))))
	  #+nochnicht(format t "~%~%The abstracted proof has ~A lines.~%~%" (length nodes))
	  (when alift*show-proof
	    (omega~output "~%The abstracted proof:~%=====================~%~%")
	    (mapc #'(lambda (node) (pp~pprint node 'pds-simple)) nodes)))))
    pds*current-proof-plan))

(defun alift~proof (&optional (proof pds*current-proof-plan))
  (declare (edited  "25-NOV-1993 14:17")
	   (authors AFIEDLER)
	   (input   "Optional a natural deduction proof.")
	   (effect  "The proof is at first copied and its lines are valued. Then all"
		    "possible rule trees are created and shown, if"
		    #{{\vb alx*show-rule-tree} is T. After that, the rule trees are#}
		    "applied to the proof. Created premise trees are shown, if"
		    #{{\vb alx*show-premise-tree} is T. Finally, #}
		    #{the shorter proof, which is now the current proof is shown.#})
	   (value   "The shorter proof."))
  (alift=proof proof t))

(defun alift~proof! (&optional (proof pds*current-proof-plan))
  (declare (edited  "25-NOV-1993 14:20")
	   (authors AFIEDLER)
	   (input   "Optional a natural deduction proof.")
	   (effect  "At first, the proof becomes the current proof and its lines are"
		    "valued. Then all possible rule trees are created and shown, "
		    #{if {\vb alx*show-rule-tree} is T.#}
		    "After that, the rule trees are applied to the proof. Created premise"
		    #{trees are shown, if {\vb alx*show-premise-tree} is T. Finally the#}
		    "shorter proof is shown."
                    #{\\ {\em Note:\/} the proof is not copied, thus the original proof#}
		    #{is lost!#})
	   (value   "The shorter proof."))
  (alift=proof proof nil))





;#########################################
;##                                     ##
;##      Anwendung bei Beweissuche      ##
;##                                     ##
;#########################################

(defun alift=help-apply-assertion (premises assertion conclusion rule-node)
  (declare (edited  "25-FEB-1994 09:28")
	   (authors AFIEDLER)
	   (input   "A list of lines, two lines, and a rule-node.")
	   (effect  "Tries to apply ASSERTION to PREMISES justifying CONCLUSION, where"
		    "CONCLUSION matches against RULE-NODE or one of its ancestors.")
	   (value   "If the application of ASSERTION to PREMISES justifies CONCLUSION, a"
		    "list of premises, where ASSERTION is the first one, otherwise NIL."))
  (let ((res nil))
    (dolist (con-res (al~match-conclusion (node~formula conclusion) rule-node) res)
      (let ((true-premises (append (list assertion) premises))
	    (premise-tree (altr~create-premise-tree (car con-res) :pos (cadr con-res))))
	(when alx*tracer
	  (omega~output "~1& Matching premises "))
	(al~match-premises true-premises premise-tree conclusion (caddr con-res)) 
	(altr~show-cond premise-tree)
	(let ((premises-list (mapcar #'altr~premise-line
				     (al~find-best-premises premise-tree))))
	  (if (apply #'append (mapcar #'(lambda (line) (member line true-premises))
				      premises-list))
	      (let ((diff (set-difference true-premises premises-list)))
		(when alx*tracer
		  (omega~output " succeeded~%"))
		(if diff
		    (warn "The premise~P ~A ~@*~[are~;is~:;are~] not necessary."
			  (length diff) diff))
		(return (setq res true-premises)))
	    (when alx*tracer
	      (omega~output " failed~%"))))))))

(defun alift~apply! (premises assertion conclusion)
  (declare (edited  "23-AUG-1993 13:49")
	   (authors AFIEDLER)
	   (input   "A list of lines, a line and a formula.")
	   (effect  "Tries to apply ASSERTION to PREMISES justifying CONCLUSION.")
	   (value   "If the application of ASSERTION to PREMISES justifies CONCLUSION,"
		    "a list of premises, from which ASSERTION is the first one,"
		    "otherwise NIL."))
  (let ((rule-tree (sys~handler-case
		    (altr~get-rule-tree (keim~name assertion))
		    (altr+tree-error () (altr~create-rule-tree assertion))))
	(premises-list nil))
    (altr~show-cond rule-tree)
    (dolist (root (altr~find-roots rule-tree)
		  (if premises-list premises-list
		    (progn (warn (sys~make-condition 'alift+apply-warning
						     :assertion assertion))
			   nil)))
      (let ((premises-list (alift=help-apply-assertion premises assertion conclusion root)))
	(if premises-list 
	    (return premises-list)
	  premises-list)))))

(defgeneric alift~apply-assertion-tactic (premises assertion conclusion)
  (declare (edited  "01-JUL-1997")
	   (authors Afiedler)
	   (input   "A list of lines, a line or a formula and a line or a formula.")
	   (effect  "Tries to apply ASSERTION to PREMISES justifying CONCLUSION."
                    "If CONCLUSION is a line, its justification is corrected, else a new"
		    "line is created. If ASSERTION is a formula, a new line is created.")
	   (value   "If the application of ASSERTION to PREMISES justifies"
		    "CONCLUSION, the conclusion line, else NIL."))
  (:method ((premises list) (assertion pdsn+node) (conclusion pdsn+node))
	   (let ((result (alift~apply! premises assertion conclusion)))
	     (when result
	       ; justification herstellen
	       (alj~add-or-replace conclusion
				   (alj~create result "unexpanded" nil nil nil
					       (node~justification conclusion))))
	     result))
    (:method ((premises list) (assertion pdsn+node) (conclusion term+term))
	   (let ((conclusion-line
		  (pdsn~open-node-create conclusion
					 (misc~union (mapcar #'pdsn~hyps premises))
					 (pds~new-node-name))))
	     (pds~only-insert-node-between! conclusion-line (cons assertion premises)
					    nil)
	     (if (alift~apply-assertion-tactic premises assertion conclusion-line)
		 conclusion-line
	       (pds~delete-node! conclusion-line)))))

(defgeneric alift~apply-command! (premises assertion conclusion)
  (declare (edited  "09-JUL-1997" "23-AUG-1993 13:49")
	   (authors Afiedler AFIEDLER)
	   (input   "A list of lines, a line or a formula and a line or a formula.")
	   (effect  "Tries to apply ASSERTION to PREMISES justifying CONCLUSION."
                    "If CONCLUSION is a line, its justification is corrected, else a new"
		    "line is created. If ASSERTION is a formula, a new line is created.")
	   (value   "If the application of ASSERTION to PREMISES justifies"
		    "CONCLUSION, the conclusion line, else NIL."))
  (:method :before (premises assertion conclusion)
	   (declare (ignore premises assertion conclusion))
	   (sys~handler-case
	    (alb~proof-hash-tables pds*current-proof-plan)
	    (alb+hash-error () (alb~make-proof-hash-tables pds*current-proof-plan))))
  (:method :around ((premises list) (assertion pdsn+node) (conclusion pdsn+node))
	   (when (call-next-method)
	     (altr~update-premise-trees! conclusion)
	     conclusion))
  (:method ((premises list) (assertion pdsn+node) (conclusion pdsn+node))
	   (nth-value
	    1
	    (infer~compute-outline
	     'assertion
	     (list* conclusion assertion
		    (mapcar #'(lambda (pre)
				(etypecase pre
				  (pdsn+node pre)
				  (term+term (let ((new (pdsn~open-node-create
							 pre
							 (pdsn~hyps conclusion)
							 (pds~new-node-name))))
					       (pds~only-insert-node-between!
						new
						nil
						(list conclusion))
					       (pds~show-node new)
					       new))))
			    premises))
	     nil)))
  (:method ((premises list) (assertion pdsn+node) (conclusion term+term))
	   (let ((conclusion-line
		  (pdsn~open-node-create conclusion
					 (misc~union (mapcar #'pdsn~hyps premises))
					 (pds~new-node-name))))
	     (pds~only-insert-node-between! conclusion-line (cons assertion premises)
					    nil)
	     (if (alift~apply-command! premises assertion conclusion-line)
		 conclusion-line
	       (pds~delete-node! conclusion-line))))
  (:method ((premises list) (assertion term+term) conclusion)
	   (format *standard-output* "~%~%What's the name of the assertion? ") 
	   (let ((assertion-line (pdsn~make-hypothesis assertion (read))))
	     (terpri)
	     (pds~only-insert-node! assertion-line)
	     (pds~show-node assertion-line)
	     (alift~apply-command! premises assertion-line conclusion))))
