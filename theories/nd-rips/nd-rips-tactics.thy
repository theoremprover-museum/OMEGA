;;; -*- syntax: common-lisp; package: OMEGA; base: 10; mode: Keim -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     OMEGA Project                                                        ;;
;;     AG Siekmann/FB Informatik                                            ;;
;;     Universitaet des Saarlandes                                          ;;
;;     Bau 36, 4. Stock                                                     ;;
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

					
; ********  nd-rips-tactics.thy ********
;
; So far, this file contains tactics for propositional logic from Rips:
; "The Psychology of Proof", 1994
; * generic routines
; * pure forward routines
; * forward/backward routines
; * pure backward routines
; * additional routines for completeness
;
; This is a ND-based proof calculus that is sound and complete for propositional logics.
;
; Marvin Schiller, 9.3.2005

; * Quantifier-Free First Order Logic *
; 
; The file contains a tactic for matching (as described
; in "The Psychology of Proof", but so far only rules
; 1 and 3
;
; Furthermore, there is a tactic for
; Rips's backward IF elimination, which
; relies on matching (so watch out!)
;
; Marvin Schiller, 20.9.2005



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GENERIC ROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nd-rips=expand-with-ATP (outline parameters)
  (declare (ignore parameters))
  (let ((node (car outline))
	(premises (just~premises (node~justification (car outline)))))
    (tacl~init outline)
    (nd-rips=call-external-atp node premises)
    (tacl~end)
    (setf (pdsj~status (node~justification node)) "untested")
    (view~update-step view*view node)))


(defun nd-rips=call-external-atp (node premises)
  (setf (just~method (node~justification node)) (infer~find-method 'pl-atp))
  (setf (just~premises (node~justification node)) premises)
  (setf (pdsj~parameters (node~justification node)) (list t))
  (setf (pds~node-supports node) premises))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PURE FORWARD ROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tactic RIPS-CONJ-MODUS-PONENS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; works & tested

;;;;;;;;;;;;;;;;;;;
;                 ;
; (P ^ Q ) => R   ; = L2
;  P              ; = L3
;  Q              ; = L4
; ----------------;
;  R              ; = L1
;;;;;;;;;;;;;;;;;;;

(infer~deftactic RIPS-CONJ-MODUS-PONENS
		 (outline-mappings (((existent existent existent existent) RIPS-CONJ-MODUS-PONENS-a)
				    ((nonexistent existent existent existent) RIPS-CONJ-MODUS-PONENS-f)))
		 (expansion-function nd-rips=expand-with-ATP)
		 (help "The RIPS tactic for conjunctive modus ponens."))

(tac~deftactic RIPS-CONJ-MODUS-PONENS-a RIPS-CONJ-MODUS-PONENS (in base)
   (premises L2 L3 L4)
   (conclusions L1)
   (computations )
   (sideconditions (nd-rips=RIPS-CONJ-MODUS-PONENS-sidecond-a L1 L2 L3 L4))
   (description "Application of Rips tactic CONJ-MODUS-PONENS."))


(defun nd-rips=RIPS-CONJ-MODUS-PONENS-sidecond-a (L1 L2 L3 L4)
  (let* ((F1 (node~formula L1))
	 (F2 (node~formula L2))
	 (F3 (node~formula L3))
	 (F4 (node~formula L4)))
    (and (logic~implication-p F2)
	 (let ((F21 (first (data~appl-arguments F2)))
	       (F22 (second (data~appl-arguments F2))))               ;;; (P ^ Q ) => R ----->  F21: P^Q  ; F22: R
	   (and (logic~conjunction-p F21)
		(let ((F211 (first (data~appl-arguments F21)))
		      (F212 (second (data~appl-arguments F21))))       ;;; F21: P^Q  -----> F211: P ; F212: Q
		  (and (term~alpha-equal F22 F1)                       ;;; matching R
		       (COND ((term~alpha-equal F211 F3) (term~alpha-equal F212 F4))  ;;; matching P/Q
			     ((term~alpha-equal F212 F3) (term~alpha-equal F211 F4))  ;;; matching P/Q
			     ((T NIL))))))))))
	

(tac~deftactic RIPS-CONJ-MODUS-PONENS-f RIPS-CONJ-MODUS-PONENS (in base)
	       (premises L2 L3 L4)
	       (conclusions L1)
	       (computations (L1 (nd-rips=RIPS-CONJ-MODUS-PONENS-compute-f L2)))
	       (sideconditions (nd-rips=RIPS-CONJ-MODUS-PONENS-sidecond-f L2 L3 L4))
	       (description "Forward Application of Rips tactic CONJ-MODUS-PONENS."))

(defun nd-rips=RIPS-CONJ-MODUS-PONENS-compute-f (L2)
  (when (logic~implication-p (node~formula L2))
    (second (data~appl-arguments (node~formula L2)))))

(defun nd-rips=RIPS-CONJ-MODUS-PONENS-sidecond-f (L2 L3 L4)
  (let* ((F2 (node~formula L2))
	 (F3 (node~formula L3))
	 (F4 (node~formula L4)))
    (and (logic~implication-p F2)
	 (let ((F21 (first (data~appl-arguments F2)))
	       (F22 (second (data~appl-arguments F2))))                ;;; (P ^ Q ) => R ----->  F21: P^Q  ; F22: R
	   (and (logic~conjunction-p F21)
		(let ((F211 (first (data~appl-arguments F21)))
		      (F212 (second (data~appl-arguments F21))))        ;;; F21: P^Q  -----> F211: P ; F212: Q
		  (COND ((term~alpha-equal F211 F3)(term~alpha-equal F212 F4))         ;;; matching P/Q
			((term~alpha-equal F212 F3) (term~alpha-equal F211 F4))        ;;; matching P/Q
			((T NIL))))
		(not (formula-holds-in-domain  F22
					       (nd-rips=domain L2)))
		)))))

 
(com~defcommand RIPS-CONJ-MODUS-PONENS
		;; this is the definition of the command to execute the tactic in command interpreter
  (argnames implication antecedent1 antecedent2 succedent)
  (argtypes ndline ndline ndline ndline)
  (arghelps "Line with implication"
	    "Line with one of the conjuncts of antecedent of implication"
	    "Line with the other conjunct of antecedent of implication"
	    "Line with conclusion")
  (function nd-rips=RIPS-CONJ-MODUS-PONENS)
      ;;; here we say which function will work on the input
  (frag-cats tactics base elimination)
  (log-p T)
  (help "The RIPS tactic for conjunctive modus ponens - only used in forward direction."))


(defun nd-rips=RIPS-CONJ-MODUS-PONENS (implication antecedent1 antecedent2 succedent)
  (infer~compute-outline 'RIPS-CONJ-MODUS-PONENS (list succedent implication antecedent1 antecedent2) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tactic RIPS-DILEMMA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; works & tested

;;;;;;;;;;;;;;;;;;;
;                 ;
; P v Q           ; = L2
; P => R          ; = L3
; Q => R          ; = L4
; ----------------;
;  R              ; = L1
;;;;;;;;;;;;;;;;;;;

(infer~deftactic RIPS-DILEMMA
		 (outline-mappings (((existent existent existent existent) RIPS-DILEMMA-a)
				    ((nonexistent existent existent existent) RIPS-DILEMMA-f)))
		 (expansion-function nd-rips=expand-with-ATP)
		 (help "The RIPS tactic for dilemma."))

(tac~deftactic RIPS-DILEMMA-a RIPS-DILEMMA (in base)
   (premises L2 L3 L4)
   (conclusions L1)
   (computations )
   (sideconditions (nd-rips=RIPS-DILEMMA-sidecond-a L1 L2 L3 L4))
   (description "Application of Rips tactic DILEMMA."))


;possibility to tidy up code: sidecond-a and sidecond-f, difference: conclusion
(defun nd-rips=RIPS-DILEMMA-sidecond-a (L1 L2 L3 L4)
  (let* ((F1 (node~formula L1))
	 (F2 (node~formula L2))
	 (F3 (node~formula L3))
	 (F4 (node~formula L4)))
    (and (logic~disjunction-p F2)
	 (logic~implication-p F3)
	 (logic~implication-p F4)
	 (let* ((F21 (first (data~appl-arguments F2)))
		(F22 (second (data~appl-arguments F2)))
		(F31 (first (data~appl-arguments F3)))
		(F32 (second (data~appl-arguments F3)))
		(F41 (first (data~appl-arguments F4)))
		(F42 (second (data~appl-arguments F4))))
	   (and (term~alpha-equal F1 F32)  ;; matching R
		(term~alpha-equal F1 F42)  ;; matching R
		(cond ((term~alpha-equal F21 F31)   ;; matching P & Q
		       (term~alpha-equal F22 F41))  ;; matching P & Q
		      ((term~alpha-equal F21 F41)   ;; matching P & Q
		       (term~alpha-equal F22 F31))  ;; matching P & Q
		      (t nil)))))))


(tac~deftactic RIPS-DILEMMA-f RIPS-DILEMMA (in base)
   (premises L2 L3 L4)
   (conclusions L1)
   (computations (L1 (nd-rips=RIPS-DILEMMA-compute-f L2 L3 L4)))
   (sideconditions (nd-rips=RIPS-DILEMMA-sidecond-f L2 L3 L4))
   (description "Forward Application of Rips tactic DILEMMA."))

;tidy up code: sidecond-a and sidecond-f, difference: conclusion
(defun nd-rips=RIPS-DILEMMA-sidecond-f (L2 L3 L4)
  (let* ((F2 (node~formula L2))
	 (F3 (node~formula L3))
	 (F4 (node~formula L4)))
    (and (logic~disjunction-p F2)
	 (logic~implication-p F3)
	 (logic~implication-p F4)
	 (let* ((F21 (first (data~appl-arguments F2)))
		(F22 (second (data~appl-arguments F2)))
		(F31 (first (data~appl-arguments F3)))
		(F32 (second (data~appl-arguments F3)))
		(F41 (first (data~appl-arguments F4)))
		(F42 (second (data~appl-arguments F4))))
		(and (term~alpha-equal F32 F42) ;; difference
		 (cond ((term~alpha-equal F21 F31)
		       (term~alpha-equal F22 F41))
		      ((term~alpha-equal F21 F41)
		       (term~alpha-equal F22 F31))

		      (t nil))
		 (not (formula-holds-in-domain  F32
					       (nd-rips=domain L2)))
		 )))))

(defun nd-rips=RIPS-DILEMMA-compute-f (L2 L3 L4)
  (let* ((F3 (node~formula L3))
	 (F32 (second (data~appl-arguments F3))))
    F32))
   
(com~defcommand RIPS-DILEMMA
		;; this is the definition of the command to execute the tactic in command interpreter
  (argnames disjunction implication1 implication2 succedent)
  (argtypes ndline ndline ndline ndline)
  (arghelps "Line with disjunction"
	    "Line with first implication"
	    "Line with second implication"
	    "Line with succedent")
  (function nd-rips=RIPS-DILEMMA)
      ;;; here we say which function will work on the input
  (frag-cats tactics base elimination)
  (log-p T)
  (help "The RIPS tactic for dilemma."))


(defun nd-rips=RIPS-DILEMMA (disjunction implication1 implication2 succedent)
  (infer~compute-outline 'RIPS-DILEMMA (list succedent disjunction implication1 implication2) nil))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FORWARD/BACKWARD ROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tactic RIPS-MODUS-PONENS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;
;                 ;
; P => R          ; = L2
; P               ; = L3
; ----------------;
; R               ; = L1
;;;;;;;;;;;;;;;;;;;

;;; works & tested

(infer~deftactic RIPS-MODUS-PONENS
		 (outline-mappings (((existent existent existent) RIPS-MODUS-PONENS-a)
				    ((nonexistent existent existent) RIPS-MODUS-PONENS-f)
				    ((existent existent nonexistent) RIPS-MODUS-PONENS-bs)))
		 (expansion-function nd-rips=expand-with-ATP)
		 (help "The RIPS tactic for disjunctive modus ponens."))

(tac~deftactic RIPS-MODUS-PONENS-a RIPS-MODUS-PONENS (in base)
   (premises L2 L3)
   (conclusions L1)
   (computations )
   (sideconditions (nd-rips=RIPS-MODUS-PONENS-sidecond-a L1 L2 L3))
   (description "Application of Rips tactic MODUS-PONENS."))


(defun nd-rips=RIPS-MODUS-PONENS-sidecond-a (L1 L2 L3)
  (and (logic~implication-p (node~formula L2))
       (term~alpha-equal (second (data~appl-arguments (node~formula L2)))
			 (node~formula L1))  ;;; matching P
       (term~alpha-equal (first (data~appl-arguments (node~formula L2)))
			 (node~formula L3)))) ;;; matching R


(tac~deftactic RIPS-MODUS-PONENS-f RIPS-MODUS-PONENS (in base)
	       (premises L2 L3)
	       (conclusions L1)
	       (computations (L1 (nd-rips=RIPS-MODUS-PONENS-compute-f L2)))
	       (sideconditions (nd-rips=RIPS-MODUS-PONENS-sidecond-f L2 L3))
	       (description "Forward Application of Rips tactic MODUS-PONENS."))

(defun nd-rips=RIPS-MODUS-PONENS-compute-f (L2)
  (when (logic~implication-p (node~formula L2))
    (second (data~appl-arguments (node~formula L2)))))

;;; old, without domain considerations
; (defun nd-rips=RIPS-MODUS-PONENS-sidecond-f (L2 L3)
;  (and (logic~implication-p (node~formula L2))
;       (term~alpha-equal (first (data~appl-arguments (node~formula L2)))
;			     (node~formula L3))))

;;; new, incorporating domains

(defun nd-rips=RIPS-MODUS-PONENS-sidecond-f (L2 L3)
    (and (logic~implication-p (node~formula L2))
	 (term~alpha-equal (first (data~appl-arguments (node~formula L2)))
			   (node~formula L3))
	 (not (formula-holds-in-domain (second (data~appl-arguments (node~formula L2)))  (nd-rips=domain L2))
	 )
    )
    )


(tac~deftactic RIPS-MODUS-PONENS-bs RIPS-MODUS-PONENS (in base)
   (premises L2 L3)
   (conclusions L1)
   (computations (L3 (nd-rips=RIPS-MODUS-PONENS-compute-bs L2)))
   (sideconditions (nd-rips=RIPS-MODUS-PONENS-sidecond-bs L1 L2))
   (description "Backward/sideways application (a la Rips) of tactic MODUS-PONENS.")) ;;; the implication and the conclusion are given.

(defun nd-rips=RIPS-MODUS-PONENS-compute-bs (L2)
  (when (logic~implication-p (node~formula L2))
    (first (data~appl-arguments (node~formula L2)))))

(defun nd-rips=RIPS-MODUS-PONENS-sidecond-bs (L1 L2)
  (and (logic~implication-p (node~formula L2))
       (term~alpha-equal (second (data~appl-arguments (node~formula L2)))
			 (node~formula L1))))

(com~defcommand RIPS-MODUS-PONENS
		;; this is the definition of the command to execute the tactic in command interpreter
  (argnames implication antecedent succedent)
  (argtypes ndline ndline ndline)
  (arghelps "Line with implication" "Line with antecedent of implication" "Line with conclusion")
  (function nd-rips=RIPS-MODUS-PONENS)
      ;;; here we say which function will work on the input
  (frag-cats tactics base elimination)
  (log-p T)
  (help "The RIPS tactic for modus ponens."))


(defun nd-rips=RIPS-MODUS-PONENS (implication antecedent succedent)
  (infer~compute-outline 'RIPS-MODUS-PONENS (list succedent implication antecedent) nil))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tactic RIPS-DISJ-MODUS-PONENS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;
;                 ;
; (P v Q) => R    ; = L2
; P               ; = L3
; ----------------;
; R               ; = L1
;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;
;                 ;
; (P v Q) => R    ; = L2
; Q               ; = L3
; ----------------;
; R               ; = L1
;;;;;;;;;;;;;;;;;;;

;;; works

(infer~deftactic RIPS-DISJ-MODUS-PONENS
		 (outline-mappings (((existent existent existent) RIPS-DISJ-MODUS-PONENS-a)
				    ((nonexistent existent existent) RIPS-DISJ-MODUS-PONENS-f)))
		 (expansion-function nd-rips=expand-with-ATP)
		 (help "The RIPS tactic for disjunctive modus ponens."))

(tac~deftactic RIPS-DISJ-MODUS-PONENS-a RIPS-DISJ-MODUS-PONENS (in base)
	       (premises L2 L3)
	       (conclusions L1)
	       (computations )
	       (sideconditions (nd-rips=RIPS-DISJ-MODUS-PONENS-sidecond1 L1 l2 l3))
	       (description "Application of Rips tactic DISJ-MODUS-PONENS."))


(defun nd-rips=RIPS-DISJ-MODUS-PONENS-sidecond1 (L1 L2 L3)
  (and (logic~implication-p (node~formula L2))
       (term~alpha-equal (second (data~appl-arguments (node~formula L2)))
			 (node~formula L1))
       (logic~disjunction-p (first (data~appl-arguments (node~formula L2))))
       (or (term~alpha-equal (first (data~appl-arguments (first (data~appl-arguments
								 (node~formula L2)))))
			     (node~formula L3))
	   (term~alpha-equal (second (data~appl-arguments (first (data~appl-arguments
								  (node~formula L2)))))
			     (node~formula L3)))))

(tac~deftactic RIPS-DISJ-MODUS-PONENS-f RIPS-DISJ-MODUS-PONENS (in base)
	       (premises L2 L3)
	       (conclusions L1)
	       (computations (L1 (nd-rips=RIPS-DISJ-MODUS-PONENS-compute1 L2)))
	       (sideconditions (nd-rips=RIPS-DISJ-MODUS-PONENS-sidecond2 L2 L3))
	       (description "Forward Application of Rips tactic DISJ-MODUS-PONENS."))

(defun nd-rips=RIPS-DISJ-MODUS-PONENS-compute1 (L2)
  (when (logic~implication-p (node~formula L2))
    (second (data~appl-arguments (node~formula L2)))))

(defun nd-rips=RIPS-DISJ-MODUS-PONENS-sidecond2 (L2 L3)
  (and (logic~implication-p (node~formula L2))
       (or (term~alpha-equal (first (data~appl-arguments (first (data~appl-arguments
								 (node~formula L2)))))
			     (node~formula L3))
	   (term~alpha-equal (second (data~appl-arguments (first (data~appl-arguments
								  (node~formula L2)))))
			     (node~formula L3)))
       (not (formula-holds-in-domain  (second (data~appl-arguments
					       (node~formula L2)))
				      (nd-rips=domain L2))
			 )
       ))



(com~defcommand RIPS-DISJ-MODUS-PONENS
		;; this is the definition of the command to execute the tactic in command interpreter
  (argnames implication antecedent succedent)
  (argtypes ndline ndline ndline)
  (arghelps "Line with implication"
	    "Line with one of the disjuncts of antecedent of implication"
	    "Line with conclusion")
  (function nd-rips=RIPS-DISJ-MODUS-PONENS)
      ;;; here we say which function will work on the input
  (frag-cats tactics base elimination)
  (log-p T)
  (help "The RIPS tactic for disjunctive modus ponens."))


(defun nd-rips=RIPS-DISJ-MODUS-PONENS (implication antecedent succedent)
  (infer~compute-outline 'RIPS-DISJ-MODUS-PONENS (list succedent implication antecedent) nil))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tactic RIPS-DISJ-SYLL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;
;                 ;
; P v Q           ; = L2
; ~Q              ; = L3
; ----------------;
; P               ; = L1
;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;
;                 ;
; P v Q           ; = L2
; ~P              ; = L3
; ----------------;
; Q               ; = L1
;;;;;;;;;;;;;;;;;;;

;;; works 

(infer~deftactic RIPS-DISJ-SYLL
		 (outline-mappings (((existent existent existent) RIPS-DISJ-SYLL-a)
				    ((nonexistent existent existent) RIPS-DISJ-SYLL-f)
				    ((existent existent nonexistent) RIPS-DISJ-SYLL-b)))
		 (expansion-function nd-rips=expand-with-ATP)
		 (help "The RIPS tactic for dilemma."))

(tac~deftactic RIPS-DISJ-SYLL-a RIPS-DISJ-SYLL (in base)
   (premises L2 L3)
   (conclusions L1)
   (computations )
   (sideconditions (nd-rips=RIPS-DISJ-SYLL-sidecond1 L1 l2 l3))
   (description "Application of Rips tactic DISJ-SYLL."))


(defun nd-rips=RIPS-DISJ-SYLL-sidecond1 (L1 L2 L3)
  (let* ((F1 (node~formula L1))
	 (F2 (node~formula L2))
	 (F3 (node~formula L3)))
    (and (logic~disjunction-p F2)
	 (logic~negation-p F3)
	 (let* ((F21 (first (data~appl-arguments F2)))
		(F22 (second (data~appl-arguments F2)))
		(F31 (first (data~appl-arguments F3))))
	   (cond ((term~alpha-equal F1 F21)
		  (term~alpha-equal F31 F22))
		 ((term~alpha-equal F1 F22)
		  (term~alpha-equal F31 F21))
		 (t nil))))))


(tac~deftactic RIPS-DISJ-SYLL-f RIPS-DISJ-SYLL (in base)
   (premises L2 L3)
   (conclusions L1)
   (computations (L1 (nd-rips=RIPS-DISJ-SYLL-compute1 L2 L3)))
   (sideconditions (nd-rips=RIPS-DISJ-SYLL-sidecond2 L2 L3))
   (description "Forward Application of Rips tactic DISJ-SYLL."))

(defun nd-rips=RIPS-DISJ-SYLL-sidecond2 (L2 L3)
  (let* ((F2 (node~formula L2))
	 (F3 (node~formula L3)))
    (and (logic~disjunction-p F2)
	 (logic~negation-p F3)
	 (let* ((F21 (first (data~appl-arguments F2)))
		(F22 (second (data~appl-arguments F2)))
		(F31 (first (data~appl-arguments F3))))
	   (or (and (term~alpha-equal F31 F21)
		    (not (formula-holds-in-domain F22  (nd-rips=domain L2))
			 ))
	       (and (term~alpha-equal F31 F22)
		    (not (formula-holds-in-domain F21  (nd-rips=domain L2))
			 ))
		    )))))

(defun nd-rips=RIPS-DISJ-SYLL-compute1 (L2 L3)
  (let* ((F2 (node~formula L2))
	 (F3 (node~formula L3))
	 (F21 (first (data~appl-arguments F2)))
	 (F22 (second (data~appl-arguments F2)))
	 (F31 (first (data~appl-arguments F3))))
    (cond ((term~alpha-equal F31 F21) F22)
	  ((term~alpha-equal F31 F22) F21))))


(tac~deftactic RIPS-DISJ-SYLL-b RIPS-DISJ-SYLL (in base)
   (premises L2 L3)
   (conclusions L1)
   (computations (L3 (nd-rips=RIPS-DISJ-SYLL-compute2 L1 L2)))
   (sideconditions (nd-rips=RIPS-DISJ-SYLL-sidecond3 L1 L2))
   (description "Forward Application of Rips tactic DISJ-SYLL."))

(defun nd-rips=RIPS-DISJ-SYLL-sidecond3 (L1 L2)
  (let* ((F1 (node~formula L1))
	 (F2 (node~formula L2)))
    (and (logic~disjunction-p F2)
	 (let* ((F21 (first (data~appl-arguments F2)))
		(F22 (second (data~appl-arguments F2))))
	   (or (term~alpha-equal F1 F21) (term~alpha-equal F1 F22))))))

(defun nd-rips=RIPS-DISJ-SYLL-compute2 (L1 L2)
  (let* ((F1 (node~formula L1))
	 (F2 (node~formula L2))
	 (F21 (first (data~appl-arguments F2)))
	 (F22 (second (data~appl-arguments F2))))
    (cond ((term~alpha-equal F1 F21) (logic~negate F22))
	  ((term~alpha-equal F1 F22) (logic~negate F21)))))

(com~defcommand RIPS-DISJ-SYLL
		;; this is the definition of the command to execute the tactic in command interpreter
  (argnames disjunction antecedent succedent)
  (argtypes ndline ndline ndline)
  (arghelps "Line with disjunction"
	    "Line with antecedent"
	    "Line with succedent")
  (function nd-rips=RIPS-DISJ-SYLL)
      ;;; here we say which function will work on the input
  (frag-cats tactics base elimination)
  (log-p T)
  (help "The RIPS tactic for dilemma."))


(defun nd-rips=RIPS-DISJ-SYLL (disjunction antecedent succedent)
  (infer~compute-outline 'RIPS-DISJ-SYLL (list succedent disjunction antecedent) nil))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tactic RIPS-DEMORGAN-1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;
;                 ;
; ~(P ^ Q)        ; = L2
; ----------------;
;  ~P v ~Q        ; = L1
;;;;;;;;;;;;;;;;;;;

;; works & tested

(infer~deftactic RIPS-DEMORGAN-1
		 (outline-mappings (((existent existent) RIPS-DEMORGAN-1-a)
				    ((nonexistent existent) RIPS-DEMORGAN-1-f)
				    ((existent nonexistent) RIPS-DEMORGAN-1-b)))
		 (expansion-function nd-rips=expand-with-ATP)
		 (help "The RIPS tactic for de morgan(1)."))

(tac~deftactic RIPS-DEMORGAN-1-a RIPS-DEMORGAN-1 (in base)
   (premises L2)
   (conclusions L1)
   (computations )
   (sideconditions (nd-rips=RIPS-DEMORGAN-1-sidecond-a L1 L2))
   (description "Application of Rips tactic DEMORGAN-1."))


(defun nd-rips=RIPS-DEMORGAN-1-sidecond-a (L1 L2)
  (let* ((F1 (node~formula L1))
	 (F2 (node~formula L2)))
    (and (logic~negation-p F2)
	 (logic~disjunction-p F1)
	 (let* ((F21 (first (data~appl-arguments F2)))
		(F11 (first (data~appl-arguments F1)))
		(F12 (second (data~appl-arguments F1))))
	   (and (logic~conjunction-p F21)
		(logic~negation-p F11)
		(logic~negation-p F12)
		(let* ((F211 (first (data~appl-arguments F21)))
			  (F212 (second (data~appl-arguments F21)))
			  (F111 (first (data~appl-arguments F11)))
			  (F121 (first (data~appl-arguments F12))))
		  (and (term~alpha-equal F211 F111)
		       (term~alpha-equal F212 F121))))))))


(tac~deftactic RIPS-DEMORGAN-1-f RIPS-DEMORGAN-1 (in base)
   (premises L2)
   (conclusions L1)
   (computations (L1 (nd-rips=RIPS-DEMORGAN-1-compute-f L2)))
   (sideconditions (nd-rips=RIPS-DEMORGAN-1-sidecond-f L2))
   (description "Forward Application of Rips tactic DEMORGAN-1."))

(defun nd-rips=RIPS-DEMORGAN-1-sidecond-f (L2)
  (let ((F2 (node~formula L2)))
    (and (logic~negation-p F2)
	 (let* ((F21 (first (data~appl-arguments F2)))
		(disj (nd-rips=RIPS-DEMORGAN-1-compute-f L2)))
	   (and (logic~conjunction-p F21)
		(not (formula-holds-in-domain  disj
					       (nd-rips=domain L2))))
		)))) 


  
(defun nd-rips=RIPS-DEMORGAN-1-compute-f (L2)
  (let* ((F2 (node~formula L2))
	 (F21 (first (data~appl-arguments F2)))
	 (F211 (first (data~appl-arguments F21)))
	 (F212 (second (data~appl-arguments F21)))
         (negleft (term~appl-create (logic~negation-constant)  (list F211)))
	 (negright (term~appl-create (logic~negation-constant) (list F212)))
	 (disj (term~appl-create (logic~disjunction-constant) (list negleft negright))))
	 disj))
   

(tac~deftactic RIPS-DEMORGAN-1-b RIPS-DEMORGAN-1 (in base)
   (premises L2)
   (conclusions L1)
   (computations (L2 (nd-rips=RIPS-DEMORGAN-1-compute-b L1)))
   (sideconditions (nd-rips=RIPS-DEMORGAN-1-sidecond-b L1))
   (description "Backward Application of Rips tactic DEMORGAN-1."))

(defun nd-rips=RIPS-DEMORGAN-1-sidecond-b (L1)
  (let ((F1 (node~formula L1)))
    (and (logic~disjunction-p F1)
	 (let ((F11 (first (data~appl-arguments F1)))
	       (F12 (second (data~appl-arguments F1))))
	   (and (logic~negation-p F11)
		(logic~negation-p F12))))))
  
 

(defun nd-rips=RIPS-DEMORGAN-1-compute-b (L1)
  (let* ((F1 (node~formula L1))
	 (F11 (first (data~appl-arguments F1)))
	 (F12 (second (data~appl-arguments F1)))
	 (F111 (first (data~appl-arguments F11)))
	 (F121 (first (data~appl-arguments F12)))
	 (conj (term~appl-create (logic~conjunction-constant) (list F111 F121)))
	 (negconj (term~appl-create (logic~negation-constant) (list conj))))
    negconj ))
   

(com~defcommand RIPS-DEMORGAN-1
		;; this is the definition of the command to execute the tactic in command interpreter
  (argnames antecedent succedent)
  (argtypes ndline ndline)
  (arghelps "Line with negated conjunction"
	    "Line with disjunction of negated formulas")
  (function nd-rips=RIPS-DEMORGAN-1)
      ;;; here we say which function will work on the input
  (frag-cats tactics base elimination)
  (log-p T)
  (help "The RIPS tactic for demorgan-1."))


(defun nd-rips=RIPS-DEMORGAN-1 (antecedent succedent)
  (infer~compute-outline 'RIPS-DEMORGAN-1 (list succedent antecedent) nil))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tactic RIPS-DEMORGAN-2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;
;                 ;
; ~(P v Q)        ; = L2
; ----------------;
;  ~P ^ ~Q        ; = L1
;;;;;;;;;;;;;;;;;;;

;;; works & tested

;;;; but note: this does not correspond exactly to the original ; there the conjuncts are separated


(infer~deftactic RIPS-DEMORGAN-2
		 (outline-mappings (((existent existent) RIPS-DEMORGAN-2-a)
				    ((nonexistent existent) RIPS-DEMORGAN-2-f)
				    ((existent nonexistent) RIPS-DEMORGAN-2-b)))
		 (expansion-function nd-rips=expand-with-ATP)
		 (help "The RIPS tactic for de morgan(2)."))

(tac~deftactic RIPS-DEMORGAN-2-a RIPS-DEMORGAN-2 (in base)
   (premises L2)
   (conclusions L1)
   (computations )
   (sideconditions (nd-rips=RIPS-DEMORGAN-2-sidecond-a L1 L2))
   (description "Application of Rips tactic DEMORGAN-2."))


(defun nd-rips=RIPS-DEMORGAN-2-sidecond-a (L1 L2)
  (let* ((F1 (node~formula L1))
	 (F2 (node~formula L2)))
    (and (logic~negation-p F2)
	 (logic~conjunction-p F1)
	 (let* ((F21 (first (data~appl-arguments F2)))
		(F11 (first (data~appl-arguments F1)))
	       (F12 (second (data~appl-arguments F1))))
	   (and (logic~disjunction-p F21)
		(logic~negation-p F11)
		(logic~negation-p F12)
		(let* ((F211 (first (data~appl-arguments F21)))
		       (F212 (second (data~appl-arguments F21)))
		       (F111 (first (data~appl-arguments F11)))
			  (F121 (first (data~appl-arguments F12))))
		  (and (term~alpha-equal F211 F111)
		       (term~alpha-equal F212 F121))))))))


(tac~deftactic RIPS-DEMORGAN-2-f RIPS-DEMORGAN-2 (in base)
	       (premises L2)
	       (conclusions L1)
	       (computations (L1 (nd-rips=RIPS-DEMORGAN-2-compute-f L2)))
   (sideconditions (nd-rips=RIPS-DEMORGAN-2-sidecond-f L2))
   (description "Forward Application of Rips tactic DEMORGAN-2."))




(defun nd-rips=RIPS-DEMORGAN-2-sidecond-f (L2)
  (let ((F2 (node~formula L2)))
    (and (logic~negation-p F2)
	 (let ((F21 (first (data~appl-arguments F2)))
	       (conj (nd-rips=RIPS-DEMORGAN-2-compute-f L2)))
	   (and (logic~disjunction-p F21)
		(not (formula-holds-in-domain  conj
					       (nd-rips=domain L2))))
	   )))) 


  
(defun nd-rips=RIPS-DEMORGAN-2-compute-f (L2)
  (let* ((F2 (node~formula L2))
	 (F21 (first (data~appl-arguments F2)))
	 (F211 (first (data~appl-arguments F21)))
	 (F212 (second (data~appl-arguments F21)))
         (negleft (term~appl-create (logic~negation-constant)  (list F211)))
	 (negright (term~appl-create (logic~negation-constant) (list F212)))
	 (conj (term~appl-create (logic~conjunction-constant) (list negleft negright))))
	 conj))
   

(tac~deftactic RIPS-DEMORGAN-2-b RIPS-DEMORGAN-2 (in base)
   (premises L2)
   (conclusions L1)
   (computations (L2 (nd-rips=RIPS-DEMORGAN-2-compute-b L1)))
   (sideconditions (nd-rips=RIPS-DEMORGAN-2-sidecond-b L1))
   (description "Backward Application of Rips tactic DEMORGAN-2."))

(defun nd-rips=RIPS-DEMORGAN-2-sidecond-b (L1)
  (let ((F1 (node~formula L1)))
    (and (logic~conjunction-p F1)
	 (let ((F11 (first (data~appl-arguments F1)))
	       (F12 (second (data~appl-arguments F1))))
	   (and (logic~negation-p F11)
		(logic~negation-p F12))))))
  
 

(defun nd-rips=RIPS-DEMORGAN-2-compute-b (L1)
  (let* ((F1 (node~formula L1))
	 (F11 (first (data~appl-arguments F1)))
	 (F12 (second (data~appl-arguments F1)))
	 (F111 (first (data~appl-arguments F11)))
	 (F121 (first (data~appl-arguments F12)))
	 (conj (term~appl-create (logic~disjunction-constant) (list F111 F121)))
	 (negconj (term~appl-create (logic~negation-constant) (list conj))))
    negconj ))
   

(com~defcommand RIPS-DEMORGAN-2
		;; this is the definition of the command to execute the tactic in command interpreter
  (argnames antecedent succedent)
  (argtypes ndline ndline)
  (arghelps "Line with negated disjunction"
	    "Line with conjunction of negated formulas")
  (function nd-rips=RIPS-DEMORGAN-2)
      ;;; here we say which function will work on the input
  (frag-cats tactics base elimination)
  (log-p T)
  (help "The RIPS tactic for demorgan-2."))


(defun nd-rips=RIPS-DEMORGAN-2 (antecedent succedent)
  (infer~compute-outline 'RIPS-DEMORGAN-2 (list succedent antecedent) nil))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tactic RIPS-ANDE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;
;                 ;
; P ^ Q           ; = L3
; ----------------;
; P               ; = L2
; Q               ; = L1
;;;;;;;;;;;;;;;;;;;


;;; tested & works

(infer~deftactic RIPS-ANDE
		 (outline-mappings (((existent existent existent) RIPS-ANDE-a)
				    ((nonexistent nonexistent existent) RIPS-ANDE-f)
				    ((existent existent nonexistent) RIPS-ANDE-b)))
		 (expansion-function nd-rips=expand-with-ATP)
		 (help "The RIPS tactic for and elimination."))

(tac~deftactic RIPS-ANDE-a RIPS-ANDE (in base)
   (premises L3)
   (conclusions L1 L2)
   (computations )
   (sideconditions (nd-rips=RIPS-ANDE-sidecond-a L1 L2 L3))
   (description "Application of Rips tactic ANDE."))


(defun nd-rips=RIPS-ANDE-sidecond-a (L1 L2 L3)
  (let* ((F1 (node~formula L1))
	 (F2 (node~formula L2))
	 (F3 (node~formula L3)))
    (and (logic~conjunction-p F3)
	 (let* ((F31 (first (data~appl-arguments F3)))
		(F32 (second (data~appl-arguments F3))))
	   (or (and (term~alpha-equal F31 F1)
	       (term~alpha-equal F32 F2))
	       (and (term~alpha-equal F31 F2)
		    (term~alpha-equal F32 F1)))))))
	       

(tac~deftactic RIPS-ANDE-f RIPS-ANDE (in base) 
   (premises L3)
   (conclusions L1 L2)
   (computations (L1 (nd-rips=RIPS-ANDE-compute-f-l L3))
		 (L2 (nd-rips=RIPS-ANDE-compute-f-r L3)))
   (sideconditions (nd-rips=RIPS-ANDE-sidecond-f L3))
   (description "Forward Application of Rips tactic ANDE."))

(defun nd-rips=RIPS-ANDE-sidecond-f (L3)
  (and (logic~conjunction-p (node~formula L3))
       (or (not (formula-holds-in-domain (first (data~appl-arguments (node~formula L3)))  (nd-rips=domain L3))
		)
	   (not (formula-holds-in-domain (second (data~appl-arguments (node~formula L3)))  (nd-rips=domain L3))
		))
       )) ;;;; note that ANDE is invoked even if one of the conjuncts is already in the domain (supports)
    
(defun nd-rips=RIPS-ANDE-compute-f-l (L3)
  (first (data~appl-arguments (node~formula L3))))

(defun nd-rips=RIPS-ANDE-compute-f-r (L3)
  (second (data~appl-arguments (node~formula L3))))
   

(tac~deftactic RIPS-ANDE-b RIPS-ANDE (in base)
   (premises L3)
   (conclusions L1 L2)
   (computations (L3 (nd-rips=RIPS-ANDE-compute-b L1 L2)))
   (sideconditions (nd-rips=RIPS-ANDE-sidecond-b L1 L2))
   (description "Backward Application of Rips tactic ANDE."))


(defun nd-rips=RIPS-ANDE-compute-b (L1 L2)
   (term~appl-create (logic~conjunction-constant)
		     (list (node~formula L1)
			   (node~formula L2) )))

(defun nd-rips=RIPS-ANDE-sidecond-b (L1 L2)
   T)

(com~defcommand RIPS-ANDE
		;; this is the definition of the command to execute the tactic in command interpreter
  (argnames conjunction conjunct1 conjunct2)
  (argtypes ndline ndline ndline  )
  (arghelps "Line with conjunction"
	    "Line with 1st conjunct"
	    "Line with 2nd conjunct")
  (function nd-rips=RIPS-ANDE)
      ;;; here we say which function will work on the input
  (frag-cats tactics base elimination)
  (log-p T)
  (help "The RIPS tactic for and elimination."))


(defun nd-rips=RIPS-ANDE (conjunction conjunct1 conjunct2)
  (infer~compute-outline 'RIPS-ANDE (list conjunct1 conjunct2 conjunction) nil))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tactic RIPS-NOTNOTE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Rips: double negation elimination

;;;;;;;;;;;;;;;;;;;
;                 ;
; ~~P             ; = L2
; ----------------;
; P               ; = L1
;;;;;;;;;;;;;;;;;;;


;;; works & tested

(infer~deftactic RIPS-NOTNOTE
		 (outline-mappings (((existent existent) RIPS-NOTNOTE-a)
				    ((nonexistent existent) RIPS-NOTNOTE-f)
				    ((existent nonexistent) RIPS-NOTNOTE-b)))
		 (expansion-function nd-rips=expand-with-ATP)
		 (help "The RIPS tactic for double negation elimination."))

(tac~deftactic RIPS-NOTNOTE-a RIPS-NOTNOTE (in base)
   (premises L2)
   (conclusions L1)
   (computations )
   (sideconditions (nd-rips=RIPS-NOTNOTE-sidecond-a L1 L2))
   (description "Application of Rips tactic NOTNOTE."))


(defun nd-rips=RIPS-NOTNOTE-sidecond-a (L1 L2)
  (let* ((F1 (node~formula L1))
	 (F2 (node~formula L2)))
    (and (logic~negation-p F2)
	 (let ((F21 (first (data~appl-arguments F2))))
	   (and (logic~negation-p F21)
		(let ((F211 (first (data~appl-arguments F21))))
		  (term~alpha-equal F211 F1)))))))
	   
	  
	   
(tac~deftactic RIPS-NOTNOTE-f RIPS-NOTNOTE (in base)
   (premises L2)
   (conclusions L1)
   (computations (L1 (nd-rips=RIPS-NOTNOTE-compute-f L2)))
   (sideconditions (nd-rips=RIPS-NOTNOTE-sidecond-f L2))
   (description "Forward Application of Rips tactic NOTNOTE."))

(defun nd-rips=RIPS-NOTNOTE-sidecond-f (L2)
  (let ((F2 (node~formula L2)))
    (and (logic~negation-p F2)
	 (let ((F21 (first (data~appl-arguments F2))))
	   (and (logic~negation-p F21)
		(not (formula-holds-in-domain (first (data~appl-arguments  F21))  (nd-rips=domain L2))
		)
		)))))
    
  
(defun nd-rips=RIPS-NOTNOTE-compute-f (L2)
  (first (data~appl-arguments (first (data~appl-arguments (node~formula L2))))))
   

(tac~deftactic RIPS-NOTNOTE-b RIPS-NOTNOTE (in base)
   (premises L2)
   (conclusions L1)
   (computations (L2 (nd-rips=RIPS-NOTNOTE-compute-b L1)))
   (sideconditions (nd-rips=RIPS-NOTNOTE-sidecond-b L1))
   (description "Backward Application of Rips tactic NOTNOTE."))

(defun nd-rips=RIPS-NOTNOTE-sidecond-b (L1)
  T)   ;;; there is no more to it ;o)
  
(defun nd-rips=RIPS-NOTNOTE-compute-b (L1)
  (term~appl-create (logic~negation-constant)
		    (list (term~appl-create (logic~negation-constant)
					    (list (node~formula L1))))))
;;;logic~negate does not work here: delivers same formula after 2 applications.


(com~defcommand RIPS-NOTNOTE
		;; this is the definition of the command to execute the tactic in command interpreter
  (argnames negnegterm term)
  (argtypes ndline ndline)
  (arghelps "Line with double negation" "Line without double negation")
  (function nd-rips=RIPS-NOTNOTE)
      ;;; here we say which function will work on the input
  (frag-cats tactics base elimination)
  (log-p T)
  (help "The RIPS tactic for double negation elimination."))


(defun nd-rips=RIPS-NOTNOTE (negnegterm term)
  (infer~compute-outline 'RIPS-NOTNOTE (list term negnegterm) nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PURE BACKWARD ROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tactic RIPS-IMPI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Rips: If Introduction

;;;;;;;;;;;;;;;;;;;
; [P]             ; = H
;  .              ;
;  .              ;
;  .	    	  ;
;  Q              ; = L2
; ----------------;
; P => Q          ; = L1
;;;;;;;;;;;;;;;;;;;

;;; works & tested

(infer~deftactic RIPS-IMPI
		 (outline-mappings (((existent existent) RIPS-IMPI-a)
				    ((existent nonexistent) RIPS-IMPI-b)))
		 (expansion-function nd-rips=expand-with-ATP)
		 (help "The RIPS tactic for implication introduction - only to be used backwards."))

(tac~deftactic RIPS-IMPI-a RIPS-IMPI (in base)
    ;; (hypotheses ((H L2) "H is a hypothesis for L2"))  ;;; ---> NOP.   --- why nop?
   (premises L2)
   (conclusions L1)
   (computations )
   (sideconditions (nd-rips=RIPS-IMPI-sidecond-a L1 (hyps L2) L2))
   (description "Application of Rips tactic IMPI."))


(defun nd-rips=RIPS-IMPI-sidecond-a (L1 hyps L2)
  (let* ((F1 (node~formula L1))
	 (F2 (node~formula L2)))
    (and (logic~implication-p F1)
	    (let ((F11 (first (data~appl-arguments F1)))
		  (F12 (second (data~appl-arguments F1))))
	      (and (some #'(lambda (hyp-node)
			     (term~alpha-equal (node~formula hyp-node)
					       F11))
			 hyps) ;; check whether antecedent is amongst hypotheses
		   (term~alpha-equal F2 F12))))))


(tac~deftactic RIPS-IMPI-b RIPS-IMPI (in base)
   (hypotheses ((H L2) "H is a hypothesis for L2"))	       
   (premises L2)
   (conclusions L1)
   (computations (H  (nd-rips=RIPS-IMPI-compute-b-h L1))
		 (L2 (nd-rips=RIPS-IMPI-compute-b-l2 L1)) )
   (sideconditions (nd-rips=RIPS-IMPI-sidecond-b L1))
   (description "Backward Application of Rips tactic IMPI."))

(defun nd-rips=RIPS-IMPI-sidecond-b (L1)
  (logic~implication-p (node~formula L1)))

(defun nd-rips=RIPS-IMPI-compute-b-h (L1)
  (let* ((F1 (node~formula L1))
	 (H (first (data~appl-arguments F1))))
    H))

(defun nd-rips=RIPS-IMPI-compute-b-l2 (L1)
  (let* ((F1 (node~formula L1))
	 (L2 (second (data~appl-arguments F1))))
    L2))
	  

(com~defcommand RIPS-IMPI
		;; this is the definition of the command to execute the tactic in command interpreter
		(argnames antecedent succedent)
		(argtypes ndline ndline)
		(arghelps "Line with the succedent of the implication"
			  "Implication")
		(function nd-rips=RIPS-IMPI)
      ;;; here we say which function will work on the input
		(frag-cats tactics base elimination)
		(log-p T)
		(help "The RIPS tactic for demorgan-2."))


(defun nd-rips=RIPS-IMPI (antecedent succedent)
  (infer~compute-outline 'RIPS-IMPI (list succedent antecedent) nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tactic RIPS-ANDI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;
; Q               ; = L3
; P               ; = L2
; ----------------;
; P ^ Q           ; = L1
;;;;;;;;;;;;;;;;;;;

;;; works & checked.

(infer~deftactic RIPS-ANDI
		 (outline-mappings (((existent existent existent) RIPS-ANDI-a)
				    ((existent nonexistent nonexistent) RIPS-ANDI-b)))
		 (expansion-function nd-rips=expand-with-ATP)
		 (help "The RIPS tactic for and introduction - only to be used backwards."))

(tac~deftactic RIPS-ANDI-a RIPS-ANDI (in base)
   (premises L2 L3)
   (conclusions L1)
   (computations )
   (sideconditions (nd-rips=RIPS-ANDI-sidecond-a L1 L2 L3))
   (description "Application of Rips tactic ANDI."))


(defun nd-rips=RIPS-ANDI-sidecond-a (L1 L2 L3)
  (let* ((F1 (node~formula L1))
	 (F2 (node~formula L2))
	 (F3 (node~formula L3)))
       (and (logic~conjunction-p F1)
	    (let ((F11 (first (data~appl-arguments F1)))
		  (F12 (second (data~appl-arguments F1))))
	      (or (and (term~alpha-equal F11 F2)
		       (term~alpha-equal F12 F3))
		  (and (term~alpha-equal F11 F3)
		       (term~alpha-equal F12 F2)))))))    
	   

(tac~deftactic RIPS-ANDI-b RIPS-ANDI (in base)       
   (premises L2 L3)
   (conclusions L1)
   (computations  (L2 (nd-rips=RIPS-ANDI-compute-b-l2 L1))
		  (L3 (nd-rips=RIPS-ANDI-compute-b-l3 L1)))
   (sideconditions (nd-rips=RIPS-ANDI-sidecond-b L1))
   (description "Backward Application of Rips tactic ANDI."))

(defun nd-rips=RIPS-ANDI-sidecond-b (L1)
    (logic~conjunction-p (node~formula L1)))
    
(defun nd-rips=RIPS-ANDI-compute-b-l2 (L1)
  (let* ((F1 (node~formula L1))
	 (L2 (first (data~appl-arguments F1))))
    L2))

(defun nd-rips=RIPS-ANDI-compute-b-l3 (L1)
  (let* ((F1 (node~formula L1))
	 (L3 (second (data~appl-arguments F1))))
    L3))
	  

(com~defcommand RIPS-ANDI
		;; this is the definition of the command to execute the tactic in command interpreter
  (argnames conjunct1 conjunct2 conjunction)
  (argtypes ndline ndline ndline)
  (arghelps "Line with the 1st conjunct" "Line with the 2nd conjunct" "Conjunction")
  (function nd-rips=RIPS-ANDI)
      ;;; here we say which function will work on the input
  (frag-cats tactics base elimination)
  (log-p T)
  (help "The RIPS tactic for and introduction."))


(defun nd-rips=RIPS-ANDI (conjunct1 conjunct2 conjunction)
  (infer~compute-outline 'RIPS-ANDI (list conjunction conjunct1 conjunct2) nil))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tactic RIPS-NOTE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;
; [~P]            ; = H
;  .              ;
;  .              ;
;  .	    	  ;
;  Q ^ ~Q         ; = L2
; ----------------;
; P               ; = L1
;;;;;;;;;;;;;;;;;;;

;;; Q is introduced as "a term".

;;; tested & works - phew!

(infer~deftactic RIPS-NOTE
		 (outline-mappings (((existent existent) RIPS-NOTE-a)
				    ((existent nonexistent) RIPS-NOTE-b)))
		 (parameter-types term)
		 (expansion-function nd-rips=expand-with-ATP)
		 (help "The RIPS tactic for not introduction - only to be used backwards."))

(tac~deftactic RIPS-NOTE-a RIPS-NOTE (in base)
   (parameters (Term term+term "A term."))   
   (premises L2)
   (conclusions L1)
   (computations )
   (sideconditions (nd-rips=RIPS-NOTE-sidecond-a L1 (hyps L2) L2))
   (description "Application of Rips tactic NOTE."))


(defun nd-rips=RIPS-NOTE-sidecond-a (L1 hyps L2)
  (let* ((F1 (node~formula L1))
	 (F2 (node~formula L2)))
    (and (some #'(lambda (hyp-node)
		   (term~alpha-equal (node~formula hyp-node)
				     (term~appl-create (logic~negation-constant) (list F1))))
	       hyps)  ;;; is "P negated" among the hypotheses?
	 (logic~conjunction-p F2) ;;; does L2 have the required form: Q^~Q ?
	 (let* ((F21 (first (data~appl-arguments F2)))
		(F22 (second (data~appl-arguments F2))))
	   (or (and (logic~negation-p F21) (term~alpha-equal (first (data~appl-arguments F21)) F22))
	       (and (logic~negation-p F22) (term~alpha-equal (first (data~appl-arguments F22)) F21)))))))

	   

(tac~deftactic RIPS-NOTE-b RIPS-NOTE (in base)
    (parameters (Term term+term "A term."))
   (hypotheses ((H L2) "H is a hypothesis for L2")) 
   (premises L2)
   (conclusions L1)
   (computations  (H (nd-rips=RIPS-NOTE-compute-b-h L1))
		  (L2 (nd-rips=RIPS-NOTE-compute-b-l2 Term)))
   (sideconditions (nd-rips=RIPS-NOTE-sidecond-b L1))
   (description "Backward Application of Rips tactic NOTE."))

(defun nd-rips=RIPS-NOTE-sidecond-b (L1)
    T) 
    
(defun nd-rips=RIPS-NOTE-compute-b-h (L1)
  (term~appl-create (logic~negation-constant) (list (node~formula L1))))

(defun nd-rips=RIPS-NOTE-compute-b-l2 (Term)
(term~appl-create (logic~conjunction-constant)
		  (list term (term~appl-create
			      (logic~negation-constant)
			      (list term)))))  ;;; P gets negated
  
   

(com~defcommand RIPS-NOTE
		;; this is the definition of the command to execute the tactic in command interpreter
  (argnames falsity conclusion term)
  (argtypes ndline ndline term)
  (arghelps "A falsity line" "A line to be proven" "a term")
  (function nd-rips=RIPS-NOTE)
      ;;; here we say which function will work on the input
  (frag-cats tactics base elimination)
  (log-p T)
  (help "The RIPS tactic for not elemination."))


(defun nd-rips=RIPS-NOTE (falsity conclusion term)
  (infer~compute-outline 'RIPS-NOTE (list conclusion falsity) (list term)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tactic RIPS-NOTI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;
; [P]             ; = H
;  .              ;
;  .              ;
;  .	    	  ;
;  Q ^ ~Q         ; = L2
; ----------------;
; ~P              ; = L1
;;;;;;;;;;;;;;;;;;;

;;; also works & tested

(infer~deftactic RIPS-NOTI
		 (outline-mappings (((existent existent) RIPS-NOTI-a)
				    ((existent nonexistent) RIPS-NOTI-b)))
		 (parameter-types term)
		 (expansion-function nd-rips=expand-with-ATP)
		 (help "The RIPS tactic for not introduction - only to be used backwards."))

(tac~deftactic RIPS-NOTI-a RIPS-NOTI (in base)
   (parameters (Term term+term "A term."))   
   (premises L2)
   (conclusions L1)
   (computations )
   (sideconditions (nd-rips=RIPS-NOTI-sidecond-a L1 (hyps L2) L2))
   (description "Application of Rips tactic NOTI."))


(defun nd-rips=RIPS-NOTI-sidecond-a (L1 hyps L2)
  (let* ((F1 (node~formula L1))
	 (F2 (node~formula L2)))
    (and (some #'(lambda (hyp-node)
		   (term~alpha-equal F1
				     (term~appl-create (logic~negation-constant)
						       (list (node~formula hyp-node)))))
			 hyps) ;;; matching hypothesis and conclusion 
	  (logic~conjunction-p F2)
	  (let* ((F21 (first (data~appl-arguments F2)))
		 (F22 (second (data~appl-arguments F2))))
	    (or (and (logic~negation-p F21)
		     (term~alpha-equal
		      (first (data~appl-arguments F21)) F22))
		(and (logic~negation-p F22)
		     (term~alpha-equal (first (data~appl-arguments F22))
				       F21)))))))

	   

(tac~deftactic RIPS-NOTI-b RIPS-NOTI (in base)
   (parameters (Term term+term "A term."))
   (hypotheses ((H L2) "H is a hypothesis for L2")) 
   (premises L2)
   (conclusions L1)
   (computations  (H (nd-rips=RIPS-NOTI-compute-b-h L1))
		  (L2 (nd-rips=RIPS-NOTI-compute-b-l2 Term)))
   (sideconditions (nd-rips=RIPS-NOTI-sidecond-b L1))
   (description "Backward Application of Rips tactic NOTI."))

(defun nd-rips=RIPS-NOTI-sidecond-b (L1)
    (logic~negation-p (node~formula L1))) 
    
(defun nd-rips=RIPS-NOTI-compute-b-h (L1)
  (first (data~appl-arguments (node~formula L1))))

(defun nd-rips=RIPS-NOTI-compute-b-l2 (Term)
(term~appl-create (logic~conjunction-constant)
		  (list term (term~appl-create (logic~negation-constant)
					       (list term)))))  ;;; creating Q ^ ~Q 
   

(com~defcommand RIPS-NOTI
		;; this is the definition of the command to execute the tactic in command interpreter
  (argnames falsity conclusion term)
  (argtypes ndline ndline term)
  (arghelps "A falsity line" "A negative line to be proven" "a term")
  (function nd-rips=RIPS-NOTI)
      ;;; here we say which function will work on the input
  (frag-cats tactics base elimination)
  (log-p T)
  (help "The RIPS tactic for not introduction."))


(defun nd-rips=RIPS-NOTI (falsity conclusion term)
  (infer~compute-outline 'RIPS-NOTI (list conclusion falsity) (list term)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tactic RIPS-ORE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; works & tested


;;;;;;;;;;;;;;;;;;;
;                 ;
; P v Q           ; = L2
; [P] ---> R      ; = L3
; [Q] ---> R      ; = L4
; ----------------;
;  R              ; = L1
;;;;;;;;;;;;;;;;;;;

(infer~deftactic RIPS-ORE
		 (outline-mappings (((existent existent existent existent) RIPS-ORE-a)
				    ((existent existent nonexistent nonexistent) RIPS-ORE-b)))
		 (expansion-function nd-rips=expand-with-ATP)
		 (help "The RIPS tactic for or elimination."))

(tac~deftactic RIPS-ORE-a RIPS-ORE (in base)
   (premises L2 L3 L4)
   (conclusions L1)
   (computations )
   (sideconditions (nd-rips=RIPS-ORE-sidecond-a L1 L2 L3 (hyps L3) L4 (hyps L4) ))
   (description "Application of Rips tactic ORE."))


(defun nd-rips=RIPS-ORE-sidecond-a (L1 L2 L3 hypsL3 L4 hypsL4)
  (let* ((F1 (node~formula L1))
	 (F2 (node~formula L2))
	 (F3 (node~formula L3))
	 (F4 (node~formula L4)))
     (and (logic~disjunction-p F2)
	  (let* ((F21 (first (data~appl-arguments F2)))
		 (F22 (second (data~appl-arguments F2))))
	    (and (term~alpha-equal F1 F3)
		 (term~alpha-equal F1 F4)
		 (or (and 
		      (some #'(lambda (hyp-node)
				(term~alpha-equal F21 (node~formula hyp-node)))
			    hypsL3)
		      (some #'(lambda (hyp-node)
				(term~alpha-equal F22 (node~formula hyp-node)))
			    hypsL4))
		     (and 
		      (some #'(lambda (hyp-node)
				(term~alpha-equal F22 (node~formula hyp-node)))
			    hypsL3)
		      (some #'(lambda (hyp-node)
				(term~alpha-equal F21 (node~formula hyp-node)))
			    hypsL4))))))))
			     

(tac~deftactic RIPS-ORE-b RIPS-ORE (in base)
   (hypotheses ((H3 L3) "H is a hypothesis for L2")
	       ((H4 L4) "H is a hypothesis for L2"))   
   (premises L2 L3 L4)
   (conclusions L1)
   (computations (L3 (nd-rips=RIPS-ORE-compute-b-l34 L1))
		 (L4 (nd-rips=RIPS-ORE-compute-b-l34 L1))
		 (H3 (nd-rips=RIPS-ORE-compute-b-h3 L2))
		 (H4 (nd-rips=RIPS-ORE-compute-b-h4 L2))
		 )
   (sideconditions (nd-rips=RIPS-ORE-sidecond-b L2))
   (description "Forward Application of Rips tactic ORE."))


(defun nd-rips=RIPS-ORE-sidecond-b (L2)
  (logic~disjunction-p (node~formula L2)))
  

(defun nd-rips=RIPS-ORE-compute-b-l34 (L1)
  (node~formula L1))

(defun nd-rips=RIPS-ORE-compute-b-h3 (L2)
  (first (data~appl-arguments (node~formula L2))))

(defun nd-rips=RIPS-ORE-compute-b-h4 (L2)
  (second (data~appl-arguments (node~formula L2))))

	 
   
(com~defcommand RIPS-ORE
		;; this is the definition of the command to execute the tactic in command interpreter
  (argnames disjunction conclusion1 conclusion2 conclusion)
  (argtypes ndline ndline ndline ndline)
  (arghelps "Line with disjunction"  "1st line with conclusion" "2nd line with conclusion" "Final line with conclusion")
  (function nd-rips=RIPS-ORE)
      ;;; here we say which function will work on the input
  (frag-cats tactics base elimination)
  (log-p T)
  (help "The RIPS tactic for backward or elimination."))


(defun nd-rips=RIPS-ORE (disjunction conclusion1 conclusion2  conclusion)
  (infer~compute-outline 'RIPS-ORE (list conclusion disjunction conclusion1 conclusion2) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tactic RIPS-ORI ;;; attention: strategy of trying first oril, then orir, needs to be implemented elsewhere.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;; ORIL

;;;;;;;;;;;;;;;;;;;
;                 ;
;   P             ; = L2
; ----------------;
;  P v Q          ; = L1
;;;;;;;;;;;;;;;;;;;

;;; works & tested

(infer~deftactic RIPS-ORIL
		 (outline-mappings (((existent existent) RIPS-ORIL-a)
				    ((existent nonexistent) RIPS-ORIL-b)))
		 (expansion-function nd-rips=expand-with-ATP)
		 (help "The RIPS tactic for or introduction - left."))

(tac~deftactic RIPS-ORIL-a RIPS-ORIL (in base)
   (premises L2)
   (conclusions L1)
   (computations )
   (sideconditions (nd-rips=RIPS-ORIL-sidecond-a L1 L2))
   (description "Application of Rips tactic ORIL."))


(defun nd-rips=RIPS-ORIL-sidecond-a (L1 L2)
  (let* ((F1 (node~formula L1))
	 (F2 (node~formula L2)))
    (and  (logic~disjunction-p F1)
	  (let* ((F11 (first (data~appl-arguments F1)))
		 (F12 (second (data~appl-arguments F1))))
	    (term~alpha-equal F2 F11)))))

(tac~deftactic RIPS-ORIL-b RIPS-ORIL (in base)
   (premises L2)
   (conclusions L1)
   (computations (L2 (nd-rips=RIPS-ORIL-compute-b L1))  )
   (sideconditions (nd-rips=RIPS-ORIL-sidecond-b L1))
   (description "Backward application of Rips tactic ORIL."))


(defun nd-rips=RIPS-ORIL-compute-b (L1)
  (let ((F1 (node~formula L1)))
	 (first (data~appl-arguments F1))))
	

(defun nd-rips=RIPS-ORIL-sidecond-b (L1)
  (let ((F1 (node~formula L1)))
    (logic~disjunction-p F1)))
       
       
(com~defcommand RIPS-ORIL
		;; this is the definition of the command to execute the tactic in command interpreter
  (argnames disjunct disjunction)
  (argtypes ndline ndline)
  (arghelps "Line with disjunct" "Line with disjunction")
  (function nd-rips=RIPS-ORIL)
      ;;; here we say which function will work on the input
  (frag-cats tactics base elimination)
  (log-p T)
  (help "The RIPS tactic for or introduction - left disjunct."))



;;;;;;; ORIR

;;;;;;;;;;;;;;;;;;;
;                 ;
;   Q            ; = L2
; ----------------;
;  P v Q          ; = L1
;;;;;;;;;;;;;;;;;;;

;;; works & tested


(defun nd-rips=RIPS-ORIR (disjunct disjunction)
  (infer~compute-outline 'RIPS-ORIR (list disjunction disjunct) nil))

(infer~deftactic RIPS-ORIR
		 (outline-mappings (((existent existent) RIPS-ORIR-a)
				    ((existent nonexistent) RIPS-ORIR-b)))
		 (expansion-function nd-rips=expand-with-ATP)
		 (help "The RIPS tactic for or introduction - right."))

(tac~deftactic RIPS-ORIR-a RIPS-ORIR (in base)
   (premises L2)
   (conclusions L1)
   (computations )
   (sideconditions (nd-rips=RIPS-ORIR-sidecond-a L1 L2))
   (description "Application of Rips tactic ORIR."))


(defun nd-rips=RIPS-ORIR-sidecond-a (L1 L2)
  (let* ((F1 (node~formula L1))
	 (F2 (node~formula L2)))
    (and  (logic~disjunction-p F1)
	  (let* ((F11 (first (data~appl-arguments F1)))
		 (F12 (second (data~appl-arguments F1))))
	    (term~alpha-equal F2 F12))))) ;;; difference

(tac~deftactic RIPS-ORIR-b RIPS-ORIR (in base)
   (premises L2)
   (conclusions L1)
   (computations (L2 (nd-rips=RIPS-ORIR-compute-b L1))  )
   (sideconditions (nd-rips=RIPS-ORIL-sidecond-b L1))    ;;; same as ORIL
   (description "Backward application of Rips tactic ORIR."))


(defun nd-rips=RIPS-ORIR-compute-b (L1)
  (let ((F1 (node~formula L1)))
	 (second (data~appl-arguments F1)))) ;; difference       
       
(com~defcommand RIPS-ORIR
		;; this is the definition of the command to execute the tactic in command interpreter
  (argnames disjunct disjunction)
  (argtypes ndline ndline)
  (arghelps "Line with disjunct" "Line with disjunction")
  (function nd-rips=RIPS-ORIR)
      ;;; here we say which function will work on the input
  (frag-cats tactics base elimination)
  (log-p T)
  (help "The RIPS tactic for or introduction - right disjunct."))


(defun nd-rips=RIPS-ORIR (disjunct disjunction)
  (infer~compute-outline 'RIPS-ORIR (list disjunction disjunct) nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tactic RIPS-CONJ-SYLLOGISM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; LEFT

;;;;;;;;;;;;;;;;;;;
;                 ;
;  ~(P ^ Q)       ; = L2
;  P              ; = L3
; ----------------;
;  ~Q		  ; = L1
;;;;;;;;;;;;;;;;;;;



;;; works & tested

(infer~deftactic RIPS-CONJ-SYLLOGISM-LEFT
		 (outline-mappings (((existent existent existent) RIPS-CONJ-SYLLOGISM-LEFT-a)
				    ((existent existent nonexistent) RIPS-CONJ-SYLLOGISM-LEFT-bs)))
		 (expansion-function nd-rips=expand-with-ATP)
		 (help "The RIPS tactic for conjunctive syllogism."))


(tac~deftactic RIPS-CONJ-SYLLOGISM-LEFT-a RIPS-CONJ-SYLLOGISM-LEFT (in base)
   (premises L2 L3)
   (conclusions L1)
   (computations )
   (sideconditions (nd-rips=RIPS-CONJ-SYLLOGISM-LEFT-sidecond-a L1 L2 L3))
   (description "Application of Rips tactic CONJ-SYLLOGISM-LEFT."))


(defun nd-rips=RIPS-CONJ-SYLLOGISM-LEFT-sidecond-a (L1 L2 L3)
  (let* ((F1 (node~formula L1))
	 (F2 (node~formula L2))
	 (F3 (node~formula L3)))
    (and (logic~negation-p F2)
	 (let ((F21 (first (data~appl-arguments F2))))
	   (and (logic~conjunction-p F21)
		(let* ((F211 (first (data~appl-arguments F21)))
		       (F212 (second (data~appl-arguments F21))))
		  (or (and (term~alpha-equal F211 F3)
			   (term~alpha-equal
			    F1
			    (term~appl-create
			     (logic~negation-constant)
			     (list F212))))
		      NIL               ; (and (term~alpha-equal F212 F3)
					;	   (term~alpha-equal
					;	    F1
					;	    (term~appl-create
					;	     (logic~negation-constant)
					;	     (list F211))))
		      )))))))

	 
(tac~deftactic RIPS-CONJ-SYLLOGISM-LEFT-bs RIPS-CONJ-SYLLOGISM-LEFT (in base)
   (premises L2 L3)
   (conclusions L1)
   (computations (L3 (nd-rips=RIPS-CONJ-SYLLOGISM-LEFT-compute-bs L1 L2)))
   (sideconditions (nd-rips=RIPS-CONJ-SYLLOGISM-LEFT-sidecond-bs L1 L2))
   (description "Backward/sidewards (a la Rips) application of tactic CONJ-SYLLOGISM-LEFT."))

(defun nd-rips=RIPS-CONJ-SYLLOGISM-LEFT-compute-bs (L1 L2)
  (let* ((F1 (node~formula L1))
	 (F2 (node~formula L2))
	 (F21 (first (data~appl-arguments F2)))
	 (F211 (first (data~appl-arguments F21))))
    F211))
	  
  
(defun nd-rips=RIPS-CONJ-SYLLOGISM-LEFT-sidecond-bs (L1 L2)
  (let* ((F1 (node~formula L1))
	 (F2 (node~formula L2)))
    (and (logic~negation-p F2)
	 (let ((F21 (first (data~appl-arguments F2))))
	   (and (logic~conjunction-p F21)
		 (let* ((F211 (first (data~appl-arguments F21)))
			(F212 (second (data~appl-arguments F21)))
			(neg-F212 (term~appl-create (logic~negation-constant) (list F212))))
		   (term~alpha-equal neg-F212 F1)))))))
		   
	 

(com~defcommand RIPS-CONJ-SYLLOGISM-LEFT
		;; this is the definition of the command to execute the tactic in command interpreter
  (argnames neg-conjunction premise conclusion)
  (argtypes ndline ndline ndline)
  (arghelps "Line with negated conjunction"
	    "Line with premise"
	    "Line with conclusion")
  (function nd-rips=RIPS-CONJ-SYLLOGISM-LEFT)
      ;;; here we say which function will work on the input
  (frag-cats tactics base elimination)
  (log-p T)
  (help "The RIPS tactic for conjunctive syllogism."))


(defun nd-rips=RIPS-CONJ-SYLLOGISM-LEFT (neg-conjunction premise conclusion)
  (infer~compute-outline 'RIPS-CONJ-SYLLOGISM-LEFT (list conclusion neg-conjunction premise) nil))


;;; RIGHT

;;;;;;;;;;;;;;;;;;;
;                 ;
;  ~(Q ^ P)       ; = L2
;  P              ; = L3
; ----------------;
;  ~Q		  ; = L1
;;;;;;;;;;;;;;;;;;;

;;; works & tested

(infer~deftactic RIPS-CONJ-SYLLOGISM-RIGHT
		 (outline-mappings (((existent existent existent) RIPS-CONJ-SYLLOGISM-RIGHT-a)
				    ((existent existent nonexistent) RIPS-CONJ-SYLLOGISM-RIGHT-bs)))
		 (expansion-function nd-rips=expand-with-ATP)
		 (help "The RIPS tactic for conjunctive syllogism."))


(tac~deftactic RIPS-CONJ-SYLLOGISM-RIGHT-a RIPS-CONJ-SYLLOGISM-RIGHT (in base)
   (premises L2 L3)
   (conclusions L1)
   (computations )
   (sideconditions (nd-rips=RIPS-CONJ-SYLLOGISM-RIGHT-sidecond-a L1 L2 L3))
   (description "Application of Rips tactic CONJ-SYLLOGISM-RIGHT."))


(defun nd-rips=RIPS-CONJ-SYLLOGISM-RIGHT-sidecond-a (L1 L2 L3)
  (let* ((F1 (node~formula L1))
	 (F2 (node~formula L2))
	 (F3 (node~formula L3)))
    (and (logic~negation-p F2)
	 (let ((F21 (first (data~appl-arguments F2))))
	   (and (logic~conjunction-p F21)
		(let* ((F211 (first (data~appl-arguments F21)))
		       (F212 (second (data~appl-arguments F21))))
		  (or NIL
		      ; (and (term~alpha-equal F211 F3)
		      ;	   (term~alpha-equal
		      ;	    F1
		      ;	    (term~appl-create
		      ;	     (logic~negation-constant)
		      ;	     (list F212))))
		      (and (term~alpha-equal F212 F3)
			   (term~alpha-equal
			    F1
			    (term~appl-create
			     (logic~negation-constant)
			     (list F211)))))))))))

	 
(tac~deftactic RIPS-CONJ-SYLLOGISM-RIGHT-bs RIPS-CONJ-SYLLOGISM-RIGHT (in base)
   (premises L2 L3)
   (conclusions L1)
   (computations (L3 (nd-rips=RIPS-CONJ-SYLLOGISM-RIGHT-compute-bs L1 L2)))
   (sideconditions (nd-rips=RIPS-CONJ-SYLLOGISM-RIGHT-sidecond-bs L1 L2))
   (description "Backward/sidewards (a la Rips) application of tactic CONJ-SYLLOGISM-RIGHT."))

(defun nd-rips=RIPS-CONJ-SYLLOGISM-RIGHT-compute-bs (L1 L2)
  (let* ((F1 (node~formula L1))
	 (F2 (node~formula L2))
	 (F21 (first (data~appl-arguments F2)))
	 (F212 (second (data~appl-arguments F21))))
    F212))
	  
  
(defun nd-rips=RIPS-CONJ-SYLLOGISM-RIGHT-sidecond-bs (L1 L2)
  (let* ((F1 (node~formula L1))
	 (F2 (node~formula L2)))
    (and (logic~negation-p F2)
	 (let ((F21 (first (data~appl-arguments F2))))
	   (and (logic~conjunction-p F21)
		 (let* ((F211 (first (data~appl-arguments F21)))
			(F212 (second (data~appl-arguments F21)))
			(neg-F211 (term~appl-create (logic~negation-constant) (list F211))))
		   (term~alpha-equal neg-F211 F1)))))))
		   
	 

(com~defcommand RIPS-CONJ-SYLLOGISM-RIGHT
		;; this is the definition of the command to execute the tactic in command interpreter
  (argnames neg-conjunction premise conclusion)
  (argtypes ndline ndline ndline)
  (arghelps "Line with negated conjunction"
	    "Line with premise"
	    "Line with conclusion")
  (function nd-rips=RIPS-CONJ-SYLLOGISM-RIGHT)
      ;;; here we say which function will work on the input
  (frag-cats tactics base elimination)
  (log-p T)
  (help "The RIPS tactic for conjunctive syllogism."))


(defun nd-rips=RIPS-CONJ-SYLLOGISM-RIGHT (neg-conjunction premise conclusion)
  (infer~compute-outline 'RIPS-CONJ-SYLLOGISM-RIGHT (list conclusion neg-conjunction premise) nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ADDITIONAL RULES FOR COMPLETENESS ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tactic RIPS-NEG-COND-TRAFO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;
;                 ;
;  ~(P => Q)      ; = L3
; ----------------;
;  P              ; = L2
;  ~Q		  ; = L1
;;;;;;;;;;;;;;;;;;;

;;; works & tested

(infer~deftactic RIPS-NEG-COND-TRAFO
		 (outline-mappings (((existent existent existent) RIPS-NEG-COND-TRAFO-a)
				    ((nonexistent nonexistent existent) RIPS-NEG-COND-TRAFO-f)
				    ((existent existent nonexistent) RIPS-NEG-COND-TRAFO-b)))
		 (expansion-function nd-rips=expand-with-ATP)
		 (help "The RIPS tactic for negated conditional transformation."))

(tac~deftactic RIPS-NEG-COND-TRAFO-a RIPS-NEG-COND-TRAFO (in base)
   (premises L3)
   (conclusions L1 L2)
   (computations )
   (sideconditions (nd-rips=RIPS-NEG-COND-TRAFO-sidecond-a L1 L2 L3))
   (description "Application of Rips tactic NEG-COND-TRAFO."))


(defun nd-rips=RIPS-NEG-COND-TRAFO-sidecond-a (L1 L2 L3)
  (let* ((F1 (node~formula L1))
	 (F2 (node~formula L2))
	 (F3 (node~formula L3)))
    (and (logic~negation-p F3)
	 (let ((F31 (first (data~appl-arguments F3))))
	   (and (logic~implication-p F31)
		(let* ((F311 (first (data~appl-arguments F31)))
		       (F312 (second (data~appl-arguments F31))))
		  (or (and (term~alpha-equal F1 F311)
			   (term~alpha-equal
			    F2
			    (term~appl-create
			     (logic~negation-constant)
			     (list F312))))
		      (and (term~alpha-equal F2 F311)
			   (term~alpha-equal
			    F1
			    (term~appl-create
			     (logic~negation-constant)
			     (list F312)))))))))))
		       
		      
	 
(tac~deftactic RIPS-NEG-COND-TRAFO-f RIPS-NEG-COND-TRAFO (in base)
   (premises L3)
   (conclusions L1 L2)
   (computations (L1 (nd-rips=RIPS-NEG-COND-TRAFO-compute-f-l1 L3))
		 (L2 (nd-rips=RIPS-NEG-COND-TRAFO-compute-f-l2 L3)))
   (sideconditions (nd-rips=RIPS-NEG-COND-TRAFO-sidecond-f L3))
   (description "Forward Application of Rips tactic NEG-COND-TRAFO."))

(defun nd-rips=RIPS-NEG-COND-TRAFO-compute-f-l1 (L3)
  (let* ((F3 (node~formula L3))
	 (F31 (first (data~appl-arguments F3)))
	 (F311 (first (data~appl-arguments F31))))
    F311))

(defun nd-rips=RIPS-NEG-COND-TRAFO-compute-f-l2 (L3)
  (let* ((F3 (node~formula L3))
	 (F31 (first (data~appl-arguments F3)))
	 (F312 (second (data~appl-arguments F31))))
    (term~appl-create (logic~negation-constant) (list F312))))



(defun nd-rips=RIPS-NEG-COND-TRAFO-sidecond-f (L3)
(let* ((F3 (node~formula L3)))
    (and (logic~negation-p F3)
	 (let ((F31 (first (data~appl-arguments F3))))
	  (logic~implication-p F31)))))
  

(tac~deftactic RIPS-NEG-COND-TRAFO-b RIPS-NEG-COND-TRAFO (in base)
   (premises L3)
   (conclusions L1 L2)
   (computations (L3 (nd-rips=RIPS-NEG-COND-TRAFO-compute-b L1 L2)))
   (sideconditions (nd-rips=RIPS-NEG-COND-TRAFO-sidecond-b L1 L2))
   (description "Backward application of tactic NEG-COND-TRAFO."))

(defun nd-rips=RIPS-NEG-COND-TRAFO-compute-b (L1 L2)
  (term~appl-create (logic~negation-constant)
		    (list (term~appl-create
			   (logic~implication-constant)
			   (list (node~formula L1)
				 (first
				  (data~appl-arguments
				   (node~formula L2))))))))
  
  
(defun nd-rips=RIPS-NEG-COND-TRAFO-sidecond-b (L1 L2)
  (let* ((F1 (node~formula L1))
	 (F2 (node~formula L2)))
    (logic~negation-p F2)))
	 

(com~defcommand RIPS-NEG-COND-TRAFO
		;; this is the definition of the command to execute the tactic in command interpreter
  (argnames implication conclusion1 conclusion2)
  (argtypes ndline ndline ndline)
  (arghelps "Line with negated implication"
	    "Line with antecedent"
	    "Line with negated succedent")
  (function nd-rips=RIPS-NEG-COND-TRAFO)
      ;;; here we say which function will work on the input
  (frag-cats tactics base elimination)
  (log-p T)
  (help "The RIPS tactic for negated conditional transformation."))


(defun nd-rips=RIPS-NEG-COND-TRAFO (implication conclusion1 conclusion2)
  (infer~compute-outline 'RIPS-NEG-COND-TRAFO (list conclusion1 conclusion2 implication) nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tactic RIPS-COND-TRAFO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;
;                 ;
;  P => Q         ; = L2
; ----------------;
;  ~P v Q         ; = L1
;;;;;;;;;;;;;;;;;;;

;;; works & tested.

(infer~deftactic RIPS-COND-TRAFO
		 (outline-mappings (((existent existent) RIPS-COND-TRAFO-a)
				    ((nonexistent existent) RIPS-COND-TRAFO-f)
				    ((existent nonexistent) RIPS-COND-TRAFO-b)))
		 (expansion-function nd-rips=expand-with-ATP)
		 (help "The RIPS tactic for conditional transformation."))

(tac~deftactic RIPS-COND-TRAFO-a RIPS-COND-TRAFO (in base)
   (premises L2)
   (conclusions L1)
   (computations )
   (sideconditions (nd-rips=RIPS-COND-TRAFO-sidecond-a L1 L2))
   (description "Application of Rips tactic COND-TRAFO."))


(defun nd-rips=RIPS-COND-TRAFO-sidecond-a (L1 L2)
  (let* ((F1 (node~formula L1))
	 (F2 (node~formula L2)))
    (and (logic~implication-p F2)
	 (logic~disjunction-p F1)
	 (let* ((F21 (first (data~appl-arguments F2)))
		(F22 (second (data~appl-arguments F2)))
		(F11 (first (data~appl-arguments F1)))
		(F12 (second (data~appl-arguments F1))))
	   (and (logic~negation-p F11)
		(let ((F111 (first (data~appl-arguments F11))))
		  (and (term~alpha-equal F111 F21)
		       (term~alpha-equal F12 F22))))))))

	   

(tac~deftactic RIPS-COND-TRAFO-f RIPS-COND-TRAFO (in base)
   (premises L2)
   (conclusions L1)
   (computations (L1 (nd-rips=RIPS-COND-TRAFO-compute-f L2)))
   (sideconditions (nd-rips=RIPS-COND-TRAFO-sidecond-f L2))
   (description "Forward Application of Rips tactic COND-TRAFO."))

(defun nd-rips=RIPS-COND-TRAFO-compute-f (L2)
   (let* ((F2 (node~formula L2))
	  (F21 (first (data~appl-arguments F2)))
	  (F22 (second (data~appl-arguments F2)))
	  (left (term~appl-create (logic~negation-constant)  (list F21)))
	  (new (term~appl-create (logic~disjunction-constant) (list left F22))))
     new))
  
  

(defun nd-rips=RIPS-COND-TRAFO-sidecond-f (L2)
  (logic~implication-p (node~formula L2)))
   

(tac~deftactic RIPS-COND-TRAFO-b RIPS-COND-TRAFO (in base)
   (premises L2)
   (conclusions L1)
   (computations (L2 (nd-rips=RIPS-COND-TRAFO-compute-b L1)))
   (sideconditions (nd-rips=RIPS-COND-TRAFO-sidecond-b L1))
   (description "Backward/sideways application (a la Rips) of tactic COND-TRAFO."))

(defun nd-rips=RIPS-COND-TRAFO-compute-b (L1)
  (let* ((F1 (node~formula L1))
	 (F11 (first (data~appl-arguments F1)))
	 (F12 (second (data~appl-arguments F1)))
	 (F111 (first (data~appl-arguments F11))))
    (term~appl-create (logic~implication-constant) (list F111 F12))))

(defun nd-rips=RIPS-COND-TRAFO-sidecond-b (L1)
  (let ((F1 (node~formula L1)))
  (and (logic~disjunction-p F1)
       (let ((F11 (first (data~appl-arguments F1))))
	 (logic~negation-p F11)))))
       
       
(com~defcommand RIPS-COND-TRAFO
		;; this is the definition of the command to execute the tactic in command interpreter
  (argnames implication disjunction)
  (argtypes ndline ndline)
  (arghelps "Line with implication" "Line with disjunction")
  (function nd-rips=RIPS-COND-TRAFO)
      ;;; here we say which function will work on the input
  (frag-cats tactics base elimination)
  (log-p T)
  (help "The RIPS tactic for conditional transformation."))


(defun nd-rips=RIPS-COND-TRAFO (implication disjunction)
  (infer~compute-outline 'RIPS-COND-TRAFO (list disjunction implication) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Quantifier Free Representations
;;;
;;; - Infrastructure -
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Little hacks to skrew up omega

(defun nd-rips=hocnf-name (var variables)
  (declare (edited  "20-SEP-2005")
	   (authors Schiller)
	   (input   "a variable and a list of variables")
	   (effect  "effects of 'gensym'")
	   (value   "creates a name that is unique for that variable,"
		    "also representing the length of the varible list"
		    "this is to be used to give names to skolem functions"))
  (gensym (format nil "TMP_~A_~A_" (length variables) (keim~name var))))

(defun nd-rips=envlookup-special (str strlength)
  (declare (edited  "20-SEP-2005")
	   (authors Schiller)
	   (input   "A string and the length of the prefix to be considered")
	   (effect  "none")
	   (value   "fetches an object in the environment that"
		    "matches the prefix of the string of given length"))
      (env~lookup-object str
			 (pds~environment omega*current-proof-plan)
			 :test (lambda (x y)
				 (equal (subseq (symbol-name x) 0 strlength)
					(subseq (symbol-name y) 0 strlength)))))

(defun nd-rips=add-fresh-var-to-environment (name type)
  (declare (edited  "20-SEP-2005")
	   (authors Schiller)
	   (input   "name and type")
	   (effect  "adds a variable to the environment")
	   (value   "the variable"))
  (let ((var (term~variable-create name type)))
  (env~enter name var  (pds~environment omega*current-proof-plan))
  var
  ))

(defun nd-rips=add-fresh-sksym-to-environment (name type)
  (declare (edited  "20-SEP-2005")
	   (authors Schiller)
	   (input   "name and type")
	   (effect  "adds a new skolem constant to the environment")
	   (value   "the constant"))
 ;; (let ((sksym (sksym~create name type 0))) old
  (let ((sksym (make-instance 'nd-rips+tmpname :name name :annotation type :arity 0)))
    (env~enter name sksym (pds~environment omega*current-proof-plan))
    sksym))

;;; Quantifier Free Representation

(defun nd-rips=skolemize (formula)
  (declare (edited  "20-SEP-2005")
	   (authors Schiller)
	   (input   "a formula")
	   (effect  "new skolem functions are added to the environment")
	   (value   "returns a formula where functions labelled by sk_ are"
		    "replaced by real skolem functions"))
  (cond ((data~appl-p formula)
	 (cond ((equal (subseq (symbol-name (keim~name (data~appl-function formula))) 0 3)
		       "TMP_")
		(let ((obj (nd-rips=envlookup-special
			    (nd-rips=hocnf-name
			     (data~appl-function formula)
			     (data~appl-arguments formula))
			    9)))
		  (if obj  (data~appl-create obj
					     (mapcar #'nd-rips=skolemize
						     (data~appl-arguments formula)))
		         ; (format t "~S" obj)
		    (let* ((head (data~appl-function formula))
			   (head (progn
				   (setf (data~annotation head)
					 (data~annotation formula))
				   head)))
		      (hocnf~new-skolem-term-create
		       head
		       (data~appl-arguments formula)
		       (pds~environment omega*current-proof-plan))))))
		  ('T (data~appl-create (data~appl-function formula)
					(mapcar #'nd-rips=skolemize
						(data~appl-arguments formula))))
		  ))
	 ('T formula)))

(defun nd-rips=skolemize-problem()
  (declare (edited  "20-SEP-2005")
	   (authors Schiller)
	   (input   "none")
	   (effect  "new skolem constants are entered into the environment ")
	   (value   "the open nodes and support nodes of a problem are replaced "
		    "according to nd-rips=skolemize"))
  (let* (
	 (open (pds~open-nodes omega*current-proof-plan))
	 (supports (pds~support-nodes omega*current-proof-plan)))
;    (setf (pds~support-nodes omega*current-proof-plan)
	  (mapc (lambda (x) (setf (node~formula x)
				    (nd-rips=skolemize (node~formula x))))
		  supports)  
;    (setf (pds~open-nodes omega*current-proof-plan)
	  (mapc (lambda (x) (setf (node~formula x)
				    (nd-rips=skolemize (node~formula x))))
		  open)))

(com~defcommand RIPS-SKOLEMIZE-PROBLEM
		;; this is the definition of the command to execute the tactic in command interpreter
  (argnames)
  (argtypes)
  (arghelps)
  (function nd-rips=skolemize-problem)
      ;;; here we say which function will work on the input
  (frag-cats tactics base elimination)
  (log-p T)
  (help "Replaces functions of the form sk_ by proper skolem functions"))

;;; Infrastructure for Working with Quantifier Free Representation


(defclass nd-rips+tmpname (sksym+sk-constant)
;;        ==============
  ()
  (:documentation "Temporary Name a la PSYCOP"))

(defmethod data~equal ((tmp1 nd-rips+tmpname) (tmp2 nd-rips+tmpname))
 ; (format t "Hellohello")
  (equal tmp1 tmp2))


;;; Predicates

(defun nd-rips=tmpname-name-p (obj)
  (declare (edited  "20-SEP-2005")
	   (authors Schiller)
	   (input   "any object")
	   (effect  "none")
	   (value   "true iff object is of class ND-RIPS+TMPNAME"))
  (equal (class-of obj) (find-class 'ND-RIPS+TMPNAME)))

(defun nd-rips=tmpname-p (obj)
  (declare (edited  "20-SEP-2005")
	   (authors Schiller)
	   (input   "any object")
	   (effect  "none")
	   (value   "true iff the object is an application term "
		    "with a tmpname head"))
  (and (data~appl-p obj)
       (nd-rips=tmpname-name-p (data~appl-function obj))))
;;; DIKKAT!: this may cause trouble because skolem-p are not skolem-term-p!!! --- names have changed

(defun nd-rips=tmp-p (obj)
  (or (nd-rips=tmpname-p obj)
      (nd-rips=tmpname-name-p obj)))

(defun nd-rips=caret-p (tmp)
  (let* ((supports (pds~support-nodes omega*current-proof-plan))
	 (collect-tmps (lambda (x) (nd-rips=formula-temporary-names (node~formula x))))
	 (support-tmps (nd-rips=fold (lambda (x y) (union (funcall collect-tmps x) y)) supports NIL)))  
    (if (or (nd-rips=tmpname-p tmp)
	    (nd-rips=tmpname-name-p tmp))
	(member tmp support-tmps))))


(defun nd-rips=application-p (obj)
  (declare (edited  "20-SEP-2005")
	   (authors Schiller)
	   (input   "some object")
	   (effect  "none")
	   (value   "true iff the object is an 'ordinary' application"))
  (and (data~appl-p obj)
       (not (nd-rips=tmpname-name-p (data~appl-function obj)))))

(defun nd-rips=simple-p (obj)
  (declare (edited  "20-SEP-2005")
	   (authors Schiller)
	   (input   "some object")
	   (effect  "none")
	   (value  "true iff the object is a skolem term, variable or constant"))
  (or (and (data~appl-p obj)
	   (nd-rips=tmpname-name-p (data~appl-function obj)))
      (data~variable-p obj)
      (data~constant-p obj)))
;;; DIKKAT!: this may cause trouble because skolem-p are not skolem-term-p!!!


;;; historic
(defun nd-rips=isomorphic-p (formula1 formula2)
  (declare (edited  "20-SEP-2005")
	   (authors Schiller)
	   (input   "two formulae")
	   (effect  "none")
	   (value   "true iff the formulae are isomorphic, as defined by Rips on page 189"))
  (let ((recursive
	 (lambda (f1 f2)
	   (reduce (lambda (x y) (and x y))
		   (mapcar #'nd-rips=isomorphic-p
			   (data~appl-arguments f1)
			   (data~appl-arguments f2))))))
    (cond ((nd-rips=simple-p formula1)
	   (nd-rips=simple-p formula2))
	  ((nd-rips=application-p formula1)
	   (and (nd-rips=application-p formula2)
		(equal (data~appl-function formula1)
		       (data~appl-function formula2))
		(funcall recursive formula1 formula2)))
	  ('T NIL))))

(defun nd-rips=isomorphic-p (formula1 formula2)
  (declare (edited  "20-SEP-2005")
	   (authors Schiller)
	   (input   "two formulae")
	   (effect  "none")
	   (value   "true iff the formulae are isomorphic, as defined by Rips on page 189"))
  (let ((recursive
	 (lambda (f1 f2)
	   (reduce (lambda (x y) (and x y))
		   (mapcar #'nd-rips=isomorphic-p
			   (data~appl-arguments f1)
			   (data~appl-arguments f2))))))
    (cond ((nd-rips=simple-p formula1)
	   (nd-rips=simple-p formula2))
	  ((nd-rips=application-p formula1)
	   (and (nd-rips=application-p formula2)
		(nd-rips=isomorphic-p (data~appl-function formula1)
				      (data~appl-function formula2))
		(funcall recursive formula1 formula2)))
	  ('T NIL))))

(defun nd-rips=variable-in-subscript? (sk-term variable)
  (declare (edited  "20-SEP-2005")
	   (authors Schiller)
	   (input   "a skolem term and a variable")
	   (effect  "none")
	   (value   "true iff the variable is an argument (a subscript) of a skolem term"))
  (if (nd-rips=tmpname-p sk-term)
      (member-if (lambda (o) (data~equal o variable))
		 (data~appl-arguments sk-term))
      NIL))

;;; Accessor Functionality

(defun nd-rips=subscript-variables (sk-term)
  (declare (edited  "20-SEP-2005")
	   (authors Schiller)
	   (input   "skolem term")
	   (effect  "none")
	   (value   "the arguments (subscripts) of the skolem term"))
  (if (nd-rips=tmpname-p sk-term)
      (data~appl-arguments sk-term)
    (list)))

(defun nd-rips=collect-set (itemlistlist)
  (declare (edited  "20-SEP-2005")
	   (authors Schiller)
	   (input   "a list of list of items")
	   (effect  "none")
	   (value   "makes a big union and recognizes skolem terms "
		    "as equal iff they have equal arguments"))  ;;;improved
  (cond ((null itemlistlist) NIL)
	('T (union (car itemlistlist)
		   (nd-rips=collect-set (cdr itemlistlist))
		   :key (lambda (x)
			  (if (nd-rips=tmpname-p x)
			      (data~appl-function x) x) )))))

(defun nd-rips=formula-temporary-names (formula)
  (declare (edited  "20-SEP-2005")
	   (authors Schiller)
	   (input   "a formula")
	   (effect  "none")
	   (value   "the temporary names (= skolem terms) of the formula"))
  (cond ((nd-rips=application-p formula)
	 (nd-rips=collect-set
	  (mapcar #'nd-rips=formula-temporary-names
		  (data~appl-arguments formula))))
	((nd-rips=tmpname-p formula) (list formula))
	((nd-rips=tmpname-name-p formula) (list formula))
	('T NIL)))

(defun nd-rips=formula-variables (formula)
  (declare (edited  "20-SEP-2005")
	   (authors Schiller)
	   (input   "a formula")
	   (effect  "none")
	   (value   "the variables contained in the formula"))
  (cond ((nd-rips=application-p formula)
	 (nd-rips=collect-set
	  (mapcar #'nd-rips=formula-variables
		  (data~appl-arguments formula))))
	((data~variable-p formula) (list formula))
	('T NIL)))

;;; special subprocedures for Rips

(defun nd-rips=assoc-proj-r (assl)
  (mapcar (lambda (x) (cadr x)) assl))

(defun nd-rips=compare-isomorphic-formulae (formula1 formula2 test1 test2)
  (declare (edited  "20-SEP-2005")
	   (authors Schiller)
	   (input   "two isomorphic formulae and two procedures working on 'simple' subterms")
	   (effect  "none")
	   (value   "true iff for each subterm where test1 is true, "
		    "then test2 holds for the corresponding subterm in formula2"))
  (let ((compare-test (lambda (x y)
			(nd-rips=compare-isomorphic-formulae x y test1 test2))))
   (cond ((and (nd-rips=application-p formula1)
	      (nd-rips=application-p formula2))
	  (reduce (lambda (x y) (and x y))
		  (mapcar compare-test (data~appl-arguments formula1)
			  (data~appl-arguments formula2))))
	 ('T (if (funcall test1 formula1) (funcall test2 formula2) 'T)))))

(defun nd-rips=variable-or-tmp-appears (obj var)
  (declare (edited  "20-SEP-2005")
	   (authors Schiller)
	   (input   "some simple term and a variable")
	   (effect  "none")
	   (value   "true iff the object equals the variable or"
		    "the objects is a temporary name with var as subscript"))
 (or (nd-rips=variable-in-subscript? obj var) (data~equal obj var)))

(defun nd-rips=add-subscripts (tmp var)
  (let ((type (type~func-create (data~annotation var) (data~annotation tmp))))
    (setf (data~annotation tmp) type)
    (let 
	 ((tm (data~appl-create tmp var)))
    (setf (sksym~arity tmp) (+ 1 (sksym~arity tmp)))
    tm)))

(defun nd-rips=subscript-skolem-term (tmp varlist)
  (if (null varlist)
      tmp
    (let ; ((type (nd-rips=subscriptstype-recursive (data~annotation tmp) (reverse varlist))))
	((type (type~func-create (mapcar (lambda (x) (data~annotation x)) varlist)  (data~annotation tmp))))
    (setf (data~annotation tmp) type)
    (let 
	((tm (data~appl-create tmp varlist)))
      (setf (sksym~arity tmp) (+ (length varlist) (sksym~arity tmp)))
      tm))))

(defun nd-rips=add-skolem-subscript (tmp var)
  (if (data~appl-p tmp)
      (let* ((functype (data~annotation (data~appl-function tmp)))
	     (rangetype (car (data~substructs functype)))
	     (domaintype (cadr (data~substructs functype)))
	     (vartype (data~annotation var))
	     (varlist (append (data~appl-arguments tmp) (list var)))
	     (argstype (mapcar (lambda (x) (data~annotation x) )(data~appl-arguments tmp)))
	     (newtype (type~func-create (append argstype (list vartype)) rangetype))
	     )
;;	(list (data~appl-function tmp) varlist newtype)
 	  (setf (data~annotation (data~appl-function tmp)) newtype)
   	(let 
  	((tm (data~appl-create (data~appl-function tmp) varlist)))
         (setf (sksym~arity (data~appl-function tmp)) (+ 1 (sksym~arity (data~appl-function tmp))))
         tm)
	)
    (nd-rips=add-subscripts tmp var)))

(defun nd-rips=formula-names-and-vars (formula)
  (cond ((nd-rips=application-p formula) (nd-rips=collect-set (mapcar #'nd-rips=formula-names-and-vars (data~appl-arguments formula))))
	((nd-rips=tmpname-p formula) (list formula))
	((or (data~variable-p formula) (data~constant-p formula)) (list formula))
	('T NIL)))

(defun nd-rips=possible-matching-names (x subgoal assertion)
(let ((vars NIL))
  (nd-rips=compare-isomorphic-formulae subgoal assertion (lambda (var1) (data~equal var1 x)) (lambda (var2) (if (not (equal var2 x)) (setf vars (cons var2 vars)))))
  vars))

;;; replacements

;; why 'replacements' and not substitutions? Substitutions only work on variables,
;; but here I want to freely substitute complex terms and constants, too 

(defclass nd-rips+replacement ()
         ((domain :initarg :domain
	           :initform NIL
		   :accessor replacement-domain)  
	  (codomain :initarg :codomain
		    :initform NIL
		    :accessor replacement-codomain) ))

(defun nd-rips=make-replacement (domain codomain)
  (make-instance 'nd-rips+replacement
		 :domain domain
		 :codomain codomain))

(defmethod nd-rips=get-replacement ((r nd-rips+replacement))
  (list (replacement-domain r) (replacement-codomain r)))

(defmethod nd-rips=replacement-merge((r1 nd-rips+replacement) (r2 nd-rips+replacement))
  (let* ((pairify (lambda (dom codom) (mapcar (lambda (x y) (list x y)) dom codom)))
	 (r1rep (nd-rips=get-replacement r1))
	 (r2rep (nd-rips=get-replacement r2))
	 (r1pairs (funcall pairify (car r1rep) (cadr r1rep)))
	 (r2pairs (funcall pairify (car r2rep) (cadr r2rep)))
	 (newpairs (union r1pairs r2pairs :key #'car))) ;; dangerous!
    (nd-rips=make-replacement
     (mapcar (lambda (x) (car x)) newpairs)
     (mapcar (lambda (x) (cadr x)) newpairs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Matching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Matching 1: variables in subgoal to variables in assertion (page 191)

;;; Condition

(defun nd-rips=matching1condition (p x p-prime y)
  (declare (edited  "20-SEP-2005")
	   (authors Schiller)
	   (input   "p(x) target formula, p'(y) isomorphic subgoal")
	   (effect  "none")
	   (value   "truth value"))
  (and (nd-rips=isomorphic-p p p-prime) ;;; P(x) is a target formula and P'(y) is an isomorphic subgoal
       (and (data~variable-p x) (data~variable-p y)) ;;; x and y are variables
       (nd-rips=compare-isomorphic-formulae
	p
	p-prime
	(lambda (var1) (data~equal var1 x))
	(lambda (var2) (nd-rips=variable-or-tmp-appears var2 y))) ;;; ok
       (if (nd-rips=compare-isomorphic-formulae
	    p
	    p-prime
	    (lambda (var1) (data~equal var1 x))
	    (lambda (var2) (nd-rips=variable-in-subscript? var2 y)))
    	   (nd-rips=compare-isomorphic-formulae
	    p-prime
	    p (lambda (var1) (nd-rips=variable-in-subscript? var1 y))
	    (lambda (var2) (data~variable-p var2)))
	 'T)
       ;;; original
       ; (nd-rips=compare-isomorphic-formulae
	; p
	; p-prime
	; (lambda (var1) (nd-rips=variable-in-subscript? var1 x))
	; (lambda (var2) (nd-rips=variable-in-subscript? var2 y)))
       ;;; \end(original)
       ;;; new (there was something strange going on here
       (nd-rips=compare-isomorphic-formulae
	p
	p-prime
	(lambda (var1) (nd-rips=variable-in-subscript? var1 x))
	(lambda (var2) (or (nd-rips=variable-in-subscript? var2 y)
			   (nd-rips=variable-in-subscript? var2 x)
			   )))
       ;;; end\new
       ))



;;; Action

(defun nd-rips=matching1action (p x p-prime y)
  (declare (edited  "20-SEP-2005")
	   (authors Schiller)
	   (input   "p(x) target formula, p'(y) isomorphic subgoal")
	   (effect  "")
	   (value   "a new subgoal and a replacement"))
  (let* ((temporarynames NIL)
	 (stepone (subst~apply (subst~create (list y) (list x)) p-prime))
	 (nd-rips=compare-isomorphic-formulae
	  p
	  p-prime
	  (lambda (var1) (data~equal var1 x))
	  (lambda (var2)
	    (if (nd-rips=variable-in-subscript? var2 y)
		(setf temporarynames (cons var2 temporarynames)))))
	 (steptwo (subst~apply
		   (subst~create temporarynames
				 (make-list
				  (length temporarynames)
				  :initial-element x))
			       stepone)))
	 ;;; step c is included in (a))
	 (list steptwo (nd-rips=make-replacement
			(cons y temporarynames)
			(cons x (make-list (length temporarynames) :initial-element x))))))


;;;; Matching 2: temporary names in subgoal to temporary or permanent names in assertion (page 191)

;;; Condition

(defun nd-rips=subscripts-equal-special (p p-prime sub1 sub2)
  (if (nd-rips=isomorphic-p p p-prime)
      (let ((check (lambda (x y p p-prime) (nd-rips=compare-isomorphic-formulae p p-prime (lambda (var1) (data~equal var1 x)) (lambda (var2) (or (data~equal var2 y) (data~variable-p var2)))))))
	(and (funcall check sub1 sub2 p p-prime)
	     (funcall check sub2 sub1 p-prime p)))
      NIL))

(defun nd-rips=matching2-special (p p-prime sub1 tees)
  (some (lambda (tee) (nd-rips=subscripts-equal-special p p-prime sub1 tee)) tees))

(defun nd-rips=matching2condition (p n p-prime tee)
  ; (format t "Matching condition 2 called with arguments p: ~S n: ~S p-prime: ~S tee: ~S" p n p-prime tee)
  (and (nd-rips=isomorphic-p p p-prime)
       (and (nd-rips=tmp-p tee) (or (nd-rips=tmp-p n) (data~constant-p n)))
       ;; (subsetp (nd-rips=subscript-variables n) (nd-rips=subscript-variables tee)) how do we see that variables are 'equal'?
       ;; maybe look at the isomorphic formulae and record what role they have
       (not (nd-rips=caret-p tee))
        ;;; the condition d is a bit strange
       (every (lambda (x) (nd-rips=matching2-special p p-prime x   (nd-rips=subscript-variables tee))) (nd-rips=subscript-variables n))
       (nd-rips=compare-isomorphic-formulae p-prime p (lambda (var1) (data~equal var1 tee)) (lambda (var2) (or (data~equal var2 n) (data~variable-p var2))))))

;;; Action

(defun nd-rips=matching2action (p n p-prime tee)
  ; (format t "Matching action 2 called")
  (let* (
	 (stepone (data~replace-structs p-prime (list tee) (list n)))
	 )
    (list stepone (nd-rips=make-replacement (list tee) (list n)))))


;;; Matching 3: permanent names in subgoal to variables in assertion

;;; Condition

(defun nd-rips=matching3condition (p x p-prime m)
  (format t "Matching 3 condition called~%")
  (and (nd-rips=isomorphic-p p p-prime)
       (data~variable-p x)
       (or (nd-rips=tmp-p m)
	   (data~constant-p m))
       (let* ((temporarynames (list))
	 (currenttemp NIL)
	 (foo (nd-rips=compare-isomorphic-formulae
	       p
	       p-prime
	       (lambda (var1)
		 (if (nd-rips=variable-in-subscript? var1 x)
		     (progn (setf currenttemp var1)
			    'T)
		   NIL))
	       (lambda (var2)
		 (if 'T (setf temporarynames (cons (list currenttemp var2) temporarynames))))))
	 (compare (lambda (tx t)
		    (nd-rips=compare-isomorphic-formulae
		     p-prime
		     p
		     (lambda (var1)
		       (data~equal var1 t))
		     (lambda (var2) (data~equal var2 tx)))))
	       (result (reduce
			(lambda (x y) (and x y))
			(cons 'T (mapcar (lambda (x) (if (consp x) (compare (car x) (cadr x)) 'T)
					   temporarynames))))) 
	       ) ;;; all this needs to be tested
	 result)))

;;; Action


;;; not used and not tested yet
(defun nd-rips=substitute-in-position (formula-source formula-target subst-fct)
  (cond ((and (nd-rips=application-p formula-source)
	      (nd-rips=application-p formula-target))
	 (let* ((subterms1 (data~appl-arguments formula-source))
		(head1 (data~appl-function formula-source))
		(subterms2 (data~appl-arguments formula-target))
		(head2 (data~appl-function formula-target))
		)
	   (data~appl-create head1
			     (mapcar
			      (lambda (x y)
				(nd-rips=substitute-in-position x y subst-fct))
			      subterms1
			      subterms2))))
	((subst-fct formula-source formula-target)
	 (subst-fct formula-source formula-target)
	 formula-source)))
	 
 
(defun nd-rips=matching3action (p x p-prime m)
   (format t "Matching 3 action called~%")
  (let* (
	 (stepone (data~replace-structs
		   p-prime
		   (list m)
		   (list x))) ;;; this might be too simple! - substituting x for m at each nonsubscript position that x occupies in P(x)
	 ;;; here is something missing!
	 ;(matching3-actionb (lambda (source target) (if (and (nd-rips=tmp-p source)
	;						 (nd-rips=variable-in-subscript? target x))
	;						(nd-rips=add-skolem-subscript source x) ;; should actually be a NEW name... 
	;					      NIL))) ... not used and not tested yet
	 )
    (list stepone (nd-rips=make-replacement (list m) (list x)))))

;;;; Matching 4: temporary names in subgoal to variables in the assertion (page 192)

;;; Condition

(defun nd-rips=matching4condition (p p-prime tee)
  (and (nd-rips=tmp-p tee)
  (if       (nd-rips=isomorphic-p p p-prime)
      (let* ((variables NIL)
	     (action (nd-rips=compare-isomorphic-formulae
		      p-prime
		      p
		      (lambda (x) (equal x tee))
		      (lambda (x) (if (data~variable-p x)
				      (setf variables (union (list x) variables))
				    (setf variables 'FAIL)))))
	     (compare (lambda (t-prime txi)
		    (nd-rips=compare-isomorphic-formulae
		     p-prime
		     p
		     (lambda (var1)
		       (data~equal var1 t-prime))
		     (lambda (var2) (data~equal var2 txi)))))
	     (tmpnames (nd-rips=formula-temporary-names p-prime))
	     (result (reduce
			(lambda (x y) (and x y))
			(cons 'T (mapcar
				  (lambda (t-prime)
				    (nd-rips=compare-isomorphic-formulae
				     p-prime
				     p
				     (lambda (x) (data~equal x t-prime))
				     (lambda (xi) (if (data~variable-p xi)
						      (funcall compare t-prime xi)
						    'T))))
				  tmpnames))))
	     )
	(and (not (equal variables 'FAIL)) result)))))


;;; Action

(defun nd-rips=matching4action (p p-prime tee)
  (let* ((subst-a NIL)
	 (action (nd-rips=compare-isomorphic-formulae
		  p-prime
		  p
		  (lambda (x) (equal x tee))
		  (lambda (x)
		    (setf subst-a (union (list x) subst-a)))))
	 ;;; action a
	 (p-prime (data~replace-structs p-prime
					(make-list (length subst-a)
						   :initial-element tee)
					subst-a))
	 (variables NIL)
	 (action (nd-rips=compare-isomorphic-formulae
		  p-prime
		  p
		  (lambda (x) (equal x tee))
		  (lambda (x)
		    (if (data~variable-p x)
			(setf variables (union (list x) variables))
		      (setf variables NIL)))))
	 
	 (foo 'blubb)
	 )
    (format t "foo ~S ~%" foo)
    (format t "before call to special-substitute ~%")
    (list (nd-rips=special4substitute p p-prime variables)
	  (nd-rips=make-replacement  (make-list (length subst-a)
						   :initial-element tee)
				     subst-a))))
		

(defun nd-rips=special4substitute (p p-prime variables)
  (cond ((nd-rips=simple-p p-prime)
	 (if (and (or (nd-rips=tmpname-p p-prime)
		      (nd-rips=tmpname-name-p p-prime))
		  (nd-rips=tmpname-p p)
		  (some (lambda (var) (nd-rips=variable-in-subscript? p var))) variables)
	     (let* ((xis (remove-if
			  (lambda (x) (null x))
			  (mapcar
			   (lambda (var)
			     (if (nd-rips=variable-in-subscript? p var) var NIL)) variables)))
		    (subst-b (nd-rips=fold (lambda (x y) (nd-rips=add-skolem-subscript y x)
					     ) xis p-prime)))
	       subst-b))
	 p-prime
	 ) 
	((and (nd-rips=application-p p-prime) (nd-rips=application-p p))
	 (let* ((p-prime-subterms (data~appl-arguments p-prime))
		(p-prime-headhead (data~appl-function p-prime))
		(p-subterms (data~appl-arguments p))
		(p-headhead (data~appl-function p))	  
		)
	   (data~appl-create p-headhead
			     (mapcar (lambda (x y)
				       (nd-rips=special4substitute x y)
				       p-subterms
				       p-prime-subterms)))))
	('T p-prime)))


;;; Special ANDI Matching condition

;; (defun nd-rips=andi-matching-condition (subgoal subgoal-var)
;;   (if (or (nd-rips=tmpname-p subgoal-var)
;; 	  (nd-rips=tmpname-name-p subgoal-var))
;; 	  (let ((new (nd-rips=andi-matching-get subgoal-var)))
;; 	    (if (or (null new) (equal new 'CANDIDATE)) NIL
;; 	      'T))
;;     NIL))

;; (defun nd-rips=andi-matching-action (assertion assertion-var subgoal subgoal-var)
;;   (let ((new (nd-rips=andi-matching-get subgoal-var)))
;;     (list (data~replace-structs subgoal (list subgoal-var) (list new))
;; 	  (nd-rips=make-replacement (list subgoal-var) (list new)))))


(defun nd-rips=andi-matching-subgoal(formula)
  (let* ((tmpnames (nd-rips=formula-temporary-names formula))
	 (andi-substitute (lambda (x y) (let ((new (nd-rips=andi-matching-get x)))
				  (if (or (null new) (equal new 'CANDIDATE))
				      y
				    (data~replace-structs formula (list x) (list new))))))
	)
    (nd-rips=fold andi-substitute tmpnames formula)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main Matching Mechanism
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nd-rips=matching-step (subgoal assertion subgoal-var assertion-var)
   (format t "matching step called with subgoal:~S~% assertion:~S~% subgoal-var:~S~% assertion-var:~S~% " subgoal assertion subgoal-var assertion-var)
  (cond 
;; 	 ((nd-rips=andi-matching-condition subgoal subgoal-var)
;; 	 (nd-rips=andi-matching-action assertion assertion-var subgoal subgoal-var))
	((nd-rips=matching1condition assertion assertion-var subgoal subgoal-var)
	 (nd-rips=matching1action assertion assertion-var subgoal subgoal-var))
	((nd-rips=matching2condition assertion assertion-var subgoal subgoal-var)
	 (nd-rips=matching2action assertion assertion-var subgoal subgoal-var))
	((nd-rips=matching3condition assertion assertion-var subgoal subgoal-var)
	 (nd-rips=matching3action assertion assertion-var subgoal subgoal-var))
	((nd-rips=matching4condition assertion subgoal subgoal-var)
	 (nd-rips=matching4action assertion subgoal subgoal-var))
	('T NIL)))

(defun nd-rips=matching-recursion (subgoal assertion subgoal-var possibilities substitution)
   (format t "matching-recursion called with subgoal:~S~% assertion:~S~% subgoal-var:~S~% poss:~S~%  subs:~S~%" subgoal assertion subgoal-var possibilities substitution)
  (cond ((null possibilities) (list subgoal substitution))
	('T (let ((result  (nd-rips=matching-step
			    subgoal
			    assertion
			    subgoal-var
			    (car possibilities))))
	       (if (null result) (nd-rips=matching-recursion
				  subgoal
				  assertion
				  subgoal-var
				  (cdr possibilities)
				  substitution)
;;;;		 (list subgoal substitution))))))
		 (let ((newsubgoal (car result))
		       (newsubst (cadr result)))
		   (progn
		     (if (nd-rips=andi-matching-candidate-p
			  (car (replacement-domain newsubst)))
			 (nd-rips=andi-matching-set
			  (car (replacement-domain newsubst))
			  (car (replacement-codomain newsubst))
			  ))
		   (nd-rips=matching-recursion
		    newsubgoal
		    assertion
		    subgoal-var
		    (cdr possibilities)
		    (nd-rips=replacement-merge substitution newsubst)))))))))

(defun nd-rips=matching-process-recursion (subgoal assertion subgoal-vars repl)
   (cond ((null subgoal-vars) (list subgoal repl))
	 ('T (let* ((result (nd-rips=matching-recursion
			     subgoal
			     assertion
			     (car subgoal-vars)
			     (nd-rips=possible-matching-names
			      (car subgoal-vars)
			      subgoal assertion) repl))
		   (newsubgoal (car result))
		   (newrepl (cadr result)))	   
	       (nd-rips=matching-process-recursion
		newsubgoal
		assertion
		; (if (equal (cadr result) repl) (cdr subgoal-vars) (nd-rips=formula-names-and-vars subgoal)) ;;; changed
		; (nd-rips=formula-names-and-vars subgoal)
		(if (equal (cadr result) repl) (cdr subgoal-vars) (mapcar (lambda (x) (if (nd-rips=tmpname-p x) (data~replace-structs x (replacement-domain (cadr result)) (replacement-codomain (cadr result)) ) x)) (cdr subgoal-vars)))
		(nd-rips=replacement-merge repl newrepl))))))

(defun nd-rips=term-equal (formula1 formula2)
  (if (nd-rips=isomorphic-p formula1 formula2)
      (cond ((nd-rips=simple-p formula1) (data~equal formula1 formula2))
	    ((nd-rips=application-p formula1) (and (nd-rips=term-equal (data~appl-function formula1) (data~appl-function formula2))
						   (every (lambda (x y)
							    (nd-rips=term-equal x y))
							  (data~appl-arguments formula1)
							  (data~appl-arguments formula2))))
	    ('T (data~equal formula1 formula2)))))

(defun nd-rips=matching-process (subgoal assertion)
  (let* ((subgoal (nd-rips=andi-matching-subgoal subgoal))
	 (subgoal-names (nd-rips=formula-names-and-vars subgoal))
	 (result (nd-rips=matching-process-recursion
		  subgoal
		  assertion
		  subgoal-names
		  (nd-rips=make-replacement NIL NIL)))
	 (newsubgoal (car result))
	 (replacement (cadr result)))
    (if (nd-rips=term-equal assertion newsubgoal) replacement NIL)))





(defun nd-rips=matching (subgoal assertion)
  (nd-rips=matching-process (node~formula subgoal) (node~formula assertion)))

(com~defcommand RIPS-MATCHING
		;; this is the definition of the command to execute the tactic in command interpreter
  (argnames subgoal assertion)
  (argtypes ndline ndline)
  (arghelps "goal line"
	    "support line")
  (function nd-rips=RIPS-MATCHING)
      ;;; here we say which function will work on the input
  (frag-cats tactics base elimination)
  (log-p T)
  (help "RIPS matching"))

(defun nd-rips=RIPS-MATCHING (subgoal assertion)
  (infer~compute-outline 'RIPS-MATCHING (list subgoal assertion) nil))

(infer~deftactic RIPS-MATCHING
		 (outline-mappings (((existent existent) RIPS-MATCHING-a)))
		 (expansion-function nd-rips=expand-with-ATP)
		 (help "The RIPS tactic for matching."))

(tac~deftactic RIPS-MATCHING-a RIPS-MATCHING (in base)
   (premises L2)
   (conclusions L1)
   (computations )
   (sideconditions (nd-rips=matching L1 L2 )
    ;(format t "L1; ~S L2; ~S" L1 L2)
    )
   (description "Application of Rips matching."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SUBSCRIPT ADJUSTMENT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun nd-rips=subscript-adjustment-condition1a (cj xj yi replacement p)
   (let* ((repl (nd-rips=get-replacement replacement))
	  (domain (car repl))
	  (codomain (cadr repl))
	  (replpairs (mapcar (lambda (x y) (list x y)) domain codomain))
	  (yixi (assoc yi replpairs))
	  (xi (if (null yixi) NIL (cadr yixi)))
	  (xi (if (data~variable-p xi) xi NIL))
	  (xi-primes (remove-if (lambda (x) (equal x NIL)) (mapcar (lambda (x)  (if (and (nd-rips=tmpname-p (car x))
			      (nd-rips=variable-in-subscript? (car x) yi)) (cdr x))) replpairs))) ;;; this corresponds to the or in (a)
	  (tempnames-inp (nd-rips=formula-temporary-names p))
	  (xi (if (null xi) (car xi-primes) xi)) ;;; this might delete some possiblities
	  )
     (if (null xi) NIL
     (every (lambda (tmp) (if (nd-rips=variable-in-subscript? t xj) (nd-rips=variable-in-subscript? t xi) NIL)) tempnames-inp)
     )))

(defun nd-rips=subscript-adjustment-condition1b (xj yi replacement)
  (let* ((repl (nd-rips=get-replacement replacement))
	  (domain (car repl))
	  (codomain (cadr repl))
	  (replpairs (mapcar (lambda (x y) (list x y)) domain codomain)) 
	  (tmp-tmp-matches (mapcar (lambda (x) (and (nd-rips=tmpname-p (car x)) (nd-rips=tmpname-p (car y)))) replpairs))
	  (tmp-tmp-matches (remove-if (lambda (x) (null x)) tmp-tmp-matches))
	  (tmp-tmp-matches (mapcar (lambda (x) (and (nd-rips=variable-in-subscript? (car x) yi) (not (nd-rips=variable-in-subscript? (cadr x) xj)))) tmp-tmp-matches))
	  (tmp-tmp-matches (remove-if (lambda (x) (null x)) tmp-tmp-matches))
	  )
    tmp-tmp-matches))

(defun nd-rips=subscript-adjustment (formula p tempname-assoc varname-assoc replacement)
  (let* ((cjs (nd-rips=assoc-proj-r varname-assoc)))
    (mapcar (lambda (cj)
	      (let ((yis (nd-rips=formula-variables formula)))
		(mapcar (lambda (yi)
			  (if (or (nd-rips=subscript-adjustment-condition1a cj (car (assoc cj tempname-assoc)) yi replacement p)
				  (nd-rips=subscript-adjustment-condition1b cj (car (assoc cj tempname-assoc)) replacement ))
			      (let* ((dummy (term~variable-create 'dummy (data~annotation cj)))
				    (dummyformula (data~replace-structs formula (list cj) (list dummy)))
				    (newcj (nd-rips=add-skolem-subscript cj yi)))
				    (setf formula (data~replace-structs dummyformula (list dummy) (list newcj)))
				    )
			       ;; (setf formula (data~replace-structs formula (list cj) (list (nd-rips=add-skolem-subscript cj yi))))
			  )) yis)))cjs)
    formula
  ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ARGUMENT REVERSAL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nd-rips=argument-reversal-special (formula tempnames vars)
  (format t "argument reversal special formula: ~S tempnames: ~S vars: ~S ~%" formula tempnames vars)
  (let* ((tempname-assoc (mapcar (lambda (a) (list a (nd-rips=add-fresh-var-to-environment (gensym "VAR") (data~annotation a)))) tempnames))
	 (vars-assoc (mapcar (lambda (a) (list a (nd-rips=add-fresh-sksym-to-environment (gensym "TMP") (data~annotation a)))) vars))
	 (vars-assoc-step3 (mapcar (lambda (x)
	      (let ((xi (car x))
		    (bi (cadr x)))
		(list xi (nd-rips=ar-step3 tempname-assoc vars-assoc xi)))
	      
	      ) vars-assoc))
	 )
    (format t "before substitutions, formula: ~S tempname-assoc ~S vars-assoc-step3 ~S~%" formula tempname-assoc vars-assoc-step3)
    (setf special*special (list formula tempname-assoc vars-assoc-step3))
    (list (nd-rips=argument-reversal-apply (nd-rips=argument-reversal-apply formula tempname-assoc) vars-assoc-step3) tempname-assoc vars-assoc)
     ))

(defun nd-rips=argument-reversal-apply (formula assoc)
  (format t "argument-reversal-apply called: formula ~S assoc: ~S~%" formula assoc)
  (if (null assoc)
      (progn (format t "argument-reversal-apply result: formula ~S~%" formula)
      formula)
    (let ((subst (car assoc)))
          (format t "doing substitution: car subst ~S cadr subst: ~S~%" (car subst) (cadr subst))
	  (nd-rips=argument-reversal-apply (data~replace-structs formula (list (car subst)) (list (cadr subst))) (cdr assoc)))))

(defun nd-rips=ar-step3 (tempnames-assoc vars-assoc xi)  
  (let* ((ai (remove-if (lambda (x) (nd-rips=variable-in-subscript? (car x) xi)) tempnames-assoc))
	 (yi (nd-rips=assoc-proj-r ai))
	 (bi (cadr (assoc xi vars-assoc)))
	 )
     (nd-rips=subscript-skolem-term bi yi)    
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BACKWARD IF ELIMINATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun nd-rips=backward-IF-elim-condition (conclusion implication)
  (if (logic~implication-p implication)
      (let* ((p-prime (car (data~appl-arguments implication)))
	     (r-prime (cadr (data~appl-arguments implication))))
	(nd-rips=matching-process conclusion r-prime)
	)
      NIL))

(defun nd-rips=backward-IF-elim-condition-tactic (conclusion implication)
  (nd-rips=backward-IF-elim-condition (node~formula conclusion) (node~formula implication)))


(defun nd-rips=backward-IF-elim (conclusion implication)
  (let ((replacement (nd-rips=backward-IF-elim-condition conclusion implication)))
    (if (null replacement) 'FAIL
      (let* ((p-prime (car (data~appl-arguments implication)))
	     (repl (nd-rips=get-replacement replacement))
	     (p (data~replace-structs p-prime (cadr repl) (car repl)));; might be interpreted wrongly 
	     (matched-arguments (car repl))
	     (unmatched-arguments (set-difference (nd-rips=formula-names-and-vars p) matched-arguments ))
	     (reversal-p (nd-rips=argument-reversal-special p (remove-if (lambda (x) (not (nd-rips=tmpname-p x))) unmatched-arguments)
						      (remove-if (lambda (x) (not (data~variable-p x))) unmatched-arguments)))
	     ) 
	(nd-rips=subscript-adjustment (car reversal-p) p (cadr reversal-p) (caddr reversal-p) replacement)
      ))))

(defun nd-rips=backward-IF-elim-tactic (conclusion implication)
  (nd-rips=backward-IF-elim (node~formula conclusion) (node~formula implication)))

(com~defcommand RIPS-BCKW-IF-ELIM
		;; this is the definition of the command to execute the tactic in command interpreter
  (argnames implication succedent)
  ;; (argtypes ndline ndline ndline)
  (argtypes ndline ndline)
  (arghelps "Line with implication"
	    ;;;  "Line with the antecedent" --- never happens
	    "Line with conclusion")
  (function nd-rips=BCKW-IF-ELIM)
      ;;; here we say which function will work on the input
  (frag-cats tactics base elimination)
  (log-p T)
  (help "The RIPS tactic for backward if elimination."))


(defun nd-rips=BCKW-IF-ELIM (implication succedent)
  (infer~compute-outline 'RIPS-BCKW-IF-ELIM (list succedent implication NIL) nil))

(infer~deftactic RIPS-BCKW-IF-ELIM
		 (outline-mappings (((existent existent nonexistent) RIPS-BCKW-IF-ELIM-b)))
		 (expansion-function nd-rips=expand-with-ATP)
		 (help "The RIPS tactic for backward if elimination"))

(tac~deftactic RIPS-BCKW-IF-ELIM-b RIPS-BCKW-IF-ELIM (in base)
   (premises L2 L3)
   (conclusions L1)
   (computations (L3 (nd-rips=backward-IF-elim-tactic L1 L2)))
   (sideconditions (nd-rips=backward-IF-elim-condition-tactic L1 L2))
   (description "Application of Rips tactic RIPS-BCKW-IF-ELIM."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BACKWARD IF INTRODUCTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nd-rips=sensible-substitute (formula substlist)
  (cond ((nd-rips=simple-p formula)
	 (if (data~variable-p formula)
	     (let ((subs (assoc formula substlist)))
	       (if (null subs) formula
		 (cadr subs)))
	   formula))
	((nd-rips=application-p formula)
	 (let* ((subterms (data~appl-arguments formula))
		(head (data~appl-function formula))
		)
	   (data~appl-create head (mapcar (lambda (x) (nd-rips=sensible-substitute x substlist)) subterms))))
	('T formula)))


(defun nd-rips=backward-IF-intro-condition (implication)
  (logic~implication-p implication))
     

(defun nd-rips=backward-IF-intro (implication)
  (let* ((impl (data~appl-arguments implication))
	 (p (car impl))
	 (r (cadr impl))
	 (p-variables (nd-rips=formula-variables p))
	 (r-variables (nd-rips=formula-variables r))
	 (shared-vars (remove-if
		       (lambda (x) (equal x NIL))
		       (mapcar (lambda (x)
				 (if (member x r-variables) x NIL))
			       p-variables)))
	 (vars-and-temps (mapcar
			  (lambda (x)
			    (list x
				  (nd-rips=add-fresh-sksym-to-environment
				   (gensym "BKW")
				   (data~annotation x)))) shared-vars))
	 (p-prime (nd-rips=sensible-substitute p vars-and-temps))
	 (r-prime (nd-rips=sensible-substitute r vars-and-temps))
	 (matched-arguments shared-vars)
	 (unmatched-arguments
	  (set-difference
	   (nd-rips=formula-names-and-vars p)
	   matched-arguments ))
	 )
         
  (list (car (nd-rips=argument-reversal-special
	p-prime
	; (remove-if (lambda (x) (data~variable-p x)) unmatched-arguments)
	; (remove-if (lambda (x) (or (nd-rips=tmpname-name-p x) (nd-rips=tmpname-p x))) unmatched-arguments) unnecessary
	unmatched-arguments
	NIL
	)) r-prime unmatched-arguments matched-arguments p-variables r-variables shared-vars vars-and-temps)
  ; p-prime
  ))

(setq nd-rips*global*if-intro-variable NIL) 

(defun nd-rips=backward-IF-intro-tactic (implication)
  (if (equal nd-rips*global*if-intro-variable NIL)
      (let ((result (nd-rips=backward-IF-intro (node~formula implication))))
	(setf nd-rips*global*if-intro-variable result)
	(car result)
	)
    (let ((result nd-rips*global*if-intro-variable))
      (setf nd-rips*global*if-intro-variable NIL)
      (car result))))

(defun nd-rips=backward-IF-intro-tactic-r (implication)
  (if (equal nd-rips*global*if-intro-variable NIL)
      (let ((result (nd-rips=backward-IF-intro (node~formula implication))))
	(setf nd-rips*global*if-intro-variable result)
	(cadr result)
	)
    (let ((result nd-rips*global*if-intro-variable))
      (setf nd-rips*global*if-intro-variable NIL)
      (cadr result))))

(defun nd-rips=backward-IF-intro-condition-tactic (implication)
  (nd-rips=backward-IF-intro-condition (node~formula implication)))

(com~defcommand RIPS-BCKW-IF-INTRO
		;; this is the definition of the command to execute the tactic in command interpreter
  (argnames implication)
  ;; (argtypes ndline ndline ndline)
  (argtypes ndline)
  (arghelps "Line with implication"
	    ;;;  "Line with the antecedent" --- never happens
	    ;;"Line with conclusion")
	    )
  (function nd-rips=BCKW-IF-INTRO)
      ;;; here we say which function will work on the input
  (frag-cats tactics base elimination)
  (log-p T)
  (help "The RIPS tactic for backward if introduction."))


(defun nd-rips=succedent (l1)
  (cadr (data~appl-arguments (node~formula l1))))

(defun nd-rips=BCKW-IF-INTRO (implication)
  (infer~compute-outline 'RIPS-BCKW-IF-INTRO (list implication NIL) nil))

(infer~deftactic RIPS-BCKW-IF-INTRO
		 (outline-mappings (((existent nonexistent ) RIPS-BCKW-IF-INTRO-b)))
		 (expansion-function nd-rips=expand-with-ATP)
		 (help "The RIPS tactic for backward if introduction"))

(tac~deftactic RIPS-BCKW-IF-INTRO-b RIPS-BCKW-IF-INTRO (in base)
   (hypotheses ((H L2) "H is a hypothesis for L2"))
   (premises L2)
   (conclusions L1)
   (computations ; (H (nd-rips=backward-IF-intro-tactic L1))
		 (L2 (nd-rips=backward-IF-intro-tactic-r L1))
                 (H (nd-rips=backward-IF-intro-tactic L1))
		 )
   (sideconditions (nd-rips=backward-IF-intro-condition-tactic L1))
   (description "Application of Rips tactic RIPS-BCKW-IF-ELIM."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Create Quantifier Free Representations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun nd-rips=gen-for-p (formula name)
  (and (data~appl-p formula)
       (equal (class-of (data~appl-function formula)) (find-class 'TERM+CONSTANT))
       (equal (keim~name (data~appl-function formula)) name)
      ;  (equal (data~appl-function formula) (env~lookup-object 'exists (pds~environment omega*current-proof-plan)))
       (term~abstr-p (car (data~appl-arguments formula)))))

(defun nd-rips=forsome-p (formula)
  (nd-rips=gen-for-p formula 'exists))

(defun nd-rips=forall-p (formula)
  (nd-rips=gen-for-p formula 'forall))

(defun nd-rips=gen-for-domain (formula)
  (data~abstr-domain (car (data~appl-arguments formula))))

(defun nd-rips=gen-for-range (formula)
  (data~abstr-range (car (data~appl-arguments formula))))


;;; Unique Names

(defun nd-rips=unique-var-names (formula)
  (cond ((data~appl-p formula)
	 (data~appl-create
	  (data~appl-function formula)
	  (mapcar #'nd-rips=unique-var-names
		  (data~appl-arguments formula))))
	((term~abstr-p formula) (let* ((formula-prime (term~abstr-create
						       (data~abstr-domain formula)
						       (nd-rips=unique-var-names
							(data~abstr-range formula))))
				      (domain (data~abstr-domain formula-prime))
				      (range  (data~abstr-range formula-prime))
				      (newdomain (mapcar
						  (lambda (x)
						    (nd-rips=add-fresh-var-to-environment
						     (gensym "VAR")
						      ;(format NIL "~S"
						;	      (keim~name x))
						     (data~annotation x)))
						  domain)))
				  (setf (data~abstr-domain formula-prime) newdomain)
				  (subst~apply (subst~create domain newdomain) formula-prime)))
	 ('T formula)))


;;; Prenex Form


(defun nd-rips=gen-prenex-rule-a (formula connective-p connective quant1-p quant2)
  (if (and (funcall connective-p formula)
	   (funcall quant1-p (car (data~appl-arguments  formula))))
      (let* ((qua (car (data~appl-arguments  formula)))
	     (quadom (nd-rips=gen-for-domain qua))
	     (quaran (nd-rips=gen-for-range qua)))
     (logic~quantification-create (funcall quant2)  quadom (data~appl-create (funcall connective) (list quaran (cadr (data~appl-arguments  formula))))))))

(defun nd-rips=gen-prenex-rule-b (formula connective-p connective quant1-p quant2)
  (if (and (funcall connective-p formula)
	   (funcall quant1-p (cadr (data~appl-arguments  formula))))
      (let* ((qua (cadr (data~appl-arguments  formula)))
	     (quadom (nd-rips=gen-for-domain qua))
	     (quaran (nd-rips=gen-for-range qua)))
     (logic~quantification-create (funcall quant2)  quadom (data~appl-create (funcall connective) (list (car (data~appl-arguments  formula)) quaran))))))

(defun nd-rips=logic-existential-quantor()
  (logic~existential-quantor :name 'exists))

(defun nd-rips=logic-universal-quantor()
  (logic~universal-quantor :name 'forall))


(defun nd-rips=prenex10a (formula)
  (nd-rips=gen-prenex-rule-a formula #'logic~conjunction-p #'logic~conjunction-constant #'nd-rips=forsome-p #'nd-rips=logic-existential-quantor))

(defun nd-rips=prenex10b (formula)
  (nd-rips=gen-prenex-rule-b formula #'logic~conjunction-p #'logic~conjunction-constant #'nd-rips=forsome-p #'nd-rips=logic-existential-quantor))

(defun nd-rips=prenex10c (formula)
  (nd-rips=gen-prenex-rule-a formula #'logic~conjunction-p #'logic~conjunction-constant #'nd-rips=forall-p #'nd-rips=logic-universal-quantor))

(defun nd-rips=prenex10d (formula)
  (nd-rips=gen-prenex-rule-b formula #'logic~conjunction-p #'logic~conjunction-constant #'nd-rips=forall-p #'nd-rips=logic-universal-quantor))

(defun nd-rips=prenex11a (formula)
  (nd-rips=gen-prenex-rule-a formula #'logic~disjunction-p #'logic~disjunction-constant #'nd-rips=forsome-p #'nd-rips=logic-existential-quantor))

(defun nd-rips=prenex11b (formula)
  (nd-rips=gen-prenex-rule-b formula #'logic~disjunction-p #'logic~disjunction-constant #'nd-rips=forsome-p #'nd-rips=logic-existential-quantor))

(defun nd-rips=prenex11c (formula)
  (nd-rips=gen-prenex-rule-a formula #'logic~disjunction-p #'logic~disjunction-constant #'nd-rips=forall-p #'nd-rips=logic-universal-quantor))

(defun nd-rips=prenex11d (formula)
  (nd-rips=gen-prenex-rule-b formula #'logic~disjunction-p #'logic~disjunction-constant #'nd-rips=forall-p #'nd-rips=logic-universal-quantor))

(defun nd-rips=prenex12a (formula)
  (nd-rips=gen-prenex-rule-a formula #'logic~implication-p #'logic~implication-constant #'nd-rips=forsome-p #'nd-rips=logic-universal-quantor))

(defun nd-rips=prenex12b (formula)
  (nd-rips=gen-prenex-rule-b formula #'logic~implication-p #'logic~implication-constant #'nd-rips=forsome-p #'nd-rips=logic-existential-quantor))

(defun nd-rips=prenex12c (formula)
  (nd-rips=gen-prenex-rule-a formula #'logic~implication-p #'logic~implication-constant #'nd-rips=forall-p #'nd-rips=logic-existential-quantor))

(defun nd-rips=prenex12d (formula)
  (nd-rips=gen-prenex-rule-b formula #'logic~implication-p #'logic~implication-constant #'nd-rips=forall-p #'nd-rips=logic-universal-quantor))

(defun nd-rips=prenex13a (formula)
  (if (and (logic~negation-p formula)
	   (nd-rips=forsome-p (car (data~appl-arguments  formula))))
      (let* ((qua (car (data~appl-arguments  formula)))
	     (quadom (nd-rips=gen-for-domain qua))
	     (quaran (nd-rips=gen-for-range qua)))
	(logic~quantification-create (nd-rips=logic-universal-quantor) quadom (data~appl-create (logic~negation-constant) quaran)))))

(defun nd-rips=prenex13b (formula)
  (if (and (logic~negation-p formula)
	   (nd-rips=forall-p (car (data~appl-arguments  formula))))
      (let* ((qua (car (data~appl-arguments  formula)))
	     (quadom (nd-rips=gen-for-domain qua))
	     (quaran (nd-rips=gen-for-range qua)))
	(logic~quantification-create (nd-rips=logic-existential-quantor) quadom (data~appl-create (logic~negation-constant) quaran)))))

(defun nd-rips=fold (function list start)
  (cond ((>= (length list) 1) (funcall function (car list) (nd-rips=fold function (cdr list) start)))
	('T start)))

(defun nd-rips=prenexform-step (formula)
 (nd-rips=fold (lambda (x y) (let ((result (funcall x formula)))
		(if (null result) y
		  result)))
       '(nd-rips=prenex10a
     nd-rips=prenex10b
      nd-rips=prenex10c 
	nd-rips=prenex10d 
  nd-rips=prenex11a 
  nd-rips=prenex11b 
  nd-rips=prenex11c 
  nd-rips=prenex11d 
  nd-rips=prenex12a 
  nd-rips=prenex12b 
  nd-rips=prenex12c 
  nd-rips=prenex12d
  nd-rips=prenex13a
  nd-rips=prenex13b) formula))


  
;;; historic version
(defun nd-rips=prenexform-recurse (formula)
  (cond ((data~appl-p formula)
	 (nd-rips=prenexform-step
	 (data~appl-create
	  (nd-rips=prenexform-recurse
	   (nd-rips=prenexform-step
	    (data~appl-function formula)))
	  (mapcar #'nd-rips=prenexform-recurse
		  (mapcar #'nd-rips=prenexform-step
			  (data~appl-arguments formula))))))
	 ('T (nd-rips=prenexform-step formula))))

(defun nd-rips=prenexform-recurse (formula)
  (cond ((data~appl-p formula)
	 (nd-rips=prenexform-step
	 (data~appl-create
	  (nd-rips=prenexform-step
	   (nd-rips=prenexform-recurse
	    (nd-rips=prenexform-step
	    (data~appl-function formula))))
	  (mapcar #'nd-rips=prenexform-recurse
		  (mapcar #'nd-rips=prenexform-step
			  (data~appl-arguments formula))))))
	((term~abstr-p formula)
	 (nd-rips=prenexform-step (term~abstr-create (data~abstr-domain formula) (nd-rips=prenexform-step (nd-rips=prenexform-recurse (nd-rips=prenexform-step (data~abstr-range formula)))))))
	 ('T (nd-rips=prenexform-step formula))))


(defun nd-rips=drop-leading-quants (formula)
      (if (or (nd-rips=forall-p formula) (nd-rips=forsome-p formula))
	  (nd-rips=drop-leading-quants (nd-rips=gen-for-range formula))
	formula
	))

(defun nd-rips=quant-free (formula)
  (cond ((or (nd-rips=forall-p formula) (nd-rips=forsome-p formula)) NIL)
	((data~appl-p formula) (and (nd-rips=quant-free (data~appl-function formula))
				    (every #'nd-rips=quant-free (data~appl-arguments formula))))
	((term~abstr-p formula) (nd-rips=quant-free (data~abstr-range formula)))
	('T 'T)))

(defun nd-rips=prenexform-fixpoint (formula)
  (let ((newformula (nd-rips=prenexform-recurse formula)))
    (if (nd-rips=quant-free (nd-rips=drop-leading-quants newformula)) newformula
      (nd-rips=prenexform-fixpoint newformula))))
	 


;;; Temporary Names

(defun nd-rips=introduce-tmp-names (formula vars) ;;; vars should be NIL in the beginning
   (cond ((nd-rips=forsome-p formula)
	  ; (format t "case1 vars: ~S~%" vars)
	  (let* ((qua (car (data~appl-arguments  formula)))
		 (quadom (data~abstr-domain qua))
		 (quaran (nd-rips=introduce-tmp-names (data~abstr-range qua) vars))
		 (sksymsdom (mapcar
			     (lambda (x)
			       (nd-rips=subscript-skolem-term
				(nd-rips=add-fresh-sksym-to-environment
				 ;; (gensym (format NIL "tmp_~S" (keim~name x)))
				 (gensym "TMP")
				 (data~annotation x)) vars)) quadom))
				       )
				 (data~replace-structs quaran quadom sksymsdom)))
	  ((nd-rips=forall-p formula) (let* ((qua (car (data~appl-arguments  formula)))
				     (quadom (data~abstr-domain qua))
				     (quaran (data~abstr-range qua)))
				(data~appl-create (data~appl-function formula) (term~abstr-create quadom (nd-rips=introduce-tmp-names quaran (append vars quadom))))))
	  ((data~appl-p formula)
	   (format t "case2~%")
	   (data~appl-create
	    (data~appl-function formula)
	    (mapcar (lambda (x) (nd-rips=introduce-tmp-names x vars))
		    (data~appl-arguments formula))))
	  ('T formula)))

;;; Drop Universal Quantifiers

(defun nd-rips=drop-leading-foralls (formula)
      (if (nd-rips=forall-p formula)
	  (nd-rips=drop-leading-foralls (nd-rips=gen-for-range formula))
	formula
	))

;;; Make a formula quantifier-free

(defun nd-rips=quantifier-free (formula)
      (nd-rips=drop-leading-foralls
       (nd-rips=introduce-tmp-names
	(nd-rips=prenexform-fixpoint
	 (nd-rips=unique-var-names formula))  NIL)))



(defun nd-rips=quantifier-free-tactic (line)
  (let* ((formula (node~formula line))
	 (newformula (nd-rips=quantifier-free formula)))
    (setf (node~formula line) newformula)))
	 

(com~defcommand RIPS-QUANTIFIER-FREE
		;; this is the definition of the command to execute the tactic in command interpreter
  (argnames line)
  (argtypes ndline)
  (arghelps "a line")
  (function nd-rips=quantifier-free-tactic)
      ;;; here we say which function will work on the input
  (frag-cats tactics base elimination)
  (log-p T)
  (help "Replaces functions of the form sk_ by proper skolem functions"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rips ANDI (nice version)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nd-rips=global-andi-matching-reset()
  (setf nd-rips*global-andi-matching (make-hash-table)))

(nd-rips=global-andi-matching-reset)

(defun nd-rips=andi-matching-set-candidate (tmpname)
  (setf (gethash tmpname nd-rips*global-andi-matching) 'CANDIDATE))

(defun nd-rips=andi-matching-candidate-p (tmpname)
  (if (equal (gethash tmpname nd-rips*global-andi-matching) 'CANDIDATE) 'T NIL))

(defun nd-rips=andi-matching-set (tmpname name)
  (setf (gethash tmpname nd-rips*global-andi-matching) name))
  
(defun nd-rips=andi-matching-get (tmp)
  (gethash tmp nd-rips*global-andi-matching))
  

(infer~deftactic RIPS-BCKW-ANDI
		 (outline-mappings (((existent nonexistent nonexistent) RIPS-BCKW-ANDI-b)))
		 (expansion-function nd-rips=expand-with-ATP)
		 (help "The RIPS tactic for and introduction - only to be used backwards."))



(defun nd-rips=bckw-andi-side (formula)
  (logic~conjunction-p (node~formula formula)))

(defun nd-rips=bckw-andi-compl (formula)
  (car (data~appl-arguments (node~formula formula))))

(defun nd-rips=bckw-andi-compr (formula)
  (let* ((left-conj (car (data~appl-arguments (node~formula formula))))
	 (right-conj (cadr (data~appl-arguments (node~formula formula))))
	 (tmpnames1  (nd-rips=formula-temporary-names right-conj))
	 (tmpnames2  (nd-rips=formula-temporary-names left-conj))
         (tmpnames (intersection tmpnames1 tmpnames2 :test #'nd-rips=term-equal)))
    (mapcan (lambda (x) (nd-rips=andi-matching-set-candidate x)) tmpnames)
    right-conj   
    ))

(tac~deftactic RIPS-BCKW-ANDI-b  RIPS-BCKW-ANDI(in base)
   (premises L2 L3)
   (conclusions L1)
   (computations (L2 (nd-rips=bckw-andi-compl L1))
		 (L3 (nd-rips=bckw-andi-compr L1)))
   (sideconditions (nd-rips=bckw-andi-side L1 ))
   (description "Application of Rips tactic ANDI."))

(com~defcommand RIPS-BCKW-ANDI
		;; this is the definition of the command to execute the tactic in command interpreter
  (argnames conjunction)
  (argtypes ndline)
  (arghelps "Conjunction")
  (function nd-rips=RIPS-BCKW-ANDI)
      ;;; here we say which function will work on the input
  (frag-cats tactics base elimination)
  (log-p T)
  (help "The RIPS tactic for and introduction."))


(defun nd-rips=RIPS-BCKW-ANDI (conjunction)
  (infer~compute-outline 'RIPS-BCKW-ANDI (list conjunction NIL NIL) nil))



;;;; representation of domains

(defun set-equal (lst1 lst2)
(and (subsetp lst1 lst2) (subsetp lst2 lst1))
)

(defun nd-rips=domain(proofline)
  (let* ((hypotheses (pdsn~hyps proofline))
	 (all-lines (prob~proof-steps omega*current-proof-plan)))
    (remove-if (lambda (x) (not (set-equal (pdsn~hyps x) hypotheses))) all-lines)))

(defun formula-holds-in-domain (formula domain)
  (some (lambda (x) (term~alpha-equal formula (node~formula x))) domain)) ;;;; the notion of equality should be checked

(defun formula-holds-under-hypotheses (formula hypslist)
   (let* ((support-lines (pds~support-nodes omega*current-proof-plan))
	  (lines-under-hypotheses (remove-if (lambda (x) (not (subsetp hypslist (pdsn~hyps x)))) support-lines))
	  (result (some (lambda (x) (term~alpha-equal formula (node~formula x)))lines-under-hypotheses)))
     (if (equal result NIL)
	 NIL
       'T)))

;;; --- a new attempt, according to my thesis :-) -- Marvin


; pds~support-nodes

  (defun nd-rips=domain (proofline)
    (let* ((all-lines (prob~proof-steps omega*current-proof-plan))
	  (all-postulated-lines (remove-if (lambda (x) (pdsn~open-node-p x)) all-lines)) ;;; remove open lines
	  ) 
      (remove-if (lambda (x) (not (set-equal (pds~node-supports x)  (pds~node-supports proofline)))) all-postulated-lines)))


   


