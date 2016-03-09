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

(eval-when (load compile eval)
  (unless (com~find-category 'learn)
    (com~defcategory learn
		     (help "Tactics of the theory learn ."))))


(defconstant learn*group-definition 'group)
(defconstant learn*associative-definition 'associative)
(defconstant learn*left-inverse-definition 'left-inverse)
(defconstant learn*right-inverse-definition 'right-inverse)
(defconstant learn*left-unit-definition 'left-unit)
(defconstant learn*right-unit-definition 'right-unit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Assoc-l
;;;
;;;  a o (b o c)
;;; ------------- assoc-l
;;;  (a o b) o c
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic assoc-l
		 (outline-mappings (((existent nonexistent) assoc-l-b)
				    ((nonexistent existent) assoc-l-f)
				    ((existent existent) assoc-l-a)))
		 (parameter-types position)
		 (expansion-function learntac=expand-assoc-l)
		 (help "Apply associativity left tactic in a certain direction."))


;;;; this rule is copied from Volker's file natural-tactics.thy, as we want to be
;;;; independent of that theory; this function should probably not be in the theory;;;; files
(defun learntac=get-rewrite-formula (Axiom)
  (declare (edited  "07-JUL-1997")
	   (authors Sorge)
	   (input   "An Axiom.")
	   (value   "Returns the equality line in the formula."))
  (let ((formula (th~ass-formula Axiom)))
    (data~struct-at-position formula
			     (pos~butlast
			      (data~position
			       formula
			       #'(lambda (x) (and (term~primitive-p x) (string-equal (keim~name x) '=))))))))

(defun learntac=compute-assoc-l-f (formula pos)
  (declare (edited  "16-MAY-2000")
	   (authors Mxj&Ceb)
	   (input   "formula is a premise line looking like T= P[(a o (b o c))]"
		    "pos is the position of the occurrence of (a o (b o c)) in T"  )
	   (effect  "None")
	   (value   "the formula constructed from this with associativity-rule"
		    "(a o (b o c)) = ((a o b) o c) given in theory learn"
                    "is P[((a o b) o c)]"))
  (let* ((term-at-position (data~struct-at-position formula pos))
	 (a-const (first (data~appl-arguments term-at-position)))
	 (b-const (first (data~appl-arguments (second (data~appl-arguments term-at-position)))))
	 (c-const (second (data~appl-arguments (second (data~appl-arguments term-at-position)))))
	 (op-const (data~appl-function term-at-position))
	 (assoc-axiom (th~find-assumption
		       ;;;;; associative rule global variable - see at the top of the file
		       learn*associative-definition
		       (prob~proof-theory omega*current-proof-plan)))
	 (assoc-rule (learntac=get-rewrite-formula assoc-axiom))
	 (left-side (first (data~appl-arguments assoc-rule)))
	 (right-side (second (data~appl-arguments assoc-rule)))
	 (a-var (first (data~appl-arguments left-side)))
	 (b-var (first (data~appl-arguments (second (data~appl-arguments left-side)))))
	 (c-var (second (data~appl-arguments (second (data~appl-arguments left-side)))))
	 (op-var (data~appl-function left-side))
	 (subst (subst~create (list op-var a-var b-var c-var)
			      (list op-const a-const b-const c-const))))
    (data~replace-at-position formula pos (subst~apply subst right-side))))


(defun learntac=compute-assoc-l-b (formula pos)
  (declare (edited  "16-MAY-2000")
	   (authors Mxj&Ceb)
	   (input   "formula is a conclusion line looking like T= P[((a o b) o c))]"
		    "pos is the position of the occurrence of ((a o b) o c) in T"  )
	   (effect  "None")
	   (value   "the formula constructed from this with associativity-rule"
		    "(a o (b o c)) = ((a o b) o c) given in theory learn"
                    "is P[(a o (b o c))]"))
  (let* ((term-at-position (data~struct-at-position formula pos))
	 (a-const (first (data~appl-arguments (first (data~appl-arguments term-at-position)))))
	 (b-const (second (data~appl-arguments (first (data~appl-arguments term-at-position)))))
	 (c-const (second (data~appl-arguments term-at-position)))
	 (op-const (data~appl-function term-at-position))
	 (assoc-axiom (th~find-assumption
		       ;;;;; associative rule global variable - see at the top of the file
		       learn*associative-definition
		       (prob~proof-theory omega*current-proof-plan)))
	 (assoc-rule (learntac=get-rewrite-formula assoc-axiom))
	 (left-side (first (data~appl-arguments assoc-rule)))
	 (right-side (second (data~appl-arguments assoc-rule)))
	 (a-var (first (data~appl-arguments (first (data~appl-arguments right-side)))))
	 (b-var (second (data~appl-arguments (first (data~appl-arguments right-side)))))
	 (c-var (second (data~appl-arguments right-side)))
	 (op-var (data~appl-function right-side))
	 (subst (subst~create (list op-var a-var b-var c-var)
			      (list op-const a-const b-const c-const))))
    (data~replace-at-position formula pos (subst~apply subst left-side))))


(defun learntac=compute-assoc-l-b-p (formula pos)
  (declare (edited  "17-MAY-2000")
	   (authors Mxj&Ceb)
	   (input   )
	   (effect  )
	   (value   ))
  (let ((term-at-position (data~struct-at-position formula pos)))
    (when (data~appl-p term-at-position) 
      (let* ((current-group-op (data~top term-at-position))
	     (all-group-hyps (learntac=get-group-hyps))
	     (all-group-ops (mapcar #'learntac=get-group-op all-group-hyps)))
	(and (some #'(lambda (x) (keim~equal x current-group-op)) all-group-ops)
	     (let* ((assoc-axiom (th~find-assumption
		       ;;;;; associative rule global variable - see at the top of the file
				  learn*associative-definition
				  (prob~proof-theory omega*current-proof-plan)))
		    (assoc-rule (learntac=get-rewrite-formula assoc-axiom))
		    (args (data~appl-arguments assoc-rule)))
	       (term~alpha-match (second args) term-at-position)))))))

(defun learntac=compute-assoc-l-f-p (formula pos)
  (declare (edited  "17-MAY-2000")
	   (authors Mxj&Ceb)
	   (input   )
	   (effect  )
	   (value   ))
  (let ((term-at-position (data~struct-at-position formula pos)))
    (when (data~appl-p term-at-position) 
      (let* ((current-group-op (data~top term-at-position))
	     (all-group-hyps (learntac=get-group-hyps))
	     (all-group-ops (mapcar #'learntac=get-group-op all-group-hyps)))
	(and (some #'(lambda (x) (keim~equal x current-group-op)) all-group-ops)
	     (let* ((assoc-axiom (th~find-assumption
				  ;;;;; associative rule global variable - see at the top of the file
				  learn*associative-definition
				  (prob~proof-theory omega*current-proof-plan)))
		    (assoc-rule (learntac=get-rewrite-formula assoc-axiom))
		    (args (data~appl-arguments assoc-rule)))
	       (term~alpha-match (first args) term-at-position)))))))


(defun learntac=get-group-hyps (&optional node)
  (let ((supports (if node (pds~node-supports node)
		    (remove-duplicates
		     ;;MP: took mex hours to get this destrucive effect on the supports!
		     ;(mapcan #'pds~node-supports (pds~open-nodes omega*current-proof-plan))
		     (mapcan #'(lambda (open)(copy-list (pds~node-supports open)))(pds~open-nodes omega*current-proof-plan))))))
    (remove-if-not #'(lambda (node)
		       (let ((top (data~top (node~formula node))))
		     ;;;; for learn*group-definition see top of this file
			 (equal learn*group-definition (keim~name top))))
		   supports)))

(defun learntac=get-associative-hyp (&optional (supports (pds~support-nodes
							     omega*current-proof-plan)))
  (let ((assoc-axiom (th~find-assumption
		      learn*associative-definition
		      (prob~proof-theory omega*current-proof-plan))))
    (find-if #'(lambda (node)
		     ;;;; for learn*associative-definition see top of this file
		 (equal (keim~name assoc-axiom)
			(keim~name (data~appl-function (node~formula node)))))
	     supports)))


(defun learntac=get-associative-hyps (&optional (supports (pds~support-nodes
							      omega*current-proof-plan)))
  (let ((assoc-axiom (th~find-assumption
		      learn*associative-definition
		      (prob~proof-theory omega*current-proof-plan))))
    (remove-if-not #'(lambda (node)
		     ;;;; for learn*associative-definition see top of this file
		       (equal (keim~name assoc-axiom)
			      (keim~name (data~appl-function (node~formula node)))))
		   supports)))


(defun learntac=get-group-op (node)
  (second (data~appl-arguments (node~formula node))))


(tac~deftactic assoc-l-b assoc-l (in learn)
	       (parameters (position pos+position "A position for the application."))
	       (premises (L2 "A premise line to be constructed by associativity left."))
	       (conclusions (L1 "An open line to apply associativity left rule to."))
	       (computations (L2 (learntac=compute-assoc-l-b (formula L1) position)))
	       (sideconditions (learntac=compute-assoc-l-b-p (formula L1) position))
	       (description "Backward application of associativity left."))


(tac~deftactic assoc-l-f assoc-l (in learn)
	       (parameters (position pos+position "A position for the application"))
	       (premises (L2 "An open line to apply associativity left rule to."))
	       (conclusions (L1 "A conclusion line to be constructed by associativity left."))
	       (computations (L1 (learntac=compute-assoc-l-f (formula L2) position)))
	       (sideconditions (learntac=compute-assoc-l-f-p (formula L2) position))
	       (description "Forward application of associativity left."))

(tac~deftactic assoc-l-a assoc-l (in learn)
	       (parameters (position pos+position "A position for the application"))
	       (premises (L2 "A premise line to apply associativity left rule to."))
	       (conclusions (L1 "A open line to apply associativity left rule to."))
	       (computations )
	       (sideconditions (learntac=compute-assoc-l-b-p (formula L1) position)
			       (learntac=compute-assoc-l-f-p (formula L2) position))
	       (description "Application of associativity left."))

(defun learntac=find-closing-hyp (node supports)
   (first (remove-if-not #'(lambda (x)
                             (term~alpha-equal (node~formula node) (node~formula x)))
                         supports)))

(defun learntac=expand-assoc-l (outline parameters)
  (declare (edited  "17-MAY-2000")
	   (authors Mxj&Ceb)
	   (input   )
	   (effect  )
	   (value   ))
  (let* ((premise (second outline))
	 (conc (first outline))
	 (conc-supports (pds~node-supports conc))
	 (pos (first parameters))
	 (term-at-pos (data~struct-at-position (node~formula premise) pos))
	 (current-group-op (data~appl-function term-at-pos))
	 (all-group-hyps (learntac=get-group-hyps conc))
	 (hyp-to-be-used (find-if #'(lambda (x) (keim~equal (learntac=get-group-op x)
							    current-group-op))
				  all-group-hyps))
	 (group-def (th~find-assumption  learn*group-definition
					 (prob~theory omega*current-proof-plan)))
	 (group-definiendum (th~definition-constant group-def))
	 (group-definiens (th~ass-node group-def))
	 (assoc-def (th~find-assumption  learn*associative-definition
					 (prob~theory omega*current-proof-plan)))
	 (assoc-definiendum (th~definition-constant assoc-def))
	 (assoc-definiens (th~ass-node assoc-def))
	 ;;; term-at-pos is looking like: a o (b o c)
	 (terms (list (first (data~appl-arguments term-at-pos)) ; a
		      (first (data~appl-arguments (second (data~appl-arguments term-at-pos)))) ; b
		      (second (data~appl-arguments (second (data~appl-arguments term-at-pos))))))) ;c
    (tacl~init outline)
    (tacl~sequence
     ((expanded-hyp dummy-premise4)
      ('defne (list nil hyp-to-be-used)
	(list group-definiendum group-definiens (pos~list-position '(0)))))
     ((ande*-res-lines dummy-premise6 dummy-premise7) ('ande* (list nil expanded-hyp) nil))
     ((expanded-hyp-asso dummy-premise5)
      ('defne (list nil (learntac=get-associative-hyp ande*-res-lines))
	(list assoc-definiendum assoc-definiens (pos~list-position '(0)))))
     ((foralle-conc1 dummy-premise1 sort-line1) ('foralle-sort
						 (list nil expanded-hyp-asso nil) (list (first terms))))
     ((foralle-conc2 dummy-premise2 sort-line2) ('foralle-sort
						 (list nil foralle-conc1 nil) (list (second terms))))
     ((foralle-conc3 dummy-premise3 sort-line3) ('foralle-sort
						 (list nil foralle-conc2 nil) (list (third terms))))
     (dummy-outline1 ('=subst (list conc premise foralle-conc3) (list pos)))
     (dummy-outline2 ('hypweaken
		      (list sort-line1 (learntac=find-closing-hyp sort-line1 conc-supports)) nil))
     (dummy-outline3 ('hypweaken
		      (list sort-line2 (learntac=find-closing-hyp sort-line2 conc-supports)) nil))
     (dummy-outline4 ('hypweaken
		      (list sort-line3 (learntac=find-closing-hyp sort-line3 conc-supports)) nil)))
    (tacl~end)))


(com~defcommand assoc-l
		(argnames conc prem position)
		(argtypes ndline ndline position)
		(arghelps "The substituted line" "The unsubstituted line"
			  "A position.")
		(function learntac=assoc-l)
		(frag-cats tactics base)
		(defaults ((oc~default-current-planline) (com~unspecified) (com~unspecified)))
		(level 1)
		(log-p T)
		(help "Associativity Left."))

(defun learntac=assoc-l (C P position)
  (infer~compute-outline 'assoc-l (list C P) (list position)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Assoc-r
;;;
;;;  (a o b) o c
;;; ------------- assoc-r
;;;  a o (b o c)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic assoc-r
		 (outline-mappings (((existent nonexistent) assoc-r-b)
				    ((nonexistent existent) assoc-r-f)
				    ((existent existent) assoc-r-a)))
		 (parameter-types position)
		 (expansion-function learntac=expand-assoc-r)
		 (help "Apply associativity right tactic in a certain direction."))




(defun learntac=compute-assoc-r-f (formula pos)
  (declare (edited  "16-MAY-2000")
	   (authors Mxj)
	   (input   "formula is a premise line looking like T= P[((a o b) o c)]"
		    "pos is the position of the occurrence of ((a o b) o c) in T"  )
	   (effect  "None")
	   (value   "the formula constructed from this with associativity-rule"
		    "(a o (b o c)) = ((a o b) o c) given in theory learn"
                    "is P[(a o (b o c))]"))
  (let* ((term-at-position (data~struct-at-position formula pos))
	 (a-const (first (data~appl-arguments (first (data~appl-arguments term-at-position)))))
	 (b-const (second (data~appl-arguments (first (data~appl-arguments term-at-position)))))
	 (c-const (second (data~appl-arguments term-at-position)))
	 (op-const (data~appl-function term-at-position))
	 (assoc-axiom (th~find-assumption
		       learn*associative-definition
		       (prob~proof-theory omega*current-proof-plan)))
	 (assoc-rule (learntac=get-rewrite-formula assoc-axiom))
	 (left-side (first (data~appl-arguments assoc-rule)))
	 (right-side (second (data~appl-arguments assoc-rule)))
	 (a-var (first (data~appl-arguments (first (data~appl-arguments right-side)))))
	 (b-var (second (data~appl-arguments (first (data~appl-arguments right-side)))))
	 (c-var (second (data~appl-arguments right-side)))
	 (op-var (data~appl-function right-side))
	 (subst (subst~create (list op-var a-var b-var c-var)
			      (list op-const a-const b-const c-const))))
    (data~replace-at-position formula pos (subst~apply subst left-side))))

(defun learntac=compute-assoc-r-b (formula pos)
  (declare (edited  "16-MAY-2000")
	   (authors Mxj)
	   (input   "formula is a conclusion line looking like T= P[(a o (b o c))]"
		    "pos is the position of the occurrence of (a o (b o c)) in T"  )
	   (effect  "None")
	   (value   "the formula constructed from this with associativity-rule"
		    "(a o (b o c)) = ((a o b) o c) given in theory learn"
                    "is P[((a o b) o c)]"))
  (let* ((term-at-position (data~struct-at-position formula pos))
	 (a-const (first (data~appl-arguments term-at-position)))
	 (b-const (first (data~appl-arguments (second (data~appl-arguments term-at-position)))))
	 (c-const (second (data~appl-arguments (second (data~appl-arguments term-at-position)))))
	 (op-const (data~appl-function term-at-position))
	 (assoc-axiom (th~find-assumption
       		       ;;;;; associative rule global variable - see at the top of the file
		       learn*associative-definition
		       (prob~proof-theory omega*current-proof-plan)))
	 (assoc-rule (learntac=get-rewrite-formula assoc-axiom))
	 (left-side (first (data~appl-arguments assoc-rule)))
	 (right-side (second (data~appl-arguments assoc-rule)))
	 (a-var (first (data~appl-arguments left-side)))
	 (b-var (first (data~appl-arguments (second (data~appl-arguments left-side)))))
	 (c-var (second (data~appl-arguments (second (data~appl-arguments left-side)))))
	 (op-var (data~appl-function left-side))
	 (subst (subst~create (list op-var a-var b-var c-var)
			      (list op-const a-const b-const c-const))))
    (data~replace-at-position formula pos (subst~apply subst right-side))))

(defun learntac=compute-assoc-r-b-p (formula pos)
  (declare (edited  "17-MAY-2000")
	   (authors Mxj&Ceb)
	   (input   )
	   (effect  )
	   (value   ))
  (let ((term-at-position (data~struct-at-position formula pos)))
    (when (data~appl-p term-at-position) 
      (let* ((current-group-op (data~top term-at-position))
	     (all-group-hyps (learntac=get-group-hyps))
	     (all-group-ops (mapcar #'learntac=get-group-op all-group-hyps)))
	(and (some #'(lambda (x) (keim~equal x current-group-op)) all-group-ops)
	     (let* ((assoc-axiom (th~find-assumption
  				  ;;;;; associative rule global variable - see at the top of the file
				  learn*associative-definition
				  (prob~proof-theory omega*current-proof-plan)))
		    (assoc-rule (learntac=get-rewrite-formula assoc-axiom))
		    (args (data~appl-arguments assoc-rule)))
	       (term~alpha-match (first args) term-at-position)))))))

(defun learntac=compute-assoc-r-f-p (formula pos)
  (declare (edited  "17-MAY-2000")
	   (authors Mxj&Ceb)
	   (input   )
	   (effect  )
	   (value   ))
  (let ((term-at-position (data~struct-at-position formula pos)))
    (when (data~appl-p term-at-position) 
      (let* ((current-group-op (data~top term-at-position))
	     (all-group-hyps (learntac=get-group-hyps))
	     (all-group-ops (mapcar #'learntac=get-group-op all-group-hyps)))
	(and (some #'(lambda (x) (keim~equal x current-group-op)) all-group-ops)
	     (let* ((assoc-axiom (th~find-assumption
 				  ;;;;; associative rule global variable - see at the top of the file
				  learn*associative-definition
				  (prob~proof-theory omega*current-proof-plan)))
		    (assoc-rule (learntac=get-rewrite-formula assoc-axiom))
		    (args (data~appl-arguments assoc-rule)))
	       (term~alpha-match (second args) term-at-position)))))))


(tac~deftactic assoc-r-b assoc-r (in learn)
	       (parameters (position pos+position "A position for the application."))
	       (premises (L2 "A premise line to be constructed by associativity right."))
	       (conclusions (L1 "An open line to apply associativity right rule to."))
	       (computations (L2 (learntac=compute-assoc-r-b (formula L1) position)))
	       (sideconditions (learntac=compute-assoc-r-b-p (formula L1) position))
	       (description "Backward application of associativity right."))


(tac~deftactic assoc-r-f assoc-r (in learn)
	       (parameters (position pos+position "A position for the application"))
	       (premises (L2 "An open line to apply associativity right rule to."))
	       (conclusions (L1 "A conclusion line to be constructed by associativity right."))
	       (computations (L1 (learntac=compute-assoc-r-f (formula L2) position)))
	       (sideconditions (learntac=compute-assoc-r-f-p (formula L2) position))
	       (description "Forward application of associativity right."))

(tac~deftactic assoc-r-a assoc-r (in learn)
	       (parameters (position pos+position "A position for the application"))
	       (premises (L2 "A premise line to apply associativity right rule to."))
	       (conclusions (L1 "A open line to apply associativity right rule to."))
	       (computations )
	       (sideconditions (learntac=compute-assoc-r-b-p (formula L1) position)
			       (learntac=compute-assoc-r-f-p (formula L2) position))
	       (description "Application of associativity right."))


(defun learntac=expand-assoc-r (outline parameters)
  (declare (edited  "17-MAY-2000")
	   (authors Mxj&Ceb)
	   (input   )
	   (effect  )
	   (value   ))
  (let* ((premise (second outline))
	 (conc (first outline))
	 (conc-supports (pds~node-supports conc))
	 (pos (first parameters))
	 (term-at-pos (data~struct-at-position (node~formula premise) pos))
	 (current-group-op (data~appl-function term-at-pos))
	 (all-group-hyps (learntac=get-group-hyps conc))
	 (hyp-to-be-used (find-if #'(lambda (x) (keim~equal (learntac=get-group-op x)
							    current-group-op))
				  all-group-hyps))
	 (group-def (th~find-assumption  learn*group-definition
					 (prob~theory omega*current-proof-plan)))
	 (group-definiendum (th~definition-constant group-def))
	 (group-definiens (th~ass-node group-def))
	 (assoc-def (th~find-assumption  learn*associative-definition
					 (prob~theory omega*current-proof-plan)))
	 (assoc-definiendum (th~definition-constant assoc-def))
	 (assoc-definiens (th~ass-node assoc-def))
	 ;;; term-at-pos is looking like: (a o b) o c
	 (terms (list (first (data~appl-arguments (first (data~appl-arguments term-at-pos)))) ; a
		      (second (data~appl-arguments (first (data~appl-arguments term-at-pos)))) ; b
		      (second (data~appl-arguments term-at-pos)) ; c
		      ))) 
    (tacl~init outline)
    (tacl~sequence
     ((expanded-hyp dummy-premise4) ('defne (list nil hyp-to-be-used)
				      (list group-definiendum group-definiens
					    (pos~list-position '(0)))))
     ((ande*-res-lines dummy-premise6 dummy-premise7) ('ande* (list nil expanded-hyp) nil))
     ((expanded-hyp-asso dummy-premise5)
      ('defne (list nil (learntac=get-associative-hyp ande*-res-lines))
	(list assoc-definiendum assoc-definiens (pos~list-position '(0)))))
     ((foralle-conc1 dummy-premise1 sort-line1) ('foralle-sort
						 (list nil expanded-hyp-asso nil) (list (first terms))))
     ((foralle-conc2 dummy-premise2 sort-line2) ('foralle-sort
						 (list nil foralle-conc1 nil) (list (second terms))))
     ((foralle-conc3 dummy-premise3 sort-line3) ('foralle-sort
						 (list nil foralle-conc2 nil) (list (third terms))))
     (dummy-outline1 ('=subst (list conc premise foralle-conc3) (list pos)))
     (dummy-outline2 ('hypweaken
		      (list sort-line1 (learntac=find-closing-hyp sort-line1 conc-supports)) nil))
     (dummy-outline3 ('hypweaken
		      (list sort-line2 (learntac=find-closing-hyp sort-line2 conc-supports)) nil))
     (dummy-outline4 ('hypweaken
		      (list sort-line3 (learntac=find-closing-hyp sort-line3 conc-supports)) nil)))
    (tacl~end)))

(com~defcommand assoc-r
		(argnames conc prem position)
		(argtypes ndline ndline position)
		(arghelps "The substituted line" "The unsubstituted line"
			  "A position.")
		(function learntac=assoc-r)
		(frag-cats tactics base)
		(defaults ((oc~default-current-planline) (com~unspecified) (com~unspecified)))
		(level 1)
		(log-p T)
		(help "Associativity Right."))

(defun learntac=assoc-r (C P position)
  (infer~compute-outline 'assoc-r (list C P) (list position)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Invl-E (inverse-left-elimination)
;;;
;;;  inv(a) o a
;;; ------------ invl-e
;;;       e
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic invl-e
		 (outline-mappings (((existent nonexistent) invl-e-b)
				    ((nonexistent existent) invl-e-f)
				    ((existent existent) invl-e-a)))
		 (parameter-types position)
		 (expansion-function learntac=expand-invl-e)
		 (help "Apply inverse left elimination tactic in a certain direction."))


(defun learntac=compute-invl-e-f (formula pos)
  (declare (edited  "16-MAY-2000")
	   (authors Mxj)
	   (input   "formula is a premise line looking like T= P[(inv(a) o a)]"
		    "pos is the position of the occurrence of (inv(a) o a) in T"  )
	   (effect  "None")
	   (value   "the formula constructed from this with inverse-rule"
		    "(inv(a) o a) = e given in theory learn"
                    "is P[e]"))
  (let* ((term-at-position (data~struct-at-position formula pos))
	 (a-const (first (data~appl-arguments (first (data~appl-arguments term-at-position)))))
	 (op-const (data~appl-function term-at-position))
	 (inv-const (data~appl-function (first (data~appl-arguments term-at-position))))
	 (all-group-hyps (learntac=get-group-hyps))
	 (hyp-to-be-used (find-if #'(lambda (x) (keim~equal (learntac=get-group-invfun x)
							    inv-const))
				  all-group-hyps))
	 (e-const (learntac=get-group-id hyp-to-be-used))
	 (inv-axiom (th~find-assumption
		       ;;;;; left-inverse rule global variable - see at the top of the file
		     learn*left-inverse-definition
		     (prob~proof-theory omega*current-proof-plan)))
	 (inv-rule (learntac=get-rewrite-formula inv-axiom))
	 (left-side (first (data~appl-arguments inv-rule)))
	 (right-side (second (data~appl-arguments inv-rule)))
	 (a-var (first (data~appl-arguments (first (data~appl-arguments left-side)))))
	 (e-var right-side)
	 (op-var (data~appl-function left-side))
	 (inv-var (data~appl-function (first (data~appl-arguments left-side))))
	 (subst (subst~create (list op-var inv-var a-var e-var)
			      (list op-const inv-const a-const e-const))))
    (data~replace-at-position formula pos (subst~apply subst right-side))))

(defun learntac=get-group-id (node)
  (third (data~appl-arguments (node~formula node))))

#|
(defun learntac=compute-invl-e-b (formula arg pos)
  (declare (edited  "16-MAY-2000")
	   (authors Mxj)
	   (input   "formula is a conclusion line looking like T= P[(e)]"
		    "pos is the position of the occurrence of (e) in T"
		    "arg is the term which will be replaced for formula")
	   (effect  "None")
	   (value   "the formula constructed from this with inverse-rule"
		    "(inv(a) o a) = e given in theory learn"
                    "is P[(inv(arg) o arg)]"))
  (let* ((inv-axiom (th~find-assumption
		     "left-inverse"
		     (prob~proof-theory omega*current-proof-plan)))
	 (inv-rule (learntac=get-rewrite-formula inv-axiom))
	 (args (data~appl-arguments inv-rule))
	 (new-formula (.....arg, first args)))
    (data~replace-at-position formula pos new-formula)))
|#

(defun learntac=compute-invl-e-f-p (formula pos)
  (let ((term-at-position (data~struct-at-position formula pos)))
    (when (and (data~appl-p term-at-position)
	       (data~appl-p (first (data~appl-arguments term-at-position))))
      (let* ((current-group-op (data~top term-at-position))
	     (current-group-invfun (data~top (first (data~appl-arguments term-at-position))))
	     (all-group-hyps (learntac=get-group-hyps))
	     (all-group-ops (mapcar #'learntac=get-group-op all-group-hyps))
	     (all-group-invfuns (mapcar #'learntac=get-group-invfun all-group-hyps))
	     (a1-const (first (data~appl-arguments (first (data~appl-arguments term-at-position)))))
	     (a2-const (second (data~appl-arguments term-at-position))))
	(and (some #'(lambda (x) (keim~equal x current-group-op)) all-group-ops)
	     (some #'(lambda (x) (keim~equal x current-group-invfun)) all-group-invfuns)
	     (keim~equal a1-const a2-const)
	     (let* ((inv-axiom (th~find-assumption
				;;;;; inverse rule global variable - see at the top of the file
				learn*left-inverse-definition
				(prob~proof-theory omega*current-proof-plan)))
		    (inv-rule (learntac=get-rewrite-formula inv-axiom))
		    (args (data~appl-arguments inv-rule)))
	       (term~alpha-match (first args) term-at-position)))))))

(defun learntac=get-group-invfun (node)
  (fourth (data~appl-arguments (node~formula node))))

#|
(defun learntac=compute-invl-e-b-p (formula arg pos)
  (let* ((term-at-position (data~struct-at-position formula pos))
	 (inv-axiom (th~find-assumption
		     "left-inverse"
		     (prob~proof-theory omega*current-proof-plan)))
	 (inv-rule (learntac=get-rewrite-formula inv-axiom))
	 (args (data~appl-arguments inv-rule))
	 (new-term (.... arg , first args)))    
    (term~alpha-match new-term term-at-position)))
;;;    (uni~syntactic-matcher (second args) term-at-position)
|#

(tac~deftactic invl-e-f invl-e (in learn)
	       (parameters (position pos+position "A position for the application"))
	       (premises (L2 "An open line to apply inverse left rule to."))
	       (conclusions (L1 "A conclusion line to be constructed by inverse left."))
	       (computations (L1 (learntac=compute-invl-e-f (formula L2) position)))
	       (sideconditions (learntac=compute-invl-e-f-p (formula L2) position))
	       (description "Forward application of inverse left."))

#|
(tac~deftactic invl-e-b invl-e (in learn)
	       (parameters (position pos+position "A position for the application."))
	       (premises (L2 "A premise line to be constructed by inverse left."))
	       (conclusions (L1 "An open line to apply inverse left rule to."))
	       (computations (L2 (learntac=compute-invl-e-b (formula L1) arg position)))
	       (sideconditions (learntac=compute-invl-e-b-p (formula L1) arg position))
	       (description "Backward application of inverse left."))

(tac~deftactic invl-e-a invl-e (in learn)
	       (parameters (position pos+position "A position for the application"))
	       (premises (L2 "A premise line to apply inverse left rule to."))
	       (conclusions (L1 "A open line to apply inverse left rule to."))
	       (computations )
	       (sideconditions (learntac=compute-invl-e-b-p (formula L1) arg position)
			       (learntac=compute-invl-e-f-p (formula L2) position))
	       (description "Application of inverse left."))

(defun learntac=expand-invl-e (outline parameters)
  (let* ((premise (second outline))
	 (conc (first outline))
	 (pos (first parameters))
	 (inv-axiom-line (tacl~insert&return-assumption 'learn 'left-inverse))
	 (term-at-pos (data~struct-at-position (node~formula premise) pos))
	 (terms
	  ))))
|#

(com~defcommand invl-e
		(argnames conc prem position)
		(argtypes ndline ndline position)
		(arghelps "The substituted line" "The unsubstituted line"
			  "A position.")
		(function learntac=invl-e)
		(frag-cats tactics base)
		(defaults ((oc~default-current-planline) (com~unspecified) (com~unspecified)))
		(level 1)
		(log-p T)
		(help "Inverse Left."))

(defun learntac=invl-e (C P position)
  (infer~compute-outline 'invl-e (list C P) (list position)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Invl-I (inverse-left-introduction)
;;;
;;;       e
;;; ------------ invl-i
;;;  inv(a) o a
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic invl-i
		 (outline-mappings (((existent nonexistent) invl-i-b)
				    ((nonexistent existent) invl-i-f)
				    ((existent existent) invl-i-a)))
		 (parameter-types position)
		 (expansion-function learntac=expand-invl-i)
		 (help "Apply inverse left introduction tactic in a certain direction."))

#|
(defun learntac=compute-invl-i-f (formula arg pos)
  (declare (edited  "16-MAY-2000")
	   (authors Mxj)
	   (input   "formula is a premise line looking like T= P[e]"
		    "pos is the position of the occurrence of (e) in T"
		    "arg is the term which will be replaced for formula")
	   (effect  "None")
	   (value   "the formula constructed from this with inverse-rule"
		    "(inv(a) o a) = e given in theory learn"
                    "is P[(inv(a) o a)]"))
  (let* ((inv-axiom (th~find-assumption
		     "left-inverse"
		     (prob~proof-theory omega*current-proof-plan)))
	 (inv-rule (learntac=get-rewrite-formula inv-axiom))
	 (args (data~appl-arguments inv-rule))
	 (new-formula (..... arg, first args)))
    (data~replace-at-position formula pos new-formula)))
|#
      
(defun learntac=compute-invl-i-b (formula pos)
  (declare (edited  "16-MAY-2000")
	   (authors Mxj)
	   (input   "formula is a conclusion line looking like T= P[(inv(a) o a)]"
		    "pos is the position of the occurrence of (inv(a) o a) in T")
	   (effect  "None")
	   (value   "the formula constructed from this with inverse-rule"
		    "(inv(a) o a) = e given in theory learn"
                    "is P[(e)]"))
  (let* ((term-at-position (data~struct-at-position formula pos))
	 (a-const (first (data~appl-arguments (first (data~appl-arguments term-at-position)))))
	 (op-const (data~appl-function term-at-position))
	 (inv-const (data~appl-function (first (data~appl-arguments term-at-position))))
	 (all-group-hyps (learntac=get-group-hyps))
	 (hyp-to-be-used (find-if #'(lambda (x) (keim~equal (learntac=get-group-invfun x)
							    inv-const))
				  all-group-hyps))
	 (e-const (learntac=get-group-id hyp-to-be-used))
	 (inv-axiom (th~find-assumption
		       ;;;;; left-inverse rule global variable - see at the top of the file
		     learn*left-inverse-definition
		     (prob~proof-theory omega*current-proof-plan)))
	 (inv-rule (learntac=get-rewrite-formula inv-axiom))
	 (left-side (first (data~appl-arguments inv-rule)))
	 (right-side (second (data~appl-arguments inv-rule)))
	 (a-var (first (data~appl-arguments (first (data~appl-arguments left-side)))))
	 (op-var (data~appl-function left-side))
	 (inv-var (data~appl-function (first (data~appl-arguments left-side))))
	 (e-var right-side)
	 (subst (subst~create (list op-var inv-var a-var e-var)
			      (list op-const inv-const a-const e-const))))
    (data~replace-at-position formula pos (subst~apply subst right-side))))

(defun learntac=compute-invl-i-b-p (formula pos)
(let ((term-at-position (data~struct-at-position formula pos)))
    (when (and (data~appl-p term-at-position)
	       (data~appl-p (first (data~appl-arguments term-at-position))))
      (let* ((current-group-op (data~top term-at-position))
	     (current-group-invfun (data~top (first (data~appl-arguments term-at-position))))
	     (all-group-hyps (learntac=get-group-hyps))
	     (all-group-ops (mapcar #'learntac=get-group-op all-group-hyps))
	     (all-group-invfuns (mapcar #'learntac=get-group-invfun all-group-hyps))
	     (a1-const (first (data~appl-arguments (first (data~appl-arguments term-at-position)))))
	     (a2-const (second (data~appl-arguments term-at-position))))
	(and (some #'(lambda (x) (keim~equal x current-group-op)) all-group-ops)
	     (some #'(lambda (x) (keim~equal x current-group-invfun)) all-group-invfuns)
	     (keim~equal a1-const a2-const)
	     (let* ((inv-axiom (th~find-assumption
				;;;;; inverse rule global variable - see at the top of the file
				learn*left-inverse-definition
				(prob~proof-theory omega*current-proof-plan)))
		    (inv-rule (learntac=get-rewrite-formula inv-axiom))
		    (args (data~appl-arguments inv-rule)))
	       (term~alpha-match (first args) term-at-position)))))))

#|
(defun learntac=compute-invl-i-f-p (formula arg pos)
  (let* ((term-at-position (data~struct-at-position formula pos))
	 (inv-axiom (th~find-assumption
		     "left-inverse"
		     (prob~proof-theory omega*current-proof-plan)))
	 (inv-rule (learntac=get-rewrite-formula inv-axiom))
	 (args (data~appl-arguments inv-rule))
	 (new-formula (..... arg, first args)))
    (term~alpha-match new-formula term-at-position)))
;;;    (uni~syntactic-matcher (first args) term-at-position)))


(tac~deftactic invl-i-f invl-i (in learn)
	       (parameters (position pos+position "A position for the application"))
	       (premises (L2 "An open line to apply inverse left rule to."))
	       (conclusions (L1 "A conclusion line to be constructed by inverse left."))
	       (computations (L1 (learntac=compute-invl-i-f (formula L2) arg position)))
	       (sideconditions (learntac=compute-invl-i-f-p (formula L2) arg position))
	       (description "Forward application of inverse left."))
|#

(tac~deftactic invl-i-b invl-i (in learn)
	       (parameters (position pos+position "A position for the application."))
	       (premises (L2 "A premise line to be constructed by inverse left."))
	       (conclusions (L1 "An open line to apply inverse left rule to."))
	       (computations (L2 (learntac=compute-invl-i-b (formula L1) position)))
	       (sideconditions (learntac=compute-invl-i-b-p (formula L1) position))
	       (description "Backward application of inverse left."))

#|
(tac~deftactic invl-i-a invl-i (in learn)
	       (parameters (position pos+position "A position for the application"))
	       (premises (L2 "A premise line to apply inverse left rule to."))
	       (conclusions (L1 "A open line to apply inverse left rule to."))
	       (computations )
	       (sideconditions (learntac=compute-invl-i-b-p (formula L1) position)
			       (learntac=compute-invl-i-f-p (formula L2) arg position))
	       (description "Application of inverse left."))


(defun learntac=expand-invl-i (outline parameters)
  (let* ((premise (second outline))
	 (conc (first outline))
	 (pos (first parameters))
	 (inv-axiom-line (tacl~insert&return-assumption 'learn 'left-inverse))
	 (term-at-pos (data~struct-at-position (node~formula premise) pos))
	 (terms
	  ))))
|#

(com~defcommand invl-i
		(argnames conc prem position)
		(argtypes ndline ndline position)
		(arghelps "The substituted line" "The unsubstituted line"
			  "A position.")
		(function learntac=invl-i)
		(frag-cats tactics base)
		(defaults ((oc~default-current-planline) (com~unspecified) (com~unspecified)))
		(level 1)
		(log-p T)
		(help "Inverse Left."))

(defun learntac=invl-i (C P position)
  (infer~compute-outline 'invl-i (list C P) (list position)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Invr-E (inverse-right-elimination)
;;;
;;;  a o inv(a)
;;; ------------ invr-e
;;;       e
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic invr-e
		 (outline-mappings (((existent nonexistent) invr-e-b)
				    ((nonexistent existent) invr-e-f)
				    ((existent existent) invr-e-a)))
		 (parameter-types position)
		 (expansion-function learntac=expand-invr-e)
		 (help "Apply inverse right elimination tactic in a certain direction."))


(defun learntac=compute-invr-e-f (formula pos)
  (declare (edited  "16-MAY-2000")
	   (authors Mxj)
	   (input   "formula is a premise line looking like T= P[(a o inv(a))]"
		    "pos is the position of the occurrence of (a o inv(a)) in T"  )
	   (effect  "None")
	   (value   "the formula constructed from this with inverse-rule"
		    "(a o inv(a)) = e given in theory learn"
                    "is P[e]"))
  (let* ((term-at-position (data~struct-at-position formula pos))
	 (a-const (first (data~appl-arguments term-at-position)))
	 (op-const (data~appl-function term-at-position))
	 (inv-const (data~appl-function (second (data~appl-arguments term-at-position))))
	 (all-group-hyps (learntac=get-group-hyps))
	 (hyp-to-be-used (find-if #'(lambda (x) (keim~equal (learntac=get-group-invfun x)
							    inv-const))
				  all-group-hyps))
	 (e-const (learntac=get-group-id hyp-to-be-used))
	 (inv-axiom (th~find-assumption
		       ;;;;; right-inverse rule global variable - see at the top of the file
		     learn*right-inverse-definition
		     (prob~proof-theory omega*current-proof-plan)))
	 (inv-rule (learntac=get-rewrite-formula inv-axiom))
	 (left-side (first (data~appl-arguments inv-rule)))
	 (right-side (second (data~appl-arguments inv-rule)))
	 (a-var (first (data~appl-arguments term-at-position)))
	 (op-var (data~appl-function term-at-position))
	 (inv-var (data~appl-function (second (data~appl-arguments term-at-position))))
	 (e-var right-side)
	 (subst (subst~create (list op-var inv-var a-var e-var)
			      (list op-const inv-const a-const e-const))))
    (data~replace-at-position formula pos (subst~apply subst right-side))))

#|
(defun learntac=compute-invr-e-b (formula arg pos)
  (declare (edited  "16-MAY-2000")
	   (authors Mxj)
	   (input   "formula is a conclusion line looking like T= P[(e)]"
		    "pos is the position of the occurrence of (e) in T"
		    "arg is the term which will be replaced for formula")
	   (effect  "None")
	   (value   "the formula constructed from this with inverse-rule"
		    "(inv(a) o a) = e given in theory learn"
                    "is P[(inv(arg) o arg)]"))
  (let* ((inv-axiom (th~find-assumption
		     "right-inverse"
		     (prob~proof-theory omega*current-proof-plan)))
	 (inv-rule (learntac=get-rewrite-formula inv-axiom))
	 (args (data~appl-arguments inv-rule))
	 (new-formula (.....arg, first args)))
    (data~replace-at-position formula pos new-formula)))
|#

(defun learntac=compute-invr-e-f-p (formula pos)
  (let ((term-at-position (data~struct-at-position formula pos)))
    (when (and (data~appl-p term-at-position)
	       (data~appl-p (second (data~appl-arguments term-at-position))))
      (let* ((current-group-op (data~top term-at-position))
	     (current-group-invfun (data~top (second (data~appl-arguments term-at-position))))
	     (all-group-hyps (learntac=get-group-hyps))
	     (all-group-ops (mapcar #'learntac=get-group-op all-group-hyps))
	     (all-group-invfuns (mapcar #'learntac=get-group-invfun all-group-hyps))
	     (a1-const (first (data~appl-arguments term-at-position)))
	     (a2-const (first (data~appl-arguments (second (data~appl-arguments term-at-position))))))
	(and (some #'(lambda (x) (keim~equal x current-group-op)) all-group-ops)
	     (some #'(lambda (x) (keim~equal x current-group-invfun)) all-group-invfuns)
	     (keim~equal a1-const a2-const)
	     (let* ((inv-axiom (th~find-assumption
				;;;;; inverse rule global variable - see at the top of the file
				learn*right-inverse-definition
				(prob~proof-theory omega*current-proof-plan)))
		    (inv-rule (learntac=get-rewrite-formula inv-axiom))
		    (args (data~appl-arguments inv-rule)))
	       (term~alpha-match (first args) term-at-position)))))))

#|
(defun learntac=compute-invr-e-b-p (formula arg pos)
  (let* ((term-at-position (data~struct-at-position formula pos))
	 (inv-axiom (th~find-assumption
		     "right-inverse"
		     (prob~proof-theory omega*current-proof-plan)))
	 (inv-rule (learntac=get-rewrite-formula inv-axiom))
	 (args (data~appl-arguments inv-rule))
	 (new-term (.... arg , first args)))    
    (term~alpha-match new-term term-at-position)))
;;;    (uni~syntactic-matcher (second args) term-at-position)
|#

(tac~deftactic invr-e-f invr-e (in learn)
	       (parameters (position pos+position "A position for the application"))
	       (premises (L2 "An open line to apply inverse right rule to."))
	       (conclusions (L1 "A conclusion line to be constructed by inverse right."))
	       (computations (L1 (learntac=compute-invr-e-f (formula L2) position)))
	       (sideconditions (learntac=compute-invr-e-f-p (formula L2) position))
	       (description "Forward application of inverse right."))

#|
(tac~deftactic invr-e-b invr-e (in learn)
	       (parameters (position pos+position "A position for the application."))
	       (premises (L2 "A premise line to be constructed by inverse right."))
	       (conclusions (L1 "An open line to apply inverse right rule to."))
	       (computations (L2 (learntac=compute-invr-e-b (formula L1) arg position)))
	       (sideconditions (learntac=compute-invr-e-b-p (formula L1) arg position))
	       (description "Backward application of inverse right."))

(tac~deftactic invr-e-a invr-e (in learn)
	       (parameters (position pos+position "A position for the application"))
	       (premises (L2 "A premise line to apply inverse right rule to."))
	       (conclusions (L1 "A open line to apply inverse right rule to."))
	       (computations )
	       (sideconditions (learntac=compute-invr-e-b-p (formula L1) arg position)
			       (learntac=compute-invr-e-f-p (formula L2) position))
	       (description "Application of inverse right."))

(defun learntac=expand-invr-e (outline parameters)
  (let* ((premise (second outline))
	 (conc (first outline))
	 (pos (first parameters))
	 (inv-axiom-line (tacl~insert&return-assumption 'learn 'right-inverse))
	 (term-at-pos (data~struct-at-position (node~formula premise) pos))
	 (terms
	  ))))
|#

(com~defcommand invr-e
		(argnames conc prem position)
		(argtypes ndline ndline position)
		(arghelps "The substituted line" "The unsubstituted line"
			  "A position.")
		(function learntac=invr-e)
		(frag-cats tactics base)
		(defaults ((oc~default-current-planline) (com~unspecified) (com~unspecified)))
		(level 1)
		(log-p T)
		(help "Inverse Right."))

(defun learntac=invr-e (C P position)
  (infer~compute-outline 'invr-e (list C P) (list position)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Invr-I (inverse-right-introduction)
;;;
;;;       e
;;; ------------ invr-i
;;;  a o inv(a)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic invr-i
		 (outline-mappings (((existent nonexistent) invr-i-b)
				    ((nonexistent existent) invr-i-f)
				    ((existent existent) invr-i-a)))
		 (parameter-types position)
		 (expansion-function learntac=expand-invr-i)
		 (help "Apply inverse right introduction tactic in a certain direction."))

#|
(defun learntac=compute-invr-i-f (formula arg pos)
  (declare (edited  "16-MAY-2000")
	   (authors Mxj)
	   (input   "formula is a premise line looking like T= P[e]"
		    "pos is the position of the occurrence of (e) in T"
		    "arg is the term which will be replaced for formula")
	   (effect  "None")
	   (value   "the formula constructed from this with inverse-rule"
		    "(inv(a) o a) = e given in theory learn"
                    "is P[(inv(a) o a)]"))
  (let* ((inv-axiom (th~find-assumption
		     "right-inverse"
		     (prob~proof-theory omega*current-proof-plan)))
	 (inv-rule (learntac=get-rewrite-formula inv-axiom))
	 (args (data~appl-arguments inv-rule))
	 (new-formula (..... arg, first args)))
    (data~replace-at-position formula pos new-formula)))
|#
      
(defun learntac=compute-invr-i-b (formula pos)
  (declare (edited  "16-MAY-2000")
	   (authors Mxj)
	   (input   "formula is a conclusion line looking like T= P[(a o (inv(a)))]"
		    "pos is the position of the occurrence of (a o inv(a)) in T")
	   (effect  "None")
	   (value   "the formula constructed from this with inverse-rule"
		    "(a o inv(a)) = e given in theory learn"
                    "is P[(e)]"))
  (let* ((term-at-position (data~struct-at-position formula pos))
	 (a-const (first (data~appl-arguments term-at-position)))
	 (op-const (data~appl-function term-at-position))
	 (inv-const (data~appl-function (second (data~appl-arguments term-at-position))))
	 (all-group-hyps (learntac=get-group-hyps))
	 (hyp-to-be-used (find-if #'(lambda (x) (keim~equal (learntac=get-group-invfun x)
							    inv-const))
				  all-group-hyps))
			   ;(all-group-id (mapcar #'learntac=get-group-id all-group-hyps)) xxx delete
	 (e-const (learntac=get-group-id hyp-to-be-used))
		  ;(first all-group-id)) ; we take the first, but could there be more? xxx fix it! delete
	 (inv-axiom (th~find-assumption
		       ;;;;; right-inverse rule global variable - see at the top of the file
		     learn*right-inverse-definition
		     (prob~proof-theory omega*current-proof-plan)))
	 (inv-rule (learntac=get-rewrite-formula inv-axiom))
	 (left-side (first (data~appl-arguments inv-rule)))
	 (right-side (second (data~appl-arguments inv-rule)))
	 (a-var (first (data~appl-arguments left-side)))
	 (op-var (data~appl-function left-side))
	 (inv-var (data~appl-function (second (data~appl-arguments left-side))))
	 (e-var right-side)
	 (subst (subst~create (list op-var inv-var a-var e-var)
			      (list op-const inv-const a-const e-const))))
    (data~replace-at-position formula pos (subst~apply subst right-side))))

(defun learntac=compute-invr-i-b-p (formula pos)
(let ((term-at-position (data~struct-at-position formula pos)))
    (when (and (data~appl-p term-at-position)
	       (data~appl-p (second (data~appl-arguments term-at-position))))
      (let* ((current-group-op (data~top term-at-position))
	     (current-group-invfun (data~top (second (data~appl-arguments term-at-position))))
	     (all-group-hyps (learntac=get-group-hyps))
	     (all-group-ops (mapcar #'learntac=get-group-op all-group-hyps))
	     (all-group-invfuns (mapcar #'learntac=get-group-invfun all-group-hyps))
	     (a1-const (first (data~appl-arguments term-at-position)))
	     (a2-const (first (data~appl-arguments (second (data~appl-arguments term-at-position))))))
	(and (some #'(lambda (x) (keim~equal x current-group-op)) all-group-ops)
	     (some #'(lambda (x) (keim~equal x current-group-invfun)) all-group-invfuns)
	     (keim~equal a1-const a2-const)
	     (let* ((inv-axiom (th~find-assumption
				;;;;; inverse rule global variable - see at the top of the file
				learn*right-inverse-definition
				(prob~proof-theory omega*current-proof-plan)))
		    (inv-rule (learntac=get-rewrite-formula inv-axiom))
		    (args (data~appl-arguments inv-rule)))
	       (term~alpha-match (first args) term-at-position)))))))

#|
(defun learntac=compute-invr-i-f-p (formula arg pos)
  (let* ((term-at-position (data~struct-at-position formula pos))
	 (inv-axiom (th~find-assumption
		     "right-inverse"
		     (prob~proof-theory omega*current-proof-plan)))
	 (inv-rule (learntac=get-rewrite-formula inv-axiom))
	 (args (data~appl-arguments inv-rule))
	 (new-formula (..... arg, first args)))
    (term~alpha-match new-formula term-at-position)))
;;;    (uni~syntactic-matcher (first args) term-at-position)))


(tac~deftactic invr-i-f invr-i (in learn)
	       (parameters (position pos+position "A position for the application"))
	       (premises (L2 "An open line to apply inverse right rule to."))
	       (conclusions (L1 "A conclusion line to be constructed by inverse right."))
	       (computations (L1 (learntac=compute-invr-i-f (formula L2) arg position)))
	       (sideconditions (learntac=compute-invr-i-f-p (formula L2) arg position))
	       (description "Forward application of inverse right."))
|#

(tac~deftactic invr-i-b invr-i (in learn)
	       (parameters (position pos+position "A position for the application."))
	       (premises (L2 "A premise line to be constructed by inverse right."))
	       (conclusions (L1 "An open line to apply inverse right rule to."))
	       (computations (L2 (learntac=compute-invr-i-b (formula L1) position)))
	       (sideconditions (learntac=compute-invr-i-b-p (formula L1) position))
	       (description "Backward application of inverse right."))

#|
(tac~deftactic invr-i-a invr-i (in learn)
	       (parameters (position pos+position "A position for the application"))
	       (premises (L2 "A premise line to apply inverse right rule to."))
	       (conclusions (L1 "A open line to apply inverse right rule to."))
	       (computations )
	       (sideconditions (learntac=compute-invr-i-b-p (formula L1) position)
			       (learntac=compute-invr-i-f-p (formula L2) arg position))
	       (description "Application of inverse right."))


(defun learntac=expand-invr-i (outline parameters)
  (let* ((premise (second outline))
	 (conc (first outline))
	 (pos (first parameters))
	 (inv-axiom-line (tacl~insert&return-assumption 'learn 'right-inverse))
	 (term-at-pos (data~struct-at-position (node~formula premise) pos))
	 (terms
	  ))))
|#

(com~defcommand invr-i
		(argnames conc prem position)
		(argtypes ndline ndline position)
		(arghelps "The substituted line" "The unsubstituted line"
			  "A position.")
		(function learntac=invr-i)
		(frag-cats tactics base)
		(defaults ((oc~default-current-planline) (com~unspecified) (com~unspecified)))
		(level 1)
		(log-p T)
		(help "Inverse Right."))

(defun learntac=invr-i (C P position)
  (infer~compute-outline 'invr-i (list C P) (list position)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Idl-E (unit-left-elimination)
;;;
;;;  e o a
;;; ------- idl-e
;;;    a
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic idl-e
		 (outline-mappings (((existent nonexistent) idl-e-b)
				    ((nonexistent existent) idl-e-f)
				    ((existent existent) idl-e-a)))
		 (parameter-types position)
		 (expansion-function learntac=expand-idl-e)
		 (help "Apply unit left elimination tactic in a certain direction."))


(defun learntac=compute-idl-e-f (formula pos)
  (declare (edited  "16-MAY-2000")
	   (authors Mxj)
	   (input   "formula is a premise line looking like T= P[(e o a)]"
		    "pos is the position of the occurrence of (e o a) in T"  )
	   (effect  "None")
	   (value   "the formula constructed from this with unit-rule"
		    "(e o a) = a given in theory learn"
                    "is P[a]"))
  (let* ((term-at-position (data~struct-at-position formula pos))
	 (a-const (second (data~appl-arguments term-at-position)))
	 (e-const (first (data~appl-arguments term-at-position)))
	 (op-const (data~appl-function term-at-position))
	 (unit-axiom (th~find-assumption
		       ;;;;; left-unit rule global variable - see at the top of the file
		     learn*left-unit-definition
		     (prob~proof-theory omega*current-proof-plan)))
	 (unit-rule (learntac=get-rewrite-formula unit-axiom))
	 (left-side (first (data~appl-arguments unit-rule)))
	 (right-side (second (data~appl-arguments unit-rule)))
	 (a-var (second (data~appl-arguments left-side)))
	 (e-var (first (data~appl-arguments left-side)))
	 (op-var (data~appl-function left-side))
	 (subst (subst~create (list op-var e-var a-var e-var)
			      (list op-const e-const a-const e-const))))
    (data~replace-at-position formula pos (subst~apply subst right-side))))

#|
(defun learntac=compute-idl-e-b (formula pos)
  (declare (edited  "16-MAY-2000")
	   (authors Mxj)
	   (input   "formula is a conclusion line looking like T= P[(a)]"
		    "pos is the position of the occurrence of (e) in T")
	   (effect  "None")
	   (value   "the formula constructed from this with unit-rule"
		    "(e o a) = a given in theory learn"
                    "is P[(e o a)]"))
  (let* ((term-at-position (data~struct-at-position formula pos))
	 (a-const term-at-position)
	 (all-group-hyps (learntac=get-group-hyps)) ;;;; xxxxxxxxxxxxxxxxxxx need to find
	                                               ;;;; line (G a) and then from this find
	                                               ;;;; the lien (group G ....) to get out
	                                               ;;;; the appropriate id
	 (hyp-to-be-used (find-if #'(lambda (x) (keim~equal (learntac=get-group-invfun x)
							    inv-const))
				  all-group-hyps))
	 (e-const (learntac=get-group-id hyp-to-be-used))
	 (all-group-ops (mapcar #'learntac=get-group-op all-group-hyps))
	 (op-const (first all-group-ops)) ; we take the first, but could there be more?
	 (unit-axiom (th~find-assumption
		       ;;;;; left-unit rule global variable - see at the top of the file
		      learn*left-unit-definition
		      (prob~proof-theory omega*current-proof-plan)))
	 (unit-rule (learntac=get-rewrite-formula unit-axiom))
	 (left-side (first (data~appl-arguments unit-rule)))
	 (right-side (second (data~appl-arguments unit-rule)))
	 (a-var (second (data~appl-arguments left-side)))
	 (e-var (first (data~appl-arguments left-side)))
	 (op-var (data~appl-function left-side))
	 (subst (subst~create (list op-var unit-var a-var e-var)
			      (list op-const unit-const a-const e-const))))
    (data~replace-at-position formula pos (subst~apply subst right-side))))
|#

(defun learntac=compute-idl-e-f-p (formula pos)
  (let ((term-at-position (data~struct-at-position formula pos)))
    (when (data~appl-p term-at-position)
      (let* ((current-group-op (data~top term-at-position))
	     (current-group-id (first (data~appl-arguments term-at-position)))
	     (all-group-hyps (learntac=get-group-hyps))
	     (all-group-ops (mapcar #'learntac=get-group-op all-group-hyps))
	     (all-group-ids (mapcar #'learntac=get-group-id all-group-hyps)))
	(and (some #'(lambda (x) (keim~equal x current-group-op)) all-group-ops)
	     (some #'(lambda (x) (keim~equal x current-group-id)) all-group-ids)
	     (let* ((unit-axiom (th~find-assumption
				;;;;; unit rule global variable - see at the top of the file
				 learn*left-unit-definition
				 (prob~proof-theory omega*current-proof-plan)))
		    (unit-rule (learntac=get-rewrite-formula unit-axiom))
		    (args (data~appl-arguments unit-rule)))
	       (term~alpha-match (first args) term-at-position)))))))

#|
(defun learntac=compute-idl-e-b-p (formula arg pos)
  (let* ((term-at-position (data~struct-at-position formula pos))
	 (unit-axiom (th~find-assumption
				;;;;; unit rule global variable - see at the top of the file
		      learn*left-unit-definition
		      (prob~proof-theory omega*current-proof-plan)))
	 (unit-rule (learntac=get-rewrite-formula unit-axiom))
	 (args (data~appl-arguments unit-rule)))
    (term~alpha-match (first args) term-at-position)))
|#

(tac~deftactic idl-e-f idl-e (in learn)
	       (parameters (position pos+position "A position for the application"))
	       (premises (L2 "An open line to apply unit left rule to."))
	       (conclusions (L1 "A conclusion line to be constructed by unit left."))
	       (computations (L1 (learntac=compute-idl-e-f (formula L2) position)))
	       (sideconditions (learntac=compute-idl-e-f-p (formula L2) position))
	       (description "Forward application of unit left."))

#|
(tac~deftactic idl-e-b idl-e (in learn)
	       (parameters (position pos+position "A position for the application."))
	       (premises (L2 "A premise line to be constructed by unit left."))
	       (conclusions (L1 "An open line to apply unit left rule to."))
	       (computations (L2 (learntac=compute-idl-e-b (formula L1) position)))
	       (sideconditions (learntac=compute-idl-e-b-p (formula L1) position))
	       (description "Backward application of unit left."))

(tac~deftactic idl-e-a idl-e (in learn)
	       (parameters (position pos+position "A position for the application"))
	       (premises (L2 "A premise line to apply unit left rule to."))
	       (conclusions (L1 "A open line to apply unit left rule to."))
	       (computations )
	       (sideconditions (learntac=compute-idl-e-b-p (formula L1) position)
			       (learntac=compute-idl-e-f-p (formula L2) position))
	       (description "Application of unit left."))

|#
#|
(defun learntac=expand-idl-e (outline parameters)
  (let* ((premise (second outline))
	 (conc (first outline))
	 (pos (first parameters))
	 (unit-axiom-line (tacl~insert&return-assumption 'learn 'left-unit))
	 (term-at-pos (data~struct-at-position (node~formula premise) pos))
	 (terms
	  ))))
|#

(com~defcommand idl-e
		(argnames conc prem position)
		(argtypes ndline ndline position)
		(arghelps "The substituted line" "The unsubstituted line"
			  "A position.")
		(function learntac=idl-e)
		(frag-cats tactics base)
		(defaults ((oc~default-current-planline) (com~unspecified) (com~unspecified)))
		(level 1)
		(log-p T)
		(help "Unit Left."))

(defun learntac=idl-e (C P position)
  (infer~compute-outline 'idl-e (list C P) (list position)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Idl-I (unit-left-introduction)
;;;
;;;    a 
;;; ------- idl-i
;;;  e o a
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~deftactic idl-i
		 (outline-mappings (((existent nonexistent) idl-i-b)
				    ((nonexistent existent) idl-i-f)
				    ((existent existent) idl-i-a)))
		 (parameter-types position)
		 (expansion-function learntac=expand-idl-i)
		 (help "Apply unit left introduction tactic in a certain direction."))


(defun learntac=compute-idl-i-b (formula pos)
  (declare (edited  "16-MAY-2000")
	   (authors Mxj)
	   (input   "formula is a conclusion line looking like  T= P [(e o a)]"
		    "pos is the position of the occurrence of (e o a) in T"  )
	   (effect  "None")
	   (value   "the formula constructed from this with unit-rule"
		    "(e o a) = a given in theory learn"
                    "is P[(a)]"))
  (let* ((term-at-position (data~struct-at-position formula pos))
	 (a-const (second (data~appl-arguments term-at-position)))
	 (e-const (first (data~appl-arguments term-at-position)))
	 (op-const (data~appl-function term-at-position))
	 (unit-axiom (th~find-assumption
		       ;;;;; left-unit rule global variable - see at the top of the file
		     learn*left-unit-definition
		     (prob~proof-theory omega*current-proof-plan)))
	 (unit-rule (learntac=get-rewrite-formula unit-axiom))
	 (left-side (first (data~appl-arguments unit-rule)))
	 (right-side (second (data~appl-arguments unit-rule)))
	 (a-var (second (data~appl-arguments left-side)))
	 (e-var (first (data~appl-arguments left-side)))
	 (op-var (data~appl-function left-side))
	 (subst (subst~create (list op-var e-var a-var e-var)
			      (list op-const e-const a-const e-const))))
    (data~replace-at-position formula pos (subst~apply subst right-side))))

#|
(defun learntac=compute-idl-i-f (formula pos)
  (declare (edited  "16-MAY-2000")
	   (authors Mxj)
	   (input   "formula is a premise line looking like T= P[(a)]"
		    "pos is the position of the occurrence of (a) in T")
	   (effect  "None")
	   (value   "the formula constructed from this with unit-rule"
		    "(e o a) = a given in theory learn"
                    "is P[(e o a)]"))
  (let* ((term-at-position (data~struct-at-position formula pos))
	 (a-const term-at-position)
	 (all-group-hyps (learntac=get-group-hyps))
	 (all-group-id (mapcar #'learntac=get-group-id all-group-hyps))
	 (e-const (first all-group-id)) ; we take the first, but could there be more?
	 (all-group-ops (mapcar #'learntac=get-group-op all-group-hyps))
	 (op-const (first all-group-ops)) ; we take the first, but could there be more?
	 (unit-axiom (th~find-assumption
		       ;;;;; left-unit rule global variable - see at the top of the file
		      learn*left-unit-definition
		      (prob~proof-theory omega*current-proof-plan)))
	 (unit-rule (learntac=get-rewrite-formula unit-axiom))
	 (left-side (first (data~appl-arguments unit-rule)))
	 (right-side (second (data~appl-arguments unit-rule)))
	 (a-var (second (data~appl-arguments left-side)))
	 (e-var (first (data~appl-arguments left-side)))
	 (op-var (data~appl-function left-side))
	 (subst (subst~create (list op-var unit-var a-var e-var)
			      (list op-const unit-const a-const e-const))))
    (data~replace-at-position formula pos (subst~apply subst right-side))))


(defun learntac=compute-idl-i-f-p (formula pos)
  (let* ((term-at-position (data~struct-at-position formula pos))
	 (unit-axiom (th~find-assumption
				;;;;; unit rule global variable - see at the top of the file
		      learn*left-unit-definition
		      (prob~proof-theory omega*current-proof-plan)))
	 (unit-rule (learntac=get-rewrite-formula unit-axiom))
	 (args (data~appl-arguments unit-rule)))
    (term~alpha-match (first args) term-at-position)))
|#

(defun learntac=compute-idl-i-b-p (formula pos)
  (let ((term-at-position (data~struct-at-position formula pos)))
    (when (data~appl-p term-at-position)
      (let* ((current-group-op (data~top term-at-position))
	     (current-group-id (first (data~appl-arguments term-at-position)))
	     (all-group-hyps (learntac=get-group-hyps))
	     (all-group-ops (mapcar #'learntac=get-group-op all-group-hyps))
	     (all-group-ids (mapcar #'learntac=get-group-id all-group-hyps)))
	(and (some #'(lambda (x) (keim~equal x current-group-op)) all-group-ops)
	     (some #'(lambda (x) (keim~equal x current-group-id)) all-group-ids)
	     (let* ((unit-axiom (th~find-assumption
				;;;;; unit rule global variable - see at the top of the file
				 learn*left-unit-definition
				 (prob~proof-theory omega*current-proof-plan)))
		    (unit-rule (learntac=get-rewrite-formula unit-axiom))
		    (args (data~appl-arguments unit-rule)))
	       (term~alpha-match (first args) term-at-position)))))))

#|
(tac~deftactic idl-i-f idl-i (in learn)
	       (parameters (position pos+position "A position for the application"))
	       (premises (L2 "An open line to apply unit left rule to."))
	       (conclusions (L1 "A conclusion line to be constructed by unit left."))
	       (computations (L1 (learntac=compute-idl-i-f (formula L2) position)))
	       (sideconditions (learntac=compute-idl-i-f-p (formula L2) position))
	       (description "Forward application of unit left."))

|#

(tac~deftactic idl-i-b idl-i (in learn)
	       (parameters (position pos+position "A position for the application."))
	       (premises (L2 "A premise line to be constructed by unit left."))
	       (conclusions (L1 "An open line to apply unit left rule to."))
	       (computations (L2 (learntac=compute-idl-i-b (formula L1) position)))
	       (sideconditions (learntac=compute-idl-i-b-p (formula L1) position))
	       (description "Backward application of unit left."))

#|
(tac~deftactic idl-i-a idl-i (in learn)
	       (parameters (position pos+position "A position for the application"))
	       (premises (L2 "A premise line to apply unit left rule to."))
	       (conclusions (L1 "A open line to apply unit left rule to."))
	       (computations )
	       (sideconditions (learntac=compute-idl-i-b-p (formula L1) position)
			       (learntac=compute-idl-i-f-p (formula L2) position))
	       (description "Application of unit left."))

|#
#|
(defun learntac=expand-idl-i (outline parameters)
  (let* ((premise (second outline))
	 (conc (first outline))
	 (pos (first parameters))
	 (unit-axiom-line (tacl~insert&return-assumption 'learn 'left-unit))
	 (term-at-pos (data~struct-at-position (node~formula premise) pos))
	 (terms
	  ))))
|#

(com~defcommand idl-i
		(argnames conc prem position)
		(argtypes ndline ndline position)
		(arghelps "The substituted line" "The unsubstituted line"
			  "A position.")
		(function learntac=idl-i)
		(frag-cats tactics base)
		(defaults ((oc~default-current-planline) (com~unspecified) (com~unspecified)))
		(level 1)
		(log-p T)
		(help "Unit Left."))

(defun learntac=idl-i (C P position)
  (infer~compute-outline 'idl-i (list C P) (list position)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Idr-E (unit-right-elimination)
;;;
;;;  a o e
;;; ------- idr-e
;;;    a
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic idr-e
		 (outline-mappings (((existent nonexistent) idr-e-b)
				    ((nonexistent existent) idr-e-f)
				    ((existent existent) idr-e-a)))
		 (parameter-types position)
		 (expansion-function learntac=expand-idr-e)
		 (help "Apply unit right elimination tactic in a certain direction."))


(defun learntac=compute-idr-e-f (formula pos)
  (declare (edited  "16-MAY-2000")
	   (authors Mxj)
	   (input   "formula is a premise line looking like T= P[(a o e)]"
		    "pos is the position of the occurrence of (a o e) in T"  )
	   (effect  "None")
	   (value   "the formula constructed from this with unit-rule"
		    "(a o e) = a given in theory learn"
                    "is P[a]"))
  (let* ((term-at-position (data~struct-at-position formula pos))
	 (a-const (first (data~appl-arguments term-at-position)))
	 (e-const (second (data~appl-arguments term-at-position)))
	 (op-const (data~appl-function term-at-position))
	 (unit-axiom (th~find-assumption
		       ;;;;; right-unit rule global variable - see at the top of the file
		     learn*right-unit-definition
		     (prob~proof-theory omega*current-proof-plan)))
	 (unit-rule (learntac=get-rewrite-formula unit-axiom))
	 (left-side (first (data~appl-arguments unit-rule)))
	 (right-side (second (data~appl-arguments unit-rule)))
	 (a-var (first (data~appl-arguments left-side)))
	 (e-var (second (data~appl-arguments left-side)))
	 (op-var (data~appl-function left-side))
	 (subst (subst~create (list op-var e-var a-var e-var)
			      (list op-const e-const a-const e-const))))
    (data~replace-at-position formula pos (subst~apply subst right-side))))

#|
(defun learntac=compute-idr-e-b (formula pos)
  (declare (edited  "16-MAY-2000")
	   (authors Mxj)
	   (input   "formula is a conclusion line looking like T= P[(a)]"
		    "pos is the position of the occurrence of (a) in T")
	   (effect  "None")
	   (value   "the formula constructed from this with unit-rule"
		    "(a o e) = a given in theory learn"
                    "is P[(a o e)]"))
  (let* ((term-at-position (data~struct-at-position formula pos))
	 (a-const term-at-position)
	 (all-group-hyps (learntac=get-group-hyps)) ;;;; xxxxxxxxxxxxxxxxxxx need to find
	                                               ;;;; line (G a) and then from this find
	                                               ;;;; the lien (group G ....) to get out
	                                               ;;;; the appropriate id
	 (hyp-to-be-used (find-if #'(lambda (x) (keim~equal (learntac=get-group-invfun x)
							    inv-const))
				  all-group-hyps))
	 (e-const (learntac=get-group-id hyp-to-be-used))
	 (all-group-ops (mapcar #'learntac=get-group-op all-group-hyps))
	 (op-const (first all-group-ops)) ; we take the first, but could there be more?
	 (unit-axiom (th~find-assumption
		       ;;;;; right-unit rule global variable - see at the top of the file
		      learn*right-unit-definition
		      (prob~proof-theory omega*current-proof-plan)))
	 (unit-rule (learntac=get-rewrite-formula unit-axiom))
	 (left-side (first (data~appl-arguments unit-rule)))
	 (right-side (second (data~appl-arguments unit-rule)))
	 (a-var (second (data~appl-arguments left-side)))
	 (e-var (first (data~appl-arguments left-side)))
	 (op-var (data~appl-function left-side))
	 (subst (subst~create (list op-var unit-var a-var e-var)
			      (list op-const unit-const a-const e-const))))
    (data~replace-at-position formula pos (subst~apply subst right-side))))
|#

(defun learntac=compute-idr-e-f-p (formula pos)
  (let ((term-at-position (data~struct-at-position formula pos)))
    (when (data~appl-p term-at-position)
      (let* ((current-group-op (data~top term-at-position))
	     (current-group-id (second (data~appl-arguments term-at-position)))
	     (all-group-hyps (learntac=get-group-hyps))
	     (all-group-ops (mapcar #'learntac=get-group-op all-group-hyps))
	     (all-group-ids (mapcar #'learntac=get-group-id all-group-hyps)))
	(and (some #'(lambda (x) (keim~equal x current-group-op)) all-group-ops)
	     (some #'(lambda (x) (keim~equal x current-group-id)) all-group-ids)
	     (let* ((unit-axiom (th~find-assumption
				;;;;; unit rule global variable - see at the top of the file
				 learn*right-unit-definition
				 (prob~proof-theory omega*current-proof-plan)))
		    (unit-rule (learntac=get-rewrite-formula unit-axiom))
		    (args (data~appl-arguments unit-rule)))
	       (term~alpha-match (first args) term-at-position)))))))

#|
(defun learntac=compute-idr-e-b-p (formula arg pos)
  (let* ((term-at-position (data~struct-at-position formula pos))
	 (unit-axiom (th~find-assumption
				;;;;; unit rule global variable - see at the top of the file
		      learn*right-unit-definition
		      (prob~proof-theory omega*current-proof-plan)))
	 (unit-rule (learntac=get-rewrite-formula unit-axiom))
	 (args (data~appl-arguments unit-rule)))
    (term~alpha-match (first args) term-at-position)))
|#

(tac~deftactic idr-e-f idr-e (in learn)
	       (parameters (position pos+position "A position for the application"))
	       (premises (L2 "An open line to apply unit right rule to."))
	       (conclusions (L1 "A conclusion line to be constructed by unit right."))
	       (computations (L1 (learntac=compute-idr-e-f (formula L2) position)))
	       (sideconditions (learntac=compute-idr-e-f-p (formula L2) position))
	       (description "Forward application of unit right."))

#|
(tac~deftactic idr-e-b idr-e (in learn)
	       (parameters (position pos+position "A position for the application."))
	       (premises (L2 "A premise line to be constructed by unit right."))
	       (conclusions (L1 "An open line to apply unit right rule to."))
	       (computations (L2 (learntac=compute-idr-e-b (formula L1) position)))
	       (sideconditions (learntac=compute-idr-e-b-p (formula L1) position))
	       (description "Backward application of unit right."))

(tac~deftactic idr-e-a idr-e (in learn)
	       (parameters (position pos+position "A position for the application"))
	       (premises (L2 "A premise line to apply unit right rule to."))
	       (conclusions (L1 "A open line to apply unit right rule to."))
	       (computations )
	       (sideconditions (learntac=compute-idr-e-b-p (formula L1) position)
			       (learntac=compute-idr-e-f-p (formula L2) position))
	       (description "Application of unit right."))

|#
#|
(defun learntac=expand-idr-e (outline parameters)
  (let* ((premise (second outline))
	 (conc (first outline))
	 (pos (first parameters))
	 (unit-axiom-line (tacl~insert&return-assumption 'learn 'right-unit))
	 (term-at-pos (data~struct-at-position (node~formula premise) pos))
	 (terms
	  ))))
|#

(com~defcommand idr-e
		(argnames conc prem position)
		(argtypes ndline ndline position)
		(arghelps "The substituted line" "The unsubstituted line"
			  "A position.")
		(function learntac=idr-e)
		(frag-cats tactics base)
		(defaults ((oc~default-current-planline) (com~unspecified) (com~unspecified)))
		(level 1)
		(log-p T)
		(help "Unit Right."))

(defun learntac=idr-e (C P position)
  (infer~compute-outline 'idr-e (list C P) (list position)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Idr-I (unit-right-introduction)
;;;
;;;    a 
;;; ------- idr-i
;;;  a o e
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~deftactic idr-i
		 (outline-mappings (((existent nonexistent) idr-i-b)
				    ((nonexistent existent) idr-i-f)
				    ((existent existent) idr-i-a)))
		 (parameter-types position)
		 (expansion-function learntac=expand-idr-i)
		 (help "Apply unit right introduction tactic in a certain direction."))


(defun learntac=compute-idr-i-b (formula pos)
  (declare (edited  "16-MAY-2000")
	   (authors Mxj)
	   (input   "formula is a conclusion line looking like  T= P [(a o e)]"
		    "pos is the position of the occurrence of (a o e) in T"  )
	   (effect  "None")
	   (value   "the formula constructed from this with unit-rule"
		    "(a o e) = a given in theory learn"
                    "is P[(a)]"))
  (let* ((term-at-position (data~struct-at-position formula pos))
	 (a-const (first (data~appl-arguments term-at-position)))
	 (e-const (second (data~appl-arguments term-at-position)))
	 (op-const (data~appl-function term-at-position))
	 (unit-axiom (th~find-assumption
		       ;;;;; right-unit rule global variable - see at the top of the file
		     learn*right-unit-definition
		     (prob~proof-theory omega*current-proof-plan)))
	 (unit-rule (learntac=get-rewrite-formula unit-axiom))
	 (left-side (first (data~appl-arguments unit-rule)))
	 (right-side (second (data~appl-arguments unit-rule)))
	 (a-var (first (data~appl-arguments left-side)))
	 (e-var (second (data~appl-arguments left-side)))
	 (op-var (data~appl-function left-side))
	 (subst (subst~create (list op-var e-var a-var e-var)
			      (list op-const e-const a-const e-const))))
    (data~replace-at-position formula pos (subst~apply subst right-side))))

#|
(defun learntac=compute-idr-i-f (formula pos)
  (declare (edited  "16-MAY-2000")
	   (authors Mxj)
	   (input   "formula is a premise line looking like T= P[(a)]"
		    "pos is the position of the occurrence of (a) in T")
	   (effect  "None")
	   (value   "the formula constructed from this with unit-rule"
		    "(e o a) = a given in theory learn"
                    "is P[(e o a)]"))
  (let* ((term-at-position (data~struct-at-position formula pos))
	 (a-const term-at-position)
	 (all-group-hyps (learntac=get-group-hyps))
	 (all-group-id (mapcar #'learntac=get-group-id all-group-hyps))
	 (e-const (first all-group-id)) ; we take the first, but could there be more?
	 (all-group-ops (mapcar #'learntac=get-group-op all-group-hyps))
	 (op-const (first all-group-ops)) ; we take the first, but could there be more?
	 (unit-axiom (th~find-assumption
		       ;;;;; right-unit rule global variable - see at the top of the file
		      learn*right-unit-definition
		      (prob~proof-theory omega*current-proof-plan)))
	 (unit-rule (learntac=get-rewrite-formula unit-axiom))
	 (left-side (first (data~appl-arguments unit-rule)))
	 (right-side (second (data~appl-arguments unit-rule)))
	 (a-var (second (data~appl-arguments left-side)))
	 (e-var (first (data~appl-arguments left-side)))
	 (op-var (data~appl-function left-side))
	 (subst (subst~create (list op-var unit-var a-var e-var)
			      (list op-const unit-const a-const e-const))))
    (data~replace-at-position formula pos (subst~apply subst right-side))))


(defun learntac=compute-idr-i-f-p (formula pos)
  (let* ((term-at-position (data~struct-at-position formula pos))
	 (unit-axiom (th~find-assumption
				;;;;; unit rule global variable - see at the top of the file
		      learn*right-unit-definition
		      (prob~proof-theory omega*current-proof-plan)))
	 (unit-rule (learntac=get-rewrite-formula unit-axiom))
	 (args (data~appl-arguments unit-rule)))
    (term~alpha-match (first args) term-at-position)))
|#

(defun learntac=compute-idr-i-b-p (formula pos)
  (let ((term-at-position (data~struct-at-position formula pos)))
    (when (data~appl-p term-at-position)
      (let* ((current-group-op (data~top term-at-position))
	     (current-group-id (second (data~appl-arguments term-at-position)))
	     (all-group-hyps (learntac=get-group-hyps))
	     (all-group-ops (mapcar #'learntac=get-group-op all-group-hyps))
	     (all-group-ids (mapcar #'learntac=get-group-id all-group-hyps)))
	(and (some #'(lambda (x) (keim~equal x current-group-op)) all-group-ops)
	     (some #'(lambda (x) (keim~equal x current-group-id)) all-group-ids)
	     (let* ((unit-axiom (th~find-assumption
				;;;;; unit rule global variable - see at the top of the file
				 learn*right-unit-definition
				 (prob~proof-theory omega*current-proof-plan)))
		    (unit-rule (learntac=get-rewrite-formula unit-axiom))
		    (args (data~appl-arguments unit-rule)))
	       (term~alpha-match (first args) term-at-position)))))))

#|
(tac~deftactic idr-i-f idr-i (in learn)
	       (parameters (position pos+position "A position for the application"))
	       (premises (L2 "An open line to apply unit right rule to."))
	       (conclusions (L1 "A conclusion line to be constructed by unit right."))
	       (computations (L1 (learntac=compute-idr-i-f (formula L2) position)))
	       (sideconditions (learntac=compute-idr-i-f-p (formula L2) position))
	       (description "Forward application of unit right."))

|#

(tac~deftactic idr-i-b idr-i (in learn)
	       (parameters (position pos+position "A position for the application."))
	       (premises (L2 "A premise line to be constructed by unit right."))
	       (conclusions (L1 "An open line to apply unit right rule to."))
	       (computations (L2 (learntac=compute-idr-i-b (formula L1) position)))
	       (sideconditions (learntac=compute-idr-i-b-p (formula L1) position))
	       (description "Backward application of unit right."))

#|
(tac~deftactic idr-i-a idr-i (in learn)
	       (parameters (position pos+position "A position for the application"))
	       (premises (L2 "A premise line to apply unit right rule to."))
	       (conclusions (L1 "A open line to apply unit right rule to."))
	       (computations )
	       (sideconditions (learntac=compute-idr-i-b-p (formula L1) position)
			       (learntac=compute-idr-i-f-p (formula L2) position))
	       (description "Application of unit right."))

|#
#|
(defun learntac=expand-idr-i (outline parameters)
  (let* ((premise (second outline))
	 (conc (first outline))
	 (pos (first parameters))
	 (unit-axiom-line (tacl~insert&return-assumption 'learn 'right-unit))
	 (term-at-pos (data~struct-at-position (node~formula premise) pos))
	 (terms
	  ))))
|#

(com~defcommand idr-i
		(argnames conc prem position)
		(argtypes ndline ndline position)
		(arghelps "The substituted line" "The unsubstituted line"
			  "A position.")
		(function learntac=idr-i)
		(frag-cats tactics base)
		(defaults ((oc~default-current-planline) (com~unspecified) (com~unspecified)))
		(level 1)
		(log-p T)
		(help "Unit Right."))

(defun learntac=idr-i (C P position)
  (infer~compute-outline 'idr-i (list C P) (list position)))



