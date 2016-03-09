;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: KEIM -*-
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Definition expansion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Definitions
;;

(infer~deftactic "DefsE"
		 (outline-mappings (((nonexistent existent) defse-f)
                                    ((existent existent) defse-a)))
		 (parameter-types thy-ass-list)
		 (expansion-function gentac=expand-defse)
		 (help "Elimination of Multiple Definition Occurrences"))

(tac~deftactic defse-f DefsE (in generic)
   (parameters (DL list "the list of definitions"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (gentac=substitute-defis (formula L1) DL)))
   (sideconditions (gentac=substituted-differs-p (formula L1) DL))
   (description "Forward application of definition elemination."))

(tac~deftactic defse-a DefsE (in generic)
   (parameters (DL list "the list of definitions"))
   (premises (L1 "a term with defined symbols"))
   (conclusions (L2 "a term with all definitions removed"))
   (computations) 
   (sideconditions (gentac=substituted-definitions-p (formula L2) (formula L1) DL))
   (description "Check definition expansion."))


(com~defcommand defse
  (argnames concl line2 defs)
  (argtypes ndline ndline thy-ass-list)
  (arghelps  "Line with expanded definitions" "Line with defined utterances" "List with prohibited Definitions")
  (function gentac=defse)
  (frag-cats tactics generic definition)
  (defaults gentac=defse-defaults)
  (log-p T)
  (level 5)
  (help "Eleminates defined expressions."))


(defun gentac=defse-defaults (conc prec defs)
  (cond ((not (com~specified-arg-p conc))
	 (list (pds~find-open-node
		#'(lambda (p) (not (orules=contained-definition p))))
	       prec defs))
	((not (com~specified-arg-p prec))
	 (if (or (null conc)
		 (when (listp conc) (null (car conc))))
           (list conc 
                 (pds~find-node-support
                  conc
                  #'(lambda (p) (orules=contained-definition p))
		 defs))
	   (list conc nil nil)))
	((not (com~specified-arg-p defs))
	 (list conc prec (list (repr~find-definition '= (prob~theory omega*current-proof-plan)))))
	(t (list conc prec defs))))


;(defun gentac=defse-defaults (conc prec defs)
;  (cond ((not (com~specified-arg-p conc))
;         (list nil (pds~find-support #'(lambda (x) x)) nil nil))
;        ((not (com~specified-arg-p prec))
;         (if (null conc)
;             (list conc (pds~find-support #'(lambda (x) x)) equality defs)
;           (list conc
;                 (pds~find-node-support
;                  conc
;                  #'(lambda (p)
;                      (gentac=substituted-definitions-p (node~formula conc)
;                                                       p defs)))
;                 defs)))
;        ((not (com~specified-arg-p equality))
;         (list conc prec T defs))
;        ((not (com~specified-arg-p defs))
;         (list conc prec equality (let ((formula (node~formula prec)))
;                           (remove-if-not #'(lambda (x)
;                                              (if equality
;                                                  (data~position
;                                                   formula
;                                                   #'(lambda (y)
;                                                       (if equality
;                                                           (data~equal (th~definition-constant x)
;                                                                       y))))
;                                                (and (not (data~equal (th~definition-constant x)
;                                                                      (logic~equality-constant)))
;                                                     (data~position
;                                                      formula
;                                                      #'(lambda (y)
;                                                          (if equality
;                                                              (data~equal (th~definition-constant x)
;                                                                          y)))))))
;                                                  (th~definitions-recursed
;                                                   (prob~theory omega*current-proof-plan))))))
;                           
;        (t (list conc prec equality defs))))


(defun gentac=defse (concl line2 defs)
  (infer~compute-outline 'defse (list concl line2) (list defs)))


;Expansion noch nicht veraendert, DEF
(defun gentac=expand-defse (outline parameters)
  (let ((conc (first outline))
	(prec-line (cdr outline))
	(prec (second outline))
	(defs (first parameters)))
    (multiple-value-bind
	(conc-eq tlist) (gentac=substitute-definitions (node~formula prec) defs)
      (declare (ignore conq-eq))
      (tacl~init outline)
      (let ((defsymbols (mapcar #'gentac=get-defsymb tlist)))
	(do* ((restlist defsymbols (cdr restlist))
	      (defsymb (car restlist) (car restlist)))
	    ((null restlist) (tacl~apply 'lambda (list conc (car prec-line)) nil))
	  (let* ((def (repr~find-definition defsymb (prob~theory omega*current-proof-plan)))
		 (definiendum (th~definition-constant def))
		 (definiens (data~copy (th~ass-node def) :downto '(term+constant
								   type+primitive)))
		 (poslist (data~substruct-positions definiendum (node~formula (car prec-line))
						    :test #'data~schema-equal)))
	    (when poslist
	      (setf prec-line
		    (tacl~apply 'defne* (list nil (car prec-line)) (list definiendum
									 definiens poslist)))))))
      (tacl~end))))


(defun gentac=get-defsymb (pair)
  (keim~name (car pair)))


;(defun gentac=substitute-definitions (formula defs)
;  (let ((new-form formula))
;    (dolist (x defs)
;      (let* ((dd (if (th~definition-p x)
;                     (th~definition-constant x)
;                   (error "~A is not a definition" (keim~name x))))
;             (ds (th~ass-node x))
;             (poslist (data~positions new-form #'(lambda (x) (data~equal x dd)))))
;        (setq new-form (gentac=compute-iterated-defne new-form dd ds poslist))))
;    new-form))

(defun gentac=substitute-defis (f prohibited) ;;;fast but not in top-down order
  (beta~normalize (thtac~substitute-defis f prohibited))) ;;;only minimal beta-normalisation inside substitute-defis, MP
;;;(thtac~substitute-defis f prohibited))

(defun gentac=substitute-definitions (f prohibited) ;;;only used for tactic-expansion, MP
  (let* ((defs (union
		(set-difference
		(th~definitions-recursed
		 (prob~theory omega*current-proof-plan))
		prohibited)
		(set-difference
		(mapcar #'repr~definition (remove-if-not #'term~special-p (repr~all-substructs f)))
		prohibited :test #'(lambda (x y) (data~equal (th~definition-constant x)
							     (th~definition-constant y)))))))
    (multiple-value-bind (fnew positions)
        (rewrite~preorder
         f
         ;; predicates
         (mapcar #'(lambda (x)
                     #'(lambda (term)
                         (when (data~constant-p term)
                           (let* ((look-pre
				   (th~definition-constant x))
				 (look (if (data~schema-p look-pre)
					   (data~schema-range look-pre)
					 look-pre)))
                             (and (data~constant-p look)
                                  (keim~equal look term))))))
                 defs)
         ;; names (we use definitions themselves)
         defs
         ;; transformations
         (mapcar #'(lambda (x)
                     #'(lambda (term)
                         (let* ((look-pre (th~ass-node x))
				(look (pds~ground-definiens look-pre term)))
                           look)))
                 defs))
    (values (beta~normalize fnew) positions))))


;(defun gentac=substitute-definitions (f prohibited) ;;;only used for tactic-expansion, MP
;  (let* ((defs (set-difference
;                (th~definitions-recursed
;                 (prob~theory omega*current-proof-plan))
;                prohibited)))
;    (multiple-value-bind (fnew positions)
;        (rewrite~preorder
;         f
;         ;; predicates
;         (mapcar #'(lambda (x)
;                     #'(lambda (term)
;                         (when (data~constant-p term)
;                           (let* ((look-pre
;                                        (env~lookup-object
;                                        (keim~name x)
;                                        (pds~environment
;                                         omega*current-proof-plan)))
;                                 (look (if (data~schema-p look-pre)
;                                           (data~schema-range look-pre)
;                                         look-pre)))
;                             (and (data~constant-p look)
;                                  (keim~equal look term))))))
;                 defs)
;         ;; names (we use definitions themselves)
;         defs
;         ;; transformations
;         (mapcar #'(lambda (x)
;                     #'(lambda (term)
;                         (let* ((look-pre (th~ass-node x))
;                                (look (pds~ground-definiens look-pre term)))
;                           look)))
;                 defs))
;    (values (beta~normalize fnew) positions))))


(defun gentac=substituted-definitions-p (f1 f2 defs)
  (term~alpha-equal f1 (gentac=substitute-defis f2 defs)))


(infer~deftactic "DefsI"
		 (outline-mappings (((existent nonexistent) defsi-b)
                                    ((existent existent) defsi-a)))
		 (parameter-types thy-ass-list)
		 (expansion-function gentac=expand-defsi)
		 (help "Elemination of Multiple Definitions"))


(com~defcommand defsi
  (argnames concl line2 defs)
  (argtypes ndline ndline thy-ass-list)
  (arghelps  "Line with definited symbols" "Line with removed definitions" "List of prohibited Definitions")
  (function gentac=defsi)
  (frag-cats tactics generic definition)
  (defaults gentac=defsi-defaults)
  (log-p T)
  (level 5)
  (help "Eleminates defined expressions."))


(defun gentac=defsi-defaults (conc prec defs)
  (cond ((not (com~specified-arg-p conc))
	 (list (oc~default-current-planline) prec defs))
	((not (com~specified-arg-p prec))
         (if (null conc)
	     (list conc nil nil)
           (list conc 
                 (pds~find-node-support
                  conc
                  #'(lambda (p) (not (orules=contained-definition p))))
		 defs)))
	((not (com~specified-arg-p defs))
	 (list conc prec (list (repr~find-definition '= (prob~theory omega*current-proof-plan)))))
;               (let ((formula (node~formula conc)))
;		 (remove-if-not #'(lambda (x)
;                                    (if equality
;                                        (data~position formula
;                                                       #'(lambda (y)
;                                                           (data~equal (th~definition-constant x)
;                                                                       y)))
;                                      (and (not (data~equal (th~definition-constant x)
;                                                            (logic~equality-constant)))
;                                           (data~position formula
;                                                          #'(lambda (y)
;                                                              (data~equal (th~definition-constant x)
;                                                                          y))))))
;                                (th~definitions-recursed
;                                 (prob~theory omega*current-proof-plan))))))
	(t (list conc prec defs))))

(defun gentac=defsi (concl line2 defs)
  (infer~compute-outline 'defsi (list concl line2) (list defs)))


(tac~deftactic defsi-b DefsI (in generic)
   (parameters (DL list "the list of definitions"))
   (premises L2)
   (conclusions L1)
   (computations (L2 (gentac=substitute-defis (formula L1) DL)))
   (sideconditions (gentac=substituted-differs-p (formula L1) DL))
   (description "Backward application of definition introduction."))

(tac~deftactic defsi-a DefsI (in generic)
   (parameters (DL list "the list of definitions"))
   (premises (L2 "a term with defined symbols"))
   (conclusions (L1 "a term with all definitions removed"))
   (computations) 
   (sideconditions (gentac=substituted-definitions-p (formula L2) (formula L1) DL))
   (description "Check definition expansion."))


(defun gentac=substituted-differs-p (formula def-list)
  (not (keim~equal formula (thtac~substitute-defis formula def-list))))

(defun gentac=expand-defsi (outline parameters)
  (let* ((conc (first outline))
	 (conc-line (list nil conc))
	 (prec (second outline))
	 (defs (first parameters)))
    (multiple-value-bind
	(prec-eq tlist) (gentac=substitute-definitions (node~formula conc) defs)
      (declare (ignore prec-eq))
      (tacl~init outline)
      (let ((defsymbols (remove-duplicates (mapcar #'gentac=get-defsymb tlist))))
	(do* ((restlist defsymbols (cdr restlist))
	      (defsymb (car restlist) (car restlist)))
	    ((null restlist) (tacl~apply 'lambda (list (cadr conc-line) prec) nil))
	  (let* ((def (repr~find-definition defsymb (prob~theory omega*current-proof-plan)))
		 (definiendum (th~definition-constant def))
		 (definiens (data~copy (th~ass-node def) :downto '(term+constant
								   type+primitive)))
		 (poslist (data~substruct-positions definiendum (node~formula (cadr conc-line))
						    :test #'data~schema-equal)))
	    (when poslist
	      (setf conc-line
		    (tacl~apply 'defni* (list (cadr conc-line) nil) (list definiendum
									  definiens poslist)))))))
      (tacl~end))))



(infer~deftactic defne*
		 (outline-mappings (((existent existent) defne*-a)
                                    ((nonexistent existent) defne*-f)
				    ))
		 (parameter-types term term position-list)
		 (expansion-function gentac=expand-defne*)
		 (help "Iterated Definition-Expansion."))


(tac~deftactic defne*-a defne* (in generic)
  (parameters (D term+term "the definiendum")
	      (B term+term "the definiens")
	      (PL list "the position list"))
  (premises L1)
  (conclusions L2)
  (computations)
  (sideconditions (gentac=defne*-a-p (formula L1) (formula L2) D B PL))
  (description "checking multiple definition expansion"))

(tac~deftactic defne*-f defne* (in generic)
  (parameters (D term+term "the definiendum")
	      (B term+term "the definiens")
	      (PL list "the position list"))
  (premises L1)
  (conclusions L2)
  (computations (L2 (gentac=compute-iterated-defne (formula L1) D B PL)))
  (description "forward application of  multiple definition expansion"))

(defun gentac=defne*-a-p (form1 form2 dd ds pl)
  (data~equal (gentac=compute-iterated-defne form1 dd ds pl) form2))

(defun gentac=compute-iterated-defne (formula dd ds poslist)
  (let ((new-form formula))
    (dolist (x poslist)
	(setq new-form (pds~replace-without-contract-at-position
			dd
			ds
			new-form
			x)))
;    (prog2 (trace beta~normalize)
	(beta~normalize new-form)
;      (untrace))
      ))

(defun gentac=expand-defne* (outline parameters) ;irgendwas stimmt da noch nicht. DEF
  (let ((L1 (second outline))
	(L2 (first outline))
	(definiendum (first parameters))
	(definiens (second parameters))
        (poslist (gentac=sort-position-list (third parameters))))
    (tacl~init outline)
    (do ((rest poslist (cdr rest)))
	((null rest) T)
      (when rest
	(setf L1
	      (car (if (null (rest rest))
		       (tacl~apply 'defne (list L2 L1) (list definiendum definiens (car rest)))
		     (tacl~apply 'defne (list nil L1) (list definiendum definiens (car rest))))))))
    (tacl~end)))


(com~defcommand defn-expand*
  (argnames line definition position-list)
  (argtypes ndline thy-assumption position-list)
  (arghelps "Line to be rewritten" "Definition to be expanded" "Positions of occurrences")
  (function gentac=defn-expand*)
  (frag-cats tactics generic definition)
  (defaults gentac=defn-expand*-defaults)
  (log-p T)
  (level 5)
  (help "Expand multiple definition occurrences in a line."))

(defun gentac=defn-expand*-defaults (line definition position-list)
  (cond ((not (com~specified-arg-p line))
	 (list (com~unspecified) (com~unspecified) (com~unspecified)))
	((not (com~specified-arg-p definition))
	 (list line (com~unspecified) (com~unspecified)))
	((not (com~specified-arg-p position-list))
	 (let ((definiendum (th~definition-constant definition)))
	   (list line definition (gentac=sort-position-list (data~positions (node~formula line) #'(lambda (x) (data~schema-equal x definiendum)))))))
	(T (list line definition (gentac=sort-position-list position-list)))))

(defun gentac=defn-expand* (line definition positions)
  (if (th~definition-p definition)
      (let ((definiendum (th~definition-constant definition))
	    (definiens (th~ass-node definition)))
	(if (every #'(lambda (x) (data~schema-equal (data~struct-at-position (node~formula line) x) definiendum))
		   positions)
	    (infer~compute-outline 'defne* (list nil line) (list definiendum definiens (gentac=sort-position-list positions)))
	  (error "~A does not appear at one of the positions given" definiendum)))
    (error "~A is not a definition" (keim~name definition))))



(infer~deftactic "defni*"
               (outline-mappings (((existent existent) defni*-a)
				  ((existent nonexistent) defni*-b)))
	       (parameter-types term term position-list)
	       (expansion-function gentac=expand-defni*)
               (help "Iterated Definition Expansion"))


(tac~deftactic defni*-a defni* (in generic)
  (parameters (D term+term "the definiendum")
	      (B term+term "the definiens")
	      (PL list "the position list"))
  (premises L1)
  (conclusions L2)
  (computations)
  (sideconditions (gentac=defne*-a-p (formula L2) (formula L1) D B PL))
  (description "checking multiple definition contraction"))


(tac~deftactic defni*-b defni* (in generic)
  (parameters (D term+term "the definiendum")
	      (B term+term "the definiens")
	      (PL list "the position list"))
  (premises L1)
  (conclusions L2)
  (computations (L1 (gentac=compute-iterated-defne (formula L2) D B PL)))
  (description "backward application of  multiple definition contraction"))

(com~defcommand defn-contract*
  (argnames line definition position-list)
  (argtypes ndline thy-assumption position-list)
  (arghelps "Line to be rewritten" "Definition to be contracted" "Positions of occurrences")
  (function gentac=defn-contract*)
  (frag-cats tactics generic definition)
  (defaults gentac=defn-contract*-defaults)
  (log-p T)
  (level 5)
  (help "Contract multiple definition occurrences in a line."))

(defun gentac=defn-contract*-defaults (line definition position-list)
  (cond ((not (com~specified-arg-p line))
	 (list (oc~default-current-planline) (com~unspecified) (com~unspecified)))
	((not (com~specified-arg-p definition))
	 (list line (orules=contained-definition (node~formula line)) (com~unspecified)))
	((not (com~specified-arg-p position-list))
	 (let ((definiendum (th~definition-constant definition)))
	   (list line definition (gentac=sort-position-list (data~positions (node~formula line) #'(lambda (x) (data~schema-equal x definiendum)))))))
	(T (list line definition (gentac=sort-position-list position-list)))))

(defun gentac=defn-contract* (line definition positions)
  (if (th~definition-p definition)
      (let ((definiendum (th~definition-constant definition))
	    (definiens (th~ass-node definition)))
	(if (every #'(lambda (x) (data~schema-equal (data~struct-at-position (node~formula line) x) definiendum))
		   positions)
	    (infer~compute-outline 'defni* (list line nil) (list definiendum definiens (gentac=sort-position-list positions)))
	  (error "~A does not appear at one of the positions given" definiendum)))
    (error "~A is not a definition" (keim~name definition))))

(defun gentac=expand-defni* (outline parameters) ;irgendwas stimmt da noch nicht. DEF
  (let ((exp (second outline))
	(unexp (first outline))
	(definiendum (first parameters))
	(definiens (second parameters))
        (poslist (gentac=sort-position-list (third parameters))))
    (tacl~init outline)
    (do ((rest poslist (cdr rest)))
	((null rest) T)
      (when rest
	(setf unexp
	      (cadr (if (null (rest rest))
			(tacl~apply 'defni (list unexp exp) (list definiendum definiens (car rest)))
		      (tacl~apply 'defni (list unexp nil) (list definiendum definiens (car rest))))))))
    (tacl~end)))

(defun gentac=sort-position-list (poslist)
  (labels ((list-greater (xl yl)
			 (cond ((null xl) nil)
			       ((null yl) t)
			       ((> (car xl) (car yl)) t)
			       ((< (car xl) (car yl)) nil)
			       (t (list-greater (cdr xl) (cdr yl))))))
    (stable-sort (copy-list poslist)
		 #'(lambda (x y)
		     (let ((xp (pos~number-list x))
			   (yp (pos~number-list y)))
		       (or (> (length xp) (length yp))
			   (list-greater xp yp)))))))
