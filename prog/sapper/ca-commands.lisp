;;; -*- syntax: common-lisp; package: OMEGA; base: 10; mode: KEIM -*-
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

(in-package :omega)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file shouldn't be compiled!!!!!!!!!!!!!!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(mod~defmod CACOM 
            :uses (ca com comint data env infer inter keim node oc omega pds pdsc pdsj pdsn pos tacl term)
            :documentation "Definition of CAS commands in OMEGA."
            :exports (
                      cacom~call-cas
                      cacom~cas-defaults
                      cacom~coeff&monomial-p
                      cacom~coefficient
                      cacom~exponent
                      cacom~exponent-equal
                      cacom~monomial
                      cacom~monomial-p
                      cacom~polynomial-p
                      cacom~suggest-cas
                      cacom~suggest-cas*
                      
                      cacom*diff-started))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The definition of th abstract CA MyCAS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eval-when (load eval compile)
  (ca~defsystem :myCAS
		(help "Polynomials and nothing but polynomials")
		(translations
		 (p-plus-r1 (ca~polynomial-addition (:realpoly :realpoly :realpoly)))
		 (p-plus-r2 (ca~polynomial-addition (:realpoly :realpoly :realpoly)))
		 (p-plus-r3 (ca~polynomial-addition (:realpoly :realpoly :realpoly)))
		 (p-plus-r4 (ca~polynomial-addition (:realpoly :realpoly :realpoly)))
		 (p-plus-q1 (ca~polynomial-addition (:ratpoly :ratpoly :ratpoly)))
		 (p-plus-q2 (ca~polynomial-addition (:ratpoly :ratpoly :ratpoly)))
		 (p-plus-q3 (ca~polynomial-addition (:ratpoly :ratpoly :ratpoly)))
		 (p-plus-q4 (ca~polynomial-addition (:ratpoly :ratpoly :ratpoly)))
		 (p-plus-z1 (ca~polynomial-addition (:intpoly :intpoly :intpoly)))
		 (p-plus-z2 (ca~polynomial-addition (:intpoly :intpoly :intpoly)))
		 (p-plus-z3 (ca~polynomial-addition (:intpoly :intpoly :intpoly)))
		 (p-plus-z4 (ca~polynomial-addition (:intpoly :intpoly :intpoly)))
		 (p-times-r1 (ca~polynomial-multiplication (:realpoly :realpoly :realpoly)))
		 (p-times-r2 (ca~polynomial-multiplication (:realpoly :realpoly :realpoly)))
		 (p-times-r3 (ca~polynomial-multiplication (:realpoly :realpoly :realpoly)))
		 (p-times-r4 (ca~polynomial-multiplication (:realpoly :realpoly :realpoly)))
		 (p-times-q1 (ca~polynomial-multiplication (:ratpoly :ratpoly :ratpoly)))
		 (p-times-q2 (ca~polynomial-multiplication (:ratpoly :ratpoly :ratpoly)))
		 (p-times-q3 (ca~polynomial-multiplication (:ratpoly :ratpoly :ratpoly)))
		 (p-times-q4 (ca~polynomial-multiplication (:ratpoly :ratpoly :ratpoly)))
		 (p-times-z1 (ca~polynomial-multiplication (:ratpoly :ratpoly :ratpoly)))
		 (p-times-z2 (ca~polynomial-multiplication (:ratpoly :ratpoly :ratpoly)))
		 (p-times-z3 (ca~polynomial-multiplication (:ratpoly :ratpoly :ratpoly)))
		 (p-times-z4 (ca~polynomial-multiplication (:ratpoly :ratpoly :ratpoly)))
		 (p-deriv-r1 (ca~differentiate-polynomial (:realpoly :nat :realpoly)))
		 (p-deriv-r2 (ca~differentiate-polynomial (:realpoly :nat :realpoly)))
		 (p-deriv-r3 (ca~differentiate-polynomial (:realpoly :nat :realpoly)))
		 (p-deriv-r4 (ca~differentiate-polynomial (:realpoly :nat :realpoly)))
		 (p-deriv-q1 (ca~differentiate-polynomial (:ratpoly :nat :ratpoly)))
		 (p-deriv-q2 (ca~differentiate-polynomial (:ratpoly :nat :ratpoly)))
		 (p-deriv-q3 (ca~differentiate-polynomial (:ratpoly :nat :ratpoly)))
		 (p-deriv-q4 (ca~differentiate-polynomial (:ratpoly :nat :ratpoly)))
		 (pol-minimum (ca~minimum (:realpoly :real)))
		 (s-times-r1 (ca~scalar-mult-on-poly (:realpoly :real :realpoly)))
		 )
		(call eval))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  The definition of the CAS-BlackBox
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defbbox cas
	       (outline-function cacom=compute-with-cas)
	       (expansion-function cacom=expand-with-cas)
	       (parameter-types position symbol)
	       (help "Compute with Computer Algebra"))

(defun cacom=compute-with-cas (outline parameters)
  (let* ((conc (car outline))
	 (prem (cadr outline))
	 (line (or prem conc))
	 (position (car parameters))
	 (cas (ca~find-system (cadr parameters)))
	 (ca*global-environment (pds~environment omega*current-proof-plan)))
    (ca~set-verbose nil)
    (if cas
	(let* ((term (data~struct-at-position (node~formula line) position))
	       (new-term
		(data~replace-at-position
		 (node~formula line)
		 position
		 (let ((addargs (when (term~appl-p term) (subseq (data~appl-arguments term) 2)))
		       ;;; here we compute according to the received object
		       (result (cacom=compute-according2termtype term cas)))   
		   (if addargs (data~appl-create result addargs)
		     result)))))
	  (unless (term~alpha-equal new-term (node~formula line))
	    (cond ((and conc prem) (warn "Don't know...."))
		  (conc (let* ((new-node (pdsn~open-node-create new-term
								(pdsn~hyps line)
								(pds~new-node-name)))
			       (old-just (node~justification conc))
			       (old-reasons (pdsj~reasons old-just))
			       (old-sponsors (pdsj~sponsors old-just))
			       (old-unsponsors (pdsj~unsponsors old-just))
			       (new-control (pdsc~create old-reasons old-sponsors old-unsponsors)))
			  (setf (node~justification conc)
				(pdsj~replace-justification! (node~justification conc)
							     (pdsj~closed-just-create
							      (infer~find-method 'cas)
							      (list new-node)
							      parameters)))
			  (setf (pdsj~control (node~justification conc)) new-control)
			  (pds~insert-node! new-node)
			  (list conc new-node)))
		  (t (let ((new-node (pdsn~create (pds~new-node-name)
						  (pdsn~hyps line)
						  new-term
						  (pdsj~closed-just-create (infer~find-method 'cas)
									   (list prem)
									   parameters))))
		       (pds~only-insert-node! new-node)
		       (list new-node prem))))))
      (warn "CAS ~A does not exist." (cadr parameters)))))


(defun cacom=compute-according2termtype (term cas)
  (cond ((cacom~polynomial-p term)
	 (ca~rebuild-object
	  (ca~build-object term :ratpoly (keim~name cas))
	  :ratpoly (keim~name cas)))
	(t (ca~translation term cas))))



(defmacro cacom=poly-symbols (&body body)
  `(let* ((env (pds~environment omega*current-proof-plan))
	  (plus (env~lookup-object :plus env))
	  (times (env~lookup-object :times env))
	  (power (env~lookup-object :power env)))
     ,@body))

(defun cacom~polynomial-p (term)
  (when (term~abstr-p term)
    (let ((bound-term (cacom=unbind-abstraction term)))
      (cacom=polynomial-p bound-term))))

(defun cacom=polynomial-p (term)
  (cacom=poly-symbols
   (declare (ignore power times))
   (when (term~appl-p term)
     (if (data~equal (data~appl-function term) plus)
	 (and (cacom~coeff&monomial-p (first (data~appl-arguments term)))
	      (cacom=polynomial-p (second (data~appl-arguments term))))
       (cacom~coeff&monomial-p term)))))

(defun cacom~coeff&monomial-p (term)
  (cacom=poly-symbols
   (declare (ignore plus power))
   (and (term~appl-p term)
	(data~equal (data~appl-function term) times)
	(or (term~constant-p (first (data~appl-arguments term)))
	    (term~variable-p (first (data~appl-arguments term))))
	(cacom~monomial-p (second (data~appl-arguments term))))))

(defun cacom~monomial-p (term)
  (cacom=poly-symbols
   (declare (ignore plus))
   (when (term~appl-p term)
     (if (data~equal (data~appl-function term) times)
	 (and (cacom~monomial-p (first (data~appl-arguments term)))
	      (cacom~monomial-p (second (data~appl-arguments term))))
       (and (data~equal (data~appl-function term) power)
	    (or (term~constant-p (first (data~appl-arguments term)))
		(term~variable-p (first (data~appl-arguments term))))
	    (or (term~constant-p (second (data~appl-arguments term)))
		(term~variable-p (second (data~appl-arguments term)))))))))

(defun cacom~exponent-equal (mon1 mon2)
  (cacom=poly-symbols
   (declare (ignore plus power))
   (when (and (term~appl-p mon1)
	      (term~appl-p mon2)
	      (data~equal (data~appl-function mon1) times)
	      (data~equal (data~appl-function mon2) times))
     (data~equal (second (data~appl-arguments mon1))
		 (second (data~appl-arguments mon2))))))

(defun cacom~coefficient (mon)
  (when (cacom~coeff&monomial-p mon)
    (let ((coeff (car (data~appl-arguments mon))))
      (if (term~number-p coeff)
	  (keim~name coeff)
	coeff))))

(defun cacom~exponent (mon)
  (when (and (cacom~monomial-p mon)
	     (not (cacom~monomial-p (car (data~appl-arguments mon)))))
    (let ((exp (cadr (data~appl-arguments mon))))
      (if (term~number-p exp)
	  (keim~name exp)
	exp))))

(defun cacom~monomial (mon)
  (cond ((cacom~coeff&monomial-p mon)
	 (cadr (data~appl-arguments mon)))
	((cacom~monomial-p mon)
	 mon)))


(defun cacom=unbind-abstraction (abstr)
  (if (data~abstr-p abstr)
      (cacom=unbind-abstraction (data~abstr-scope abstr))
    abstr))

(defun cacom~cas-defaults (line position cas)
  (cond ((not (com~specified-arg-p line))
	 (list (oc~default-current-planline) (com~unspecified) (com~unspecified)))
	((not (com~specified-arg-p position))
	 (let ((suggestion (when (pdsn~p line) (cacom=find-cas-functions (node~formula line)))))
	   (if suggestion
	       (list line (pos~list-position (butlast (pos~number-list (caar suggestion)))) (com~unspecified))
	     (list line (com~unspecified) (com~unspecified)))))
	((not (com~specified-arg-p cas))
	 (list line position :mycas))
	(t (list line position cas))))



(defun cacom~call-cas (line position system)
  (if (pdsn~open-node-p line)
      (infer~compute-outline 'cas (list line nil) (list position system))
    (infer~compute-outline 'cas (list nil line) (list position system))))


(defun cacom~suggest-cas (line)
  (mapc #'(lambda (x)
	    (inter~output-object (comint~interface comint*current-comint)
				 (keim~name line))
	    (inter~output-object (comint~interface comint*current-comint)
				 "   ")
	    (inter~output-object (comint~interface comint*current-comint)
				 (butlast (pos~number-list (car x))))
	    (inter~output-object (comint~interface comint*current-comint)
				 "   ")
	    (inter~output-object (comint~interface comint*current-comint)
				 (keim~name (cadr x)))
	    (inter~terpri (comint~interface comint*current-comint)))
	(cacom=find-cas-functions (node~formula line))))

(defun cacom~suggest-cas* (list)
  (mapc #'cacom=suggest-cas list))

;;(com~defcommand reset-var-state       ;;; works   VS
;;  (argnames )
;;  (argtypes )
;;  (arghelps )
;;  (function cacom=reset-var-state)
;;  (frag-cats omega)
;;  (defaults )
;;  (log-p nil)
;;  (help "Resets the counter for variable renamings. Use with CAUTION!"))
;;
;;(defun cacom=reset-var-state ()
;;  (setq keim::sym*ren-var-hashtable (make-hash-table :test #'equal)))
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some auxiliary functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cacom=poly-ring (term)
  (declare (edited  "13-NOV-1995 21:20")
	   (authors SORGE)
	   (input   "A term")
	   (value   "The dimension and field of the polynomial ring"))
  (let* ((func (keim~name
		(data~struct-at-position term
					 (pos~list-position '(0)))))
	 (leng (1- (length func)))
	 (dim (do ((n leng (1- n))
		   (j 1 (* j 10))
		   (d 0))
		  ((or (> (- (char-code (aref func n))
			     (char-code #\0))
			  9)
		       (< (- (char-code (aref func n))
			     (char-code #\0))
			  0))
		   d)
		(setf d (+ d
			   (* j
			      (- (char-code (aref func n))
				 (char-code #\0)))))
		(when (= n 0) (error ";;; not a valid term for input"))))
	 (field (aref func (- leng (do ((n dim (/ n 10))
					(d 0 (1+ d)))
				       ((< n 1) d))))))
    (values dim field)))

(defun cacom=find-cas-functions (formula)
  (declare (edited  "26-FEB-1996 16:18")
	   (authors SORGE)
	   (input   "A formula.")
	   (value   "A list of lists each containing a position and a CAS."))
  (cacom=relate-position
   (cacom=relate-cas
    (remove-if #'term~special-p
	       (cacom=get-all-subterms formula)))
   formula
   ))

(defun cacom=get-all-subterms (term)
  (declare (edited  "26-FEB-1996 16:20")
	   (authors SORGE)
	   (input   "A term.")
	   (value   "A list of all different subterms in the term."))
  (labels ((cacom=interior-gas (term)
			       (let ((term-list (data~substructs term)))
				 (cond ((null term-list) (list term))
				       ((= (length term-list) 1) (cacom=interior-gas (car term-list)))
				       ((data~abstr-p (car term-list))
					(append (cacom=interior-gas (car term-list))
						(apply #'append (mapcar #'cacom=interior-gas
									(cdr term-list)))))
				       (t (append (list (car term-list)) 
						  (apply #'append (mapcar #'cacom=interior-gas
									  (cdr term-list)))
						  ))))))
    (remove-duplicates (cacom=interior-gas term) :test #'keim~equal)))

(defun cacom=relate-cas (termlist)
  (declare (edited  "26-FEB-1996 17:24")
	   (authors SORGE)
	   (input   "A list of terms")
	   (value   "A list where terms are related to a list of CAS"))
  (when termlist
    (let* ((cas (ca~get-all-systems))
	   (term (car termlist))
	   (caslist (remove nil
			    (mapcar
			     #'(lambda (y)
				 (when (ca~sys-table-lookup y (keim~name term))
				   y))
			     cas))))
      (if caslist (append (list (cons term (list caslist)))
			  (cacom=relate-cas (cdr termlist)))
	(cacom=relate-cas (cdr termlist))))))


(defun cacom=relate-position (list formula)	
  (declare (edited  "26-FEB-1996 18:45")
	   (authors SORGE)
	   (input   "A list where terms are related to a list of CAS")
	   (value   "A list where one termpositions are related to one CAS"))
  (when list
    (let ((felem (car list)))
      (append 
       (apply #'append
	      (mapcar #'(lambda (y)
			  (mapcar #'(lambda (z)
				      (cons y (list z))
				      )
				  (cadr felem)))
		      (data~substruct-positions (car felem) formula)))
       (cacom=relate-position (cdr list) formula)))))


(defvar cacom*diff-started nil
  "A little hack for the expansion of differentation")

(defvar cacom*expandable '(:mycas :mass)
  "A list of all CAS with verbose mode.")

(defun cacom=expand-with-cas (outline parameters)
  (if (find (cadr parameters) cacom*expandable :test #'string-equal)
      (let* ((conclusion (car outline))
	     (premise (cadr outline))
	     (position (car parameters))
	     (cas (ca~find-system (cadr parameters)))
	     (term1 (data~struct-at-position (node~formula conclusion) position))
	     (term2 (data~struct-at-position (node~formula premise) position))
	     (pattern (pdsj~outline-pattern (node~justification conclusion)))
	     (direction (cond ((and pattern (infer~nonexistent-pattern-p (car pattern)))
			       :forward)
			      ((and pattern (infer~existent-pattern-p (car pattern)))
			       :backward)
			      ((and (term~appl-p term1)
				    (ca~sys-table-lookup cas 
							 (keim~name (data~appl-function term1))))
			       :backward)
			      (t :forward)))
	     (cacom*diff-started)
	     (ca*global-environment (pds~environment omega*current-proof-plan)))
	(ca~reset)
	(ca~set-verbose direction)
	(if (string-equal direction :backward)
	    (ca~translation term1 cas)
	  (ca~translation term2 cas))
	(let ((tactics (ca~computed-methods))
	      (current-position position)
	      (rest-positions)
	      (new-outline outline))
	  (tacl~init outline)
	  (do* ((rest-tactics tactics (cdr rest-tactics))
		(tactic (car rest-tactics) (car rest-tactics)))
	      ((null rest-tactics))
	    (when current-position
	      (multiple-value-setq (current-position rest-positions)
		(cacom=control-expansion tactic current-position rest-positions))
	      (omega~trace "~%Tactic: ~A; Position: ~A;  Rest-Positions: ~{~A ~}" tactic current-position rest-positions)
	      (multiple-value-setq (new-outline current-position)
		(if (null (cdr rest-tactics))
		    (if (string-equal direction :backward)
			(cacom=execute-tactic tactic new-outline current-position (cadr outline))
		      (cacom=execute-tactic tactic new-outline current-position (car outline)))
		  (cacom=execute-tactic tactic new-outline current-position )))
	      (multiple-value-setq (current-position rest-positions)
		(cacom=control-expansion tactic current-position rest-positions :after))
	      ))
	  (tacl~end))
	(ca~set-verbose nil))
    (omega~warn "Computations of the CAS ~A cannot be expanded!" (cadr parameters))))


(defun cacom=control-expansion (tactic position rest-position &optional (order :before))
  (declare (edited  "20-APR-1998")
	   (authors Sorge)
	   (input   "A symbol or a list, a position and a list of positions.")
	   (effect  "Changes the expansion behaviour of sapper by providing certain control knowledge.")
	   (value   "The new position and the new rest-position list."))
  (let ((tac (if (atom tactic)
		 tactic
	       (car tactic)))
	(args (unless (atom tactic) (cdr tactic))))
    (if (string-equal order :before)
	(cond ((or (string-equal tac :distribute-right) (string-equal tac :cummulate-right))
	       (values position
		       (if (assoc :start rest-position :test #'string-equal)
			   (append (list position (pos~empty)) rest-position)
			 (append (list position (pos~empty) (cons :start position)) rest-position))))
	      ((or (string-equal tac :pop-last) (string-equal tac :push-last))
	       (values (car rest-position) (cdr rest-position)))
	      ((string-equal tac :reset-poly-mult)
	       (values (cdr (assoc :start rest-position :test #'string-equal))
		       (remove-if #'(lambda (x) (and (listp x) (string-equal (car x) :start))) rest-position)))
	      (t (values position rest-position)))
      (cond ((or (string-equal tac :mon-times) (string-equal tac :mon-split-times))
	     (values position (cons (car rest-position)
				    (cons (pos~add-front 2 (second rest-position))
					  (cddr rest-position)))))
	    ((or (string-equal tac :pop-last) (string-equal tac :push-last))
	     (values (pos~concatenate position (car rest-position))
		     (cdr rest-position)))
	    (t (values position rest-position))))))

(defun cacom=diff-started ()
  (not (unless cacom*diff-started
	 (setf cacom*diff-started t))))

(defun cacom=diff-ended ()
  (when cacom*diff-started
    (not (setf cacom*diff-started nil))))

(defun cacom=execute-tactic (tactic outline position &optional (last-step nil))
  (declare (edited  "11-JUL-1997")
	   (authors Sorge)
	   (input   "A tactic.")
	   (effect  "Executes the tactic in omega*current-proof-plan.")
	   (value   "The new position to execute the next tactic and the newly computed outline."))
  (let ((tac (if (atom tactic)
		 tactic
	       (car tactic)))
	(args (unless (atom tactic) (cdr tactic))))
    ;; polynomial addition
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (cond ((string-equal tac :poly-plus)
	   (let* ((term (data~struct-at-position (node~formula (cadr outline))
						 position))
		  (addargs (when (term~appl-p term) (subseq (data~appl-arguments term) 2)))
		  (new-outline (tacl~apply 'poly-plus
					   (list last-step (cadr outline))
					   (list position))))
	     (values new-outline
                     (pos~concatenate position
                                      (pos~list-position (make-list (1+ (length addargs))
                                                                    :initial-element 0))))))
	  ((string-equal tac :poly-split-plus)
	   (let* ((term (data~struct-at-position (node~formula (car outline))
						 position))
		  (addargs (when (term~appl-p term) (subseq (data~appl-arguments term) 2)))
		  (new-outline (tacl~apply 'poly-split-plus
					   (list (car outline) last-step)
					   (list position))))
	     (values new-outline
                     (pos~concatenate position
                                      (pos~list-position (make-list (1+ (length addargs))
                                                                    :initial-element 0))))))
	  ((string-equal tac :pop-first)
	   (values (tacl~apply 'pop-first
			       (list last-step (car outline))
			       (list position))
		   (pos~add-end 2 position)))
	  ((string-equal tac :pop-second)
	   (values (tacl~apply 'pop-second
			       (list last-step (car outline))
			       (list position))
		   (pos~add-end 2 position)))
	  ((string-equal tac :mon-plus)
	   (values (tacl~apply 'mon-plus
			       (list last-step (car outline))
			       (list position))
		   (pos~add-end 2 position)))
	  ((string-equal tac :push-first)
	   (values (tacl~apply 'push-first
			       (list (cadr outline) last-step)
			       (list position))
		   (pos~add-end 2 position)))
	  ((string-equal tac :push-second)
	   (values (tacl~apply 'pop-second
			       (list (cadr outline) last-step)
			       (list position))
		   (pos~add-end 2 position)))
	  ((string-equal tac :mon-split-plus)
	   (values (tacl~apply 'mon-split-plus
			       (list (cadr outline) last-step)
			       (list position))
		   (pos~add-end 2 position)))
	  ;; polynomial scalar-multiplication
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ((string-equal tac :stimes-poly)
	   (let* ((term (data~struct-at-position (node~formula (cadr outline))
						 position))
		  (addargs (when (term~appl-p term) (subseq (data~appl-arguments term) 2)))
		  (new-outline (tacl~apply 'stimes-poly
					   (list last-step (cadr outline))
					   (list position))))
	     (values new-outline
		     (pos~concatenate position
				      (pos~list-position (make-list (1+ (length addargs))
								    :initial-element 0))))))
	  ((string-equal tac :split-stimes-poly)
	   (let* ((term (data~struct-at-position (node~formula (car outline))
						 position))
		  (addargs (when (term~appl-p term) (subseq (data~appl-arguments term) 2)))
		  (new-outline (tacl~apply 'split-stimes-poly
					   (list (car outline) last-step)
					   (list position))))
	     (values new-outline
		     (pos~concatenate position
				      (pos~list-position (make-list (1+ (length addargs))
								    :initial-element 0))))))
	  ((string-equal tac :mon-stimes)
	   (values (tacl~apply 'mon-stimes
			       (list last-step (car outline))
			       (list position))
		   (pos~add-end 2 (pos~butlast position))))
	  ((string-equal tac :distribute-left)
	   (values (tacl~apply 'distribute-left
			       (list last-step (car outline))
			       (list position))
		   (pos~add-end 1 position)))
	  ((string-equal tac :mon-split-stimes)
	   (values (tacl~apply 'mon-split-stimes
			       (list (cadr outline) last-step)
			       (list position))
		   (pos~add-end 2 (pos~butlast position))))
	  ((string-equal tac :cummulate-left)
	   (values (tacl~apply 'cummulate-left
			       (list (cadr outline) last-step)
			       (list position))
		   (pos~add-end 1 position)))
	  ;; polynomial multiplication
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ((string-equal tac :poly-times)
	   (let* ((term (data~struct-at-position (node~formula (cadr outline))
						 position))
		  (addargs (when (term~appl-p term) (subseq (data~appl-arguments term) 2)))
		  (new-outline (tacl~apply 'poly-times
					   (list last-step (cadr outline))
					   (list position))))
	     (values new-outline
                     (pos~concatenate position
                                      (pos~list-position (make-list (1+ (length addargs))
                                                                    :initial-element 0))))))
	  ((string-equal tac :poly-split-times)
	   (let* ((term (data~struct-at-position (node~formula (car outline))
						 position))
		  (addargs (when (term~appl-p term) (subseq (data~appl-arguments term) 2)))
		  (new-outline (tacl~apply 'poly-split-times
					   (list (car outline) last-step)
					   (list position))))
	     (values new-outline
                     (pos~concatenate position
                                      (pos~list-position (make-list (1+ (length addargs))
                                                                    :initial-element 0))))))
	  ((string-equal tac :mon-times)
	   (values (tacl~apply 'mon-times
			       (list last-step (car outline))
			       (list position))
		   (pos~add-end 2 position)))
	  ((string-equal tac :mon-split-times)
	   (values (tacl~apply 'mon-split-times
			       (list (cadr outline) last-step)
			       (list position))
		   (pos~add-end 2 position)))
	  ((string-equal tac :distribute-right)
	   (values (tacl~apply 'distribute-right
			       (list last-step (car outline))
			       (list position))
		   (pos~add-end 1 position)))
	  ((string-equal tac :cummulate-right)
	   (values (tacl~apply 'cummulate-right
			       (list (cadr outline) last-step)
			       (list position))
		   (pos~add-end 1 position)))
	  ((string-equal tac :push-last)
	   (values (tacl~apply 'push-last
			       (list last-step (car outline))
			       (list position))
		   position))
	  ((string-equal tac :pop-last)
	   (values (tacl~apply 'pop-last
			       (list (cadr outline) last-step)
			       (list position))
		   position))
	  ((string-equal tac :poly-sort-back)
	   (let ((pos1 (first args))
		 (pos2 (second args)))
	     (if (<= pos1 pos2)
		 (values outline position)
	       (values (tacl~apply 'poly-sort-back
				   (list (cadr outline) last-step)
				   (cons position args))
		       position))))
	  ((string-equal tac :poly-sort-forw)
	   (let ((pos1 (first args))
		 (pos2 (second args)))
	     (if (<= pos1 pos2)
		 (values outline position)
	       (values (tacl~apply 'poly-sort-forw
				   (list last-step (car outline))
				   (cons position args))
		       position))))
	  ((string-equal tac :reduce-sum)
	   (let ((tmp-pos (pos~concatenate position
					   (pos~list-position (make-list (car args)
									 :initial-element 2)))))
	     (values (tacl~apply 'reduce-sum
				 (list last-step (car outline))
				 (list tmp-pos))
		     position)))
	  ((string-equal tac :expand-sum)
	   (let ((tmp-pos (pos~concatenate position
					   (pos~list-position (make-list (car args)
									 :initial-element 2)))))
	     (values (tacl~apply 'expand-sum
				 (list (cadr outline) last-step)
				 (list tmp-pos))
		     position)))
	  ((string-equal tac :reduce-0)
	   (let ((tmp-pos (pos~concatenate position
					   (pos~list-position (make-list (car args)
									 :initial-element 2)))))
	     (values (tacl~apply 'reduce-0
				 (list last-step (car outline))
				 (list tmp-pos))
		     position)))
	  ((string-equal tac :expand-0)
	   (let ((tmp-pos (pos~concatenate position
					   (pos~list-position (make-list (car args)
									 :initial-element 2)))))
	     (values (tacl~apply 'expand-0
				 (list (cadr outline) last-step)
				 (list tmp-pos))
		     position)))
	  ;; polynomial differentiation
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ((string-equal tac :mon-diff)
	   (if (cacom=diff-started)
	       (values (tacl~apply 'mon-diff
				   (list last-step (car outline))
				   (list position))
		       (pos~add-end 2 position))
	     (values (tacl~apply 'mon-diff
				 (list last-step (cadr outline))
				 (list position))
		     (pos~add-end 2 position))))
	  ((string-equal tac :mon-integ)
	   (if (cacom=diff-started)
	       (values (tacl~apply 'mon-integ
				   (list (cadr outline) last-step)
				   (list position))
		       (pos~add-end 2 position))
	     (values (tacl~apply 'mon-integ
				 (list (car outline) last-step)
				 (list position))
		     (pos~add-end 2 position))))
	  ((string-equal tac :const-diff)
	   (if (cacom=diff-started)
	       (values (tacl~apply 'const-diff
				   (list last-step (car outline))
				   (list position))
		       (pos~add-end 2 position))
	     (values (tacl~apply 'const-diff
				 (list last-step (cadr outline))
				 (list position))
		     (pos~add-end 2 position))))
	  ((string-equal tac :const-integ)
	   (if (cacom=diff-started)
	       (values (tacl~apply 'const-integ
				   (list (cadr outline) last-step)
				   (list position))
		       (pos~add-end 2 position))
	     (values (tacl~apply 'const-integ
				 (list (car outline) last-step)
				 (list position))
		     (pos~add-end 2 position))))
	  ((string-equal tac :add-mon)
	   (let ((pos (if (cacom=diff-ended)
			  (pos~butlast position 2)
			position)))
	     (values (tacl~apply 'add-mon
				 (list last-step (car outline))
				 (list pos))
		     (pos~butlast pos))))
	  ((string-equal tac :separate-mon)
	   (let ((pos (if (cacom=diff-ended)
			  (pos~butlast position 2)
			position)))
	     (values (tacl~apply 'separate-mon
				 (list (cadr outline) last-step)
				 (list pos))
		     (pos~butlast pos))))
	  
;;;;;;;;;;;; MASS-Tactics ;;;;;;;;;;;;;;;;;;;


	  ;; Forward
	  
	  ((string-equal tac :init-mass-f)
	   (values (list (cadr outline) (cadr outline))
		   position))
	  
	  ((string-equal tac :sink-init)
	   (values (tacl~apply 'sink-init
			       (list last-step (car outline))
			       (list (pos~concatenate position (car args))))
		   position))

	  ((string-equal tac :pop-plus)
	   (values (tacl~apply 'pop-plus
			       (list last-step (car outline))
			       (list (pos~concatenate position (car args))))
		   position))

	  ((string-equal tac :pop-times)
	   (values (tacl~apply 'pop-times
			       (list last-step (car outline))
			       (list (pos~concatenate position (car args))))
		   position))

	  ((string-equal tac :cross-swap-plus)
	   (values (tacl~apply 'cross-swap-plus
			       (list last-step (car outline))
			       (list (pos~concatenate position (car args))))
		   position))

	  ((string-equal tac :cross-swap-times)
	   (values (tacl~apply 'cross-swap-times
			       (list last-step (car outline))
			       (list (pos~concatenate position (car args))))
		   position))

          ((string-equal tac :simplify-num)
	   (values (tacl~apply 'simplify-num
			       (list last-step (car outline))
			       (list (pos~concatenate position (car args)) (cadr args)))
		   position))

	  ((string-equal tac :add-monomials)
	   (values (tacl~apply 'add-monomials
			       (list last-step (car outline))
			       (cons (pos~concatenate position (car args))
				     (cdr args)))
		   position))

	  ((string-equal tac :mult-monomials)
	   (values (tacl~apply 'mult-monomials
			       (list last-step (car outline))
			       (cons (pos~concatenate position (car args))
				     (cdr args)))
		   position))

	  ((string-equal tac :add-first)
	   (values (tacl~apply 'push-first
			       (list last-step (car outline))
			       (list (pos~concatenate position (car args))))
		   position))

	  ((string-equal tac :1*i)
	   (values (tacl~apply '1*i
			       (list last-step (car outline))
			       (list (pos~concatenate position (car args))))
		   position))

	  ((string-equal tac :0+e)
	   (values (tacl~apply '0+e
			       (list last-step (car outline))
			       (list (pos~concatenate position (car args))))
		   position))

	  ((string-equal tac :+0e)
	   (values (tacl~apply '+0e
			       (list last-step (car outline))
			       (list (pos~concatenate position (car args))))
		   position))

      	  ((string-equal tac :0*e)
	   (values (tacl~apply '0*elim
			       (list last-step (car outline))
			       (list (pos~concatenate position (car args))
				     (cadr args)))
		   position))

      	  ((string-equal tac :*0e)
	   (values (tacl~apply '*0elim
			       (list last-step (car outline))
			       (list (pos~concatenate position (car args))
				     (cadr args)))
		   position))

      	  ((string-equal tac :1*e)
	   (values (tacl~apply '1*e
			       (list last-step (car outline))
			       (list (pos~concatenate position (car args))))
		   position))

      	  ((string-equal tac :*1e)
	   (values (tacl~apply '*1e
			       (list last-step (car outline))
			       (list (pos~concatenate position (car args))))
		   position))

       	  ((string-equal tac :distribute-r)
	   (values (tacl~apply 'distribute-right
			       (list last-step (car outline))
			       (list (pos~concatenate position (car args))))
		   position))

      	  ((string-equal tac :distribute-l)
	   (values (tacl~apply 'distribute-left
			       (list last-step (car outline))
			       (list (pos~concatenate position (car args))))
		   position))

      	  ((string-equal tac :minus2plus)
	   (values (tacl~apply 'minus2plus
			       (list last-step (car outline))
			       (list (pos~concatenate position (car args))))
		   position))

      	  ((string-equal tac :sqrt2power)
	   (values (tacl~apply 'sqrt2power
			       (list last-step (car outline))
			       (list (pos~concatenate position (car args))))
		   position))

      	  ((string-equal tac :c-times)
	   (values (tacl~apply 'c-times
			       (list last-step (car outline))
			       (list (pos~concatenate position (car args))))
		   position))

      	  ((string-equal tac :a-times-left)
	   (values (tacl~apply 'a-times-left
			       (list last-step (car outline))
			       (list (pos~concatenate position (car args))))
		   position))

      	  ((string-equal tac :a-times-right)
	   (values (tacl~apply 'a-times-right
			       (list last-step (car outline))
			       (list (pos~concatenate position (car args))))
		   position))

      	  ((string-equal tac :^1i)
	   (values (tacl~apply '^1i
			       (list last-step (car outline))
			       (list (pos~concatenate position (car args))))
		   position))

      	  ((string-equal tac :^1e)
	   (values (tacl~apply '^1e
			       (list last-step (car outline))
			       (list (pos~concatenate position (car args))))
		   position))

	  ((string-equal tac :^0e)
	   (values (tacl~apply '^0e
			       (list last-step (car outline))
			       (cons (pos~concatenate position (car args))
				     (cdr args)))
		   position))

	  ((string-equal tac :^0i)
	   (values (tacl~apply '^0i
			       (list last-step (car outline))
			       (cons (pos~concatenate position (car args))
				     (cdr args)))
		   position))

	  ((string-equal tac :1^e)
	   (values (tacl~apply '1^e
			       (list last-step (car outline))
			       (cons (pos~concatenate position (car args))
				     (cdr args)))
		   position))

	  ((string-equal tac :1^i)
	   (values (tacl~apply '1^i
			       (list last-step (car outline))
			       (cons (pos~concatenate position (car args))
				     (cdr args)))
		   position))

      	  ((string-equal tac :mult-power)
	   (values (tacl~apply 'mult-power
			       (list last-step (car outline))
			       (list (pos~concatenate position (car args))))
		   position))

      	  ((string-equal tac :split-base)
	   (values (tacl~apply 'split-base
			       (list last-step (car outline))
			       (list (pos~concatenate position (car args))))
		   position))

      	  ((string-equal tac :mult-exp)
	   (values (tacl~apply 'mult-exp
			       (list last-step (car outline))
			       (list (pos~concatenate position (car args))))
		   position))

      	  ((string-equal tac :inflate-power)
	   (values (tacl~apply 'inflate-power
			       (list last-step (car outline))
			       (list (pos~concatenate position (car args))))
		   position))

       	  ((string-equal tac :c-plus)
	   (values (tacl~apply 'c-plus
			       (list last-step (car outline))
			       (list (pos~concatenate position (car args))))
		   position))

      	  ((string-equal tac :a-plus-left)
	   (values (tacl~apply 'a-plus-left
			       (list last-step (car outline))
			       (list (pos~concatenate position (car args))))
		   position))

      	  ((string-equal tac :a-plus-right)
	   (values (tacl~apply 'a-plus-right
			       (list last-step (car outline))
			       (list (pos~concatenate position (car args))))
		   position))
          
      	  ((string-equal tac :div2times)
	   (values (tacl~apply 'div2times
			       (list last-step (car outline))
			       (list (pos~concatenate position (car args))))
		   position))

	  ;; Backward
	  
	  ((string-equal tac :init-mass-b)
	   (values (list (car outline) (car outline))
		   position))
	  	  
	  ((string-equal tac :=ref-b)
;	   (values (tacl~sequence
;                    (s1 ('=ref (list last-step)
;                               (list (data~struct-at-position
;                                      (pds~node-formula last-step)
;                                      (pos~list-position '(1))))))
;                    (s2 ('=ref (list (cadr outline))
;                                    (list (car args)))))
;                   position))
;           (values (tacl~apply 'hypweaken (list (cadr outline) last-step) nil)
;		   position))
          (values (tacl~apply '=ref
                               (list (cadr outline))
			       (list (car args)))
                  position))

	  ((string-equal tac :sink-init-b)
	   (values (tacl~apply 'sink-init-back
			       (list (cadr outline) last-step)
			       (list (pos~concatenate position (car args))))
		   position))

	  ((string-equal tac :plus2minus)
	   (values (tacl~apply 'plus2minus
			       (list (cadr outline) last-step)
			       (list (pos~concatenate position (car args))))
		   position))

	  ((string-equal tac :power2sqrt)
	   (values (tacl~apply 'power2sqrt
			       (list (cadr outline) last-step)
			       (list (pos~concatenate position (car args))))
		   position))

	  ((string-equal tac :expand-num-b)
	   (values (tacl~apply 'expand-num
			       (list (cadr outline) last-step)
			       (list (pos~concatenate position (car args)) (cadr args)))
		   position))

	  ((string-equal tac :pop-plus-b)
	   (values (tacl~apply 'pop-plus
			       (list (cadr outline) last-step)
			       (list (pos~concatenate position (car args))))
		   position))

	  ((string-equal tac :pop-times-b)
	   (values (tacl~apply 'pop-times
			       (list (cadr outline) last-step)
			       (list (pos~concatenate position (car args))))
		   position))

	  ((string-equal tac :cross-swap-plus-b)
	   (values (tacl~apply 'cross-swap-plus
			       (list (cadr outline) last-step)
			       (list (pos~concatenate position (car args))))
		   position))

	  ((string-equal tac :cross-swap-times-b)
	   (values (tacl~apply 'cross-swap-times
			       (list (cadr outline) last-step)
			       (list (pos~concatenate position (car args))))
		   position))

	  ((string-equal tac :split-monomials-plus-b)
	   (values (tacl~apply 'split-monomials-plus
			       (list (cadr outline) last-step)
			       (cons (pos~concatenate position (car args))
				     (cdr args)))
		   position))

	  ((string-equal tac :split-monomials-times-b)
	   (values (tacl~apply 'split-monomials-times
			       (list (cadr outline) last-step)
			       (cons (pos~concatenate position (car args))
				     (cdr args)))
		   position))

	  ((string-equal tac :split-power-b)
	   (values (tacl~apply 'split-power
			       (list (cadr outline) last-step)
			       (list (pos~concatenate position (car args))))
		   position))

	  ((string-equal tac :mult-base-b)
	   (values (tacl~apply 'mult-base
			       (list (cadr outline) last-step)
			       (list (pos~concatenate position (car args))))
		   position))

	  ((string-equal tac :split-exp-b)
	   (values (tacl~apply 'split-exp
			       (list (cadr outline) last-step)
			       (list (pos~concatenate position (car args))))
		   position))

	  ((string-equal tac :deflate-power-b)
	   (values (tacl~apply 'deflate-power
			       (list (cadr outline) last-step)
			       (list (pos~concatenate position (car args))))
		   position))

	  ((string-equal tac :0*i-b)
	   (values (tacl~apply '0*intro
			       (list (cadr outline) last-step)
			       (cons (pos~concatenate position (car args))
				     (cdr args)))
		   position))

	  ((string-equal tac :1*i-b)
	   (values (tacl~apply '1*i
			       (list (cadr outline) last-step)
			       (list (pos~concatenate position (car args))))
		   position))

	  ((string-equal tac :*1i-b)
	   (values (tacl~apply '*1i
			       (list (cadr outline) last-step)
			       (list (pos~concatenate position (car args))))
		   position))

	  ((string-equal tac :+0i-b)
	   (values (tacl~apply '+0i
			       (list (cadr outline) last-step)
			       (list (pos~concatenate position (car args))))
		   position))

	  ((string-equal tac :0+i-b)
	   (values (tacl~apply '0+i
			       (list (cadr outline) last-step)
			       (list (pos~concatenate position (car args))))
		   position))

	  ((string-equal tac :^1i-b)
	   (values (tacl~apply '^1i
			       (list (cadr outline) last-step)
			       (list (pos~concatenate position (car args))))
		   position))

	  ((string-equal tac :^1e-b)
	   (values (tacl~apply '^1e
			       (list (cadr outline) last-step)
			       (list (pos~concatenate position (car args))))
		   position))

	  ((string-equal tac :^0e-b)
	   (values (tacl~apply '^0e
			       (list (cadr outline) last-step)
			       (cons (pos~concatenate position (car args))
				     (cdr args)))
		   position))

	  ((string-equal tac :^0i-b)
	   (values (tacl~apply '^0i
			       (list (cadr outline) last-step)
			       (cons (pos~concatenate position (car args))
				     (cdr args)))
		   position))

	  ((string-equal tac :1^e-b)
	   (values (tacl~apply '1^e
			       (list (cadr outline) last-step)
			       (cons (pos~concatenate position (car args))
				     (cdr args)))
		   position))

	  ((string-equal tac :1^i-b)
	   (values (tacl~apply '1^i
			       (list (cadr outline) last-step)
			       (cons (pos~concatenate position (car args))
				     (cdr args)))
		   position))

	  ((string-equal tac :a-plus-left-b)
	   (values (tacl~apply 'a-plus-right
			       (list (cadr outline) last-step)
			       (list (pos~concatenate position (car args))))
		   position))

	  ((string-equal tac :a-plus-right-b)
	   (values (tacl~apply 'a-plus-left
			       (list (cadr outline) last-step)
			       (list (pos~concatenate position (car args))))
		   position))

	  ((string-equal tac :c-plus-b)
	   (values (tacl~apply 'c-plus
			       (list (cadr outline) last-step)
			       (list (pos~concatenate position (car args))))
		   position))

	  ((string-equal tac :a-times-left-b)
	   (values (tacl~apply 'a-times-right
			       (list (cadr outline) last-step)
			       (list (pos~concatenate position (car args))))
		   position))

	  ((string-equal tac :a-times-right-b)
	   (values (tacl~apply 'a-times-left
			       (list (cadr outline) last-step)
			       (list (pos~concatenate position (car args))))
		   position))

	  ((string-equal tac :c-times-b)
	   (values (tacl~apply 'c-times
			       (list (cadr outline) last-step)
			       (list (pos~concatenate position (car args))))
		   position))

	  ((string-equal tac :cummulate-l-b)
	   (values (tacl~apply 'cummulate-left
			       (list (cadr outline) last-step)
			       (list (pos~concatenate position (car args))))
		   position))

	  ((string-equal tac :cummulate-r-b)
	   (values (tacl~apply 'cummulate-right
			       (list (cadr outline) last-step)
			       (list (pos~concatenate position (car args))))
		   position))

	  ((string-equal tac :times2div-b)
	   (values (tacl~apply 'times2div
			       (list (cadr outline) last-step)
			       (list (pos~concatenate position (car args))))
		   position))

	  (t (values outline position))
	  )))
