;;; -*- syntax: common-lisp; package: OMEGA; base: 10; mode: keim -*-
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
 
(mod~defmod ot :uses (mod sys com arg term prob th omega keim type inter 
			  asi pdsn pds rule)
	    :documentation "Definition of OMEGA argument types."
	    :exports ())

;;;begin{latex}
;;;\chapter{Omega argument types}
;;; We define various argument types for the Omega commands. 
;;;end{latex}


;;; The TERM argtype is the class of all terms.

(eval-when (load compile eval)
(arg~deftype term
 (predicate term~p)
 (read-function ot~read-term)
 (help "a term"))
)

(defun ot=read-term (obj type)
  (sys~handler-case 
   ;;(term~read obj (prob~environment omega*current-proof-plan))
   (post~read-object obj (pds~environment omega*current-proof-plan) :existing-term-closed)
   (error () (arg~signal-wrong-type type obj))))

(defmethod ot~read-term ((obj term+term) &rest others)
  (declare (ignore others))
  obj)

(defmethod ot~read-term ((obj t) &rest others)
  (declare (ignore others))
  (ot=read-term obj 'term))

;;; The TYPE argtype is the class of all types.

(eval-when (load compile eval)
  (arg~deftype type
	       (predicate type~p)
	       (read-function ot~read-type)
	       (help "a type"))
  )

(defun ot=read-type (obj type)
  (sys~handler-case 
   ;;(term~read obj (prob~environment omega*current-proof-plan))
   (post~read-object obj (pds~environment omega*current-proof-plan) :existing-type)
   (error () (arg~signal-wrong-type type obj))))

(defmethod ot~read-type ((obj type+type) &rest others)
  (declare (ignore others))
  obj)

(defmethod ot~read-type ((obj t) &rest others)
  (declare (ignore others))
  (ot=read-type obj 'type))

;;; The FORMULA argtype is the class of all formulas.

(eval-when (load compile eval)
(arg~deftype formula
 (predicate ot~formula-p)
 (read-function ot~read-formula)
 (help "a formula"))
)

(defun ot~formula-p (obj)
  (declare
   (authors nesmith)
   (input "a lisp object")
   (value "T if obj is a formula, otherwise nil"))
  (and (term~p obj)
       (or (and (null (data~schema-p obj))
		(keim~equal (type~o) (term~type obj)))
	   (and (data~schema-p obj)
		(keim~equal (type~o) (term~type (data~schema-range obj)))))))

(defmethod ot~read-formula (obj &rest others)
  (declare (ignore others))
  (let ((newobj (ot~read-term obj)))
    (if (ot~formula-p newobj)
	newobj
	(arg~signal-wrong-type 'formula obj))))

;;; The TERMSYM argtype is the class of all terms which are symbols.

(eval-when (load compile eval)
(arg~deftype termsym
 (predicate term~primitive-p)
 (read-function ot~read-termsym)
 (help "a term symbol"))
)

(defmethod ot~read-termsym (obj &rest others)
  (declare (ignore others))
  (let ((newobj (ot~read-term obj)))
    (if (term~primitive-p newobj)
	newobj
	(arg~signal-wrong-type 'termsym obj))))

;;; The TERMVAR argtype is the class of all terms which are variables.

(eval-when (load compile eval)
(arg~deftype termvar
 (predicate term~variable-p)
 (read-function ot~read-termvar)
 (help "a term variable"))
)

(defmethod ot~read-termvar (obj &rest others)
  (declare (ignore others))
  (let ((newobj (ot~read-term obj)))
    (if (term~variable-p newobj)
	newobj
	(arg~signal-wrong-type 'termvar obj))))


;;; The INFERENCE argtype is the class of all inference methods.

(eval-when (load compile eval)
(arg~deftype inference
 (predicate infer~p)
 (read-function ot~read-inference)
 (help "an inference rule"))
)

(defun ot=read-inference (obj)
  ;;  (sys~handler-case 
  (let ((rule (infer~find-method obj)))
    (if rule
	rule
      (arg~signal-wrong-type 'inference obj))))

(defmethod ot~read-inference ((obj infer+inference) &rest others)
  (declare (ignore others))
  obj)

(defmethod ot~read-inference ((obj symbol) &rest others)
  (declare (ignore others))
  (ot=read-inference obj))

(defmethod ot~read-inference ((obj string) &rest others)
  (declare (ignore others))
  (ot=read-inference obj))


;;; The NDLINE argtype is the class of natural deduction lines.

(eval-when (load compile eval)
(arg~deftype ndline 
 (predicate ot~ndline-p)
 (read-function ot~read-ndline)
 (help "a natural deduction line"))
)

(defun ot~ndline-p (line)
  (or (null line) (pdsn~p line)))

(defmethod ot~read-ndline ((obj pdsn+node) &rest others)
  (declare (ignore others))
  obj)

(defun ot=read-ndline-sym-or-string (obj)
  (if (pds~proof-plan-p omega*current-proof-plan)
    (let* ((keim::pds*current-proof-plan omega*current-proof-plan)
	   (line (pds~label2node obj)))
      (if (or line (null obj))
	  line
	(arg~signal-wrong-type 'ndline obj)))
    (progn 
      (inter~print-error (comint~interface comint*current-comint) 
			 "There is no current proof.")
      (arg~signal-wrong-type 'ndline obj))))

(defmethod ot~read-ndline ((obj symbol) &rest others)
  (declare (ignore others))
  (ot=read-ndline-sym-or-string obj)
)

(defmethod ot~read-ndline ((obj string) &rest others)
  (declare (ignore others))
  (ot=read-ndline-sym-or-string obj))


;;; The NDLINE-OR-TERM argtype is the class of list of natural deduction lines.
(eval-when (load compile eval)
(arg~deftype ndline-or-term
  (predicate ot~ndline-or-formula-p)
  (read-function ot~read-ndline-or-term)
  (help "a natural deduction line or a term"))
)

(defun ot~ndline-or-formula-p (obj)
  (declare (edited  "26-AUG-1993 08:56")
	   (authors AFIEDLER)
	   (input   "A lisp object")
	   (value   "T if OBJ is a natural deduction line or a term, otherwise NIL"))
  (or (ot~term-p obj)
      (pdsn~node-p obj)))

(defmethod ot~read-ndline-or-term (obj &rest others)
  (declare (ignore others))
  (sys~handler-case
   (ot~read-ndline obj)
   (error () (sys~handler-case
	      (ot~read-term obj)
	      (error () (arg~signal-wrong-type 'ndline-or-term obj))))))

(defmethod ot~read-ndline-or-term ((obj term+term) &rest others)
  (declare (ignore others))
  (ot~read-term obj))


;;; The PDS-CLSD-NODE argtype is the class of PDS nodes which are closed.

(eval-when (load compile eval)
(arg~deftype pds-clsd-node
 (predicate ot~pds-clsd-node-p)
 (read-function ot~read-pds-clsd-node)
 (help "a pds closed node"))
)

(defun ot~pds-clsd-node-p (obj)
  (declare (edited  "02-MAY-1997")
	   (authors Lassaad)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "T, iff OBJ a closed node in the current PDS."))
  (and (find obj (prob~proof-steps omega*current-proof-plan))
       (not (pdsn~open-node-p obj))))

(defun ot=read-pds-clsd-node-sym-or-string (obj)
  (if (pds~proof-plan-p omega*current-proof-plan)
      (let ((line (sys~handler-case
		   (ot=read-ndline-sym-or-string obj)
		   (arg+wrong-type-error (c)
					 (setf (arg~input-error-expected c)
					       (arg~find-argtype 'ndplanline))
					 (sys~signal c)))))
	(if (pdsn~open-node-p line)
	    (arg~signal-wrong-type 'pds-clsd-node obj)
	    line
	    ))
      (progn 
	(inter~print-error (comint~interface comint*current-comint) 
			   "There is no current proof.")
	(arg~signal-wrong-type 'pds-clsd-node obj))))

(defmethod ot~read-pds-clsd-node ((obj string) &rest others)
  (declare (ignore others))
  (ot=read-pds-clsd-node-sym-or-string obj))

(defmethod ot~read-pds-clsd-node ((obj symbol) &rest others)
  (declare (ignore others))
  (ot=read-pds-clsd-node-sym-or-string obj))

(defmethod ot~read-pds-clsd-node ((obj pdsn+node) &rest others)
  (declare (ignore others))
  (if (pdsn~open-node-p obj)
      (arg~signal-wrong-type 'pds-clsd-node obj)
      obj
      ))

;;; The NDPLANLINE argtype is the class of natural deduction lines which are
;;; planned lines.

(eval-when (load compile eval)
(arg~deftype ndplanline
 (predicate pdsn~open-node-p)
 (read-function ot~read-ndplanline)
 (help "a planned line"))
)

(defun ot=read-ndplanline-sym-or-string (obj)
  (if (pds~proof-plan-p omega*current-proof-plan)
      (let ((line (sys~handler-case
		   (ot=read-ndline-sym-or-string obj)
		   (arg+wrong-type-error (c)
					 (setf (arg~input-error-expected c)
					       (arg~find-argtype 'ndplanline))
					 (sys~signal c)))))
	(if (pdsn~open-node-p line)
	    line
	    (arg~signal-wrong-type 'ndplanline obj)))
      (progn 
	(inter~print-error (comint~interface comint*current-comint) 
			   "There is no current proof.")
	(arg~signal-wrong-type 'ndplanline obj))))

(defmethod ot~read-ndplanline ((obj string) &rest others)
  (declare (ignore others))
  (ot=read-ndplanline-sym-or-string obj))

(defmethod ot~read-ndplanline ((obj symbol) &rest others)
  (declare (ignore others))
  (ot=read-ndplanline-sym-or-string obj))

(defmethod ot~read-ndplanline ((obj pdsn+node) &rest others)
  (declare (ignore others))
  (if (pdsn~open-node-p obj)
      obj
      (arg~signal-wrong-type 'ndplanline obj)))

;;; The PROOF-PLAN argtype is the class of natural deduction proofs.
;;; If given a PROB+PROBLEM, the read function will construct a new
;;; PROOF-PLAN with the assumptions, conclusion and environment of the
;;; given problem.  If a symbol or string is given, it will be looked up
;;; in the global hash-table of all proof-plans. 

(eval-when (load compile eval)
(arg~deftype proof-plan
 (predicate pds~proof-plan-p)
 (read-function ot~read-proof-plan)
 (help "a natural deduction proof"))
)

(defun ot=read-proof-plan-sym-or-string (obj)
  (let ((proof (pds~find-proof-plan obj))
	(prob (prob~find-problem obj)))
    (cond (proof (ot~read-proof-plan proof))
	  (prob (ot~read-proof-plan prob))
	  (t 
	   (arg~signal-wrong-type 'proof-plan obj)))))

(defmethod ot~read-proof-plan ((obj string) &rest others)
  (declare (ignore others))
  (ot=read-proof-plan-sym-or-string obj))

(defmethod ot~read-proof-plan ((obj symbol) &rest others)
  (declare (ignore others))
  (ot=read-proof-plan-sym-or-string obj))

(defmethod ot~read-proof-plan ((obj pds+proof-plan) &rest others)
  (declare (ignore others))
  (setq keim::pds*current-proof-plan obj)
  obj)

(defmethod ot~read-proof-plan ((obj prob+problem) &rest others)
  (declare (ignore others))
  obj)

(defun ot~new-proof-plan-name (problem)
  (declare (edited  "11-JUN-1997")
	   (authors Sorge)
	   (input   "A problem.")
	   (value   "A new standard proof-plan name for the problem."))
  (let ((name (keim~name problem)))
    (do* ((x 1 (1+ x))
	  (symb (make-symbol (format nil "~A-~A" name x)) (make-symbol (format nil "~A-~A" name x))))
	((not (prob~find-proof symb)) symb))))
  
;;; The RESOLUTION-PROOF argtype is the class of the resolution proofs.

(eval-when (load compile eval)
(arg~deftype resolution-proof
 (predicate res~proof-p)
 (read-function ot~read-resolution-proof)
 (help "a resolution proof"))
)

(defmethod ot~read-resolution-proof ((obj res+proof) &rest others)
  (declare (ignore others))
  obj)

(defmethod ot~read-resolution-proof ((obj symbol) &rest others)
  (declare (ignore others))
  (res~find-proof obj))

;;; The PROBLEM argtype is the class of natural deduction proofs.
;;; If given a PROB+PROBLEM, the read function will construct a new
;;; PROBLEM with the assumptions, conclusion and environment of the
;;; given problem.  If a symbol or string is given, it will be looked up
;;; in the global hash-table of all PROBLEMs. 

(eval-when (load compile eval)
(arg~deftype problem
 (predicate prob~p)
 (read-function ot~read-problem)
 (help "a KEIM problem"))
)

(eval-when (load compile eval)
(arg~deftype theorem
 (predicate ot=theorem-p)
 (read-function ot~read-theorem)
 (help "a KEIM problem"))
)

(defun ot=theorem-p (prob)
  (and (prob~p prob) (prob~proven-p prob)))

(defmethod ot~read-theorem (obj &rest others)
  (let ((thm (ot~read-problem obj)))
    (if (and thm (prob~proven-p thm))
	thm
      (arg~signal-wrong-type 'theorem obj))))

(defun ot=read-problem-sym-or-string (obj)
  (let ((prob (prob~find-problem obj)))
    (if prob 
	prob
	(arg~signal-wrong-type 'problem obj))))

(defmethod ot~read-problem ((obj string) &rest others)
  (declare (ignore others))
  (ot=read-problem-sym-or-string obj))

(defmethod ot~read-problem ((obj symbol) &rest others)
  (declare (ignore others))
  (ot=read-problem-sym-or-string obj))

(defmethod ot~read-problem ((obj prob+problem) &rest others)
  (declare (ignore others))
  obj)


;;; THY-ASSUMPTION is the class of all assumptions available in the theory of
;;; the current proof plan OMEGA*CURRENT-PROOF-PLAN.

(eval-when (load compile eval)
(arg~deftype thy-assumption
 (predicate ot~thy-assumption-p)
 (read-function ot~read-thy-assumption)
 (help "a theory theorem"))
)

(defun ot~thy-assumption-p (obj)
  (repr~find-definition (keim~name obj)
		      (if omega*current-proof-plan
			  (prob~proof-theory omega*current-proof-plan)
			omega*current-theory)))

(defun ot=read-thy-assumption-sym-or-string (obj)
  (let* ((theory (if omega*current-proof-plan
		     (prob~proof-theory omega*current-proof-plan)
		   omega*current-theory))
	 (ass (repr~find-definition obj theory)))
    (if ass
	ass
      (arg~signal-wrong-type 'thy-assumption obj))))

(defmethod ot~read-thy-assumption((obj string) &rest others)
  (declare (ignore others))
  (ot=read-thy-assumption-sym-or-string obj))

(defmethod ot~read-thy-assumption ((obj symbol) &rest others)
  (declare (ignore others))
  (ot=read-thy-assumption-sym-or-string obj))

(defmethod ot~read-thy-assumption ((obj list) &rest others)  ; for special terms sets/lists/cycles
  (declare (ignore others))
  (ot=read-thy-assumption-sym-or-string obj))

(defmethod ot~read-thy-assumption ((obj integer) &rest others)  ; for special terms number
  (declare (ignore others))
  (ot=read-thy-assumption-sym-or-string obj))


;;; THY-PROBLEM is the class of all problems available in a theory 
(eval-when (load compile eval)
(arg~deftype thy-problem
 (predicate ot~thy-problem-p)
 (read-function ot~read-thy-problem)
 (help "a theory problem"))
)

(defun ot~thy-problem-p (obj)
  (th~find-problem (keim~name obj) (prob~theory omega*current-proof-plan)))

(defun ot=read-thy-problem-sym-or-string (obj)
  (let* ((prob (prob~find-problem obj)))
    (if prob
	prob
      (arg~signal-wrong-type 'thy-problem obj))))

(defmethod ot~read-thy-problem((obj string) &rest others)
  (declare (ignore others))
  (ot=read-thy-problem-sym-or-string obj))

(defmethod ot~read-thy-problem ((obj symbol) &rest others)
  (declare (ignore others))
  (ot=read-thy-problem-sym-or-string obj))


;;; The POSITION argtype is the class of positions.

(eval-when (load compile eval)
(arg~deftype position
 (predicate pos~p)
 (read-function ot~read-position)
 (help "a position"))
)

(defun ot=read-position (obj)
  (sys~handler-case 
   (pos~list-position obj)
   (error () (arg~signal-wrong-type 'position obj))))

(defmethod ot~read-position ((obj pos+position) &rest others)
  (declare (ignore others))
  obj)

(defmethod ot~read-position ((obj list) &rest others)
  (declare (ignore others))
  (ot=read-position obj))

;;; some methods to output some of our datastructures in OMEGA

(defmethod inter~output-object ((inter asi+inter) (line pdsn+node))
  (inter~output-object inter (symbol-name (keim~name line))))

(defmethod inter~output-object ((inter asi+inter) (just just+justification))
  (let ((prem (just~premises just)))
    (inter~output-object (comint~interface comint*current-comint)
			 (format nil "justified by ~A" (keim~name (just~method just))))
    (when prem (inter~output-object (comint~interface comint*current-comint)
				    (format nil " from ~A" (mapcar #'keim~name prem))))))

(defmethod inter~output-object ((inter asi+inter) (problem prob+problem))
  (inter~output-object inter (format nil "Problem ~A~%" (keim~name problem)))
  (dolist (x (prob~assumptions problem))
    (inter~output-object inter (format nil "Assumption:  ~A         ~S"
				       (keim~name x) (node~formula x)))
    (inter~terpri inter))
  (inter~output-object inter (format nil "Conclusion:  ~A        ~S" (keim~name (prob~conclusion problem))
				     (node~formula (prob~conclusion problem)))))



;; Substitution Type

(eval-when (load compile eval)
  (arg~deftype substitution
	       (predicate subst~p)
	       (read-function ot~read-substitution)
	       (help "a substitution"))
  )

(defun ot=read-substitution (obj type)
  (sys~handler-case 
   (post~read-object obj (pds~environment omega*current-proof-plan) :substitution)
   (error () (arg~signal-wrong-type type obj))))

(defmethod ot~read-substitution ((obj term+term) &rest others)
  (declare (ignore others))
  obj)

(defmethod ot~read-substitution ((obj t) &rest others)
  (declare (ignore others))
  (ot=read-substitution obj 'substitution))


#{ The {\vb existing-\{pds|post|rpy\}-file} argtype consists of all \lisp\ pathnames of
 existing pds, post, or rpy files.  If given a string, it will make a pathname directly 
 from it. The appropriate extension will be added if not given.
 If given a symbol, it will lower case the symbol's 
 print name and make a pathname from it. In any case it will 
 check to see if the file actually exists before returning the pathname.
#}
(eval-when (load compile eval)
(arg~deftype existing-pds-file
  (read-function ot~read-existing-pds-file)
  (predicate ot~existing-pds-file-p)
  (help "an existing pds file"))
)

(defun ot~pds-pathname (path)
  (declare (edited  "09-SEP-1997")
	   (authors Afiedler)
	   (input   "A path.")
	   (value   "A path with extension \".pds\", if no extension is given, otherwise"
		    "PATH."))
  (merge-pathnames path "*.pds"))

(defun ot~existing-pds-file-p (pathname)
  (declare
   (authors nesmith)
   (input "a pathname")
   (value "T if pathname names an existing pds file, otherwise nil"))
  (let ((result (probe-file (ot~pds-pathname pathname))))
    (if result t nil)))

(defmethod ot~read-existing-pds-file (object &rest others)
  (declare (ignore others))
  (let ((obj (ot~pds-pathname object)))
    (if (typep obj 'pathname)
	(let ((result (probe-file obj)))
	  (if result
	      result
	    (arg~signal-wrong-type 'existing-file obj)))
      (error "~A is not of a valid type" obj))))

(defmethod ot~read-existing-pds-file ((object symbol) &rest others)
  (declare (ignore others))
  (ot~read-existing-pds-file (string-downcase (symbol-name object))))


(eval-when (load compile eval)
(arg~deftype existing-post-file
  (read-function ot~read-existing-post-file)
  (predicate ot~existing-post-file-p)
  (help "an existing post file"))
)

(defun ot~post-pathname (path)
  (declare (edited  "09-SEP-1997")
	   (authors Afiedler)
	   (input   "A path.")
	   (value   "A path with extension \".post\", if no extension is given, otherwise"
		    "PATH."))
  (merge-pathnames path "*.post"))

(defun ot~existing-post-file-p (pathname)
  (declare
   (authors nesmith)
   (input "a pathname")
   (value "T if pathname names an existing post file, otherwise nil"))
  (let ((result (probe-file (ot~post-pathname pathname))))
    (if result t nil)))

(defmethod ot~read-existing-post-file (object &rest others)
  (declare (ignore others))
  (let ((obj (ot~post-pathname object)))
    (if (typep obj 'pathname)
	(let ((result (probe-file obj)))
	  (if result
	      result
	    (arg~signal-wrong-type 'existing-file obj)))
      (error "~A is not of a valid type" obj))))

(defmethod ot~read-existing-post-file ((object symbol) &rest others)
  (declare (ignore others))
  (ot~read-existing-post-file (string-downcase (symbol-name object))))

(eval-when (load compile eval)
(arg~deftype existing-rpy-file
  (read-function ot~read-existing-rpy-file)
  (predicate ot~existing-rpy-file-p)
  (help "an existing rpy file"))
)

(defun ot~rpy-pathname (path)
  (declare (edited  "09-SEP-1997")
	   (authors Afiedler)
	   (input   "A path.")
	   (value   "A path with extension \".rpy\", if no extension is given, otherwise"
		    "PATH."))
  (merge-pathnames path "*.rpy"))

(defun ot~existing-rpy-file-p (pathname)
  (declare
   (authors nesmith)
   (input "a pathname")
   (value "T if pathname names an existing rpy file, otherwise nil"))
  (let ((result (probe-file (ot~rpy-pathname pathname))))
    (if result t nil)))

(defmethod ot~read-existing-rpy-file (object &rest others)
  (declare (ignore others))
  (let ((obj (ot~rpy-pathname object)))
    (if (typep obj 'pathname)
	(let ((result (probe-file obj)))
	  (if result
	      result
	    (arg~signal-wrong-type 'existing-file obj)))
      (error "~A is not of a valid type" obj))))

(defmethod ot~read-existing-rpy-file ((object symbol) &rest others)
  (declare (ignore others))
  (ot~read-existing-rpy-file (string-downcase (symbol-name object))))


;;; We also define list argtype for each of the above: TERM-LIST, TERMSYM-LIST,
;;; TERMVAR-LIST, FORMULA-LIST, NDLINE-LIST, NDPLANLINE-LIST, and PROOF-PLAN-LIST.

(arg~deflisttype term-list term)

(arg~deflisttype type-list type)

(arg~deflisttype termsym-list termsym)

(arg~deflisttype termvar-list termvar)

(arg~deflisttype formula-list formula)

(arg~deflisttype inference-list inference)

(arg~deflisttype ndline-list ndline)

(arg~deflisttype ndline-or-term-list ndline-or-term)

(arg~deflisttype ndplanline-list ndplanline)

(arg~deflisttype proof-plan-list proof-plan)

(arg~deflisttype thy-ass-list thy-assumption)

(arg~deflisttype thy-prob-list thy-problem)

(eval-when (load compile eval)
  (arg~deflisttype position-list position)
)

(arg~deflisttype position-list-list position-list)





