;;; -*- syntax: common-lisp; package: keim; base: 10; mode: lisp -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1994 by AG Siekmann, Fachbereich Informatik,             ;;
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

(in-package :keim)

(mod~defmod tex :uses (env keim pds th node post pp prob 
			   term type logic)
	       :exports
	       (tex~print-proof tex~style-p)
	       :documentation
	       "Definitions for pretty-printing formulas and ND proofs in
TeX style.")


#{
\section{\TeX\ Pretty-printing } 

This module defines some pretty printing styles for printing formulas and
ND proofs in a \TeX\ format.  The output is pure \TeX\ but requires the
file {\it proof.tex\/}, which should be found in the \keim\ doc directory.

We use the style definition mechanism defined in the module {\tt PP}.
In order to get various effects, we must define a number of new Lisp types. 
These are not exported. 

Below we summarize the styles defined and give an example formula as they
would print it. 
\begin{itemize}
\item {\tt TEX-STYLE}: A general \TeX\ style. $\Pi [\lambda X_I [\vee [P_{(OII)} X_I X_I] [P_{(OII)} A_I A_I]]]$
\item {\tt TEX-STYLE-FO}: Inherits from {\tt TEX-STYLE}, but first-order terms
and atoms are printed differently, types are not printed, and quantifiers are
used instead of $\Pi$ and $\Sigma$: 
$\forall X [\vee P[X,X] P [A,A]]$
\item {\tt TEX-STYLE-FO-INFIX}: Inherits from {\tt TEX-STYLE-FO}, with infix
operators (at the moment only the ordinary connectives) being printed infix:
$\forall X [P[X,X] \vee  P[A,A]]$
\item {\tt TEX-STYLE-HO}: Inherits from {\tt TEX-STYLE}.  Differs only in that
quantified formulas use normal quantifiers instead of $\Pi$ and $\Sigma$.
$\forall X_I [\vee [P_{(OII)} X_I X_I] [P_{(OII)} A_I A_I]]$
\item {\tt TEX-STYLE-HO-INFIX}: Inherits from {\tt TEX-STYLE-HO}.  Prints infix
connectives infix. $\forall X_I [[P_{(OII)} X_I X_I] \vee [P_{(OII)} A_I A_I]]$
\end{itemize}

Current problems with this implementation include:
\begin{itemize}
\item The breaks made in formulas are not always optimal. Some editing of
the \TeX\ output file may be necessary.
\item The relative widths of the various fields for lines 
(labels, hypotheses, formula, justification) are not dynamically chosen,
but are preset.  If one of them is too wide or narrow, the \TeX\
parameters should be reset (see {\tt proof.tex}).
\item The \TeX\ output is done in an abstract way to allow the formula-breaking
algorithms in \TeX\ to function; this makes reading the \TeX\ file
dependent upon understanding the macros in {\tt proof.tex}.
\end{itemize}
#}

(deftype tex=quantor ()
  '(and term+constant (satisfies tex=quantor-p)))
(deftype tex=junctor ()
  '(and term+constant (satisfies tex=junctor-p)))
(deftype tex=const-atom ()
  '(and term+constant (satisfies logic~atom-p)))
(deftype tex=quantification ()
  '(and term+appl (or (satisfies logic~universal-quantification-p)
		      (satisfies logic~existential-quantification-p))))
(deftype tex=junction ()
  '(and term+appl (satisfies tex=junction-p)))
(deftype tex=appl-atom ()
  '(and term+appl (satisfies logic~atom-p)))
(deftype tex=left-bracket ()
  '(satisfies tex=left-bracket-p))
(deftype tex=right-bracket ()
  '(satisfies tex=right-bracket-p))
(deftype tex=comma ()
  '(satisfies tex=comma-p))

(eval-when (load compile eval)
(defconstant tex*left-bracket (cons nil nil))
(defconstant tex*right-bracket (cons nil nil))
(defconstant tex*comma (cons nil nil))
)

(defun tex=left-bracket-p (x)
  (eq x tex*left-bracket))
(defun tex=right-bracket-p (x)
  (eq x tex*right-bracket))
(defun tex=comma-p (x)
  (eq x tex*comma))

(eval-when (load compile eval)
(defvar tex*env nil)
(defvar tex*junctordepth 0)

)

(eval-when (compile eval load)
(defmacro tex=term-equal-p (term name)
  `(let ((lookup (env~lookup-object ',name tex*env)))
     (and lookup
	  (data~equal-p ,term lookup)))))

  
(defun tex=quantor-p (term)
  (declare (edited  "07-APR-1993 16:48")
	   (authors SCHEJA)
	   (input  "A term." )
	   (effect "None." )
	   (value  "True, iff term is a quantor." ))
  (or (logic~existential-quantor-p term)
      (logic~universal-quantor-p term)))

(defun tex=and-p (term)
  (declare (edited  "08-APR-1993 17:23")
	   (authors SCHEJA)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "True, if term is an 'and out of
the global environment."))
  (tex=term-equal-p term and))


(defun tex=or-p (term)
  (declare (edited  "08-APR-1993 17:23")
	   (authors SCHEJA)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "True, if term is an 'or out of
the global environment."))
  (tex=term-equal-p term or))


(defun tex=impl-p (term)
  (declare (edited  "08-APR-1993 17:23")
	   (authors SCHEJA)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "True, if term is an 'impl out of the global environment."))
  (tex=term-equal-p term implies))

(defun tex=equiv-p (term)
  (declare (edited  "08-APR-1993 17:23")
	   (authors SCHEJA)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "True, if term is an 'and out of
the global environment."))
  (tex=term-equal-p term equiv))


(defun tex=not-p (term)
  (declare (edited  "08-APR-1993 17:23")
	   (authors SCHEJA)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "True, if term is an 'not out of the global environment."))
  (tex=term-equal-p term not))
  
(defun tex=junction-p (term)
  (declare (edited  "06-JUL-1993 20:08")
	   (authors SCHEJA)
	   (input  "A term." )
	   (effect "None." )
	   (value  "T, iff term is a conjunction, an equivalence, an implication,
a disjunction or negation."))
  (tex=junctor-p (data~top term)))

(defun tex=junctor-p (term)
  (declare (edited  "07-APR-1993 16:48")
	   (authors SCHEJA)
	   (input  "A term." )
	   (effect "None." )
	   (value  "True, iff term is a junctor." ))
  (or (tex=term-equal-p term not)
      (tex=term-equal-p term or)
      (tex=term-equal-p term and)
      (tex=term-equal-p term implies)
      (tex=term-equal-p term equiv)))


(eval-when (load compile eval)
  (defmacro tex=print-appl-basic (stream appl)
    `(let ((*standard-output* ,stream)
	   (appl ,appl))
       (pprint-logical-block 
	(nil nil)
	(write tex*left-bracket)
	(write (data~appl-function appl))
	(pprint-logical-block 
	 (nil (data~appl-arguments appl))
	 (loop (pprint-exit-if-list-exhausted)
	       (write (pprint-pop))))
	(write tex*right-bracket)))))


; start with all priorities of 10
(pp~defstyle tex-style
	     :help "A simple TeX style for printing."
	     :pprint-methods 
	     ((symbol
	       (lambda (stream sym)
		 (write-string (symbol-name sym) stream)))
	      (pdsn+node
	       (lambda (stream line)
		 (let ((label (keim~name line))
		       (hyps (pdsn~hyps line))
		       (formula (node~formula line))
		       (just (node~justification line))
		       (*standard-output* stream))
		   (pprint-newline :mandatory)
		   (write-string "%%--------------- Line with label ")
		   (write label)
		   (write-string "----------------------------")
		   (pprint-newline :mandatory)
		   (when (pdsn~open-node-p line)
		     (pprint-newline :mandatory)
		     (write-string "\\centerline{\\vdots}" stream)
		     (pprint-newline :mandatory))
		   (write-string "\\proofline%%")
		   (pprint-newline :mandatory)
		   (pprint-logical-block
		    (nil nil)
		    (write-string "{\\label\\labelwidth{")
		    (write label)
		    (write-string "}}")
		    (pprint-newline :mandatory)
		    (pprint-logical-block 
		     (nil (mapcar #'keim~name hyps)
			  :prefix "{\\hyps\\hypwidth{{\\hyp{}}"
			  :suffix "}}")
		     (loop (pprint-exit-if-list-exhausted)
			   (write-string "{\\hyp{")
			   (write (pprint-pop))
			   (write-string "}}")))
		    (pprint-newline :mandatory)
		    (write-string "{\\formula\\formulawidth{")
		    (write formula)
		    (write-string "}}")
		    (pprint-newline :mandatory)
		    (write just)
		    ))) 10)
	      (pdsj+justification
	       (lambda (stream just)
		 (pprint-logical-block 
		  (stream (mapcar #'keim~name (just~premises just))
			  :prefix "{\\just\\justwidth{\\justrule" 
			  :suffix "}}")
		  (write-char #\{)
		  (write (keim~name (just~method just)) :stream stream)
		  (write-char #\})
;		  (pprint-logical-block
;		   (stream
;		    (pdsn~parameters just))
;		   (pprint-exit-if-list-exhausted)
;		   (loop (write-char #\$)
;			 (write (pprint-pop))
;			 (write-char #\$)
;			 (pprint-exit-if-list-exhausted)
;			 (write-char #\space)))
		  (pprint-exit-if-list-exhausted)
		  (loop (write-string "\\justline{")
			(write (pprint-pop))
			(write-string "}")
			(pprint-exit-if-list-exhausted)
			(write-char #\,)))) 10)
	      (pds+proof-plan
	       (lambda (stream proof)
		 (dolist (line (prob~proof-steps proof))
		   (write line :stream stream)
		   )) 10)
	      (term+variable
	       (lambda (stream term)
		 (write-string "\\var{{" stream)
		 (write (keim~name term) :stream stream)
		 (write (term~type term) :stream stream)
		 (write-string "}}" stream)) 10)
	      (term+constant
	       (lambda (stream term)
		 (write-string "\\const{{" stream)
		 (write (keim~name term) :stream stream)
		 (write (term~type term) :stream stream)
		 (write-string "}}" stream)) 10)
	      (tex=quantor
	       (lambda (stream term)
		 (write-string "\\quantor{{" stream)
		 (write-string (if (logic~universal-quantor-p term)
			    "\\uniquant"
			    "\\exquant") stream)
		 (write (term~type term) :stream stream)
		 (write-string "}}" stream)) 11)
	      (tex=junctor
	       (lambda (stream term)
		 (write-string "\\junctor" stream)
		 (write-string 
		  (cond ((tex=and-p term) "\\wedge")
			((tex=or-p term) "\\vee")
			((tex=impl-p term) "\\Rightarrow")
			((tex=equiv-p term) "\\Leftrightarrow")
			((tex=not-p term) "\\neg")
			(t 
			 (error "~a is not declared as junctor up to now." 
				term)))
		  stream)) 11)
	      (tex=const-atom
	       (lambda (stream term)
		 (pprint-newline :mandatory stream)
		 (write-string "\\beginning\\atom\\const{{" stream)
		 (write (keim~name term) :stream stream)
		 (write (term~type term) :stream stream)
		 (write-string "}}\\endof\\atom" stream)) 11)
	      (term+appl
	       (lambda (stream term)
		 (tex=print-appl-basic stream term)) 10)
	      (tex=quantification
	       (lambda (stream term)
		 (pprint-newline :mandatory stream)
		 (write-string "\\beginning\\quantification" stream)
		 (tex=print-appl-basic stream term)
		 (write-string "\\endof\\quantification" stream)) 11)
	      (tex=junction
	       (lambda (stream term)
		 (let ((tex*junctordepth (1+ tex*junctordepth)))
		   (pprint-newline :mandatory stream)
		   (write-string "\\beginning\\junction" stream)
		   (tex=print-appl-basic stream term)
		   (write-string "\\endof\\junction" stream))) 11)
	      (tex=appl-atom 
	       (lambda (stream term)
		   (pprint-newline :mandatory stream)
		   (write-string "\\beginning\\atom" stream)
		   (tex=print-appl-basic stream term)
		   (write-string "\\endof\\atom" stream)) 11)
	      (term+abstr
	       (lambda (stream term)
		 (write tex*left-bracket :stream stream)
		 (write-string "\\lam\\lambda " stream)
		 (write (data~abstr-domain term) :stream stream)
		 (write-string " \\abstrpoint\\point " stream)
		 (write (data~abstr-range term) :stream stream)
		 (write tex*right-bracket :stream stream)) 10)
	      (tex=left-bracket
	       (lambda (stream x)
		 (declare (ignore x))
		 (write-string "\\bracket\\lb" stream)) 10)
	      (tex=right-bracket
	       (lambda (stream x)
		 (declare (ignore x))
		 (write-string "\\bracket\\rb" stream)) 10)
	      (tex=comma
	       (lambda (stream x)
		 (declare (ignore x))
		 (write-string "\\sign\\comma" stream)) 10)
	      (type+type
	       (lambda (stream type)
		 (write-string "\\type{" stream)
		 (write-string (post~string type) stream)
		 (write-string "}" stream)
		 ) 10 )
	      ))

(defun tex=fo-appl-term-p (term)
  (fo~function-p (data~appl-function term)))

(defun tex=fo-appl-atom-p (term)
  (fo~predicate-p (data~appl-function term)))

(deftype tex=fo-appl-term () 
  `(and term+appl (satisfies tex=fo-appl-term-p)))
(deftype tex=fo-appl-atom () ;(fo~predicate-p function)
  `(and term+appl (satisfies tex=fo-appl-atom-p)))


(deftype tex=infix-appl () ;(fo~predicate-p function)
  `(and term+appl (satisfies tex=infix-appl-p)))

(eval-when (load compile eval)
(defvar tex*infix-names 
  (list "AND" "OR" "IMPLIES" "EQUIV" "="))
)
(defun tex=infix-appl-p (term)
  (and (data~appl-p term)
       (term~primitive-p (data~appl-function term))
       (member (keim~name (data~appl-function term))
	       tex*infix-names
	       :test #'string=)))


; bump priorities up to 12
(pp~defstyle tex-style-fo  :parent tex-style
	     :help "A simple TeX style for printing first order formulas."
	     :pprint-methods 
	     ((tex=quantor
	       (lambda (stream term)
		 (write-string (if (logic~universal-quantor-p term)
			    "\\q\\forall"
			    "\\q\\exists") stream)) 12)
	      (tex=quantification
	       (lambda (stream term)
		 (write-string "\\beginning\\quantification" stream)
		 (write tex*left-bracket :stream stream)
		 (write (data~appl-function term) 
			:stream stream)
		 (write (logic~quantification-bound-variable term)
			:stream stream)
		 (write (logic~quantification-scope term)
			:stream stream)
		 (write-string "\\bracket\\rb\\endof\\quantification"
			       stream)) 12)
	      (tex=fo-appl-atom 
	       (lambda (stream term)
		 (let ((function (data~appl-function term))
		       (arguments (data~appl-arguments term))
		       (*standard-output* stream))
		   (write-string "\\beginning\\atom")
		   (write function)
		   (write tex*left-bracket)
		   (pprint-logical-block (nil arguments)
		     (loop (pprint-exit-if-list-exhausted)
			   (write (pprint-pop))
			   (pprint-exit-if-list-exhausted)
			   (write tex*comma)))
		   (write tex*right-bracket)
		   (write-string "\\endof\\atom"))) 12)
	      (tex=fo-appl-term 
	       (lambda (stream term)
		 (let ((function (data~appl-function term))
		       (arguments (data~appl-arguments term)))
		   (write function :stream stream)
		   (write tex*left-bracket :stream stream)
		   (pprint-logical-block (stream arguments)
		     (loop (pprint-exit-if-list-exhausted)
			   (write (pprint-pop))
			   (pprint-exit-if-list-exhausted)
			   (write tex*comma)))
		   (write tex*right-bracket :stream stream))) 12)
	      (type+type
	       (lambda (stream type)
		 (declare (ignore type))
		 (write-string "" stream)
		 ) 12)	      
	      ))

; bump priorities up to 13
(pp~defstyle tex-style-fo-infix  :parent tex-style-fo
	     :help "A simple TeX style for printing first order formulas with
infix symbols."
	     :pprint-methods 	
	     ((tex=infix-appl
	       (lambda (stream appl)
		 (let ((args (data~appl-arguments appl))
		       (fun (data~appl-function appl)))
		   (if (>= (length args) 2)
		       (progn
			 (write tex*left-bracket
				:stream stream)
			 (write (car args)
				:stream stream)
			 (dolist (arg
				  (subseq args 1))
			   (write fun
				  :stream stream)
			   (write arg
				  :stream stream))
			 (write tex*right-bracket
				:stream stream))
		       (tex=print-appl-basic stream appl))))
	       13)
	      ))
 
; bump priorities up to 13
(pp~defstyle tex-style-ho :parent tex-style
	     :help "A TeX style for printing higher order formulas."
	     :pprint-methods
	     ((tex=quantification
	       (lambda (stream appl)
		 (write tex*left-bracket :stream stream)
		 (write (data~appl-function appl) :stream stream) 
		 (write (logic~quantification-bound-variable appl) 
			:stream stream)
		 (write (logic~quantification-scope appl)
			:stream stream)
		 (write tex*right-bracket :stream stream)) 14)
	      (tex=quantor
	       (lambda (stream term)
		 (write-string (if (logic~universal-quantor-p term)
			    "\\q\\forall"
			    "\\q\\exists") stream)) 14)
	      (term+appl
	       (lambda (stream appl)
		 (write tex*left-bracket :stream stream)
		 (write (data~appl-function appl)
			:stream stream)
		 (dolist (arg (data~appl-arguments appl))
		   (write arg :stream stream))
		 (write tex*right-bracket :stream stream))
	       13)))


; bump priorities up to 15
(pp~defstyle tex-style-ho-infix  :parent tex-style-ho
	     :help "A TeX style for printing higher order formulas with
infix symbols."
	     :pprint-methods
	     ((tex=infix-appl
	       (lambda (stream appl)
		 (let ((args (data~appl-arguments appl))
		       (fun (data~appl-function appl)))
		   (if (>= (length args) 2)
		       (progn
			 (write tex*left-bracket
				:stream stream)
			 (write (car args)
				:stream stream)
			 (dolist (arg (subseq args 1))
			   (write fun
				  :stream stream)
			   (write arg
				  :stream stream))
			 (write tex*right-bracket
				:stream stream))
		       (tex=print-appl-basic stream appl))))
	       15))
	     )

(defun tex~print-proof (proof style outfile)
  (declare (edited  "07-MAR-1994 12:07")
	   (authors SCHEJA nesmith)
	   (input   "A proof, a symbol or string naming a style, and a pathname.")
	   (effect  "The TeX representation of proof according to STYLE"
		    "is written to the file named by OUTFILE.  The file proof.tex is required"
		    "to run tex on the resulting file. This macro file is assumed to be in"
		    "the directory *omega-tex-macro-dir*.")
	   (value  "Undefined." ))
    (let ((tex*env (pds~environment proof)))
      (setq style (intern (string-upcase (symbol-name style))
			  (find-package "KEYWORD")))
      (with-open-file (stream outfile
			      :direction :output
			      :if-does-not-exist :create
			      :if-exists :supersede)
	(format t "Writing the TeX representation of ~A to ~A~%" 
		(string (keim~name proof)) (namestring outfile))
	(format stream "\\input ~Aproof~%" *omega-tex-macro-dir*)
	(tex=print-term-mode style stream)
	(tex=print-type-mode style stream)
	(tex=print-leftbracket-mode style stream)
	(tex=print-rightbracket-mode style stream)
	(pp~pprint proof style stream)
	(format stream "~%\\bye~%")))
    )

(defun tex=print-term-mode (style stream)
  (declare (edited  "15-APR-1993 16:39")
	   (authors SCHEJA)
	   (input   "A style and a stream.")
	   (effect  "The macro definition that refer to the classes (var, const, appl ..)
is written to stream following the style.")
	   (value  "Undefined." )
	   (ignore style))
  (format stream "\\def\\const#1{{\\rm#1}}~%")
  (format stream "\\def\\var#1{{\\rm#1}}~%")
  (format stream "\\def\\lam#1{#1}~%"))
	   

(defgeneric tex=print-type-mode (style stream)
  (declare (edited  "15-APR-1993 13:37")
	   (authors SCHEJA)
	   (input   "A style and a stream.")
	   (effect  "The macro definition of \type is written to stream.")
	   (value  "Undefined." ))
  (:method ((style (eql :tex-style)) stream)
	   (format stream "\\def\\type#1{_{#1}}"))
  (:method ((style (eql :tex-style-ho)) stream)
	   (format stream "\\def\\type#1{_{#1}}"))
  (:method ((style (eql :tex-style-ho-infix)) stream)
	   (format stream "\\def\\type#1{_{#1}}"))
  (:method ((style (eql :tex-style-fo)) stream)
    (declare (ignore stream)))
  (:method ((style (eql :tex-style-fo-infix)) stream)
    (declare (ignore stream))))


(defun tex=print-leftbracket-mode (style stream)
  (declare (edited  "15-APR-1993 17:36")
	   (authors SCHEJA)
	   (input   "A style and a stream.")
	   (effect  "The macro definition of \lbracket is
written to stream. \lbracket is the control macro for the
leftbracket.")
	   (value   "Undefined.")
	   (ignore style))
  (format stream "\\def\\lbracket{\\relax}"))

(defun tex=print-rightbracket-mode (style stream)
  (declare (edited  "15-APR-1993 17:36")
	   (authors SCHEJA)
	   (input   "A style and a stream.")
	   (effect  "The macro definition of \rbracket is
written to stream. \rbracket is the control macro for the
leftbracket.")
	   (value   "Undefined.")
	   (ignore style))
  (format stream "\\def\\rbracket{\\relax}"))


#{
\subsection{The \TeX\ Style Argumenttype}

In order to use \TeX\ styles as arguments to commands, we define the
argument type {\tt TEX-STYLE}.
#}

(defvar tex*styles (list 'tex-style 'tex-style-fo 'tex-style-fo-infix
			 'tex-style-ho 'tex-style-ho-infix)
  "List of current TeX styles.")

(defun tex~style-p (symbol-or-string)
  (if  (member symbol-or-string tex*styles :test #'string-equal)
       t
       nil))

(eval-when (load compile eval)
(arg~deftype tex-style
 (predicate tex~style-p)
 (read-function tex~read-tex-style)
 (help "a TeX style name"))
)

(defun tex=read-style (obj type)
  (if (tex~style-p obj)
      obj
      (arg~signal-wrong-type type obj)))

(defmethod tex~read-tex-style ((obj symbol) &rest others)
  (declare (ignore others))
  (tex=read-style obj 'tex-style))

(defmethod tex~read-tex-style ((obj string) &rest others)
  (declare (ignore others))
  (tex=read-style obj 'tex-style))

