;;; -*- syntax: common-lisp; package: keim; base: 10; mode: lisp -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
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

(mod~defmod post :uses (mod env  sys )
                 :documentation "All KEIM objects have POST representations, this module has the input/output functions."
		 :exports (
			   post~read-object
			   post~read-object-list
			   post~read-problem
			   post~read-symbol
			   post~indicators
			   post~print-indicators
			   post~print
			   post~string
			   post~pprint
			   post~print-declaration
			   post~error
			   post~all-types-checker
			   )
		 )

#{\section{{\post} input and output}
\subsection{{\post} input}
{\post} is the machine-oriented lingua franca for \keim. It is intended that any
{\keim} object has an ASCII representation (its {\post} representation) that can be written to a file and read
from it. Thus the {\post} representation of internal {\keim} objects is always there as a fallback strategy for
data exchange. This has proven very valuable for developing {\keim} and first {\keim} applications.
It is strongly advised for implementors of  extensions and applications of {\keim} to define {\post}
representations for all objects and specialize the generic functions defined in this module to the newly
defined classes. It is an explicit design decision of {\keim} that {\post} syntax conforms with {\lisp} syntax, so
that {\post} files can be read
 with the {\lisp} reader and then interpreted by {\vb POST~READ-OBJECT}.

Every {\post} object should have a Lisp input form, suitable for reading into \keim. This module defines the
functions which perform this input; it does not define the {\post} forms themselves.  Those definitions, in the
form of methods, are spread among the various modules which define the objects themselves. For this we have
provided a set of {\LaTeX} macros, they should be used in the code in the following form

\begin{verbatim}
\begin{postsyntax}
\syntax{\nt{clause} ::= (clause \nt{head} \{\nt{literal}\}*).
 \nt{ende}.}
\syntaxcomment{Some crazy comment goes here, but it is optional}
\syntax{\nt{END}.}
end{postsyntax}
\end{verbatim}

The BNF notation here has the meaning

\begin{tabular}{r@{ stands for }l}
{\tt ::=} & definition      \\
{\tt |} & or                \\
{\tt [\ldots]} & optional    \\
{\tt \{  \}}  & brackets for grouping    \\
{\tt *}   & repetition $\geq0$ \\
{\tt +}   & repetition $\geq1$  \\
{\tt .}   & end of rule
\end{tabular}

{\it Non-terminal symbols} will be written in italic whereas the {\tt terminal symbols} will be displayed in
the typewriter font.  

Here, if somewhere before there is an occurrence of {\vb $\backslash$makepostsyntax} the argument in {\vb $\backslash$syntax} will be
formatted in a verbatim-like environment (code.sty) and will be written to a file {\vb ***.postsyntax}, which
will eventually contain all post syntax declarations.  This file can then be formatted into a section with the
command {\vb $\backslash$makepostsyntaxsection}.  
%%%% whatever you intend here, it isn't working michael!  TeX error city!
%However at the moment the linefeeds in the argument of {\vb syntax} will
%be listed as {\vb ^^M} in the {\vb ***.postsyntax file}.  
The macro {\vb $\backslash$postindex} will write its argument into
an index.


The main {\post} input function is {\vb POST~READ-OBJECT}.  It converts a Lisp form into an object of the
proper type based on the keyword indicator (the indicator is the principal way methods are specialized).  If
the indicator is NIL, then if the Lisp form is a list, its first element (interned into the keyword package)
is used as the indicator.  {\vb POST~READ-OBJECT-LIST} applies {\vb POST~READ-OBJECT} to each element in a
list, using the first element of the object as the keyword.

Here's an example of how this proceeds:
\begin{verbatim}
(post~read-object-list '((position 1 2 3)) env) ->
(post~read-object '(position 1 2 3) env nil) ->
(post~read-object '(1 2 3) env :position)
\end{verbatim}
For {\keim} objects, where the {\post} representation does not begin (e.g. terms, types,\ldots) with a keyword
the indicator has to be known or guessed. Use the functions {\vb post~indicators} or {\vb post~print-indicators}
to obtain a list of all possible indicators of the system.

The convention for defining {\post} Syntax is the following set of rules:
\begin{itemize}
\item Structured objects should be lists, where the CAR is a keyword (usually the name of the object) and the
subsequent items correspond to the constructive parts of the object.
\item Objects that are recursively defined (like types and terms) should not begin with a keyword, since
otherwise the concrete syntax gets big and unwieldy  
\item Components without keyword of structured objects have to be distinguished by their position in the
{\post} list representation.
\item Concrete {\post} syntax should have enough keywords to be readable by humans
\end{itemize}
#}


(defgeneric post~read-object (thing env indicator)
  (declare (edited  "9-Feb-1993")
	   (authors nesmith)
	   (input   "An object to be interpreted as a POST object, usually"
		    "written in a Lisp list syntax; an environment ENV; and a keyword symbol INDICATOR"
		    "which indicates what kind of object the reader is expecting.  If INDICATOR"
		    "is null, and the object is a list, then the first element of the list is"
		    "taken as the INDICATOR.")
	   (effect  "The object is constructed based on the POST syntax.")
	   (value   "Whatever the various methods return. In general, terms 
and larger structures should be returned, but declarations of symbols will
generally return nil."))
  (:method ((thing t) (env t) (indicator t))
    (error "POST~~READ-OBJECT: Don't know how to read ~A with indicator of ~S"
	   thing indicator))
  (:method ((thing cons) (env env+environment) (indicator null))
      (post~read-object (cdr thing) env
       (if (symbolp (car thing))
	   (intern (symbol-name (car thing))
		   (find-package :keyword))
	 (car thing)))))

(defun post~read-object-list (list env)
  (declare (edited  "9-Feb-1993")
	   (authors nesmith)
	   (input   "A list of expressions to be interpreted as POST objects and an environment.")
	   (effect  "The objects are constructed in turn based on their POST syntax."
		    "Merely calls post~read-object for each element of the list.")
	   (value   "All non-nil results of the individual calls to post~read-object."))
  (let ((returns nil))
    (dolist (thing list (nreverse (delete-if #'null returns)))
      (push (post~read-object thing env nil) returns))))
	
#{Here we have another name for the previous function, which is more intuitive for use with the \keim\
problem facility.#}

(defun post~read-problem (list env)
  (declare (edited  "9-Feb-1993")
	   (authors nesmith)
	   (input   "Same as post~read-object-list.")
	   (effect  "Same as post~read-object-list. This function is the 
same as post~read-object-list and is defined only for backward compatibility.")
	   (value "Same as post~read-object-list."))
  (post~read-object-list list env))


(defmethod post~read-object (thing env (indicator (eql :symbol)))
  (if (symbolp thing)
      thing
    (post~error "~A is not a SYMBOL." thing)))

(defun post~read-symbol (thing env)
  (declare (authors nesmith)
	   (input "A Lisp object THING and an environment ENV.")
	   (effect "None.")
	   (value "A Lisp symbol if this THING can be so interpreted."))
  (post~read-object thing env :symbol))

#{It is always a problem to know the valid indicators for {\vb post~read-object} and {\vb post~read-object-list} therefore {\keim} provides the following functions.#}

(defun post~indicators ()
  (declare (edited  "20-AUG-1993 10:48")
	   (authors KOHLHASE)
	   (input "None.")
	   (effect "None.")
	   (value  "A list of all valid indicators for POST~READ-OBJECT an POST~READ-OBJECT-LIST."))
  (delete-duplicates
   (mapcar #'clos::eql-specializer-object
	   (remove-if-not #'clos::eql-specializer-p
			  (mapcar #'(lambda (method)
				      (third (sys~method-specializers method)))
				  (sys~generic-function-methods 'post~read-object))))))


(defun post~print-indicators (&optional (stream t))
  (declare (edited  "20-AUG-1993 10:58")
	   (authors KOHLHASE)
	   (input "An optional stream which defaults to T." )
	   (effect "The list of all valid indicators for POST~READ-OBJECT an POST~READ-OBJECT-LIST is printed to STREAM.")
	   (value  "Undefined."))
  (format stream "~S" (post~indicators)))

#{\subsection{{\post} output}
Each {\post} object should be able to be printed out in a form, such that it 
later can be read in again. {\vb POST~PRINT} is the main function which
does this.#}


(defgeneric post~print (object stream)
  (declare (edited  "27-JAN-1993 17:12")
	   (authors RICHTS)
	   (input   "An KEIM-object and a stream (or NIL or T).")
	   (effect  "The POST Representation of OBJECT is printed. It is printed without any formatting like linefeeds etc.  If STREAM is NIL, then
a string containing the output will be produced; if STREAM is T, then
output will be to *standard-output*.")
	   (value   "Like format, a string if STREAM is NIL or NIL else."))
					;  (:argument-precedence-order object stream)
  (:method :around (object stream)
	   (cond ((null stream)
		  (with-output-to-string (string-stream)
					 (post~print object string-stream)))
		 ((eq t stream)
		  (post~print object *standard-output*))
		 (t (call-next-method))))
  (:method ((object t) stream)
	   (declare (ignore stream key-list))
	   (error "(KEIM) post~~print: There is no POST-Representation for ~A. (Or at least no method printing it.)" object))
  (:method ((object-list list) stream)
	   (when object-list
	     (post~print (first object-list) stream)
	     (mapc #'(lambda (object)
		       (format stream " ")
		       (post~print object stream))
		   (rest object-list))))
  (:method ((sym symbol) stream)
	   (format stream "~A" sym)))

#{There is also a variant of {\vb POST~PRINT}, that prettyprints the output to a stream.#}

(defun post~string (object)
  (declare (edited  "27-JAN-1993 17:12")
	   (authors RICHTS)
	   (input   "An KEIM-object.")
	   (effect  "Prints the POST representation of OBJECT to a string.") 
	   (value   "The POST form of OBJECT in a string."))
  (with-output-to-string (intern-stream)
    (post~print object intern-stream)))

(defun post~pprint (object stream)
  (declare (edited  "27-JAN-1993 17:12")
	   (authors RICHTS)
	   (input   "An KEIM-object and a stream (or NIL or T).")
	   (effect  "Pretty prints the POST representation of OBJECT to STREAM. If STREAM is NIL, *standard-output* is used. If STREAM is T, *terminal-io* is used.")
	   (value   "No values."))
  (pprint (read-from-string (post~string object)) stream))

#{Some {\keim} objects (like variables) require different {\post} representations, when they occur in other
objects and when they are declared. {\vb POST~PRINT} only prints the declaration of the representation
for normal occurrence whereas {\vb POST~PRINT-DECLARATION} prints the representation for declarations.  

For example we have 
\begin{code}
KEIM(1): (setq env (env~create))
#<ENV  >
KEIM(2): (post~read-object '(type-constants I) env nil)
NIL
[1] KEIM(3): (setq var (sym~variable-create 'v (env~lookup-object 'I env)))
V
[1] KEIM(4): (post~print var nil)
"V"
[1] KEIM(5): (post~print var t)
V
NIL
KEIM(6): (post~print-declaration var nil)
"(V I)"
\end{code}#}

(defgeneric post~print-declaration (object stream)
  (declare (edited  "27-JAN-1993 17:12")
	   (authors RICHTS)
	   (input   "An KEIM-object and a stream (or NIL or T).")
	   (effect  "The POST Representation of the declaration of OBJECT is printed.")
	   (value   "Like in format a string if STREAM is NIL or NIL else."))
  (:method ((object t) stream)
	   (declare (ignore stream key-list))
	   (error "(KEIM) post~print: There is no POST-Representation for the declaration of ~A. (Or at least no method printing it.)" object))
  (:method ((object-list list) stream)
	   (when object-list
	     (post~print-declaration (first object-list) stream)
	     (mapc #'(lambda (object)
		       (format stream " ")
		       (post~print-declaration object stream))
		   (rest object-list)))))

#{The following function provides specialized error handling while parsing  {\post} expressions. It is used exactly
like the {\commonlisp} {\vb error} function.#}


(defun post~error (error-string &rest args)
  (declare (edited  "9-Feb-1993")
	   (authors richts nesmith)
	   (input   "An error format string with appropriate arguments.")
	   (effect  "ERROR is called with the string and args, prepending
a note that this was a POST error.")
	   (value   "Not defined"))
  (apply #'error (concatenate 'string "POST ERROR: " error-string) args))


;; environments are defined before post, so have to do this here.
(defmethod post~print ((env env+environment) stream)
  (princ "(" stream)
  (dolist (key (reverse (env~class-keys env 't)))
    (env~post-print key (env~lookup-object key env) stream))
  (princ ")" stream)
  (values)
  )

(defmethod env~post-print (key (object t) stream)
  (declare (ignore key))
  (post~print object stream))

(defmethod post~read-object ((parent-decl cons) (env env+environment) 
			    (indicator (eql :parent)))
  (let* ((name (post~read-symbol (first parent-decl) env))
	 (object (env~lookup-object name env)))
    (cond ((typep object 'env+environment) object)
	  ((null object) (post~read-object nil env name))
	  (t (error "No environment defined with name ~A." name)))))

;;; test


(defun post~all-types-checker (list-of-symbols &key (forbidden nil))
  (declare (edited  "26-NOV-1998")
	   (authors Ameier)
	   (input   "A list of recursive listed symbols.")
	   (effect  "NOne.")
	   (value   "If flag forbidden is nil:"
		    "T the symbol all-types is at most used only as first symbol in the top-list"
		    "(if the first thing in the top-list is a symbol at all) and nowhere else."
		    "Otherwise nil."
		    "If flag forbidden is t:"
		    "T if the symbol all-types isn't used, nil otherwise."))
  (let* ((platte-string-liste (mapcar #'(lambda (sym) (format nil "~A" sym))
				      (post=all-down-things list-of-symbols))))
    (if (null forbidden)
	(cond ((null (find "ALL-TYPES" platte-string-liste :test #'string=))
	       't)
	      ((null (find "ALL-TYPES" (rest platte-string-liste) :test #'string=))
	       ;; -> NUR erstes Element ist ALL-TYPES
	       (if (symbolp (first list-of-symbols))
		   't
		 nil))
	      (t
	       nil))
      (null (find "ALL-TYPES" platte-string-liste :test #'string=)))))

(defun post=all-down-things (liste)
  (declare (edited  "26-NOV-1998")
	   (authors Ameier)
	   (input   "A list of possibly lists.")
	   (effect  "None.")
	   (value   "A list of all things in all down lists in the right order."))  
  (if (listp liste)
      (apply 'append (mapcar #'post=all-down-things liste))
    (list liste)))
