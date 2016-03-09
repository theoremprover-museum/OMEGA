;;; -*- syntax: common-lisp; package: keim; base: 10; mode: lisp -*-
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

(in-package :keim)

(mod~defmod argdefs :uses (mod arg sys loui)
	    :documentation "Definition of common argument types."
	    :exports (
		      argdefs~read-symbol
		      argdefs~read-number
		      argdefs~read-integer
		      argdefs~read-posinteger
		      argdefs~read-boolean
		      argdefs~read-anything
		      argdefs~read-string
		      argdefs~read-pathname
		      argdefs~read-existing-file
		      argdefs~read-existing-category-name
		      argdefs~read-existing-directory
		      argdefs~read-existing-url
		      argdefs~read-creating-directory
		      argdefs~read-existing-system
		      argdefs~read-existing-theory
		      argdefs~read-defsystem-force
		      )
	    )

#{
\section{Common argument types}
\label{mod:argdefs}
There are some argument types that will almost always be useful.  They
are based on \lisp\ types.  Here we define some of them.

A {\vb symbol} is an often used argument type.  Here we mean \lisp\ symbol.

#}


(eval-when (load compile eval)
(arg~deftype symbol
  (read-function argdefs~read-symbol)
  (predicate symbolp)
  (help "a Lisp symbol"))
)

(defmethod argdefs~read-symbol ((obj symbol) &rest others)
  (declare (ignore others))
  obj)

(defmethod argdefs~read-symbol ((obj string) &rest others)
  (declare (ignore others))
  (with-input-from-string (in obj)
    (let* ((special-symbol (gensym))
	   (thing nil))
      (multiple-value-bind (val condition)
	  (setq thing (ignore-errors (read in nil special-symbol)))
	(when (typep condition 'condition)
	  (sys~signal condition))
	(when (or (eq thing special-symbol)
		  (not (symbolp thing)))
	  (arg~signal-wrong-type 'symbol obj))
	val))))


#{
 A {\vb number} is an often used argument type. Any object that is
{\vb numberp} is in this class of objects.
#}

(eval-when (load compile eval)
(arg~deftype number
  (read-function argdefs~read-number)
  (predicate numberp)
  (help "a number"))
)

(defmethod argdefs~read-number ((obj number) &rest others)
  (declare (ignore others))
  obj)

(defmethod argdefs~read-number ((obj string) &rest others)
  (declare (ignore others))
  (with-input-from-string (in obj)
    (let* ((special-symbol (gensym))
	   (thing nil))
      (multiple-value-bind (val condition)
	  (setq thing (ignore-errors (read in nil special-symbol)))
	(when (typep condition 'condition)
	  (sys~signal condition))
	(when (or (eq thing special-symbol)
		  (not (numberp thing)))
	  (arg~signal-wrong-type 'number obj))
	val))))


#{ The {\vb integer} argtype is the class of all integers. #}

(eval-when (load compile eval)
(arg~deftype integer
  (predicate integerp)
  (read-function argdefs~read-integer)
  (help "an integer"))
)

(defmethod argdefs~read-integer ((obj integer) &rest others)
  (declare (ignore others))
  obj)

(defmethod argdefs~read-integer ((obj string) &rest others)
  (declare (ignore others))
  (with-input-from-string (in obj)
    (let* ((special-symbol (gensym))
	   (thing nil))
      (multiple-value-bind (val condition)
	  (setq thing (ignore-errors (read in nil special-symbol)))
	(when (typep condition 'condition)
	  (sys~signal condition))
	(when (or (eq thing special-symbol)
		  (not (integerp thing)))
	  (arg~signal-wrong-type 'integer obj))
	val))))

#{ The {\vb posinteger} argtype is the class of all integers greater 
than zero.
#}

(eval-when (load compile eval)
(arg~deftype posinteger
  (read-function argdefs~read-posinteger)
  (predicate argdefs=posinteger-p)
  (help "a positive integer greater than zero"))
)

(defun argdefs=posinteger-p (x)
  (and (integerp x)
       (> x 0)))

(defmethod argdefs~read-posinteger ((obj integer) &rest others)
  (declare (ignore others))
  (if (> obj 0) 
      obj
      (arg~signal-wrong-type 'posinteger obj)))

(defmethod argdefs~read-posinteger ((obj string) &rest others)
  (declare (ignore others))
  (with-input-from-string (in obj)
    (let* ((special-symbol (gensym)))
      (multiple-value-bind (thing condition)
	  (ignore-errors (read in nil special-symbol))
	(when (typep condition 'condition)
	  (sys~signal condition))
	(when (or (eq thing special-symbol)
		  (not (integerp thing)))
	  (arg~signal-wrong-type 'posinteger obj))
	thing))))


#{ The {\vb boolean} argtype is the set consisting of {\vb t} and {\vb nil}.  
We define
 its read-function to allow symbols like y, yes, ja to mean {\vb t}, and
 symbols such as n, no, nein to mean {\vb nil}.
#}

(eval-when (load compile eval)
(arg~deftype boolean
  (read-function argdefs~read-boolean)
  (predicate argdefs=boolean-p)
  (help "a boolean"))
)

(defun argdefs=boolean-p (x)
  (or (eq x t) (eq x nil)))

(defmethod argdefs~read-boolean ((obj null) &rest others)
  (declare (ignore others))
  obj)

(defmethod argdefs~read-boolean ((obj (eql t)) &rest others)
  (declare (ignore others))
  obj)

(defmethod argdefs~read-boolean ((obj symbol) &rest others)
  (declare (ignore others))
  (cond ((member obj (list "YES" "Y" "JA" "J" )
		 :test #'string-equal)
	 t)
	((member obj (list "NO" "N" "NEIN" "N")
		 :test #'string-equal)
	 nil)
	(t (arg~signal-wrong-type 'boolean obj))))


#{
 The {\vb anything} argtype consists of all objects.
#}
(eval-when (load compile eval)
(arg~deftype anything
  (read-function argdefs~read-anything)
  (predicate argdefs=anything-p)
  (help "anything"))
)

(defun argdefs=anything-p (x)
  (declare (ignore x)) 
  t)

(defmethod argdefs~read-anything ((obj t) &rest others)
  (declare (ignore others))
  obj)


#{The {\vb string} argtype consists of all \lisp\ strings.  The read
 function is defined such that given a \lisp\ symbol, it will return
 the symbol's print name.
#}
(eval-when (load compile eval)
(arg~deftype string
  (read-function argdefs~read-string)
  (predicate stringp)
  (help "a Lisp string"))
)

(defmethod argdefs~read-string ((obj string) &rest others)
  (declare (ignore others))
  obj)

(defmethod argdefs~read-string ((obj symbol) &rest others)
  (declare (ignore others))
  (when (not (null obj)) (symbol-name obj)))


#{ The {\vb pathname} argtype consists of all \lisp\  pathnames.  If given a
 string, it will make a pathname directly from it.  If given a
 symbol, it will lower case the symbol's print name and make a pathname
 from it.
#}
(eval-when (load compile eval)
(arg~deftype pathname
  (read-function argdefs~read-pathname)
  (predicate pathnamep)
  (help "a Lisp pathname"))
)

(defmethod argdefs~read-pathname (obj &rest others)
  (declare (ignore others))
  (if (typep obj 'pathname)
      obj
    (error "~A is not of a valid type" obj)))

(defmethod argdefs~read-pathname ((obj string) &rest others)
  (declare (ignore others))
  (pathname obj))

(defmethod argdefs~read-pathname ((obj symbol) &rest others)
  (declare (ignore others))
  (pathname (string-downcase (symbol-name obj))))

#{ The {\vb existing-file} argtype consists of all \lisp\ pathnames of
 existing files.  If given a string, it will make a pathname directly 
 from it.  If given a symbol, it will lower case the symbol's 
 print name and make a pathname from it. In any case it will 
 check to see if the file actually exists before returning the pathname.
#}
(eval-when (load compile eval)
(arg~deftype existing-file
  (read-function argdefs~read-existing-file)
  (predicate argdefs=existing-file-p)
  (help "an existing file"))
)

(defun argdefs=existing-file-p (pathname)
  (declare
   (authors nesmith)
   (input "a pathname")
   (value "T if pathname names an existing file, otherwise nil"))
  (let ((result (probe-file pathname)))
    (if result t nil)))

(defmethod argdefs~read-existing-file (obj &rest others)
  (declare (ignore others))
  (if (typep obj 'pathname)
      (let ((result (probe-file obj)))
	(if result
	    result
	  (arg~signal-wrong-type 'existing-file obj)))
    (error "~A is not of a valid type" obj)))


(defmethod argdefs~read-existing-file ((obj string) &rest others)
  (declare (ignore others))
  (let ((result (probe-file (pathname obj))))
    (if result
	result
	(arg~signal-wrong-type 'existing-file obj))))

(defmethod argdefs~read-existing-file ((obj symbol) &rest others)
  (declare (ignore others))
  (let ((result (probe-file (pathname (string-downcase (symbol-name obj))))))
    (if result
	result
	(arg~signal-wrong-type 'existing-file obj))))


#{ The {\vb existing-category-name} argtype consists of all names of categories
 used for classifying the commands. If given a symbol that is the
 name of an existing category, it will return the same 
 symbol. Otherwise the given symbol is not of this type.
#}
(eval-when (load compile eval)
(arg~deftype existing-category-name
  (read-function argdefs~read-existing-category-name)
  (predicate argdefs=existing-category-name-p)
  (help "an existing category name"))
)

(defun argdefs=existing-category-name-p (cat-name)
  (declare (edited  "21-JUL-1995")
	   (authors Lassaad)
	   (input   "a symbol")
	   (effect  "None")
	   (value   "T if this symbol a name of an existing category, otherwise nil."))
  (let ((category (com~find-category cat-name)))
    (if category t nil)))

(defmethod argdefs~read-existing-category-name ((obj symbol) &rest others)
  (declare (ignore others))
  (let ((result (com~find-category obj)))
    (if result
	obj
	(arg~signal-wrong-type 'existing-category-name obj))))


#{ The {\vb existing-directory} argtype consists of all \lisp\ pathnames of
 existing files.  If given a string, it will make a pathname directly 
 from it.  If given a symbol, it will lower case the symbol's 
 print name and make a pathname from it. In any case it will 
 check to see if the file actually exists before returning the pathname.
#}
(eval-when (load compile eval)
(arg~deftype existing-directory
  (read-function argdefs~read-existing-directory)
  (predicate argdefs=existing-directory-p)
  (help "an existing directory"))
)

(defun argdefs=existing-directory-p (pathname)
  (declare
   (authors nesmith)
   (input "a pathname")
   (value "The true name of PATHNAME if it names an existing directory, otherwise nil;"
	  "PATHNAME is an existing directory if its name is NIL and it exists a file with name '.'."))
  (and (null (pathname-name pathname))
       (probe-file (merge-pathnames (make-pathname :name ".") pathname))))

(defmethod argdefs~read-existing-directory (obj &rest others)
  (declare (ignore others))
  (if (typep obj 'pathname)
      (let ((result (or (argdefs=existing-directory-p obj)
			(argdefs=existing-directory-p (merge-pathnames (make-pathname :name nil :type nil) obj)))))
	(if result
	    result
	  (arg~signal-wrong-type 'existing-directory obj)))
    (error "~A is not of a valid type" obj)))

(defmethod argdefs~read-existing-directory ((obj symbol) &rest others)
  (declare (ignore others))
  (let ((result (argdefs=existing-directory-p (pathname (string-downcase (symbol-name obj))))))
    (if result
	result
	(arg~signal-wrong-type 'existing-directory obj))))

#{ The {\vb existing-url} argtype consists of all urls and existing directories.
 An URL is either the \lisp\ pathname of an existing directory or a string with
 the URL.
#}

(eval-when (load compile eval)
(arg~deftype existing-url
  (read-function argdefs~read-existing-url)
  (predicate argdefs=existing-url-p)
  (help "an existing url"))
)

(defun argdefs=existing-url-p (pathname)
  (declare
   (authors pollet)
   (input "an url")
   (value "The true name of PATHNAME if it names an existing url, otherwise nil;"
	  "PATHNAME is an existing url if it is an existing-directory or (loui~load-url PATHNAME) is not nil."))
  (when (loui~load-url pathname) pathname))

(defmethod argdefs~read-existing-url (obj &rest others)
  (declare (ignore others))
  (if (typep obj 'string)
      (let ((result (or (argdefs=existing-directory-p obj)  ;; if it is a local directory we want the lisp pathname
			(argdefs=existing-url-p obj))))
	(if result
	    result
	  (arg~signal-wrong-type 'existing-url obj)))
    (error "~A is not of a valid type" obj)))

#{ The {\vb creating-directory} argtype:
   Checks wether a directory exists, when not the directory will be created. 
#}


(eval-when (load compile eval)
(arg~deftype creating-directory
  (read-function argdefs~read-creating-directory)
  (predicate argdefs=creating-directory-p)
  (help "an creating directory"))
)

(defun argdefs=creating-directory-p (pathname)
  (declare (edited  "02-MAR-1999")
	   (authors Pollet)
	   (input "a pathname")
	   (value "The true name of PATHNAME if it names an existing directory"
		  "or the directory can be created, otherwise nil;"
		  "PATHNAME is a directory if its name is NIL and it exists a file with name '.'."))
  (and (null (pathname-name pathname))
       (let ((directory (merge-pathnames (make-pathname :name ".") pathname)))
           (when (not (probe-file directory))
	     (sys~call-system (format nil "mkdir ~A" directory)))
	   directory)))

(defmethod argdefs~read-creating-directory (obj &rest others)
  (declare (ignore others))
  (if (typep obj 'pathname)
      (let ((result (or (argdefs=creating-directory-p obj)
			(argdefs=creating-directory-p (merge-pathnames (make-pathname :name nil :type nil) obj)))))
	(if result
	    result
	  (arg~signal-wrong-type 'creating-directory obj)))
    (error "~A is not of a valid type" obj)))

(defmethod argdefs~read-creating-directory ((obj symbol) &rest others)
  (declare (ignore others))
  (let ((result (argdefs=creating-directory-p (pathname (string-downcase (symbol-name obj))))))
    (if result
	result
	(arg~signal-wrong-type 'creating-directory obj))))


#{ The {\vb existing-system} argtype consists of all \lisp\ symbols naming existing systems.
   If given a symbol, it will check to see if
   the file of the system defintion actually exists in the central registry before returning the symbol.  #}

(eval-when (load compile eval)
(arg~deftype existing-system
  (read-function argdefs~read-existing-system)
  (predicate argdefs=existing-system-p)
  (help "an existing system"))
)

(defun argdefs=existing-system-p (sym)
  (declare
   (input "a symbol")
   (value "T if SYM names an existing system, otherwise nil"))
  (let ((result (some #'(lambda (dir)
			  (probe-file (merge-pathnames (pathname dir)
						       (make-pathname 
							:name (string-downcase sym)
							:type "system"))))
		      (if (listp mk::*central-registry*)
			  mk::*central-registry*
			(list mk::*central-registry*)))))
    (if result t nil)))

(defmethod argdefs~read-existing-system ((obj symbol) &rest others)
  (declare (ignore others))
  (if (argdefs=existing-system-p obj)
      obj
    (arg~signal-wrong-type 'existing-system obj)))


#{ The {\vb defsystem-force} argtype consists of all \lisp\ keyword parameters for the {\vb :force} key in the
   defsystem {\vb operate-on-system} function. #}

(eval-when (load compile eval)
(arg~deftype defsystem-force
	     (read-function argdefs~read-defsystem-force)
	     (predicate argdefs=defsystem-force-p)
	     (help "a valid parameter for the :force in the defsystem facility, i.e. a keyword or a list of existing modules.")))

(defun argdefs=defsystem-force-p (obj)
  (declare
   (input "a symbol")
   (value "T if SYMBOL names an existing system, otherwise nil"))
  (let ((result (or (member obj '(:all :new-source :new-source-and-dependents))
		    (and (consp obj)
			 (every #'mod~find-module obj)))))
    (if result t nil)))

(defmethod argdefs~read-defsystem-force ((obj t) &rest others)
  (declare (ignore others))
  (if (argdefs=defsystem-force-p obj)
      obj
    (arg~signal-wrong-type 'defsystem-force obj)))


#{ The {\vb existing-theory} argtype consists of all \lisp\ symbols naming existing theories.
   If given a symbol, it will check to see if the file of the theory defintion actually
   exists in the central registry given by 
   {\vb *theory-registry*} before returning the symbol.  #}

(eval-when (load compile eval)
(arg~deftype existing-theory
  (read-function argdefs~read-existing-theory)
  (predicate argdefs=existing-theory-p)
  (help "an existing theory"))
)

(defun argdefs=existing-theory-p (sym)
  (declare
   (input "a symbol")
   (value "T if SYM names an existing theory, otherwise nil"))
  (let* ((url-protocols (list "http:" "ftp:"))
	 (result (some #'(lambda (direc)
			   (let ((dir (namestring direc)))
			   (if (some #'(lambda (urltype)
					 (equal (subseq dir 0 (length urltype))
						urltype))
				     url-protocols)
			       (loui~load-url (concatenate 'string
							   dir (string-downcase sym)))
			     (probe-file (make-pathname :directory (concatenate 'string dir (string-downcase sym))
							:name (string-downcase sym)
							:type "thy")))))
		       (if (listp user::*theory-registry*)
			   user::*theory-registry*
			 (list user::*theory-registry*)))))
    (if result t nil)))

(defmethod argdefs~read-existing-theory ((obj symbol) &rest others)
  (declare (ignore others))
  (if (argdefs=existing-theory-p obj)
      obj
    (arg~signal-wrong-type 'existing-theory obj)))


#{ We define {\vb symbol-list}, {\vb number-list}, {\vb integer-list}, {\vb posinteger-list},
 {\vb string-list},  {\vb boolean-list} and {\vb anything-list}.
#}

(arg~deflisttype symbol-list symbol)

(arg~deflisttype number-list number)

(arg~deflisttype integer-list integer)

(arg~deflisttype posinteger-list posinteger)

(arg~deflisttype string-list string)

(arg~deflisttype boolean-list boolean)

(arg~deflisttype anything-list anything)

(arg~deflisttype existing-theory-list existing-theory)

(arg~deflisttype existing-url-list existing-url)



