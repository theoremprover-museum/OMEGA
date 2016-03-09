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

(mod~defmod OHLP 
            :uses (com comint help keim omega pp)
            :documentation "OMEGAs help command."
            :exports (
                      ohlp+help
                      
                      ohlp~apropos
                      ohlp~defhelp
                      ohlp~help
		      ohlp~html-formula
		      ohlp~html-object
		      ohlp~html-post-pprint
		      ohlp~index
                      ohlp~list2html-list
		      ohlp~make-online-manual
		      ohlp~object-list
                      ohlp~pprint-help-object-list
                      ohlp~pprint-list
                      ohlp~pprint-list2columns
		      ohlp~pprint-object
                      ohlp~pprint-sorted-help-object-list
                      ohlp~pprint-sorted-list
                      ohlp~pprint-sorted-list2columns
		      ohlp~show-categories
                      ohlp~show-commands
                      ohlp~show-fragments
                      ohlp~start-string
                      
                      ohlp*help-hash-table
                      ohlp*tab))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The new OMEGA help facility.
;; Started on May the 6th 1998 while writing a Workshop-Paper.... VS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reimplemented with a dynamic and generic approach ....
;; (June 23rd 1998) VS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass ohlp+help (help+help)
  ((start-string :initarg :start-string
		 :accessor ohlp~start-string
		 :initform nil
		 :documentation "A string that will precede help paragraphs.")
   (object-list :initarg :object-list
		:accessor ohlp=object-list
		:initform nil
		:documentation "A function that returns a list of HELP+HELP objects or a fixed list of such objects.")
   )
  (:documentation "The general help structures for OMEGA.")
  )

(defvar ohlp*help-hash-table (make-hash-table :test #'equal)
  "A hash table containing all help-structures.")


(defun ohlp~object-list (obj)
  (let ((list-func (ohlp=object-list obj)))
    (if (atom list-func)
	(funcall list-func)
      list-func)))

(defsetf ohlp~object-list (obj) (list)
  `(setf (ohlp=object-list ,obj) ,list))

(defmacro ohlp~defhelp (name &rest attribs)
  (declare (edited  "20-JUN-1997 21:08")
	   (authors SORGE)
	   (input   "A written expression of an help-object tactic."
		    "Example:
 \\begin{code}
 (ohlp~defhelp commands 
   (start-string "commands")
   (object-list ohlp~commands)
   (help "Help for commands. The start-string is not really necessary...."))
 \\end{code}")
	   (effect  "Creates an instance of ohlp+help.")
	   (value   "Returns the new help structure."))
  `(block defhelp
     (let* ((name (quote ,name))
	    (attribs (quote ,attribs))
	    (start-string) (object-list) (pretty-style) (help))
       (do ((attribs (cdr attribs) (cdr attribs))
	    (attrib (car attribs) (car attribs)))
	   ((and (null attrib) (null attribs)))
	 (if (consp attrib)
	     (cond 
	      ((string-equal (car attrib) :start-string) (setq start-string (cadr attrib)))
	      ((string-equal (car attrib) :object-list)  (setq object-list  (cadr attrib)))
	      ((string-equal (car attrib) :help)         (setq help         (cadr attrib)))
	      (t (return-from defhelp (omega~error ";;;OLHP~~DEFHELP: Not expecting ~A" (car attrib)))))
	   (return-from defhelp (omega~error ";;;OHLP~~DEFHELP: Not expecting ~A" attrib))))
       (let ((newhelp (make-instance 'ohlp+help
				     :name name
				     :start-string (if start-string start-string
						     (symbol-name name))
				     :object-list object-list
				     :help help)))
	 (setf (gethash (symbol-name name)  ohlp*help-hash-table)
	       newhelp)))))
  

(defun ohlp~apropos (string)
  (let ((hlp-list (ohlp=lexicographic-sort (ohlp~hash2list ohlp*help-hash-table t))))
    (omega~output
     (with-output-to-string (str)
	   (dolist (obj hlp-list)
	     (let ((res (ohlp=apropos string obj)))
	       (when (not (equal res ""))
		 (format str res)
		 (terpri str))))
	   str))))


(defun ohlp=apropos (string obj)
  (declare (edited  "23-JUN-1998 19:40")
	   (authors SORGE)
	   (input   "A string and a apropos-object.")
	   (effect  "None.")
	   (value   "A string containing corresponding apropos output."))
  (let* ((list (ohlp~object-list obj))
	 (name (ohlp~start-string obj))
	 (found (ohlp=find-substring-list string (mapcar #'keim~name list))))
    (if found
	(with-output-to-string (str)
       (format str "~%Matching ~A:" name)
       (format str
	       (ohlp~pprint-help-object-list
		(ohlp=apropos-order-help-objects string found list)))
       str)
      "")))

    
(defun ohlp~apropos* (string)
  (let ((hlp-list (ohlp=lexicographic-sort (ohlp~hash2list ohlp*help-hash-table t))))
    (omega~output
     (with-output-to-string (str)
	   (dolist (obj hlp-list)
	     (let ((res (ohlp=apropos* string obj)))
	       (when (not (equal res ""))
		 (format str res)
		 (terpri str))))
	   str))))

(defun ohlp=apropos* (string obj)
  (declare (edited  "06-MAY-2000" )
	   (authors Sorge)
	   (input   "A string and a apropos-object.")
	   (effect  "None.")
	   (value   "A string containing corresponding apropos output."))
  (let* ((list (ohlp~object-list obj))
	 (name (ohlp~start-string obj))
	 (found (mapcan #'(lambda (x y)
			    (when (or (ohlp=find-substring string x)
				      (ohlp=find-substring string y))
			      (list x)))
			(mapcar #'keim~name list)
			(mapcar #'help~help-string list))))
    (if found
	(with-output-to-string (str)
       (format str "~%Matching ~A:" name)
       (format str
	       (ohlp~pprint-help-object-list
		(ohlp=apropos-order-help-objects string found list)))
       str)
      "")))

    
(defun ohlp~help (object)
  (let* ((hlp-list (ohlp=lexicographic-sort (ohlp~hash2list ohlp*help-hash-table t)))
	 (str-list (mapcar #'(lambda (hlp) (ohlp=help object hlp)) hlp-list))
	 (output-list (mapcan #'(lambda (x y)
				  (unless (and (equal (car x) "")
					       (equal (cadr x) ""))
				    (list (cons (ohlp~start-string y) x))))
			      str-list hlp-list)))
    (if output-list
	(omega~output
	 (format nil "~A~%~A"
		      (with-output-to-string (str)
			    (dolist (x output-list)
			      (let ((name (first x))
				    (direct (second x))
				    (related (third x)))
				(format str "~%~%%%%%%%%%%%%%%% ~A %%%%%%%%%%%%%%" name)
				(if (equal direct "")
				    (format str "~%Related ~A:~%~A" name related)
				  (if (equal related "")
				      (format str "~%~A" direct)
				    (format str "~%~A~%~%Other related ~A:~%~A" direct name related)))))
			    str)
		      (ohlp=signature)))
      (omega~output "Nothing appropriate!"))))

(defun ohlp=help (string hlp)
  (declare (edited  "24-JUN-1998")
	   (authors Sorge)
	   (input   "A string and a help-structure.")
	   (effect  "None.")
	   (value   "A list with two strings. The first string represents the help output"
		    "for a direct matching object in the help-structure. The second string"
		    "contains a list all directly objects (that is their prefix is equal to STR)."
		    "Either one or both strings may be empty."))
  (let* ((list (ohlp~object-list hlp))
	 (found (ohlp=find-prefix-list string (mapcar #'keim~name list))))
    (if found
	(if (string-equal string (car found))
	    (list
	     (ohlp~pprint-object (ohlp=find-object (car found) list))
	     (ohlp~pprint-help-object-list
	      (ohlp=apropos-order-help-objects
	       string (cdr found) list)))
	  (list ""
		(ohlp~pprint-help-object-list 
		 (ohlp=apropos-order-help-objects
		  string found list))))
      (list "" ""))))


(defun ohlp~index ()
  (declare (edited  "21-AUG-1998")
	   (authors Sorge)
	   (input   "None.")
	   (effect  "Prints a list of all objects Help is available for.")
	   (value   "Undefined."))
  (let ((object-list
	 (mapcan #'ohlp~object-list
		 (ohlp~hash2list ohlp*help-hash-table t))))
    (omega~output
     (ohlp~pprint-sorted-list2columns object-list :duplicates nil))))

(defun ohlp~sections ()
  (declare (edited  "14-NOV-2002")
	   (authors Vxs)
	   (input   "None.")
	   (effect  "Prints a list of all sections (i.e. types of help elements).")
	   (value   "Undefined."))
  (omega~output "The following help sections are available:~%~%")
  (omega~output
   (ohlp~pprint-sorted-list2columns (ohlp~hash2list ohlp*help-hash-table) :duplicates nil)))

(defun ohlp~topics (section)
  (declare (edited  "14-NOV-2002")
	   (authors Vxs)
	   (input   "A string.")
	   (effect  "Prints a list of all help topics (i.e. help elements) in a given section.")
	   (value   "Undefined."))
  (let ((found (ohlp=find-prefix-list section (ohlp~hash2list ohlp*help-hash-table))))
    (if found
	(let ((topics (ohlp~object-list (gethash (car found) ohlp*help-hash-table))))
	  (omega~output (ohlp~pprint-sorted-list2columns topics :duplicates nil)))
      (progn
	(omega~output "Help section ~A not found!~%" section)
	(ohlp~sections)))))

;;; recover the old command functionality

(defun ohlp~show-categories ()
  (omega~output
   (ohlp~pprint-sorted-help-object-list (ohlp=categories))))

(defun ohlp~show-fragments ()
  (omega~output
   (ohlp~pprint-sorted-help-object-list (ohlp=fragments))))

(defun ohlp~show-commands (category)
  (omega~output
   (ohlp~pprint-sorted-help-object-list
    (comint~commands comint*current-comint (com~find-category category)))))


    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pretty printing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(let ((ohlp*tab 40))
  
  (defun ohlp~pprint-help-object-list (list)
    (declare (edited  "24-JUN-1998")
	     (authors Sorge)
	     (input   "A list of HELP+HELP objects.")
	     (value   "A string containing the names and help strings of the objects in"
		      "formatted output."))
    (setf ohlp*tab (+ (ohlp=compute-pprint-tab list) 3))
    (with-output-to-string (str)
			   (mapc #'(lambda (obj)
				     (format str (pp~pprint-to-string obj 'ohlp=simple-help-style)))
				 list)
			   str))

  (defun ohlp~pprint-sorted-help-object-list (list)
    (declare (edited  "24-JUN-1998")
	     (authors Sorge)
	     (input   "A list of HELP+HELP objects.")
	     (value   "A string containing the names and help strings of the objects in"
		      "formatted and alphabetically sorted output."))
    (setf ohlp*tab (+ (ohlp=compute-pprint-tab list) 3))
    (with-output-to-string (str)
			   (mapc #'(lambda (obj)
				     (format str (pp~pprint-to-string obj 'ohlp=simple-help-style)))
				 (ohlp=lexicographic-sort list))
			   str))

  (defun ohlp=pprint-help-object (obj)
    (let ((help-string (help~help-string obj)))
      (pprint-logical-block (nil nil)
			    (write-string (string (keim~name obj)))
			    (write-char #\:)
			    (write-char #\space)
			    (pprint-indent :current 0)
			    (pprint-tab :section 0 ohlp*tab)
			    (pprint-logical-block (nil (ohlp=get-words help-string))
						  (loop (pprint-exit-if-list-exhausted)
							(write (pprint-pop))
							(write-char #\space)
							(pprint-newline :fill))))))
  
  (defun ohlp=pprint-condfuncs-object (obj)
    (let ((help-string (format nil "A method ~A. Call HELP for details." (car (ohlp=condfunc-help-list obj)))))
      (pprint-logical-block (nil nil)
			    (write-string (string (keim~name obj)))
			    (write-char #\:)
			    (write-char #\space)
			    (pprint-indent :current 0)
			    (pprint-tab :section 0 ohlp*tab)
			    (pprint-logical-block (nil (ohlp=get-words help-string))
						  (loop (pprint-exit-if-list-exhausted)
							(write (pprint-pop))
							(write-char #\space)
							(pprint-newline :fill))))))
  
  )

(defun ohlp=pprint-string-list (list)
  (pprint-logical-block (nil list)
			(loop (pprint-exit-if-list-exhausted)
			      (write (pprint-pop))
			      (write-char #\space)
			      (pprint-newline :fill))))

(defun ohlp=compute-pprint-tab (list)
  (let ((maxlength 0))
    (dolist (obj list)
      (let ((length (length (if (stringp obj) obj
			      (symbol-name (keim~name obj))))))
	(when (< maxlength length)
	  (setf maxlength length))))
    maxlength))
	

(defun ohlp=get-words (string)
  (let ((newstring (substitute #\newline #\space 
			       string :test #'char=)))
    (with-input-from-string (in newstring)
			    (do ((res nil)
				 (word (read-line in nil :eof) (read-line in nil :eof)))
				((eq word :eof) (nreverse res))
			      (push word res)))))


(defgeneric ohlp~pprint-object (object)
  (declare (edited  "24-JUN-1998")
	   (authors Sorge)
	   (input   "A KEIM+OBJECT.")
	   (effect  "None.")
	   (value   "A string containing the pretty printed object.")
	   (remark  "Methods have to be written for this generic function for objects that are"
		    "included in the HELP functionality."))
  (:method (object)
	   (declare (ignore object))
	   ""))
	   
(defun ohlp~pprint-list (object-list)
  (declare (edited  "24-JUN-1998")
	   (authors Sorge)
	   (input   "A list of KEIM objects or strings.")
	   (value   "A string containing the names of the objects (or the strings)"
		    "formatted with line-breaks."))
  (pp~pprint-to-string 
   (mapcar #'(lambda (x)
	       (etypecase x
		 (string x)
		 (symbol (symbol-name x))
		 (keim+name (symbol-name (keim~name x)))))
	   object-list)
    'ohlp=simple-help-style))

(defun ohlp~pprint-sorted-list (object-list)
  (declare (edited  "24-JUN-1998")
	   (authors Sorge)
	   (input   "A list of KEIM objects or strings.")
	   (value   "Like OHLP~~PPRINT-LIST with exception that the list is alphabetically sorted."))
  (pp~pprint-to-string 
   (ohlp=lexicographic-sort
    (mapcar #'(lambda (x)
		(etypecase x
		  (string x)
		  (symbol (symbol-name x))
		  (keim+name (symbol-name (keim~name x)))))
	    object-list))
   'ohlp=simple-help-style))

(defun ohlp~pprint-list2columns (object-list &key (duplicates t))
  (declare (edited  "22-AUG-1998 02:29")
	   (authors SORGE)
	   (input   "A list of KEIM objects or strings.")
	   (value   "A string containing the names of the objects printed in columns."))
  (let* ((pre-list (mapcar #'(lambda (x)
			       (etypecase x
				 (string x)
				 (symbol (symbol-name x))
				 (keim+name (symbol-name (keim~name x)))))
			   object-list))
	 (str-list (if duplicates pre-list
		     (remove-duplicates pre-list :test #'string=)))
	 (tab (+ (ohlp=compute-pprint-tab object-list) 3))
	 (string (with-output-to-string (str)
					(pprint-tabular str str-list nil nil tab))))
    (remove #\"
	    (string-right-trim `(#\)) 
			       (string-left-trim `(#\( )
						 string)))))

(defun ohlp~pprint-sorted-list2columns (object-list &key (duplicates t))
  (declare (edited  "22-AUG-1998 02:29")
	   (authors SORGE)
	   (input   "A list of KEIM objects or strings.")
	   (value   "Like OHLP~~PPRINT-LIS2COLUMNS with exception that the list is"
		    "alphabetically sorted."))
  (let* ((pre-list (ohlp=lexicographic-sort
		    (mapcar #'(lambda (x)
				(etypecase x
				  (string x)
				  (symbol (symbol-name x))
				  (keim+name (symbol-name (keim~name x)))))
			    object-list)))
	 (str-list (if duplicates pre-list
		     (remove-duplicates pre-list :test #'string=)))
	 (tab (+ (ohlp=compute-pprint-tab object-list) 3))
	 (string (with-output-to-string (str)
					(pprint-tabular str str-list nil nil tab))))
    (remove #\"
	    (string-right-trim `(#\)) 
			       (string-left-trim `(#\( )
						 string)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auxiliary functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ohlp=find-substring (substr string &key (case-sensitive nil))
  (flet ((string2list (string)
	  (do* ((n (length string) (1- n))
		(char-list nil (append char-list (list (aref string n)))))
	      ((= n 0) (reverse char-list)))))
    (if case-sensitive
	(ohlp=sublist-p (string2list substr) (string2list string))
      (ohlp=sublist-p (string2list (string-upcase substr)) (string2list (string-upcase string))))))

(defun ohlp=find-substring-list (substr str-list &key (case-sensitive nil))
  (mapcan #'(lambda (com)
	      (when (ohlp=find-substring substr com :case-sensitive case-sensitive)
		(list com)))
	  str-list))

(defun ohlp=sublist-p (sublist list)
  (let ((sublength (length sublist)))
    (when (>= (length list) sublength)
      (or (tree-equal (subseq list 0 sublength) sublist)
	  (ohlp=sublist-p sublist (cdr list))))))
  
(defun ohlp=string-prefix-p (pref string &key (case-sensitive nil))
  (cond ((equal pref "") string)
	((equal string "") nil)
	(case-sensitive
	 (and (string= (aref pref 0) (aref string 0))
	      (ohlp=string-prefix-p (subseq pref 1)
				    (subseq string 1)
				    :case-sensitive case-sensitive)))
	(t (and (string-equal (aref pref 0) (aref string 0))
		(ohlp=string-prefix-p (subseq pref 1)
				      (subseq string 1)
				      :case-sensitive case-sensitive)))))
		       
(defun ohlp=find-prefix-list (substr str-list &key (case-sensitive nil))
  (mapcar #'car
	  (ohlp=preference-order 
	   (mapcan #'(lambda (com)
		       (let ((rest (ohlp=string-prefix-p substr
							 (etypecase com
							   (string com)
							   (symbol (symbol-name com)))
							 :case-sensitive case-sensitive)))
			 (when rest (list (cons com rest)))))
		   str-list))))



(defun ohlp~hash2list (hash-table &optional (direction nil))
  (let ((list))
    (maphash #'(lambda (x y) (if direction
				 (push y list)
			       (push x list)))
	     hash-table)
    list))

(defun ohlp=preference-order (list)
  (stable-sort list #'(lambda (x y) (<= (length (cdr x)) (length (cdr y))))))


(defgeneric ohlp=find-object (obj list)
  (declare (edited  "24-JUN-1998")
	   (authors Sorge)
	   (input   "A KEIM object or a string and a list of KEIM objects.")
	   (value   "The KEIM object in the list that is either KEIM~~EQUAL to OBJ"
		    "or whose name is string-equal to OBJ."))
  (:method ((obj keim+name) (list list))
	   (find obj list :test #'keim~equal))
  (:method ((obj string) (list list))
	   (find obj list :test #'string-equal :key #'keim~name))
  (:method ((obj symbol) (list list))
	   (ohlp=find-object (symbol-name obj) list))
  (:method ((obj list) (list list))
	   (mapcar #'(lambda (x)
		       (ohlp=find-object x list))
		   obj)))

(defun ohlp=apropos-order-help-objects (string list1 list2)
  (mapcar #'(lambda (x)
	      (ohlp=find-object x list2))
	  (ohlp=apropos-order string (mapcar #'symbol-name list1))))

(defun ohlp=apropos-order (string list &key (case-sensitive nil))
  (let* ((prefixes (ohlp=find-prefix-list string list :case-sensitive case-sensitive))
	 (rest (set-difference list prefixes)))
    (append (ohlp=lexicographic-sort prefixes)
	    (ohlp=lexicographic-sort rest))))


(defun ohlp=lexicographic-sort (string-list)
  (let ((new-list (copy-list string-list)))
    (sort new-list #'(lambda (x y) (string-lessp (if (or (symbolp x) (stringp x)) x (keim~name x))
						 (if (or (symbolp y) (stringp y)) y (keim~name y)))))))


;;; to be removed one day.... VS

(defun ohlp=signature ()
  (with-output-to-string (str)
      (format str "~%~%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
      (format str "~%How do you like the NEW OMEGA help facilities?")
      (format str "~%Please direct any questions, suggestions, critique, complaints or donations to:")
      (format str "~%sorge@ags.uni-sb.de")
      (format str "~%~%Please test also the new APROPOS command.")
      (format str "~%In case you are missing the old help facilities, try the commands:")
      (format str "~%SHOW-CATEGORIES, SHOW-FRAGMENTS, and SHOW-COMMANDS.")
      str))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A first attempt at an automatic generation of an ONLINE manual.
;; (started Sunday August 23rd 3:48 a.m. (Goodness, am I drunk)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant ohlp*alphabet '(@ A B C D E F G H I J K L M N O P Q R S T U V W X Y Z))
(defconstant ohlp*assoc-category '((commands . command) (fragments . fragment)
				   (problems . problem) (axioms . axiom)
				   (categories . category) (theories . theory)
				   (definitions . definition) (theorems . theorem)
				    (methods . method) (condfuncs . condfunc)))

(defun ohlp=make-html-index (obj-list file-name &optional (idx-name "Index") no-dir)
  (declare (edited  "23-AUG-1998 3:51")
	   (authors SORGE)
	   (input   "An assoc-list of keim-objects together with their help-category,"
		    "a string specifying a valid file name and optionally the name of the index."
		    "Finally a flag indicating whether directories should be preceding"
		    "addresses of every entry.")
	   (effect  "Writes an index file.")
	   (value   "Undefined."))
  (let (head-list
	(tail-list obj-list))
    (with-open-file (out file-name
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
		    (ohlp=write-header out idx-name idx-name)
		    (mapc #'(lambda (letter)
			      (format out "<a href=#~A>~A</A> "
				      letter (if (string-equal letter '@) #\# letter)))
			  ohlp*alphabet)
		    (format out "<P>")
		    (mapc #'(lambda (letter)
			      (ohlp=write-label out letter)
			      (ohlp=write-separator out 5)
			      (format out "<H1>~A</H1>" (if (string-equal letter '@) #\# letter))
			      (multiple-value-setq (head-list tail-list)
				(ohlp=split-list-at-letter tail-list letter))
			      (dolist (link head-list)
				(let ((name (symbol-name (keim~name (car link)))))
				  (if no-dir
				      (format out "<A href=\"~(~A~).lml\">~A</A><br>"
					      name (ohlp=parse-html-string
						    (format nil "~:(~A~)" name)))
				    (format out "<A href=\"~(~A~)/~(~A~).lml\">~A</A>  <I>~:(~A~)</I><br>"
					    (cdr link) name (ohlp=parse-html-string
							     (format nil "~:(~A~)" name))
					    (cdr (assoc (cdr link) ohlp*assoc-category :test #'string-equal)))))))
			  ohlp*alphabet)
		    (ohlp=write-separator out 1)
		    (if no-dir (ohlp=write-end out "..") (ohlp=write-end out))
		    )
    ))

(defun ohlp=make-index-file (pathname help-objects)
  (declare (edited  "25-AUG-1998 02:14")
	   (authors SORGE)
	   (input   "A pathname and a list of help-objects.")
	   (effect  "Writes the index.lml file for the OMEGA manual.")
	   (value   "Undefined."))
  (with-open-file (out (make-pathname :directory pathname
				      :name "index.lml")
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
	(ohlp=write-header out "Main Page" "The OMEGA Help Index")
	(ohlp=write-separator out 1)
	(format out "</H3>")
	(format out "<UL>~%")
	(dolist (obj (mapcar #'ohlp~start-string (ohlp=lexicographic-sort help-objects)))
	  (format out "<LI> <A HREF=\"~(~A~)/~(~A~)-index.lml\">~:(~A~)</A></LI>"
		  obj obj obj))
	(format out "<p>")
	(format out "<LI> <A HREF=\"omega-index.lml\">Index</A></LI>")
	(format out "</UL>~%")
	(ohlp=write-separator out 1)
	(format out "</H3><P>")
	(ohlp=write-end out)))

(defgeneric ohlp=anything2string (anything)
  (:method ((anything keim+name))
	   (string (keim~name anything)))
  (:method ((anything list))
	   (with-output-to-string (char-str)
				  (format char-str "~A" anything)))
  (:method ((anything string))
	   (concatenate 'string anything " "))
  (:method ((anything t))
	   (string anything)))

(defun ohlp=write-lml (stream command-list text-list &key (withmenu nil))
  (let ((command-string
	 (do* ((command command-list (rest command))
	       (str (ohlp=anything2string (car command))
		    (concatenate 'string str (ohlp=anything2string (car command)))))
	     ((null (rest command)) (remove #\newline str))))
	(text-string
	 (do* ((text text-list (rest text))
	       (str (ohlp=anything2string (car text))
		    (concatenate 'string str (ohlp=anything2string (car text)))))
	     ((null (rest text))  str)))
	(oz (if withmenu "command" "write")))
    (format stream "<FONT SIZE=+1>LOUI: <A HREF=\"/oza:~A('~A')\">~A</A></FONT>~%" oz command-string text-string)))

(defun ohlp=write-label (stream label)
  (format stream "<A Name=~A>~%" label))

(defun ohlp=write-separator (stream number)
  (format stream "<hr size=~A>~%" number))

(defun ohlp=write-header (stream title1 title2)
  (format stream "<html><head><title>OMEGA Help Index - ~A</title></head>~%" title1)
  (format stream "<body background=\"../../../../images/backyellow.gif\" bgcolor=ffffff><hr size=3><H1>~A</H1><p>~%" (ohlp=parse-html-string title2)))

(defun ohlp=write-end (stream &optional (dir "."))
  (format stream "<A href=\"~A/index.lml\">[Top]</A>~%" dir)
  (format stream "<P><address>OMEGA Help Index</address><BR>")
  (multiple-value-bind (sec min hour day month year) (get-decoded-time)
        (format stream "<DIV align=right><font size=-2> ~A-~A-~A/~A:~A.~A </font></div>"
		month day year hour min sec))
  (format stream "</body></html>~%"))


(defgeneric ohlp=parse-html-string (string)
  (declare (edited  "02-SEP-1998 02:00")
	   (authors SORGE)
	   (input   "A string.")
	   (effect  "None.")
	   (value   "HTML specific characters are masked (i.e. <,>,&)."))
  (:method ((string string))
	   (unless (equal string "")
	     (let ((fchar (aref string 0)))
	       (concatenate 'string
			    (case fchar
			      (#\< "&lt;")
			      (#\> "&gt;")
			      (#\& "&amp;")
			      (t (format nil "~A" fchar)))
			    (ohlp=parse-html-string (subseq string 1))))))
  (:method ((symbol symbol))
	   (ohlp=parse-html-string (symbol-name symbol))))

(defun ohlp=split-list-at-letter (list letter)
  (declare (edited  "23-AUG-1998 23:41")
	   (authors SORGE)
	   (input   "An assoc-list and a letter")
	   (effect  "None.")
	   (value   "Two lists where the first contains all elements of the original list that"
		    "begin with the given letter. The second list contains the rest."))
  (do ((head nil (append head (list (car tail))))
       (tail list (cdr tail)))
      ((or (string-lessp letter (aref (symbol-name (keim~name (caar tail))) 0))
	   (null tail))
       (values head tail))))

(defgeneric ohlp=sorted-pairlist (help-obj)
  (declare (edited  "23-AUG-1998 15:40")
	   (authors SORGE)
	   (input   "An OMEGA help object or a list of such objects.")
	   (effect  "None.")
	   (value   "A lexicographically sorted assoc-list associating the name of an"
		    "object with its help-category."))
  (:method ((help-obj ohlp+help))
	   (let* ((obj-list (ohlp~object-list help-obj))
		  (cat-list (make-list (length obj-list)
				       :initial-element (ohlp~start-string help-obj))))
	     (reverse (pairlis (ohlp=lexicographic-sort obj-list) cat-list))))
  (:method ((help-obj list))
	   (let* ((obj-list-list (mapcar #'ohlp~object-list help-obj))
		  (obj-list (apply #'append obj-list-list))
		  (cat-list (mapcan #'(lambda (ho ol)
					(make-list (length ol)
						   :initial-element (ohlp~start-string ho)))
				    help-obj
				    obj-list-list)))
	     (sort (pairlis obj-list cat-list)
		   #'(lambda (x y) (let ((x (car x))
					 (y (car y)))
				     (string-lessp (if (stringp x) x (keim~name x))
						   (if (stringp y) y (keim~name y)))))))))

(defgeneric ohlp=make-directory (pathname)
  (:method ((pathname pathname))
	   (unless (probe-file pathname)
	     (sys~call-system (format nil "mkdir ~A" (namestring pathname)))))
  (:method ((pathname string))
	   (let ((path (make-pathname :directory pathname)))
	     (unless (probe-file path)
	       (sys~call-system (format nil "mkdir ~A" (namestring path)))))))

(defun ohlp=make-html-file (pathname obj-cons &optional verbose)
  (declare (edited  "25-AUG-1998 00:23")
	   (authors SORGE)
	   (input   "A pathname and a cons cell relating an HELP+HELP object"
		    "to its help category." )
	   (effect  "Writes a HTML file to disk and calls the OHLP~~HTML-OBJECT method"
		    "for the given object.")
	   (value   "Undefined."))
  (let* ((object (car obj-cons))
	 (obj-str (ohlp~html-object object))
	 (name (keim~name object))
	 (category (cdr obj-cons)))
    (when verbose (omega~output "Making the file: ~(~A~)/~(~A~).lml" category name))
    (with-open-file (out (make-pathname :directory (format nil "~A/~(~A~)" pathname category)
					:name (format nil "~(~A~).lml" name))
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
		    (ohlp=write-header out category name)
		    (ohlp=write-separator out 1)
		    (format out obj-str)
		    (ohlp=write-separator out 1) 
		    (ohlp=write-end out ".."))
    ))
  

(defun ohlp~make-online-manual (&optional (pathname ".") verbose)
  (declare (edited  "24-AUG-1998 23:19")
	   (authors SORGE)
	   (input   "A pathname (it defaults to the current directory).")
	   (effect  "Writes the OMEGA online documentation to disk.")
	   (value   "Undefined."))
  (if (probe-file pathname)
      (let* ((help-objects (ohlp~hash2list ohlp*help-hash-table t))
	     (div-list (mapcar #'ohlp=sorted-pairlist help-objects))
	     (app-list (ohlp=sorted-pairlist help-objects)))
	(when verbose (omega~output "Making the manual main file."))
	(ohlp=make-index-file pathname help-objects)
	(mapc #'(lambda (obj ind)
		  (let* ((name (ohlp~start-string obj))
			 (new-path (make-pathname :directory
						  (format nil "~A/~(~A~)/" pathname name))))
		    (ohlp=make-directory new-path)
		    (when verbose (omega~output "Making the index file: ~(~A~)-index.lml" name))
		    (ohlp=make-html-index ind
					  (merge-pathnames new-path
							   (format nil "~(~A~)-index.lml" name))
					  (format nil "~:(~A~) Index" name)
					  t)))
	      help-objects
	      div-list)
	(when verbose (omega~output "Making the general index file: omega-index.lml"))
	(ohlp=make-html-index app-list (make-pathname :directory pathname
						      :name "omega-index.lml"))
	(dolist (obj app-list)
	  (ohlp=make-html-file pathname obj (when (and (numberp verbose) (> verbose 1)) verbose)))
	)
    (omega~error "Path ~A does not exist!" pathname)))

;;(defun ohlp~html-sorted-list (object-list)
;;  (declare (edited  "24-JUN-1998")
;;           (authors Sorge)
;;           (input   "A list of KEIM objects or strings.")
;;           (value   "Like OHLP~~PPRINT-SORTED-LIST with exception that the list is formated for html output."))
;;  (with-output-to-string (str)
;;     (dolist (obj (ohlp=lexicographic-sort
;;                   (mapcar #'(lambda (x)
;;                               (etypecase x
;;                                 (string x)
;;                                 (symbol (symbol-name x))
;;                                 (keim+name (symbol-name (keim~name x)))))
;;                           object-list)))
;;       (format str ""
;;    
;;  )
;;

(defun ohlp=write-subtitle (stream title)
  (format stream "<h1><I>~:(~A~)</I></h1><p>~%" title)
  (ohlp=write-separator stream 1))

(defun ohlp=list2string (list &optional (div-symbol " "))
  (declare (edited  "27-AUG-1998")
	   (authors Sorge)
	   (input   "A list of strings")
	   (effect  "None.")
	   (value   "A string as concatenation of all strings with the div-symbol inbetween."))
  (if (cdr list)
      (concatenate 'string (car list) div-symbol (ohlp=list2string (cdr list)))
    (car list)))


(defconstant ohlp*infix-symbols '("or" "and" "implies" "equiv" "="
				  ;; numerical stuff
				  "minus" "plus" "div" "times" "leq" "less" "greater" "geq" "power"
				  ;; sets etc.
				  "subset" "superset" "intersection" "union" "setminus" "smaller-cardinality"
				  "in"))

;;; unicode table
;;(defconstant ohlp*predefined-symbols '(("or" . "&or;") ("and" . "&and;") ("implies" . "&rArr;") ("not" . "&minus;")
;;                                       ("equiv" . "&hArr;") ("=" . "=") ("forall" . "&forall;")
;;                                       ("exists" . "&exists;") ("true" . "True") ("false" . "False") 
;;                                       ;; numerical stuff
;;                                       ("minus" . "-") ("plus" . "+") ("div" . "/") ("times" . "*") ("power" . "^")
;;                                       ("less" . "<") ("leq" . "&le;") ("greater" . ">") ("geq" . "&ge;")  
;;                                       ;; sets etc.
;;                                       ("subset" . "&sube;") ("superset" . "&supe;")
;;                                       ("intersection" . "&cap;") ("union" . "&cup;")
;;                                       ("setminus" . "\\") ("set-complement" . "C")
;;                                       ("smaller-cardinality" . "smaller-cardinality") ("in" . "&isin;")
;;                                       ("emptyset" . "&empty;") ("powerset" . "&weierp;")
;;                                       ))

;;; symbol font numbers
(defconstant ohlp*predefined-symbols '(("or" . "218;") ("and" . "217;") ("implies" . "222;") ("not" . "216;")
				       ("equiv" . "219;") ("=" . "61;") ("forall" . "147;")
				       ("exists" . "36;") ("true" . "84;") ("false" . "136;")
				       ("exists-unique" . "36;!")
				       ;; numerical stuff
				       ("minus" . "45;") ("plus" . "43;") ("div" . "47;") ("times" . "42;") ("power" . "173;")
				       ("less" . "60;") ("leq" . "163;") ("greater" . "62;") ("geq" . "179;")  
				       ;; sets etc.
				       ("subset" . "205;") ("superset" . "202;")
				       ("intersection" . "199;") ("union" . "200;")
				       ("setminus" . "-") ;;;("set-complement" . "45;")
				       ("smaller-cardinality" . "smaller-cardinality") ("in" . "206;")
				       ("emptyset" . "198;") ("powerset" . "195;")
				       ))

(defgeneric ohlp=infix-symbol-p (term)
  (:method ((term keim+name))
	   (ohlp=infix-symbol-p (keim~name term)))
  (:method ((term term+complex))
	   nil)
  (:method (term)
	   (find term ohlp*infix-symbols :test #'string-equal)))

(defgeneric ohlp~html-formula (formula)
  (declare (edited  "27-AUG-1998")
	   (authors Sorge)
	   (input   "A post formula of type TERM+TERM.")
	   (effect  "None.")
	   (value   "A string representing the formula in HTML syntax."))
  (:method (formula)
	   (omega~error "~A is not a valid term." formula))
  (:method ((formula term+schema))
	   (ohlp~html-formula (data~schema-range formula)))
  (:method ((appl term+appl))
	   (let* ((function (data~appl-function appl))
		  (arguments (data~appl-arguments appl))
		  (infix (ohlp=infix-symbol-p function))
		  (html-func (ohlp~html-formula function))
		  (html-args (mapcar #'ohlp~html-formula arguments)))
	     (if infix
;;                (let ((term (do* ((arg-list html-args (cdr arg-list))
;;                                  (new-list (list (car arg-list))
;;                                            (append new-list (list html-func (car arg-list)))))
;;                                ((null arg-list) (ohlp=list2string (butlast new-list))))))
;;                  (format nil "(~A)" term))
		 (format nil "[~A]" (ohlp=list2string html-args (format nil "~A" html-func)))
	       (format nil "~A(~A)" (ohlp~html-formula function) (ohlp=list2string html-args ", ")))))
  (:method ((abstr term+abstr))
	   (let ((scope (data~abstr-range abstr))
		 (vars (data~abstr-domain abstr)))
	     ;;(format nil "&lambda ~A . ~A";; unicode
	     (format nil "<FONT FACE=\"Symbol\"> &#108;</FONT> ~A . ~A"
		     (ohlp=list2string (mapcar #'ohlp~html-formula vars))
		     (ohlp~html-formula scope))))
  (:method ((variable term+variable))
	   (string-upcase (keim~name variable)))
  (:method ((variable term+number))
	   (format nil "~A" (keim~name variable)))
  (:method ((constant term+constant))
	   (let* ((const (string-downcase (keim~name constant)))
		  (predefined (find const ohlp*predefined-symbols :test #'(lambda (x y)
									    (string-equal x (car y))))))
;;;	     (if predefined (cdr predefined) const)))	    ;;;unicode 
	     (if predefined (format nil "<FONT FACE=\"Symbol\">&#~A</FONT>" (cdr predefined)) const)))	    ;;; symbol font
  )

(defun ohlp~list2html-list (list pathname &optional (columns 5))
  (declare (edited  "02-SEP-1998 01:30")
	   (authors SORGE)
	   (input   "A list, a pathname and a number.")
	   (effect  "None.")
	   (value   "A string corresponding to a html-list with the specified number of columnds."))
  (with-output-to-string (str)
      (flet ((column-separator (out)
		 (format str "<td> <a href=\"~A~(~A~).lml\">~A</A> </td>"
			 pathname out
			 (ohlp=parse-html-string
			  (format nil "~:(~A~)" out)))))
	(format str "<table>")
	;;	(format str "<table><tr> <th align ='left'> Name </th><th align ='left'> Type </th><th align ='left'> Description </th></tr>~%")
	(do* ((tmplist list (nthcdr columns tmplist))
	      (outlist (subseq tmplist 0 columns) (subseq tmplist 0 columns)))
	    ((null outlist))
	  (format str "<tr>")
	  (mapc #'column-separator outlist)
	  (format str "</tr>~%"))
	(format str "</table>~%"))))

(defun ohlp~html-post-pprint (term string)
  (declare (edited  "19-JAN-1999")
	   (authors Sorge)
	   (input   "A TERM and a STRING")
	   (effect  "None.")
	   (value   "Pretty prints the post syntax of TERM to STRING"))
  (format string "~A" 
	  (doc~replace-char-by-string  
	   (with-output-to-string (str)
				  (post~pprint term str)
				  str)
	   '((#\NEWLINE . "<br>~%") (#\SPACE . "&nbsp;&nbsp;&nbsp;")))))

(defgeneric ohlp~html-object (object)
  (declare (edited  "25-AUG-1998 00:53")
	   (authors SORGE)
	   (input   "A HELP+HELP.")
	   (effect  "None.")
	   (value   "A string containing the html output for the object.")
	   (remark  "Methods have to be written for this generic function for objects that are"
		    "included in the HELP functionality."))
  (:method (object)
	   (declare (ignore object))
	   ""))
	   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test stuff for commmands etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ohlp~defhelp commands
	      (object-list ohlp=commands)
	      (help "Help for commands."))

(defun ohlp=commands ()
  (remove-duplicates
   (mapcan #'com~commands
	   (ohlp~hash2list com*fragment-hash-table t))))

(defmethod ohlp~pprint-object ((command com+command))
  (declare (edited  "25-MAY-1998" "12-JUL-1995") 
	   (authors Sorge Kohlhase)
	   (input   "A command.")
	   (effect  "None.")
	   (value   "A prettily formated output string containing all necessary informations on the command."))
  (let* ((argnames (com~argnames command))
	 (argtypes (com~argtypes command))
	 (arghelps (com~arghelps command))
	 (help (com~help command))
	 (line1* nil)
	 (line2* nil)
	 (argnamehelppairs (mapcar #'(lambda (x y) (list x y)) 
				   argnames arghelps))
	 (fragment (keim~name (com~fragment command)))
	 (categories (mapcar #'keim~name (com~categories command))))
    (do* ((line1 (mapcar #'symbol-name argnames) (cdr line1))
	  (line2 (mapcar #'symbol-name argtypes) (cdr line2))
	  (line1-lengths (mapcar #'length line1) (cdr line1-lengths))
	  (line2-lengths (mapcar #'length line2) (cdr line2-lengths))
	  (tmp (format nil "~A: ~A" (keim~name comint*current-comint) (keim~name command)))
	  (ltmp (length tmp)))
	((null line1) (setq line1* (cons tmp (nreverse line1*)))
	 (setq line2* (cons (make-string ltmp :initial-element #\space)
			    (nreverse line2*))))
      (let ((l1 (car line1))(l1l (car line1-lengths))
	    (l2 (car line2))(l2l (car line2-lengths)))
	(if (> l1l l2l)
	    (let* ((diff (- l1l l2l)) (diff2 (floor diff 2)))
	      (push l1 line1*)
	      (push (format nil "~A~A~A" 
			    (make-string diff2 
					 :initial-element
					 #\space)
			    l2
			    (make-string (- diff diff2)
					 :initial-element
					 #\space))
		    line2*))
	  (let* ((diff (- l2l l1l)) (diff2 (floor diff 2)))
	    (push l2 line2*)
	    (push (format nil "~A~A~A" 
			  (make-string diff2 
				       :initial-element
				       #\space)
			  l1
			  (make-string (- diff diff2)
				       :initial-element
				       #\space))
		  line1*)))))
    ;; a cheapo way to do it, not very good 
    (format nil "~%~A is a command.~%~
                       ~A~%~%The command format for ~A is:~%~
                       ~{~A  ~}~%~{~A  ~}~%~
                       ~@[The arguments have the following meanings:~%~
                       ~:{~A : ~A~%~}~%~]~%~
                       It is defined in the fragment ~A~%and member of the categories~%~{~A ~}"
	    (keim~name command) help (keim~name command) 
	    line1* line2* argnamehelppairs
	    fragment categories)))

(defmethod ohlp~html-object ((command com+command))
  (declare (edited  "25-AUG-1998 01:08")
	   (authors SORGE))
  (let* ((argnames (com~argnames command))
	 (argtypes (com~argtypes command))
	 (arghelps (com~arghelps command))
	 (help (help~help-string command))
	 (fragment (keim~name (com~fragment command)))
	 (categories (mapcar #'keim~name (com~categories command))))
    (with-output-to-string (str)
       (ohlp=write-subtitle str "Command")
       (format str "<h3>~%")
       (format str "It is defined in the fragment: <a href=\"../fragments/~(~A~).lml\">~A</A><br>~%"
	       fragment (ohlp=parse-html-string (format nil "~:(~A~)" fragment)))
       (format str "It is registered in the following categories: ")
       (format str "~A~%" (ohlp~list2html-list (ohlp=lexicographic-sort categories) "../categories/"))
       (format str "<p>~%")
       (format str "Its purpose:    ~A<p>~%" help)
       (if argnames
	   (progn
	     (format str "It has ~A arguments:<br>~%" (length argnames))
	     (format str "</h3>")
	     (format str "<h2>")
	     (format str "<table><tr> <th align ='left'> Name </th><th align ='left'> Type </th><th align ='left'> Description </th></tr>~%")
	     (mapcar #'(lambda (arg-name arg-type arg-help)
			 (format str "<tr> <td><b> ~A </b></td><td> ~A </td><td><i> ~A </i></td></tr>~%"
				 arg-name arg-type arg-help))
		     argnames argtypes arghelps)
	     (format str "</table>~%")
	     (format str "</h2><p><p>"))
	 (format str "No arguments required.</h2><p><p>~%"))
       (ohlp=write-separator str 1)
       (ohlp=write-lml str (list command) (list "Execute the command.") :withmenu T)
       str)))
		   

(ohlp~defhelp fragments
	      (object-list ohlp=fragments)
	      (help "Help for fragments."))

(defun ohlp=fragments ()
  (ohlp~hash2list com*fragment-hash-table t))

(defmethod ohlp~pprint-object ((frag com+fragment))
  (let* ((commands (ohlp~hash2list (com~frag-direct-commands frag)))
	 (categories (com~frag-direct-categories frag))
	 (help (help~help-string frag))
	 (fragments (com~frag-uses-comms-of frag)))
    (with-output-to-string (str)
         (format str "~%~A is a FRAGMENT." (keim~name frag))
	 (format str "~%~A" help)
	 (format str "~%~%Commands:")
	 (format str "~%~A" (ohlp~pprint-sorted-list2columns commands))
	 (format str "~%~%Includes the commands of fragments:")
	 (format str "~A" (ohlp~pprint-sorted-list fragments))
	 (format str "~%~%And has as categories:")
	 (format str "~A" (ohlp~pprint-sorted-list (remove-duplicates categories)))
	 str)))

(defmethod ohlp~html-object ((frag com+fragment))
  (let* ((commands (ohlp~hash2list (com~frag-direct-commands frag)))
	 (categories (com~frag-direct-categories frag))
	 (help (help~help-string frag))
	 (fragments (com~frag-uses-comms-of frag)))
    (with-output-to-string (str)
       (ohlp=write-subtitle str "Fragment")
       (format str "<h3>~%")
       (format str "~A<p>~%" help)
       (format str "Commands:<br>~%")
       (format str "~A~%" (ohlp~list2html-list (ohlp=lexicographic-sort commands)
					       "../commands/"))
       (format str "<p>~%")
       (format str "Includes the commands of fragments:<br>")
       (format str "~A~%" (ohlp~list2html-list
			   (mapcar #'keim~name (ohlp=lexicographic-sort fragments))
			   "../fragments/"))
       (format str "<p>~%")
       (format str "~%~%And has as categories:")
       (format str "~A~%" (ohlp~list2html-list
			   (mapcar #'keim~name (ohlp=lexicographic-sort categories))
			   "../categories/"))
       (format str "<p></h3>~%")
       str)))

(ohlp~defhelp categories
	      (object-list ohlp=categories)
	      (help "Help for categories."))

(defun ohlp=categories ()
  (ohlp~hash2list (com~category-hash-table) t))

(defmethod ohlp~pprint-object ((cat com+category))
  (let* ((commands (mapcar #'keim~name (com~commands cat)))
	 (help (help~help-string cat)))
    (with-output-to-string (str)
         (format str "~%~%~A is a CATEGORY." (keim~name cat))
	 (format str "~%~A" help)
	 (format str "~%~%Commands:")
	 (format str "~%~A" (ohlp~pprint-sorted-list2columns commands))
	 str)))

(defmethod ohlp~html-object ((cat com+category))
  (let* ((commands (com~commands cat))
	 (help (help~help-string cat)))
    (with-output-to-string (str)
       (ohlp=write-subtitle str "Category")
       (format str "<h3>~%")
       (format str "~A<p>~%" help)
       (format str "Commands:<br>~%")
       (format str "~A~%" (ohlp~list2html-list (ohlp=lexicographic-sort commands)
					       "../commands/"))
       (format str "<p></h3>~%")
       str)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stuff for theories and their objects....
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(ohlp~defhelp problems
	      (object-list ohlp=problems)
	      (help "Help for problems"))

(defun ohlp=problems ()
  (mapcan #'th~problems (th~all-theories)))


(ohlp~defhelp theorems
	      (object-list ohlp=theorems)
	      (help "Help for theorems"))

(defun ohlp=theorems ()
  (mapcan #'th~theorems (th~all-theories)))


(ohlp~defhelp axioms
	      (object-list ohlp=axioms)
	      (help "Help for axioms"))

(defun ohlp=axioms ()
  (mapcan #'th~axioms (th~all-theories)))

(defmethod ohlp~pprint-object ((axiom th+axiom))
  (let ((theory (keim~name (th~ass-theory axiom)))
	(help (help~help-string axiom))
	(formula (th~ass-formula axiom)))
    (with-output-to-string (str)
         (format str "~%~%~A is an AXIOM of the theory ~A." (keim~name axiom) theory)
	 (format str "~%~%~A" help)
	 (format str "~%~%Formula: ~A" formula)
	 str)))

(defmethod ohlp~html-object ((axiom th+axiom))
  (let* ((theory (keim~name (th~ass-theory axiom)))
	 (help (help~help-string axiom))
	 (formula (th~ass-formula axiom)))
    (with-output-to-string (str)
       (ohlp=write-subtitle str "Axiom")
       (format str "<h3>~%")
       (format str "~A<p>~%" help)
       (format str "It is defined in the theory: <a href=\"../theories/~(~A~).lml\">~:(~A~)</A><P>~%" theory theory)
       (format str "Formula: <font size=+3> ~A </font><P>" (ohlp~html-formula formula))
       (format str "Formula in post syntax: <br><font size=+1> ")
       (ohlp~html-post-pprint formula str)
       (format str " </font>")
       (format str "</h3>~%")
       (ohlp=write-separator str 1)
       (ohlp=write-lml str (list "import-ass" axiom)
		           (list "Import the axiom" axiom
				 " as assumption to your current problem."))
       str)))

(defmethod ohlp~html-object ((theory th+theory))
  (labels ((make-names-list (list)
			   (mapcar #'(lambda (elem) (keim~name elem)) list)))

  (let* ((theoryname (keim~name theory))
	 (help (help~help-string theory))
	 (problems (make-names-list (th~problems theory)))
	 (theorems (make-names-list (th~theorems theory)))
	 (axioms (make-names-list (th~axioms theory)))
	 (defis (make-names-list (th~definitions theory)))
	 (simpis (make-names-list (th~simplifiers theory)))
	 (parents (make-names-list (th~imports theory)))
	 (allparents (make-names-list (th~imports-recursed theory)))
	 (methods (make-names-list (mapcan #'(lambda (meth) (when (eq (meth~theory meth) theory) (list meth)))
								   (ohlp=methods)))))

    (with-output-to-string (str)
        (ohlp=write-subtitle str "Theory")
	(format str "<h3>~%")
	(format str "~A<p>~%" help)
	(when parents
	  (format str "Direct parent theories:")
	  (format str "~A~%" (ohlp~list2html-list (ohlp=lexicographic-sort parents)
						  "../theories/")))
	(when allparents
	  (format str "<P>All parent theories:")
	  (format str "~A~%" (ohlp~list2html-list (ohlp=lexicographic-sort allparents)
						  "../theories/")))
	(when axioms
	  (format str "<P>Axioms:")
	  (format str "~A~%" (ohlp~list2html-list (ohlp=lexicographic-sort axioms)
						  "../axioms/")))
	(when defis
	  (format str "<P>Definitions:")
	  (format str "~A~%" (ohlp~list2html-list (ohlp=lexicographic-sort defis)
						  "../definitions/")))
	(when theorems
	  (format str "<P>Theorems:")
	  (format str "~A~%" (ohlp~list2html-list (ohlp=lexicographic-sort theorems)
						  "../theorems/")))
	(when problems
	  (format str "<P>Problems:")
	  (format str "~A~%" (ohlp~list2html-list (ohlp=lexicographic-sort problems)
						  "../problems/")))
	(when methods
	  (format str "<P>Methods:")
	  (format str "~A~%" (ohlp~list2html-list (ohlp=lexicographic-sort methods)
						  "../methods/")))
;        (when simpis
;          (format str "<P>Simplifiers:")
;          (format str "~A~%" (ohlp~list2html-list (ohlp=lexicographic-sort simpis)
;                                                  "../simplifiers/")))
	(format str "</h3>~%")
	str))))

(defmethod ohlp~html-object ((problem prob+problem))
  (let* ((theory (string (keim~name (prob~theory problem))))
	 (help (help~help-string problem))
	 (asslist (prob~assumptions problem))
	 (conclusion (prob~conclusion problem))
	 (theorem! (prob~proven-p problem)))
    (with-output-to-string (str)
       (if theorem! (ohlp=write-subtitle str "Theorem") (ohlp=write-subtitle str "Problem")) 
       (format str "<h3>~%")
       (format str "~A<p>~%" help)
       (format str "It is defined in the theory: <a href=\"../theories/~(~A~).lml\">~:(~A~)</A><P>~%" theory theory)
;;;the assumptions
       (format str "It has ~A assumptions:<br>~%" (length asslist))
       (format str "</h3>")
       (format str "<h2>")
       (format str "<table>~%")
       (dolist (ass asslist)
	 (format str "<tr valign=top nosave> <td><BR> ~A </td><td> " (keim~name ass))
	 (ohlp~html-post-pprint (node~formula ass) str)
	 (format str "</td></tr>~% <tr> <td> </td><td> ~A </td></tr>~%"
		 (ohlp~html-formula (node~formula ass))))
       (format str "</table>~%")
;;; the conclusion
       (format str "<H3> It has the conclusion:<br>~%")
       (format str "</h3>")
       (format str "<h2>")
       (format str "<table>~%")
       (format str "<tr valign=top nosave> <td><BR> ~A </td><td> "
	           (keim~name conclusion))
       (ohlp~html-post-pprint (node~formula conclusion) str)
       (format str " </td></tr>~%")
       (format str "<tr> <td> </td><td> ~A </td></tr>~%"
	              (ohlp~html-formula (node~formula conclusion)))
       (format str "</table>~%")
       (format str " </font>")
       (format str "</h3>~%")
       (ohlp=write-separator str 1)
;;;LML for theorem
       (if theorem!
	   (ohlp=write-lml str (list "import-ass" problem)
			   (list "Import the theorem" problem " as assumption to your current problem."))
;;;LML for problem
	 (progn
	   (ohlp=write-lml str (list "load-theory-problems" theory)
			   (list "Load the Problem"))
	   (format str "<BR>~%")
	   (ohlp=write-lml str (list "prove" problem)
			   (list "Make the problem" problem
					" to your current problem."))
	   (let ((replay (th~lml problem)))
	     (when replay
	       (format str "<P><FONT SIZE=+1><B>Now click on the following links to prove the problem.</B></FONT>~%")
	       (dolist (command replay)
		 (format str "<BR>~%")
		 (ohlp=write-lml str (list "execute-command" command)
				 (list "Apply the" (first command)
					"-command" (second command) ".")))))))
       str)))

(ohlp~defhelp definitions
	      (object-list ohlp=definitions)
	      (help "Help for definitions"))

(defun ohlp=definitions ()
  (mapcan #'th~definitions (th~all-theories)))

(defmethod ohlp~pprint-object ((def th+def))
  (let ((theory (keim~name (th~ass-theory def)))
	(help (help~help-string def))
	(formula (th~ass-formula def)))
    (with-output-to-string (str)
         (format str "~%~%~A is a DEFINITION of the theory ~A." (keim~name def) theory)
	 (format str "~%~%~A" help)
	 (format str "~%~%It is an abbreviation of the concept: ~A" formula)
	 str)))

(defmethod ohlp~html-object ((axiom th+def))
  (let* ((theory (keim~name (th~ass-theory axiom)))
	 (help (help~help-string axiom))
	 (formula (th~ass-formula axiom)))
    (with-output-to-string (str)
       (ohlp=write-subtitle str "Definition")
       (format str "<h3>~%")
       (format str "~A<p>~%" help)
       (format str "It is defined in the theory: <a href=\"../theories/~(~A~).lml\">~:(~A~)</A><P>~%" theory theory)
       (format str "~%~%It is an abbreviation of the concept: <font size=+3> ~A </font><P>" (ohlp~html-formula formula))
       (format str "Formula in post syntax: <br><font size=+1> ")
       (ohlp~html-post-pprint formula str)
       (format str " </font>")
       (format str "<p></h3>~%")
       str)))

(ohlp~defhelp theories
	      (object-list th~all-theories)
	      (help "Help for theories"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Condfuncs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass ohlp+condfuncs (help+help)
  ((start-string :initarg :helplist
		 :accessor ohlp=condfunc-help-list
		 :initform nil
		 :documentation "A string that will precede help paragraphs."))
  (:documentation "The general help structures for Condfuncs.")
  )

(ohlp~defhelp condfuncs
	      (object-list ohlp=condfunc)
	      (help "Help for Cond-Funcs."))

(defun ohlp=condfunc ()
 (let ((list))
    (maphash #'(lambda (x y)
		 (push
		 (make-instance 'ohlp+condfuncs  :helplist y
				                 :name  x)
		 list))
	     meth*help4condfunc)
    list))




  (defmethod ohlp~pprint-object ((condfunc ohlp+condfuncs))
  (let* ((helplist (ohlp=condfunc-help-list condfunc))
	 (outlist (mapcan #'(lambda (x) (when (and (listp x)
						   (not (or (string-equal (car x) 'edited)
							    (string-equal (car x) 'authors))))
					  (list x)))
			  helplist))
	;;; (maxlength (apply #'max (mapcar #'(lambda (x) (length (car x))) outlist))))
	 )
    (flet ((paragraph (header rest)    
              (pprint-logical-block (nil nil)
				    (write-string (string header))
				    (write-char #\:)
				    (write-char #\space)
				    (pprint-indent :current 0)
				    (pprint-tab :section 0 10)
				    (pprint-logical-block (nil  (mapcan #'ohlp=get-words rest))
							  (loop (pprint-exit-if-list-exhausted)
								(write-string (pprint-pop))
								(write-char #\space)
								(pprint-newline :fill))))))

    (with-output-to-string (str)
         (format str "~%~%~A is a METHOD ~A." (string-upcase (keim~name condfunc)) (car helplist))
	 (dolist (x '(input effect value))
	   (let ((attrib (assoc x outlist :test #'string-equal))
		 (*standard-output* str))
	       (format str "~%")
	     (when (second attrib)
	       (format str "~%")
	       (paragraph (car attrib) (rest attrib)))
	     (setq outlist (remove attrib outlist))))
	 (dolist (attrib outlist)
	   (let ((*standard-output* str))
	       (format str "~%")
	     (when (second attrib)
	       (format str "~%")
	       (paragraph (car attrib) (rest attrib)))
	     ))))))


(defmethod ohlp~html-object ((condfunc ohlp+condfuncs))
  (let ((helplist (ohlp=condfunc-help-list condfunc))) 
  (with-output-to-string (str)
       (ohlp=write-subtitle str (car helplist))
       (format str "<h3>~%")
       (format str "<TABLE BORDER>~%")
       (dolist (entry (rest helplist))
	 (when (listp entry)
	   (format str "<TR> <TD> ~A </TD> <TD> ~A </TD> </TR>~%"
		   (first entry)
		   (if (null (rest entry))
		       "-"
		     (do* ((lines
			    (cons (ohlp=parse-html-string (cadr entry)) (cddr entry))
			    (cons (ohlp=parse-html-string  (cadr lines)) (cddr lines)))
			   (helpstring  (car lines) (concatenate 'string helpstring " <BR> " (car lines))))
			 ((null (cdr lines)) helpstring))))))
       (format str "</TABLE>~%")
       (format str "<p></h3>~%")
       str)))



(pp~defstyle 
 ohlp=simple-help-style
 :help "Style used for printing commands, categories, fragments etc."
 :pprint-methods
 ((ohlp+condfuncs (lambda (s l)
		    (let ((*standard-output* s))
		      (ohlp=pprint-condfuncs-object l))))
  (help+help (lambda (s category)
                  (let ((*standard-output* s))
                    (ohlp=pprint-help-object category))))
  (string (lambda (s l) (write-string l s)))
  (list (lambda (s l) 
          (let ((*standard-output* s))
            (ohlp=pprint-string-list l))))
  ))

