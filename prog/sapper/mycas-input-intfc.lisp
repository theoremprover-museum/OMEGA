;;; -*- syntax: common-lisp; package: OMEGA; base: 10; mode: LISP -*-
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This box of goodies is all the stuff that allows you, me,
;; everybody, to enter a polynomial in a more convenient way
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(in-package "KEIM")


(eval-when (compile load eval)
(defun ca=generate-chars (char1 char2)
  (declare (edited  "02-APR-1995")
	   (authors Sorge)
	   (input   "Two characters")
	   (effect  "None")
	   (value   "A list containing the ascii code of all the characters inbetween char1 and char2"))
  (if (>= (char-int char1) (char-int char2))
      (list char2)
    (append (list char1)
	    (ca=generate-chars (code-char (1+ (char-int char1))) char2))))
)

;;; some constants, representing frequently used lists of characters

(defconstant ca*number-chars
  (remove-duplicates (append (ca=generate-chars #\0 #\9)
			     '(#\. #\-))
		     :from-end t))
   
(defconstant ca*variable-chars
  (remove #\e (append (ca=generate-chars #\a #\z)
		      (ca=generate-chars #\A #\Z)
		      '(#\_))
	  :from-end t))
 
(defconstant ca*power-char #\^)

(defconstant ca*mult-char #\*)

(defconstant ca*add-char #\+)

(defconstant ca*sub-char #\-)

(defconstant ca*field-chars '(#\n #\c #\r #\q #\z))

(defconstant ca*sep-char #\,)

(defconstant ca*junk-chars '(#\Space #\Newline #\Tab))

(defgeneric ca=read-from-console (string)
  (declare (edited  "02-APR-1995")
	   (authors Sorge)
	   (input   "A string")
	   (effect  "None")
	   (value   "A data-list for a polynomial, a field-character and a variable number"))
  (:method (string)
	   (error "something must have gone wrong, ~A is not a string" string))
  (:method ((string string))
	   (let ((field (ca=get-field string))
		 (polystr (ca=get-poly-string string))
		 (varlist (ca=get-varlist (ca=string-left-right-trim string) ca*sep-char)))
	     (values
	      (mapcar #'(lambda (x) (ca=convert-monomial x varlist)) (ca=get-varlist polystr ca*add-char))
	      field
	      (length varlist)
	     ))))


(defun ca=get-field (string)
  (declare (edited  "05-APR-1995")
	   (authors Sorge)
	   (input   "A string")
	   (effect  "None")
	   (value   "A field character from the string"))
  (cond ((= (length string) 0)
	 (error "no field-symbol"))
	((member (char string 0) ca*field-chars)
	 (char string 0))
	(t (ca=get-field (delete (char string 0) string :count 1)))))

(defun ca=get-poly-string (string)
  (declare (edited  "05-APR-1995")
	   (authors Sorge)
	   (input   "A string")
	   (effect  "None")
	   (value   "A string representing the polynomial in the input string"))
  (if (find ca*sep-char string)
      (subseq string (1+ (position ca*sep-char string :from-end t)) (length string))
    (error "no polynomial in sight")))

(defun ca=string-left-right-trim (string)
  (declare (edited  "05-APR-1995")
	   (authors Sorge)
	   (input   "A string")
	   (effect  "Cuts of the first part till , and the last part beginning from , of the string")
	   (value   "The modified string"))
  (setf string (subseq string (1+ (position ca*sep-char string)) (length string)))
  (when (find ca*sep-char string)
    (setf string (subseq string 0 (position ca*sep-char string :from-end t))))
  string)
  
(defun ca=get-varlist (string sep-char)
  (declare (edited  "05-APR-1995")
	   (authors Sorge)
	   (input   "A string and a character")
	   (effect  "None")
	   (value   "A list of strings, seperated by the character in the original string"))
  (if (find sep-char string)
      (append (list (string-trim ca*junk-chars (subseq string 0 (position sep-char string))))
	      (ca=get-varlist (subseq string (1+ (position sep-char string)) (length string)) sep-char))
    (list (string-trim ca*junk-chars string))))
    

(defun ca=convert-monomial (string varlist)
  (declare (edited  "03-APR-1995")
	   (authors Sorge)
	   (input   "A string (monomial) and a list of variables in the polynomial")
	   (effect  "None")
	   (value   "Coherent data for the interior polynomial representation"))
  (ca=reduce-to-data
   (mapcar #'(lambda (x) (string-trim ca*junk-chars x))
	   (ca=get-term (ca=get-varlist string ca*mult-char)))
   varlist))
  


(defun ca=get-term (strlist)
  (declare (edited  "05-APR-1995")
	   (authors Sorge)
	   (input   "A list of strings")
	   (effect  "None")
   	   (value   "A list of all terms in the monomial"))
  (cond ((null strlist) strlist)
	((member (char (car strlist) 0) ca*number-chars)                          ;a normal number
	 (append (list (subseq (car strlist)
			       0
			       (ca=occurence-of (car strlist) ca*variable-chars)))
		 (progn (if (ca=occurence-of (car strlist) ca*variable-chars)
			    (ca=get-term
			     (append
				  (list (subseq (car strlist)
						(ca=occurence-of (car strlist) ca*variable-chars)
						(length (car strlist))))
				  (cdr strlist)))
			  (ca=get-term (cdr strlist))))))
	((equal (char (car strlist) 0) #\#)                               ; a complex number
	 (append (progn (if (ca=variable-after-complex (car strlist))
			    (list (subseq (car strlist)
					  0
					  (+ 2 (ca=variable-after-complex (car strlist)))))
			  (list (car strlist))))
	         (progn (if (ca=variable-after-complex (car strlist))
			    (ca=get-term
			     (append
			      (list (subseq (car strlist)
					    (+ 2 (ca=variable-after-complex (car strlist)))
					    (length (car strlist))))
			      (cdr strlist)))
			  (ca=get-term (cdr strlist))))))
	(t (append (ca=get-varlist (car strlist) #\ )                                   ; a variable
		   (ca=get-term (cdr strlist))))))


(defun ca=variable-after-complex (string)
  (declare (edited  "24-APR-1995")
	   (authors Sorge)
	   (input   "A string")
	   (effect  "None")
	   (value   "The position of the first vafter a complex number or nil if there is none."))
  (ca=occurence-of (subseq string
			   2
			   (length string))
		   ca*variable-chars))
   


(defun ca=occurence-of (string charlist)
  (declare (edited  "04-APR-1995")
	   (authors Sorge)
	   (input   "A string and a characterlist")
	   (effect  "None")
	   (value   "Determines the first occurence of one member of charlist in string"))
  (position
   (car (some #'(lambda (x) (member x charlist)) string))
   string))


(defun ca=reduce-to-data (strlist varlist)
  (declare (edited  "04-APR-1995")
	   (authors Sorge)
	   (input   "A strlist and a variable list")
	   (effect  "None")
	   (value   "A list with usable data, i.e. coefficients and exponents for variables according to their position in the varlist"))
  (let ((data (append (list 1) (make-list (length varlist) :initial-element 0))))
    (mapcar #'(lambda (x)
		(cond ((member (char x 0) ca*number-chars)
		       (setf (car data) (* (car data) (ca=convert-to-number x))))
		      ((equal (char x 0) #\#)
		       (setf (car data) (* (car data) (ca=convert-to-complex x))))
		      (t (setf (nth (ca=get-variable-position (ca=get-variable-name x)
							      varlist)
				    data)
			       (+ (nth (ca=get-variable-position (ca=get-variable-name x)
								 varlist)
				       data)
				  (ca=convert-exponent x))))))
	    strlist)
    data))


(defun ca=get-variable-name (string)
  (declare (edited  "05-APR-1995")
	   (authors Sorge)
	   (input   "A string consisting of a variable and an exponent")
	   (effect  "None")
	   (value   "The variable inside the string"))
  (subseq string 0 (position ca*power-char string)))


(defun ca=convert-exponent (string)
  (declare (edited  "05-APR-1995")
	   (authors Sorge)
	   (input   "A string consisting of a variable and an exponent")
	   (effect  "None")
	   (value   "An positive integer"))
  (if (find ca*power-char string)
      (parse-integer (subseq string
			     (1+ (position ca*power-char string))
			     (length string))
		     :junk-allowed t)
    1))


(defun ca=get-variable-position (string list)
  (declare (edited  "04-APR-1995")
	   (authors Sorge)
	   (input   "A string and a list")
	   (effect  "None")
	   (value   "Determines the position (counting from 1) of string in the list"))
  (if (member string list :test #'string=)
      (1+ (position string list :test #'string=))
    (error "undeclared variable ~A" string)))
	 

(defun ca=convert-to-number (string)
  (declare (edited  "04-APR-1995")
	   (authors Sorge)
	   (input   "A string")
	   (effect  "None")
	   (value   "A number corresponding to that string"))
  (cond ((find #\/ string)
	 (/ (ca=convert-to-number (subseq string 0 (position #\/ string)))
	    (ca=convert-to-number (subseq string (1+ (position #\/ string)) (length string)))))
	((find #\e string)
	 (* (ca=convert-to-number (subseq string 0 (position #\e string)))
	    (expt 10 (ca=convert-to-number (subseq string (1+ (position #\e string)) (length string))))))
	((equal (char (string-trim ca*junk-chars string) 0) #\.)
	 (ca=convert-to-float (subseq string 1 (length string))))
	((equal (char (string-trim ca*junk-chars string) 0) ca*sub-char)
	 (* -1 (ca=convert-to-number (subseq string 1 (length string)))))
	((find #\. string)
	 (+ (ca=convert-to-number (subseq string 0 (position #\. string)))
	    (ca=convert-to-float (subseq string (1+ (position #\. string)) (length string)))))
	(t (parse-integer string :junk-allowed t))))


(defun ca=convert-to-float (string)
  (declare (edited  "05-APR-1995")
	   (authors Sorge)
	   (input   "A string")
	   (effect  "None")
	   (value   "The fraction of floating point number"))
  (let ((h1) (h2))
    (multiple-value-setq (h1 h2) (ca=convert-to-number string))
    (cond ((null h1) (error "something's most definitly wrong here"))
	  ((null h2) 0)
	  (t (float (/ h1 (expt 10 h2)) 1.0d0)))))
	

(defun ca=convert-to-complex (string)
  (declare (edited  "21-APR-1995")
	   (authors Sorge)
	   (input   "A string")
	   (effect  "None")
	   (value   "A complex number"))
  (let ((c1 (ca=convert-to-number (subseq string
					  (1+ (position #\( string))
					  (position #\  string))))
	(c2 (ca=convert-to-number (subseq string
					  (1+ (position #\  string))
					  (position #\) string)))))
    (complex c1 c2)))
