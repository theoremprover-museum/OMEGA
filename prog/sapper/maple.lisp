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


(mod~defmod MAPLE
            :uses (serv)
            :documentation "Interface for MAPLE."
            :exports (maple~enter
                      maple~leave
                      maple~restart
                      maple~apply
                      maple~call-maple
		      ))

;;; new functions using the mathweb-service "MAPLE"
;;; 1999/04/22 Andreas Franke (afranke@ags)

(defvar maple*default-syntax 'maple2maple)
(defun maple~default-syntax () maple*default-syntax)
(defun maple~set-default-syntax (syntax)
  (cond ((string-equal :MAPLE2MAPLE syntax) (setq maple*default-syntax 'maple2maple))
        ((string-equal :POST2POST  syntax) (setq maple*default-syntax 'post2post))
        ((string-equal :POST2MAPLE syntax) (setq maple*default-syntax 'post2maple))
        ((string-equal :MAPLE2POST  syntax) (setq maple*default-syntax 'maple2post))
	(t (error "illegal value for maple*default-syntax"))))
         
(defun maple~enter   ()       (serv~enter   "MAPLE"))
(defun maple~leave   ()       (serv~leave   "MAPLE"))
(defun maple~restart ()       (serv~restart "MAPLE"))
(defun maple~apply   (method) (serv~apply   "MAPLE" method :timeout 600))

(defun maple~call-maple (expr &key ((:syntax syntax) maple*default-syntax))
  (let* ((args (if (cdr expr) (format nil "[~{\"~a\"~}]" (cdr expr)) "nil"))
         (method (format nil "compute(cmd:\"~A\" args:~A syntax:~A $)"
			 (car expr) args (string-downcase syntax))))
    (maple~enter)
    (maple~apply method)
    ))

;(defun maple~program ()
;  (declare (edited  "22-AUG-1998 20:41")
;	   (authors SORGE)
;	   (value   "The location of the MAPLE executable in the file system."))
;  (sys~getenv 'maplehome))

;(defun maple~call-maple (expr)
;  (declare (edited  "22-AUG-1998 20:30")
;	   (authors SORGE)
;	   (input   "A list of strings corresponding to a expression of MAPLE.")
;	   (effect  "Calls the CAS MAPLE and waits for the result of its computations.")
;	   (value   "A string corresponding to the result of the computation."))
;  (let* ((expr-string (maple=preprocess-expression expr))
;	 (tmp-dir (maple=tmp-directory))
;	 (in-file (make-pathname :directory tmp-dir :name "maple.in"))
;	 (out-file (make-pathname :directory tmp-dir :name "maple.out")))
;    (omega~message "Writing MAPLE input file.")
;    (with-open-file (out in-file
;			 :direction :output
;			 :if-does-not-exist :create
;			 :if-exists :supersede)
;		    (format out "~A~%" expr-string))
;    (omega~message "Calling MAPLE.")
;    (sys~call-system (format nil
;			     ;;;"cat ~A | ~A -q -f -w0 > ~A"    ;;;; call for Maple V Release 4
;			     "cat ~A | ~A -q -w0 > ~A"          ;;;; call for Maple V Release 5
;			     (namestring in-file)
;			     (maple~program)
;			     (namestring out-file)))
;    (omega~message "Reading MAPLE output file.")
;    (with-open-file (in out-file
;			:direction :input
;			:if-does-not-exist :error)
;		    (do* ((line (read-line in nil) (read-line in nil))
;			  (prt-string (when line line) (if line
;							   (concatenate 'string prt-string line)
;							 prt-string)))
;			((null line) (remove #\" prt-string))))))

;(defun maple=tmp-directory ()
;  (declare (edited  "22-AUG-1998 20:50")
;	   (authors SORGE)
;	   (input   "Nothing.")
;	   (effect  "None.")
;	   (value   "The default directory for MAPLE's temporary files."))
;  (let* ((default-dir (mk::append-directories user::*dir-sep* "tmp"
;					      user::*dir-sep* (format nil "~A-maple-dir" (sys~getenv "USER"))
;					      user::*dir-sep* )))
;    (when (not (probe-file default-dir))
;      (sys~call-system (format nil "mkdir ~A" default-dir)))
;    default-dir))

;(defun maple=preprocess-expression (expr)
;  (let ((arguments (apply #'concatenate
;			  (cons 'string
;				(mapcan #'(lambda (str)
;					    (list str " , "))
;					(butlast (cdr expr))))))
;	(length (length expr)))
;    ;; here could go some test for infix functions of MAPLE
;    (cond ((= length 1) (format nil "convert(~A,string);" (car expr)))
;	  ((= length 2) (format nil "convert(~A( ~A ),string);" (car expr) (cadr expr)))
;	  (t (format nil "convert(~A( ~A~A ),string);" (car expr) arguments (car (last expr)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The definition of th abstract CA MAPLE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (load eval compile)
  (ca~defsystem :maple
		(help "Translations for some MAPLE functions. It is important to have a correctly configured .mapleinit file that pre-loads some libraries (combinat, student,...).")
		(translations
		 (fib ("fibonacci" (:nat :nat)))
		 (simp ("simplify" (:nat)))
		 )
		(call maple~call-maple))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Object translation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Building 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod ca~build-object (term (object (eql :nat)) (system (eql :maple)))
  (let ((number (ca=build-mycas-number term)))
    (unless (typep number '(integer 1))
      (error "~A is not a positive integer" number))
    number))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rebuilding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod ca~rebuild-object ((term string) (object (eql :nat)) (system (eql :maple)))
  (let ((number (read-from-string term)))
    (unless (typep number '(integer 0))
      (error "~A is not a natural number." number))
    (ca=rebuild-mycas-number number)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auxiliary Translation Function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgeneric maple=post2maple (expr)
  (declare (edited  "07-APR-1999")
	   (authors Sorge)
	   (input   "An expression in POST syntax.")
	   (effect  "None.")
	   (value   "The corresponding expression in MAPLE syntax."))
  (:method ((term term+appl))
	   (cons (maple=post2maple (data~appl-function term))
		 (mapcar #'maple=post2maple (data~appl-arguments term))))
  (:method ((term term+abstr))
	   (omega~error "Don't know how to deal with abstractions..."))
  (:method ((term term+primitive))
	   (let* ((symb (keim~name term))
		  (arith (cdr (assoc symb '((plus . +) (minus . -) (times . *) (div . /) (power . ^))))))
	     (if arith arith
	       (let* ((number
		       (cdr (assoc symb
				   '((zero . 0) (one . 1) (two . 2)  (three . 3) (four . 4)
				     (five . 5) (six . 6) (seven . 7) (eight . 8) (nine . 9)
				     (ten . 10)))))
		      )
		 (if number number
		   (progn 
		     (casex=insert-post-term symb term)
		     symb))))))
  )
  
