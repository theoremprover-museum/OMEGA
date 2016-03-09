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


(mod~defmod MAGMA
            :uses ()
            :documentation "Interface for MAGMA."
            :exports (
		      ))


(defun magma~program ()
  (declare (edited  "22-AUG-1998 20:41")
	   (authors SORGE)
	   (value   "The location of the MAGMA executable in the file system."))
  (sys~getenv 'magmahome))
  
(defun magma~call-magma (expr)
  (declare (edited  "22-AUG-1998 20:30")
	   (authors SORGE)
	   (input   "A list of strings corresponding to a expression of MAGMA.")
	   (effect  "Calls the CAS MAGMA and waits for the result of its computations.")
	   (value   "A string corresponding with the reult of the computation."))
  (let* ((expr-string (magma=preprocess-expression expr))
	 (tmp-dir (magma=tmp-directory))
	 (in-file (make-pathname :directory tmp-dir :name "magma.in"))
	 (out-file (make-pathname :directory tmp-dir :name "magma.out")))
    (omega~message "Writing MAGMA input file.")
    (with-open-file (out in-file
			 :direction :output
			 :if-does-not-exist :create
			 :if-exists :supersede)
		    (format out "~A~%" expr-string)
		    (format out "quit;"))
    (omega~message "Calling MAGMA.")
    (sys~call-system (format nil
			     "~A -b -s ~A > ~A"
			     (magma~program)
			     (namestring in-file)
			     (namestring out-file)))
    (omega~message "Reading MAGMA output file.")
    (with-open-file (in out-file
			:direction :input
			:if-does-not-exist :error)
		    (do* ((line (read-line in nil) (read-line in nil))
			  (prt-string (when line line) (if line
							   (concatenate 'string prt-string line)
							 prt-string)))
			((null line) prt-string)))))

(defun magma=tmp-directory ()
  (declare (edited  "22-AUG-1998 20:50")
	   (authors SORGE)
	   (input   "Nothing.")
	   (effect  "None.")
	   (value   "The default directory for MAGMA's temporary files."))
  (let* ((default-dir (mk::append-directories user::*dir-sep* "tmp"
					      user::*dir-sep* (format nil "~A-magma-dir" (sys~getenv "USER"))
					      user::*dir-sep* )))
    (when (not (probe-file default-dir))
      (sys~call-system (format nil "mkdir ~A" default-dir)))
    default-dir))

(defun magma=preprocess-expression (expr)
  (let ((arguments (apply #'concatenate
			  (cons 'string
				(mapcan #'(lambda (str)
					    (list str " , "))
					(butlast (cdr expr))))))
	(length (length expr)))
    ;; here could go some test for infix functions of MAGMA
    (cond ((= length 1) (format nil "~A;" (car expr)))
	  ((= length 2) (format nil "~A( ~A );" (car expr) (cadr expr)))
	  (t (format nil "~A( ~A , ~A );" (car expr) arguments (car (last expr)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The definition of th abstract CA MAGMA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (load eval compile)
  (ca~defsystem :magma
           (help "Translations for some MAGMA functions.")
           (translations
            (fib ("Fibonacci" (:nat :nat))))
           (call magma~call-magma))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Object translation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Building 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod ca~build-object (term (object (eql :nat)) (system (eql :magma)))
  (let ((number (ca=build-mycas-number term)))
    (unless (typep number '(integer 1))
      (error "~A is not a positive integer" number))
    number))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rebuilding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod ca~rebuild-object ((term string) (object (eql :nat)) (system (eql :magma)))
  (let ((number (read-from-string term)))
    (unless (typep number '(integer 0))
      (error "~A is not a natural number." number))
    (ca=rebuild-mycas-number number)))

