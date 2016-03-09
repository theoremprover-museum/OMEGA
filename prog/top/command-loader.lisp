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


(mod~defmod CLD 
            :uses (omega)
            :documentation "Dynamic command loading facility for the OMEGA system."
            :exports (
                      cld~command-files
                      cld~display-directory
                      cld~load-command-file
                      cld~load-registry-files

                      cld*commands-directory))


(defvar cld*registry-hash-table (make-hash-table :test #'equal))
(defvar cld*commands-directory (concatenate 'string
					    (if (*user-top-dir*)
						(*user-top-dir*)
					      *omegahome*)
					    "omega-3/commands/"))

(defun cld=make-command-file-name (name)
  (declare (edited  "05-AUG-1998")
	   (authors Sorge)
	   (input   "A string specifying a command file.")
	   (effect  "None.")
	   (value   "A complete pathname for the command file."))
  (make-pathname :directory cld*commands-directory :name name :type "com"))
  

(defun cld=read-directory ()
  (declare (edited  "05-AUG-1998")
	   (authors Sorge)
	   (value   "A list containing *.com files in the OMEGA commands directory."))
  (let ((dir (directory (make-pathname :directory cld*commands-directory)))
	(wild (cld=make-command-file-name :wild)))
    (mapcan #'(lambda (file)
		(when (pathname-match-p file wild)
		  (list file)))
	    dir)))

(defun cld~command-files ()
  (declare (edited  "05-AUG-1998")
	   (authors Sorge)
	   (value   "A list of all command files (without extension and path)."))
  (mapcar #'pathname-name (cld=read-directory)))

(defun cld~display-directory ()
  (declare (edited  "05-AUG-1998")
	   (authors Sorge)
	   (effect  "Prints a list of command files names to the omega~message stream.")
	   (value   "Undefined."))
  (mapc #'omega~message (cld~command-files))
  (values))

(defun cld=read-registry ()
  (declare (edited  "05-AUG-1998")
	   (authors Sorge)
	   (value   "A list containing the pathnames of all files loadable with the registry."))
  (let ((reg (make-pathname :directory cld*commands-directory :name "commands.reg"))
	files)
    (when (probe-file reg)
      (with-open-file (in reg
			  :direction :input)
		      (do* ((file (read-line in nil) (read-line in nil)))
			  ((null file) files)
			(let ((real-file (cld=make-command-file-name file)))
			  (when (probe-file real-file)
			    (push real-file files))))))))
					     

(defun cld~registry-files ()
  (declare (edited  "05-AUG-1998")
	   (authors Sorge)
	   (value   "A list of strings representing the command-files in the registry."))
  (mapcar #'pathname-name (cld=read-registry)))
  
(defun cld~load-registry-files ()
  (declare (edited  "05-AUG-1998")
	   (authors Sorge)
	   (effect  "Loads all the command files specified in the commands registry file.")
	   (value   "Undefined."))
  (let ((files (cld=read-registry)))
    (dolist (file files)
      (when (probe-file file)
	(load file)
	(setf (gethash file cld*registry-hash-table) (file-write-date file))))))

(defun cld~load-command-file (name)
  (declare (edited  "05-AUG-1998")
	   (authors Sorge)
	   (input   "A string specifying a command file.")
	   (effect  "Loads the content of the file if it exists.")
	   (value   "Undefined."))
  (let ((file (cld=make-command-file-name name)))
    (if (probe-file file)
	(progn
	  (load file)
	  (setf (gethash file cld*registry-hash-table) (file-write-date file)))
      (omega~error "Command file ~A does not exist." name))))

(defun cld~load-new-registry-files ()
  (declare (edited  "06-AUG-1998")
	   (authors Sorge)
	   (input   "None.")
	   (effect  "Loads all the command files specified in the commands registry file"
		    "that either haven't been loaded yet or have been changed since visited last.")
	   (value   "Undefined."))
  (let ((files (cld=read-registry)))
    (dolist (file files)
      (when (probe-file file)
	(let ((old-date (gethash file cld*registry-hash-table))
	      (new-date (file-write-date file)))
	  (when (or (null old-date) (< old-date new-date))
	    (load file)
	    (setf (gethash file cld*registry-hash-table) new-date)))))))
