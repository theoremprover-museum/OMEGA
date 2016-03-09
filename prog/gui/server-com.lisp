;; -*- syntax: common-lisp; package: OMEGA; base: 10; mode: LISP -*-
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
 
(mod~defmod sc :uses (mod sys com comint omega pdsn pds ot inter keim help rule
		      just node asi arg prob post env th socket os)
	    :documentation "Definition of the basic SERVER commands."
	    :exports ())

(com~defcommand socketread-problem      ;;; works VS SH
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats omega-basic file-io)
  (function sc=socketread-problem)
  (log-p T)
  (help "Read the file from the socket, which should contain a POST representation of a problem, and make the read problem the new current proof. A new environment is made to read the problem into."))

(defun sc=socketread-problem ()
  (let* ((list (read-from-string (socket~read)))
	 (problem (prob~find-problem (cadr list)))) 
    (when problem
      (inter~output-object (comint~interface comint*current-comint)
			   (format nil "Redefining problem ~A~%" (keim~name problem)))
      (when (prob~proofs problem)
	(dolist (x (prob~proofs problem))
	  (pds~remove-proof-plan x))))
    (let ((newobj (post~read-object list (env~create) nil)))
      (oc=prove-pre (ot~read-proof-plan newobj)))))
    ; format and send method
;    (let* ((name (parse~atom (keim~name omega*current-proof-plan)))
;	   (proof (parse~proof omega*current-proof-plan))
;	   (method (format nil "newProof(~a ~a)" name proof)))
;      (socket~write method))))

(com~defcommand socketread-pds        
  (argnames )
  (argtypes ) 
  (arghelps )
  (frag-cats omega-basic file-io)
  (function sc=socketread-pds)
  (log-p T)
  (help "Read the file, which should contain a POST representation of a
proof plan, and make the read problem the new current proof. A new
environment is made to read the problem into."))

(defun sc=socketread-pds ()
  (let* ((list (read-from-string (socket~read)))
	 (newobj (post~read-object (append (list (car list) nil) (cdr list))
				   (env~create)
				   nil))
	 (new-plan (ot~read-proof-plan newobj)))
    (when (foci~in-use) (foci~compute-pcs))
    (oc=prove (ot~read-proof-plan newobj))))

(com~defcommand socketread-resolution-proof
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats omega-basic file-io)
  (function sc=socketread-resolution-proof)
  (log-p T)
  (help "Read the file, which should contain a POST representation of a
resolution-proof, and make the read proof the new current resolution proof. A new
environment is made to read the problem into."))

(defun sc=socketread-resolution-proof ()
  (let ((newobj (res~proof-read (read-from-string (socket~read)))))
    (setq omega*current-resolution-proof (ot~read-resolution-proof newobj))
    (res~add-proof-in-hash-table newobj)))
;;
;; Commands for writing
;;

(com~defcommand socketwrite-problem      ;;; works VS SH
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats omega-basic file-io)
  (function sc=socketwrite-problem)
  (log-p T)
  (help "Write the current proof in the form of a POST expression to the socket."))

(defun sc=socketwrite-problem ()
  (let* ((problem (if (prob~proof-p omega*current-proof-plan)
		      (let ((only-problem (if (pds~proof-plan-p omega*current-proof-plan)
					      (prob~proof-problem omega*current-proof-plan)
					    omega*current-proof-plan)))
			(post~print only-problem nil))
		    nil))
	 (method (format nil "writeProblem(~a)" (if problem (write-to-string problem)
						  "nil"))))
    (socket~write method :inout)))

(com~defcommand socketwrite-pds          
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats omega-basic file-io)
  (function sc=socketwrite-pds)
  (log-p T)
  (help "Write the current proof in the form of a POST expression to the file
established. Second argument determines if the file, when it exists, will be
superseded or appended to."))

(defun sc=socketwrite-pds ()
  (let* ((pds (if (pds~proof-plan-p omega*current-proof-plan)
		  (post~print omega*current-proof-plan nil)
		nil))
	 (method (format nil "writePDS(~a)" (if pds (write-to-string pds)
					      "nil"))))
    (socket~write method :inout)))

(com~defcommand socketwrite-resolution-proof
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats omega-basic file-io)
  (function sc=socketwrite-resolution-proof)
  (log-p T)
  (help "Write the current resolution proof in the form of a POST expression to the file
established. Second argument determines if the file, when it exists, will be
superseded or appended to."))

(defun sc=socketwrite-resolution-proof ()
  (let* ((proof (if (res~proof-p omega*current-resolution-proof)
		    (post~print omega*current-resolution-proof nil)
		  nil))
	 (method (format nil "writeProof(~a)" (if proof (write-to-string proof)
						"nil"))))
    (socket~write method :inout)))

(com~defcommand socketload-file
  (function sc=socketload-file)
  (argnames )
  (argtypes )
  (arghelps ) 
  (frag-cats comint lisp file-io)
  (log-p T)
  (help "Load a lisp file into the current command interpreter."))

(defun sc=socketload-file ()
  (let ((file (socket~read :inout))
	(name (os~tmpnam)))
    (with-open-file (stream
		     name
		     :direction :output 
		     :if-does-not-exist :create
		     :if-exists :supersede)
		    (write-string (subseq file 0 (1- (length file))) stream))
    (load name)))
