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

(in-package "OMEGA")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;; Module for generating TeX output of an ND proof in a tree notation       ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;


;;; Plenty of global variables to keep things easy to adjust to specific changes, like:
;;; different LaTeX, different style files, different paper size, etc.

(defvar p2ttree*documentclass "\\documentclass[10pt]{article}")
(defvar p2ttree*usepackage "\\usepackage{proof,rotating}")
(defvar p2ttree*dina4 "\\oddsidemargin 6pt\\evensidemargin 6pt\\marginparwidth 48pt\\marginparsep 10pt 
\\topmargin -18pt\\headheight 12pt\\headsep 25pt\\footskip 30pt 
\\textheight 625pt\\textwidth 431pt\\columnsep 10pt\\columnseprule 0pt")
(defvar p2ttree*documentbegin "\\begin{document}")
(defvar p2ttree*documentend "\\end{document}") 
(defvar p2ttree*envend "\\end{footnotesize}\\end{sideways}")
(defvar p2ttree*envbegin "\\begin{sideways}\\begin{footnotesize}")


;;; Additional Stuff...
(defvar p2ttree*subproof 0)
(defvar p2ttree*nesting-depth 35)
(defvar p2ttree*hyp-cite (make-hash-table))
(defvar p2ttree*hyp-count 0)


(defun p2ttree~write-nd-tree-to-tex-file (file &optional (proof omega*current-proof-plan))
  (declare (edited  "15-NOV-1999")
	   (authors Sorge)
	   (input   "A filename and optionally a proof or a proof node. Defaults to the current OMEGA PDS.")
	   (effect  "Writes a TeX tree representation of PROOF to FILE.")
	   (value   "Undefined."))
  (let* ((post2tex*typed-symbols nil)
	 (post2tex*left-bracket "(")
	 (post2tex*right-bracket ")")
	 (p2ttree*subproof 0)
	 (p2ttree*hyp-cite (clrhash p2ttree*hyp-cite))
	 (p2ttree*hyp-count 0)
	 (string (with-output-to-string (out)
					(p2ttree~proof-2-tree proof (post2tex~ho-infix-style) out))))
    (with-open-file (outstream file :direction :output :if-exists :supersede)
		    (write-string p2ttree*documentclass outstream)
		    (terpri outstream)
		    (terpri outstream)
		    (write-string p2ttree*usepackage outstream)
		    (terpri outstream)
		    (terpri outstream)
		    (write-string p2ttree*dina4 outstream)
		    (terpri outstream)
		    (terpri outstream)
		    (write-string "%% The predefined standard macros for logical symbols." outstream)
		    (terpri outstream)
		    (post2tex~print-predefined outstream)
		    (terpri outstream)
		    (terpri outstream)
		    (write-string "%% The macro-file for the ND-proof style" outstream)
		    (terpri outstream)
		    (write-string post2tex*include-macros outstream)
		    (terpri outstream)
		    (terpri outstream)
		    (write-string p2ttree*documentbegin outstream)
		    (terpri outstream)
		    (if post2tex*predefined-file
			(progn
			  (write-string "%% The macros defining type symbols." outstream)
			  (terpri outstream)
			  (post2tex~print-types outstream)
			  (terpri outstream)
			  (write-string "%% The predefined macro-file." outstream)
			  (terpri outstream)
			  (write-string post2tex*predefined-file outstream))
		      (let ((def-string
			      (with-output-to-string (outstring)
						     (post2tex~print-definition outstring (post2tex~ho-infix-style)))))
			(write-string "%% The macros defining type symbols." outstream)
			(terpri outstream)
			(post2tex~print-types outstream)
			(terpri outstream)
			(write-string "%% The macros defining additional signature in the proof." outstream)
			(terpri outstream)
			(write-string def-string outstream))) 
		    (terpri outstream)
		    ;;(post2tex~print-object proof (post2tex~ho-infix-style) outstream)
		    (write-string "%% The ND-proof tree." outstream)
		    (terpri outstream)
		    (write-string p2ttree*envbegin outstream)
		    (terpri outstream)
		    (write-string string outstream)
		    (terpri outstream)
		    (write-string p2ttree*envend outstream)
		    (terpri outstream)
		    (write-string p2ttree*documentend outstream)))
  (values))

(defgeneric p2ttree~proof-2-tree (proof style stream)
  (declare (edited  "15-NOV-1999")
	   (authors Sorge)
	   (input   "A filename and optionally a proof or a proof node. Defaults to the current OMEGA PDS.")
	   (effect  "Writes a TeX tree representation of PROOF to FILE.")
	   (value   "Undefined."))
  (:method ((proof prob+proof) style stream)
	   (p2ttree~proof-2-tree (prob~proof-root proof) style stream))
  (:method ((node pdsn+node) style stream)
	   (p2ttree=proof-2-tree node style stream 1)))

(defun p2ttree=proof-2-tree (node style stream nesting)
  (let* ((hyps (pdsn~hyps node))
	 (formula (node~formula node))
	 (just (node~justification node))
	 (premises (just~premises just))
	 (method (keim~name (just~method just)))
	 (post2tex*actual-string-counter 0)
	 (hyp-number-in (p2ttree=introduced-hyps hyps premises))
	 (hyp-number-out (gethash node p2ttree*hyp-cite)))
    (cond ((pdsn~open-node-p node)
	   (let ((supports (pds~node-supports node)))
	     (write-string "\\infer*" stream)
	     (terpri stream)
	     (write-string "   {" stream)
	     (post2tex~print-object formula style stream)
	     (write-string "}" stream)
	     (terpri stream)
	     (write-string "   {" stream)
	     (when supports
	       (p2ttree=proof-2-tree (car supports) style stream (1+ nesting))
	       (dolist (x (cdr supports))
		 (terpri stream)
		 (write-string "&" stream)
		 (terpri stream)
		 (p2ttree=proof-2-tree x style stream (1+ nesting))))
	     (write-string "}" stream)))
	  ((and (not premises) hyp-number-out)
	   (progn
	     (write-string "[" stream)
	     (post2tex~print-object formula style stream)
	     (write-string (format nil "]^{~A}" hyp-number-out) stream)))
	  (t (progn
	       (write-string "\\infer[" stream)
	       (if hyp-number-in
		   (write-string (format nil "~A^{~A}" (post2tex~rule-2-tex method) hyp-number-in) stream)
		 (write-string (post2tex~rule-2-tex method) stream))
	       (write-string "]" stream)
	       (terpri stream)
	       (write-string "   {" stream)
	       (post2tex~print-object formula style stream)
	       (write-string "}" stream)
	       (terpri stream)
	       (write-string "   {" stream)
	       (when premises 
		 (p2ttree=proof-2-tree (car premises) style stream (1+ nesting))
		 (dolist (x (cdr premises))
		   (terpri stream)
		   (write-string "&" stream)
		   (terpri stream)
		   (p2ttree=proof-2-tree x style stream (1+ nesting))))
	       (write-string "}" stream))))))
    
(defun p2ttree=introduced-hyps (hyps premises)
  (declare (edited  "15-NOV-1999")
	   (authors Sorge)
	   (input   "Two lists of nodes.")
	   (effect  "Can enter new hypothesis references into the reference-hashtable,"
		    "if the premises contain hypotheses which are not in the HYPS-list.")
	   (value   "If new hypotheses are entered a reference number for the"
		    "respective rule is returned, o/w NIL."))
  (when premises
    (let ((new-hyps (set-difference (tac~set-manage-list #'union (mapcar #'pdsn~hyps premises)) hyps)))
      (when new-hyps
	(incf p2ttree*hyp-count)
	(dolist (x new-hyps)
	  (setf (gethash x p2ttree*hyp-cite) p2ttree*hyp-count))
	p2ttree*hyp-count))))



