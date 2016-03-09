;;; -*- syntax: common-lisp; package: OMEGA; base: 10; mode: LISP -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1997 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     OMEGA Project                                                        ;;
;;     AG Siekmann/FB Informatik                                            ;;
;;     Universitaet des Saarlandes                                          ;;
;;     Bau 36, 2. Stock                                                     ;;
;;     D-66041 Saarbruecken                                                 ;;
;;     Germany                                                              ;;
;;   electronic mail: omega@ags.uni-sb.de                                    ;;
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

(mod~defmod TPS-COM
	    :uses ()
	    :documentation "TPS OMEGA Commands"
	    :exports (
		      ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Stuff:
;; Environment commands, concurrent calls,
;; blackbox inference functions: outline and expansion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tps~program ()
  (if (sys~getenv "TPSLISP")
      (if (sys~getenv "TPSPROGRAM")
	  (format nil "~A -I ~A"
		  (sys~getenv "TPSLISP")
		  (sys~getenv "TPSPROGRAM"))
	(warn "Environment variable TPSPROGRAM not set."))
    (or (sys~getenv "TPSPROGRAM")
	(warn "Environment variable TPSPROGRAM not set."))))

(defun tps~fonts ()
  (or (sys~getenv "TPSFONTS")
      (warn "Environment variable TPSFONTS not set.")))


(defun tps~generate-tps-problem-default! (node-to-prove tps-tactic tps-mode pds)
  (let ((tps-problem (p2t~subproblem node-to-prove pds)))
    (setf (atpprb~problem-atp-in-string tps-problem)
	  (tps=work-file-string-concurrent tps-tactic tps-mode))
    (keim~put node-to-prove 'atp-problems
	      (cons tps-problem (keim~get node-to-prove 'atp-problems)))))


;;; A variable keeping track of TPS problems

(defvar tps*problem-list nil)

;; Justification stuff for tps commands
;; -------------------------------------

;; bbox tps in globaler Stelle eingefuegt

;; The coresponding  function to determine the just.'s content
(defun tps=compute-justification-content (outline parameters)
  (let* ((new-just (pdsj~closed-just-create (infer~find-method 'tps)
					    (rest outline)
					    parameters
					    "unexpanded")))
    (setf (node~justification (first outline)) new-just))
  outline)


(defun tps=expand-tps-justified-node (outline parameters)
  (declare (ignore parameters))
  (setq *my-outline* outline)
  (t2p~translate-and-insert-tps-proof (car outline) omega*current-proof-plan)
  (setf (pdsj~status (pdsj~above (node~justification (car outline)))) "expanded")
  )
  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The main commands to call tps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun tps=call-tps (to-prove resource-in-sec mode allow-rulep strategy defs &optional (hyp-list nil))
  (declare (edited  "26-SEP-1997")
	   (authors gebhardChrisSorge)
	   (input  "The Line, which is to prove, the names and directorie"
		   "for the file to write, the override mode, and a resource"
		   "(integer) specifiying the maximum proving time time in seconds"
		   "and a strategy.")
	   (effect "Writes the tps-problem corresponding to to-prove"
		   "and starts tps in batch-mode on it." )
	   (value  ""))
  (let* ((strategy-sym (intern strategy))
	 (new-strategy (let ((found (member strategy-sym
					    (list NIL 'MS88 'MS89 'MS91-6 'MS92-9 'MS93-1
						  'MS90-3 'MS90-9 'MS91-7 'MS98-1 'MS03-7 'MS04-2))))
			 (if found
			     strategy-sym
			   (progn (omega~error "~%Mating-search strategy ~A not known. Using mode's strategy instead.~%" strategy)
				  nil))))
	 (supports (if hyp-list hyp-list
		     (pds~node-supports to-prove))))
					;    (mixin~save-and-set-input)
    (th~require-completely 'tps)
    (if (not (pdsn~open-node-p to-prove))
	(omega~error "Node ~A is not open." (keim~name to-prove))
      (if (not (serv~available "TPS"))
	  (omega~error "TPS service  not available.")
	(progn
	  (serv~enter "TPS" :mode "emacs")
	  (omega~message " Translating the subproblem to TPS syntax!")
	  (let*  ((tps-proof-obj (p2t~subproblem to-prove omega*current-proof-plan defs supports
						 ))
		  (prob-obj (atpprb~tps-problem-file tps-proof-obj))
		  (prob-name (second prob-obj))
		  (ping (format nil  "send(\"(PING \\\"~A\\\")\")" prob-name))
		  (pong (read-from-string (serv~apply  "TPS" ping :timeout 60)))
		  (new-sym (gensym "TPS"))
		  )
	    (if (string-equal (symbol-name (second pong))
			      "PONG")
		(let* ((tps-server-name (intern (third pong)))
		       (prob-message (format nil "send(~S timeout: ~A)"
					     (format nil "~S"
						     (list tps-server-name
							   (if allow-rulep
							       'DIY
							     'BASIC-DIY)
							   prob-name nil
							   prob-obj
							   (intern mode)
							   new-strategy
							   resource-in-sec))
					     resource-in-sec))
		       (tps-says (serv~apply  "TPS" prob-message :timeout
					      resource-in-sec))
		       (tps-says2 (if (stringp tps-says) (read-from-string tps-says nil
									   nil)))
		       )
		  (if (and (consp tps-says2)
			   (string-equal (car tps-says2) prob-name))
		      (let ((defsavedproof (read-from-string (cadr tps-says2)))
			    (pretty-proof-string (caddr tps-says2)))
			(case (tps=complete-tps-proof-p defsavedproof
							(node~formula to-prove))
			  (COMPLETE
			   (omega~message "~%PROOF FOUND BY TPS. Justifying the node!")
			   (infer~compute-outline (infer~find-method 'tps)
						  (cons to-prove supports)
						  (list new-sym))
			   (setf (pds~open-nodes omega*current-proof-plan)
				 (remove to-prove (pds~open-nodes omega*current-proof-plan)))
			   (tps=insert-tps-defsavedproof tps-proof-obj defsavedproof pretty-proof-string)
			   (setf tps*problem-list (acons new-sym tps-proof-obj
							 tps*problem-list))
			   :complete-proof)
			  (INCOMPLETE
			   (omega~message " TPS returned a partial proof still containing open nodes.")
			   (tps=insert-tps-defsavedproof tps-proof-obj defsavedproof pretty-proof-string)
			   (keim~put to-prove 'atp-problems
				     (cons tps-proof-obj (keim~get to-prove 'atp-problems)))
			   :partial-proof)
			  (COMPLETE-WRONG-ASSERTION
			   (omega~message " TPS returned a complete proof of the wrong assertion!"))
			  (INCOMPLETE-WRONG-ASSERTION
			   (omega~message " TPS returned an incomplete proof of the wrong assertion!"))
			  (t (omega~message "Did not understand the response from TPS"))))
		    (progn
		      (serv~apply "TPS" (format nil "send(~S)" ; in case it hasn't died yet
						(format nil "~S"
							(list tps-server-name
							      'KILL
							      prob-name))))
		      (omega~message " TPS failed to find a proof within the alloted time"))))
	      (omega~message "TPS failed to respond"))))))))

(defun tps=call-tps-bool-prop-mode (to-prove resource-in-sec allow-rulep defs)
  (tps=call-tps to-prove resource-in-sec "BOOL-PROP-MODE" allow-rulep nil
		defs))

(defun tps=call-tps-bool-prop-mode2 (to-prove resource-in-sec allow-rulep defs)
  (tps=call-tps to-prove resource-in-sec "BOOL-PROP-MODE2" allow-rulep nil
		defs))

(defun tps=call-tps-gazing-mode (to-prove resource-in-sec allow-rulep defs)
  (tps=call-tps to-prove resource-in-sec "GAZING-MODE" allow-rulep nil
		defs))

(defun tps=call-tps-gazing-mode2 (to-prove resource-in-sec allow-rulep defs)
  (tps=call-tps to-prove resource-in-sec "GAZING-MODE2" allow-rulep nil
		defs))

(defun tps=call-tps-basic-ms04-2-mode (to-prove resource-in-sec allow-rulep defs)
  (tps=call-tps to-prove resource-in-sec "BASIC-MS04-2-MODE" allow-rulep nil
		defs))

(defun tps=call-tps-ms98-fo-mode (to-prove resource-in-sec allow-rulep defs)
  (tps=call-tps to-prove resource-in-sec "MS98-FO-MODE" allow-rulep nil
		defs))

(defun tps=call-tps-ms98-ho-mode (to-prove resource-in-sec allow-rulep defs)
  (tps=call-tps to-prove resource-in-sec "MS98-HO-MODE" allow-rulep nil
		defs))

(defun tps=call-tps-ms98-ho-mode2 (to-prove resource-in-sec allow-rulep defs)
  (tps=call-tps to-prove resource-in-sec "MS98-HO-MODE2" allow-rulep nil
		defs))

(defun tps=call-tps-ms98-ho-primsubs (to-prove resource-in-sec allow-rulep defs)
  (tps=call-tps to-prove resource-in-sec "MS98-HO-PRIMSUBS2" allow-rulep nil
		defs))

(defun tps=call-tps-interactive (to-prove t-name t-dir supersede mode strategy defs &optional (hyp-list nil))
  (declare (edited  "24-JULI-1998")
	   (authors ChrisSorge)
	   (input  "The Line, which is to prove, the names and directorie"
		   "for the file to write, the override mode,"
		   "a strategy, a mode, and a list of definitions.")
	   (effect "Writes the tps-problem corresponding to to-prove"
		   "and starts tps in interactive mode on it." )
	   (value  ""))
  (let* ((tps-dir (merge-pathnames (format nil "~A/" (string-downcase t-name))
				   t-dir))
	 (current-dir  (sys~current-directory))
	 (work-filename "tps.work")
	 (prob-filename "tps.problem")
	 (prt-filename  "tps.prt")
	 (proof-filename "tps.prf")
	 (work-file    (merge-pathnames work-filename     tps-dir))
	 (prob-file    (merge-pathnames prob-filename     tps-dir))
	 (prt-file     (merge-pathnames prt-filename      tps-dir))
	 (proof-file   (merge-pathnames proof-filename    tps-dir))
	 (new-sym      (gensym "tps"))
	 (strategy-sym (intern strategy))
	 (new-strategy (if (member strategy-sym
				   (list NIL 'MS88 'MS89 'MS91-6 'MS92-9 'MS93-1
					 'MS90-3 'MS90-9 'MS91-7 'MS98-1 'MS03-7 'MS04-2))
			   strategy-sym
			 (progn (omega~error "~%Mating-search strategy ~A not known.
Using mode's strategy instead.~%" strategy-sym)
				nil)))
	 (supports (if hyp-list hyp-list
		     (pds~node-supports to-prove))))
;    (mixin~save-and-set-input)
    (if (not (pdsn~open-node-p to-prove))
	(omega~error "Node ~A is not open." (keim~name to-prove))
      (if (not (tps~program))
	  (omega~error "TPS executable ~A not found. Please check your .omegarc file." (tps~program))
	(progn
	  (th~require-completely 'tps)
	  (let* ((exists-dir  (probe-file tps-dir))
		 (exists-files (or (probe-file work-file)
				   (probe-file prob-file)
				   (probe-file prt-file)
				   (probe-file proof-file)))
		 (allowed (cond ((and exists-files supersede)
				 (sys~call-system (format nil "\\rm ~A/*" tps-dir))
				 t)
				((and exists-files (not supersede)) nil)
				((not exists-dir)
				 (sys~call-system (format nil "mkdir ~A"
							  (string-right-trim '(#\/) (namestring tps-dir))))
				 t)
				(t t))))
	    (when allowed
	      (omega~message " Translating the subproblem to TPS syntax!")
	      (let*  ((tps-proof-obj (p2t~subproblem to-prove omega*current-proof-plan defs supports)))
		(omega~message " Writing the TPS work file tps.work.")
		(tps=write-work-file-interactive tps-proof-obj work-file prob-file mode new-strategy)
		(omega~message " Writing the TPS problem file tps.problem.")
		(tps=write-problem-file tps-proof-obj prob-file)
		(sys~chdir tps-dir)
		(tps=call-interactive tps-dir work-filename proof-filename)
		(sys~chdir current-dir)
		(cond ((not (probe-file proof-file))
		       (omega~message " TPS has failed to find a proof."))
		      ((tps=failed-to-proof-p proof-file)
		       (omega~message " TPS returned a partial proof still containing open nodes.")
		       (tps=insert-tps-results tps-proof-obj prt-file proof-file)
		       (omega~message "~%~%The partial TPS proof: ~%~% ~A~%~%"
				      (or (atpprb~tps-formatted-proof tps-proof-obj)
					  (atpprb~tps-proof-string tps-proof-obj)))
		       (keim~put to-prove 'atp-problems
				 (cons tps-proof-obj (keim~get to-prove 'atp-problems)))
		       (let ((query (omega~query
                                       "Insert partial proof ? [y/n] "
                                       (arg~find-argtype 'boolean))))
;                              (progn
;                                      (mixin~save-and-set-input)
;                                      (inter~prompt-for-input
;                                       (comint~interface comint*current-comint)
;                                       "Insert partial proof ? [y/n] "
;                                       (arg~find-argtype 'boolean)))))
;                         (mixin~reset-input)
			 (when query
			   (setf (pds~open-nodes omega*current-proof-plan)
				 (remove to-prove (pds~open-nodes
						   omega*current-proof-plan)))
			   (t2p~translate-and-insert-partial-tps-proof to-prove tps-proof-obj))))
		      (t (omega~message "~%PROOF FOUND BY TPS. Justifying the node!")
			 (infer~compute-outline (infer~find-method 'tps)
						(cons to-prove supports)
						(list new-sym))
			 (setf (pds~open-nodes omega*current-proof-plan)
			       (remove to-prove (pds~open-nodes omega*current-proof-plan)))
			 (tps=insert-tps-results tps-proof-obj prt-file proof-file)
			 (setf tps*problem-list (acons new-sym tps-proof-obj tps*problem-list))))))))))
;    (mixin~reset-input)
    ))

(defun tps=insert-tps-results (tps-proof-obj prt-file proof-file)
  (declare (edited  "22-JUL-1998")
	   (authors SorgeChris)
	   (input   "A object of type ATPPRB+TPS-PROBLEM and two file names.")
	   (effect  "Inserts the contents of the TPS-proof files into the object.")
	   (value   "The modified object."))
  (let ((prt (when (probe-file prt-file)
	       (with-open-file (prt-in prt-file
				       :direction :input)
			       (do* ((line (tps=format-prt-line (read-line prt-in nil))
					   (tps=format-prt-line (read-line prt-in nil)))
				     (prt-string (when line line) (if line
								      (concatenate 'string prt-string line)
								    prt-string)))
				   ((null line) prt-string))))) 
	(proof (when (probe-file proof-file)
		 (with-open-file (proof-in proof-file
					   :direction :input)
				 (read proof-in)))))
    (setf (atpprb~tps-formatted-proof tps-proof-obj) prt)
    (setf (atpprb~tps-proof-string tps-proof-obj) proof)))

; a version of insert-tps-results that does not use files
(defun tps=insert-tps-defsavedproof (tps-proof-obj defsavedproof pretty-proof-string)
  (declare (edited  "2-AUG-2001")
	   (authors Chad)
	   (input   "A object of type ATPPRB+TPS-PROBLEM, a defsavedproof S-expression,
and a string with pretty print of the proof")
	   (effect  "Inserts the defsavedproof into the object.")
	   (value   "The modified object."))
  (let* ((prt-in (make-string-input-stream pretty-proof-string))
	 (prt (do* ((line (tps=format-prt-line (read-line prt-in nil))
			  (tps=format-prt-line (read-line prt-in nil)))
		    (prt-string (when line line) (if line
						     (concatenate 'string prt-string line)
						   prt-string)))
		  ((null line) (close prt-in) prt-string))))
    (setf (atpprb~tps-proof-string tps-proof-obj) defsavedproof)
    (setf (atpprb~tps-formatted-proof tps-proof-obj) prt)
    tps-proof-obj))

(defun tps=set-tps-results (tps-proof-obj)
  (declare (edited  "05-AUG-1998")
	   (authors Chris)
	   (input   "A object of type ATPPRB+TPS-PROBLEM and two file names.")
	   (effect  "Reads the tps-out-string-iles into the object.")
	   (value   "The modified object."))
  )

(defun tps=format-prt-line (line)
  (declare (edited  "05-AUG-1998")
	   (authors Chris)
	   (input   "A proofline generated by TPS (i.e. a string).")
	   (effect  "None.")
	   (value   "Replaces ~ by ~~ and adds ~% to the string."))
  (when line
    (if (string= "" line) ""
      (let ((num (- (length line) 1)))
	(do* ((count 0 (incf count))
	      (el (char line count)
		  (char line count))
	      (el-new (if (char= #\~ el) "~~" (string el))
		      (if (char= #\~ el) "~~" (string el)))
	      (erg el-new
		   (concatenate 'string erg el-new)))
	    ((>= count num) (concatenate 'string erg "~%")))))))


  
(defgeneric tps=write-problem-file (problem file)
  (declare (edited  "21-JUL-1998 18:35")
	   (authors SORGE)
	   (input   "A TPS-problem and filename.")
	   (effect  "Writes information from the TPS-problem to the given file.")
	   (value   "Undefined."))
  (:method (problem file)
	   (format t "Something went wrong with given arguments ~A and ~A" problem file))
  (:method ((problem list) file)
	   (with-open-file (out file
				:direction :output 
				:if-does-not-exist :create
				:if-exists :error)
			   (format out "~S" problem)))
  (:method ((problem atpprb+tps-problem) file)
	   (tps=write-problem-file (atpprb~tps-problem-file problem) file)))
			   
; obsolete - cebrown 2/8/01
;(defun tps=write-work-file (proof-obj work-file problem-file prt-file proof-file mode strategy)
;  (declare (edited  "21-JUL-1998 17:54")
;	   (authors SORGE Chris)
;	   (input   "A proof object, a work-file name and a problem-file name.")
;	   (effect  "Writes the TPS work file using the problem-file information.")
;	   (value   "Undefined."))
;  (let ((work-file-string (tps=write-work-file-string problem-file prt-file proof-file mode
;						      strategy)))
;    (with-open-file (out work-file
;			 :direction :output 
;			 :if-does-not-exist :create
;			 :if-exists :error)
;		    (format out "~A" work-file-string))
;    (setf (atpprb~problem-atp-in-string proof-obj) work-file-string)))

  
; obsolete - cebrown 2/8/01
;(defun tps=write-work-file-string (problem-file prt-file proof-file mode strategy)
;  (declare (edited  "21-JUL-1998 17:54")
;	   (authors SORGE Chris)
;	   (input   "A couple of things")
;	   (effect  "None")
;	   (value   "The work file string for TPS."))
;  (multiple-value-bind (sec min hour day month year)
;      (decode-universal-time (get-universal-time))
;    (concatenate 'string
;		 (format nil ";;File produced by OMEGA at ~A:~A:~A on ~A/~A/~A.~%"
;			 hour min sec month day year)
;		 (format nil "(restoreproof \"~A\")" problem-file)
;		 (format nil "~%(set-flag 'default-ms '~A)" strategy)
;		 (when (and mode
;			    (not (string= mode "NIL"))
;			    (tps=valid-mode mode))
;		   (format nil "~%(when (core::retrieve-libobject '~A :type nil
;:multiple nil :fail-gently nil) (mode '~A))~%" mode mode))
;		 (format nil "~%(set-flag 'USE-RULEP NIL)~%") ; cebrown 20/7/01
;		 (format nil "~%(set-flag 'REMOVE-LEIBNIZ NIL)~%") ; cebrown 23/7/01
;		 (format nil "~%(diy (caar (get dproof 'plans)) (cdar (get dproof 'plans)) nil)~%")
;		 (format nil "~%(cleanup-for-omega)~%~%") ; defined in the tps3.ini file for omega - cebrown 19/7/01
;		 (format nil "~%(printproof \"~A\")~%(set-flag 'allscopeflag t)~%
;(set-flag 'printtypes t)~%(set-flag 'printtypes-all t)~%(set-flag 'infix-notation nil)~%
;(saveproof \"~A\")~%"
;			 prt-file proof-file)
;                 (format nil "(setf (proof-lines dproof) nil)~%")
;; to clear this proof in case we want to prove the same theorem again (ow, restoreproof complains)
;)))

(defun tps=write-work-file-interactive (proof-obj work-file problem-file mode strategy)
  (declare (edited  "21-JUL-1998 17:54")
	   (authors ChrisSorge)
	   (input   "A TPS proof object, A work-file name, a problem-file name, two more filenames for"
		    "the ascii-proof and the proof in tps-defsavedproof format,"
		    "furthermore a mode and a strategy.")
	   (effect  "Writes the TPS work file using the problem-file information.")
	   (value   "Undefined."))
  (let ((work-file-string (tps=write-work-file-string-interactive
			   work-file problem-file mode strategy)))
    (with-open-file (out work-file
			 :direction :output 
			 :if-does-not-exist :create
			 :if-exists :error)
		    (format out "~A" work-file-string))
    (setf (atpprb~problem-atp-in-string proof-obj) work-file-string)))


(defun tps=write-work-file-string-interactive (work-file problem-file mode strategy)
   (declare (edited  "21-JUL-1998 17:54")
	   (authors SORGE Chris)
	   (input   "A couple of things")
	   (effect  "None")
	   (value   "The interactive work file string for TPS."))
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (get-universal-time))
    (concatenate 'string
		 (format nil ";;File produced by OMEGA at ~A:~A:~A on ~A/~A/~A.~%"
			    hour min sec month day year)
		    (format nil "restoreproof \"~A\"" problem-file)
		    (unless (tps~fonts) (format nil "~%style generic"))
		    (when (and mode
			       (not (string= mode "NIL"))
			       (tps=valid-mode mode))
		      (format nil "~%mode ~A~%~%~%" mode))
		    (when strategy
		      (format nil "~%default-ms ~A" strategy))
		    (format nil "~%pall"))))


(defun tps=work-file-string-concurrent (tps-tactic tps-mode)
   (declare (edited  "21-JUL-1998 17:54")
	   (authors Chris)
	   (input   "A tactic and a mode for TPS (strings or symbols)")
	   (effect  "None")
	   (value   "The (partial) concurrent work file string for TPS."))
    (concatenate 'string
		    (format nil "~%default-ms ~A~%" tps-tactic)
		    (when (and tps-mode
			       (not (string= tps-mode "NIL"))
                               (tps=valid-mode tps-mode))
		      (format nil "~%mode ~A~%~%~%" tps-mode))
		    (format nil "diy~%~%~%~%~%")
		    ))

(defun tps=valid-mode (mode)
  (declare (ignore mode))
  t)  ;;; hier soll spaeter eine sinnvolle Abfrage hin



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Call with only certain support lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun tps=call-tps-interactive-with-supports (node supports tpsname dir-name supersede mode strategy defs)

  (if (subsetp supports (pds~node-supports node))
      (tps=call-tps-interactive node tpsname dir-name supersede mode strategy defs supports)
    (omega~error "Please check the support nodes.")))

(defun tps=call-tps-with-supports (node supports resource
					mode allow-rulep strategy defs)

  (if (subsetp supports (pds~node-supports node))
      (tps=call-tps node resource mode allow-rulep strategy defs supports)
    (omega~error "Please check the support nodes.")))

(defun tps=ctws-defaults (node supports resource
			       mode allow-rulep strategy defs)
  (cond ((not (com~specified-arg-p node))
	 (list (oc~default-current-planline) 
	       (com~unspecified) (com~unspecified) (com~unspecified) 
	       (com~unspecified) (com~unspecified) (com~unspecified)))
	((not (com~specified-arg-p supports))
	 (list node (pds~node-supports node) 
	       (com~unspecified) (com~unspecified) (com~unspecified) 
	       (com~unspecified) (com~unspecified)))
	((not (com~specified-arg-p resource))
	 (list node supports 60 (com~unspecified) 
	       (com~unspecified) (com~unspecified)(com~unspecified)))
	((not (com~specified-arg-p mode))
	 (list node supports resource "BASIC-MS04-2-MODE" 
	       (com~unspecified) (com~unspecified) (com~unspecified)))
	((not (com~specified-arg-p allow-rulep))
	 (list node supports resource mode t
	       (com~unspecified) (com~unspecified)))
	((not (com~specified-arg-p strategy))
	 (list node supports resource mode allow-rulep nil (com~unspecified)))
	((not (com~specified-arg-p defs))
	 (list node supports resource mode allow-rulep strategy nil))
	(t (list node supports resource mode allow-rulep strategy defs))))

(defun tps=ctiws-defaults (node supports tpsname dir-name supersede
			       mode strategy defs)
  (cond ((not (com~specified-arg-p node))
	 (list (oc~default-current-planline) (com~unspecified) (com~unspecified)
	       (com~unspecified) (com~unspecified) (com~unspecified)
	       (com~unspecified) (com~unspecified)))
	((not (com~specified-arg-p supports))
	 (list node (pds~node-supports node) (com~unspecified)
	       (com~unspecified) (com~unspecified) (com~unspecified)
	       (com~unspecified) (com~unspecified)))
	((not (com~specified-arg-p tpsname))
	 (list node supports 
	       (format nil "tps-problems-~A" (sys~getenv "USER"))(com~unspecified)
	       (com~unspecified) (com~unspecified) (com~unspecified) (com~unspecified)))
	((not (com~specified-arg-p dir-name))
	 (list node supports  tpsname "/tmp/" 
	       (com~unspecified) (com~unspecified) (com~unspecified)(com~unspecified)))
	((not (com~specified-arg-p supersede))
	 (list node supports  tpsname dir-name t (com~unspecified) (com~unspecified)(com~unspecified)))
	((not (com~specified-arg-p mode))
	 (list node supports  tpsname dir-name supersede "BASIC-MS04-2-MODE" (com~unspecified)(com~unspecified)))
	((not (com~specified-arg-p strategy))
	 (list node supports  tpsname dir-name supersede mode nil (com~unspecified)))
	((not (com~specified-arg-p defs))
	 (list node supports  tpsname dir-name supersede mode strategy nil))
	(t (list node supports  tpsname dir-name supersede mode strategy defs))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Resource management for the TPS calls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun tps=call-interactive (tps-dir tps-start-file tps-out-name)
  (declare (edited  "29-JAN-1998")
	   (authors ChrisSorge)
	   (input   "A directory and two filenames (for tps-input and tps-output)")
	   (effect  "Starts an external tps3 process interactively"
		    "and waits for an tps-proof file.")
	   (value   "Undefined"))
  (let* ((call-string (if (tps~fonts)
			  (format nil "cd ~A; xset fp+ ~A; xterm -n \"TPS3 from OMEGA\" -fn vtsingle -fb vtsymbold -exec ~A -- -omega -batch ~A -outfile ~A"
				  tps-dir
				  (tps~fonts)
				  (tps~program)
				  tps-start-file
				  tps-out-name)
			 (format nil "xterm -n \"TPS3 from OMEGA\" -exec ~A -- -omega -batch ~A -outfile ~A"
				 (tps~program)
				 tps-start-file
				 tps-out-name))))
    (format t call-string)
    (excl::run-shell-command call-string
			     :wait t :output nil)))

#| Ehm, the following tries to call TPS during compilation of Omega, commented it out. MP

; to start TPS when Omega starts - is this the best way to do this? - cebrown 25/7/01
(excl::run-shell-command (format nil "cd ~A; ~A -- -omegacom ~A > /dev/null &"
				 (atptop~get-default-directory)
				 (tps~program)
				 (merge-pathnames (string-downcase (format nil "omega-tps-~A.com"
									   (sys~getenv "USER")))
						  (atptop~get-default-directory))))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Resource management for the TPS calls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(defun tps=get-pid-from-stream (streami)
;  (declare (edited  "26-MAY-1998")
;	   (authors Ameier)
;	   (input   "A stream (output-stream from calling atp).")
;	   (effect  "Read from the stream.")
;	   (value   "The pid of the atp-process."))
;  (let* ((line (read-line streami nil '+-*/))
;	 (pid nil))
;    
;    (loop while (not (equal line '+-*/))
;	  do
;	  (setq pid (tps=extract-pid-from-string line))
;	  (if pid
;	      (setq line '+-*/)
;	    (setq line (read-line streami nil '+-*/))))
;    (unless pid (omega~error "Konnte PID nicht finden"))
;    
;    (omega~trace "~%~% THE PID: ~A" pid)
;    pid))
;
;(defun tps=char-to-int (c)
;  (case c
;    (#\0 0)
;    (#\1 1)
;    (#\2 2)
;    (#\3 3)
;    (#\4 4)
;    (#\5 5)
;    (#\6 6)
;    (#\7 7)
;    (#\8 8)
;    (#\9 9)))
;
;(defun tps=string-to-int (str)
;  (reduce #'(lambda (a b) (+ (* 10 a) b))
;	  (map 'list #'tps=char-to-int str)))
;
;(defun tps=extract-pid-from-string (str)
;  (let ((pos (position #\] str :test #'char=))
;	; (string-stream (make-string-output-stream))
;	)
;    (if pos
;	(tps=string-to-int (subseq str (+ 2 pos)))))
;  )
;
;
;(defun tps=back-counter (killing-time tps-out-name &optional (start-time nil))
;  (declare (edited  "29-JAN-1998")
;	   (authors ChrisSorge)
;	   (input   "A killing (universal) time and a tps-output location."
;		    "Optionally a start time given in the first call.")
;	   (effect  "Waits for killing time and then kills running tps processes. If a"
;		    "proof-file gets written before killing-time is reached, then nothing"
;		    "happens")
;	   (value   "Undefined"))
;  (if (probe-file tps-out-name)
;      (with-open-file (test tps-out-name)
;		      (do* ((old-length 0 length)
;			    (length (file-length test) (file-length test)))
;			  ((= old-length length) t)
;			(sleep 1)))
;    (cond  ((and (> killing-time (get-universal-time))
;		 (> (- killing-time (get-universal-time)) 0))
;	    (omega~trace " ~Asec (total run-time ~Asec)"
;			 (- killing-time (get-universal-time))
;			 (if start-time (- (get-universal-time) start-time)
;			   '?))
;	    (sleep (cond ((> (- killing-time (get-universal-time)) 100) 20)
;			 ((> (- killing-time (get-universal-time)) 10) 5)
;			 (t 1)))
;	    (tps=back-counter killing-time tps-out-name start-time))
;	   (t (omega~message "~% No Time Resources left. ")
;	      (let ((query (progn
;			     (mixin~save-and-set-input)
;			     (omega~query "Quit TPS process [y] or spent a new Time Resource [n]? " (arg~find-argtype 'boolean) t))))
;		(mixin~reset-input)
;		(if query nil
;		  (let ((new-resource
;			      (omega~query " New Time Resource in seconds: "
;                                                                (arg~find-argtype
;                                                                 'integer) 60)))
;;		  (progn
;;                      (mixin~save-and-set-input)
;;                                        (inter~prompt-for-input  (comint~interface comint*current-comint)
;;                                                                 " New Time Resource in seconds: "
;;                                                                (arg~find-argtype
;;                                                                 'integer)))))
;;		    (mixin~reset-input)
;		    (tps=back-counter (+ new-resource (get-universal-time))
;				      tps-out-name
;				      start-time))))))))
;
;(defun tps=kill (pid)
;    (declare (edited  "29-JAN-1998")
;	   (authors Chris)
;	   (input   "A pid of a TPS process")
;	   (effect  "Kills this process and possibly some other running tps3 processe.")
;	   (value   "Undefined"))
;  (omega~message "~% Killing TPS Process with pid ~A. " pid)
;  (sys~call-system (format nil "kill -9 ~A" pid))
;  (multiple-value-bind
;	(stream-a dummy-a dummy-b) 
;	(excl::run-shell-command (format nil "pidof ~A" (pathname-name (pathname (tps~program))))
;				 :wait nil :output :stream)
;    (declare (ignore dummy-a))
;    (declare (ignore dummy-b))
;    (let ((other-pid (read stream-a nil nil)))
;      (do ()
;	  ((null other-pid) nil)
;	(when  (omega~query
;		(format nil "~% Another TPS Process (~A) found. Delete it (y/n) ?" other-pid)
;		(arg~find-argtype 'boolean) t)
;;            (progn
;;                (mixin~save-and-set-input)
;;                (inter~prompt-for-input
;;                 (comint~interface comint*current-comint)
;;                 (format nil "~% Another TPS Process (~A) found. Delete it (y/n) [n]?" other-pid)
;;                 (arg~find-argtype 'boolean)))
;;	  (mixin~reset-input)
;	  (omega~message "~% Killing TPS Process with pid ~A." other-pid)
;	  (sys~call-system (format nil "kill -9 ~A" other-pid)))
;	(setf other-pid (read stream-a nil nil))))))


(defun tps=failed-to-proof-p (tps-proof-file)
  (declare (edited  "16-OCT-1997")
	   (authors Gebhard)
	   (input   "A filename of a tps-proof file.")
	   (effect  "None")
	   (value   "True if the file doesn't contain a tps-proof, TPS couldn't find a proof."))
  (if (probe-file tps-proof-file)
      (with-open-file (pfile tps-proof-file
			     :direction :input)
		      (tps=search-failure (read pfile nil)))
    (progn
      (omega~message "~%No TPS proof-file created. ")
      t)))
	

(defun tps=search-failure (contentlist)
  (declare (edited  "16-OCT-1997")
	   (authors GebhardChris)
	   (input   "TPS-proof in list format.")
	   (effect  "None")
	   (value   "True if the symbol PLAN1 occurs somewhere in the proof."))
  (cond ((not (listp (first contentlist)))
	 (tps=search-failure (rest contentlist)))
	((eq 'lines (first (first contentlist)))
	 (find-if #'(lambda (x) (tps=planned-line x))
		  (rest (first contentlist))))
	(t (tps=search-failure (rest contentlist)))))

(defun tps=complete-tps-proof-p (defsavedproof assertion)
  (and (consp defsavedproof)
       (eq (car defsavedproof) 'defsavedproof)
       (let ((l (find-if #'(lambda (y)
			     (and (consp y)
				  (eq (car y) 'lines)))
			 (cdr defsavedproof)))
	     (a (find-if #'(lambda (y)
			     (and (consp y)
				  (eq (car y) 'assertion)))
			 (cdr defsavedproof)))
	     (complete nil)
	     (same-assertions nil))
	 (when l
	   (setq complete
		 (not (find-if #'(lambda (x)
				   (tps=planned-line x))
			       (cdr l)))))
	 (when a
	   (setq same-assertions
		 T)) ; for now, just returning T because we need an environment to call t2p~translate-formula
;		 (term~alpha-equal
;		  assertion
;		  (t2p~translate-formula (cadr a)))))
	 (if complete
	     (if same-assertions
		 'COMPLETE
	       'COMPLETE-WRONG-ASSERTION)
	   (if same-assertions
	       'INCOMPLETE
	     'INCOMPLETE-WRONG-ASSERTION)))))

(defun tps=planned-line (line)
  (declare (authors GebhardChris)
	   (input   "TPS-proof-line in list format.")
	   (effect  "None")
	   (value   "True if the justification starts with symbol PLAN ."))
  (let* ((hrule (fourth line))
	 (rule (etypecase hrule
		 (string (string-upcase hrule))
		 (symbol (symbol-name hrule)))))
    (and (string-equal (aref rule 0) #\P)
	 (string-equal (aref rule 1) #\L)
         (string-equal (aref rule 2) #\A)
         (string-equal (aref rule 3) #\N)
	 )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Displaying proof output of TPS in OMEGA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun tps=last-tps-justified-line (&optional (pds omega*current-proof-plan))
  (find-if #'(lambda (node) (eq (just~method (node~justification node))
				(infer~find-method 'tps)))
	   (pds~support-nodes pds)))
	
			


(defun tps=show-tps-proof (tps-node)
  (declare (edited  "09-OCT-1997")
	   (authors Chris)
	   (input   "A node (normally justified by TPS).")
	   (effect  "None")
	   (value   "Unimportant."))
  (if (eq (pdsn~just-method tps-node) (infer~find-method 'tps))
      (let ((proof (atpprb~tps-formatted-proof
		    (cdr (assoc (car (pdsj~parameters (node~justification tps-node)))
				tps*problem-list)))))
	(when proof
	  (omega~message proof)))
    (omega~error "Node not justified by a TPS proof!")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TPS in the concurrent proving mechanism
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun tps~complete-tps-problem! (tps-problem)
  )
  

