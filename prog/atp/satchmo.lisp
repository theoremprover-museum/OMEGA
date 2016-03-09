;;; -*- syntax: common-lisp; package: OMEGA; base: 10; mode: LISP -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     PROVERB Project                                                      ;;
;;     AG Siekmann/FB Informatik                                            ;;
;;     Universitaet des Saarlandes                                          ;;
;;     Bau 36, 4. Stock                                                     ;;
;;     D-66041 Saarbruecken                                                 ;;
;;     Germany                                                              ;;
;;   electronic mail: proverb@cs.uni-sb.de                                  ;;
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



(mod~defmod SATCH 
            :uses (atpprb atptop hocnf just keim node omega otter p2f res sys)
            :documentation "Calling Satchmo in Omega."
            :exports (
                      
                      satch~add-in-string!
                      satch~call-satchmo
                      satch~complete-satchmo-problem!
                      satch~generate-satchmo-problem
                      satch~program
                      ))

;;; The following functions are internal in other modules and should not be used:
;;; (otter=add-string-to-in-string otter=compute-convert-list otter=print-clause otter=set-current-type-var-subst!)



(defun satch~program ()
  (let* ((prog (sys~getenv "SATCHMOHOME")))

    (when (or (null prog) (null (probe-file prog)))
      (error "There is no SATCHMO-executable at ~A, please check your path to the SATCHMO-executable." prog))
    
    prog))

(defun satch=print (clauses)
  (declare (edited  "23-JUL-1998")
	   (authors Ameier)
	   (input   "A set of clauses.")
	   (effect  "None.")
	   (value   "Creates the usable-list in otter*in-string."))

  (setq otter*in-string "")

  (mapc #'(lambda (clause)
	    (otter=print-clause clause)
	    (otter=add-string-to-in-string #\Newline))
	clauses))
  
(defun satch~add-in-string! (satchmo-problem clauses)
  (declare (edited  "23-JUL-1998")
	   (authors Ameier)
	   (input   "The satchmo-problem and the clauses.")
	   (effect  "The otter*in-string is produced and added to the satchmo-problem.")
	   (value   "Undefined."))

  ;; konstruiert das satchmo.in file im otter*in-string
  (satch=print clauses)

  ;; setzt atp-in-string im satchmo-problem
  (setf (atpprb~problem-atp-in-string satchmo-problem) otter*in-string)
  )

(defun satch~generate-satchmo-problem (conclusion-node assumption-nodes ho-pds)
  (declare (edited  "23-JUL-1998")
	   (authors Ameier)
	   (input   "A conclusion node, a set of assumption nodes and the pds the nodes are from.")
	   (effect  "None.")
	   (value   "A atp-problem of type satchmo, with a partial resolution proof"
		    "Since at the moment we have no parsing back of satchmo outputs we"
		    "need no storage of global vars and."
		    "Remark: atp-in-sting is not set !!"))

  ;; da die ganzen Sachen ueber das Otter Zeug laufen !
  (setq otter*convert-counter 0)       ;; setzt counter fuer neue Namen auf 0
  (setq otter*temporary-variables nil) ;; setzt temporaere Variablen auf nil
  (setq otter*just-counter 0)          ;; setzt justification counter fuer neue justification auf 0

  (let* ((res-proof (atptop~resolution-proof-create-from-nodes conclusion-node assumption-nodes ho-pds)))
    
    (setq otter*current-problem res-proof)
    (setq otter*current-environment (res~proof-environment res-proof))
        
    ;; translate the initial resolution proof res-proof to f.o. and normalize it
    (p2f~translate res-proof)
    
    (omega~message "~%Normalizing ...")
    (hocnf~normalize-res-proof! res-proof)
    
    ;; Compute the convertion of names
    (otter=compute-convert-list (res~proof-clauses res-proof))

    (atpprb~create-fo-problem (gensym "satchmo-problem-")
			      'satchmo
			      nil
			      nil
			      res-proof
			      nil
			      nil)))

    
(defun satch~call-satchmo (open-node ho-pds dir ressource)
  (declare (edited  "23-JUL-1998")
	   (authors Ameier)
	   (input   "An open node and the according pds, that contains this node. A directory where"
		    "the new files should stand and the time ressource.")
	   (effect  "None.")
	   (value   "If satchmo finds a model the message 'model found' will is printed and nil is returned"
		    ", if satchmo finds no model the message 'no model found' is printed and t is returned."))

  (let* ((problem-name (keim~name ho-pds))
	 (satchmo-problem-dir (merge-pathnames (format nil "~A/" (string-downcase problem-name)) dir))
	 (in-file (merge-pathnames "satchmo.in" satchmo-problem-dir))
	 (out-file (merge-pathnames "satchmo.out" satchmo-problem-dir))
	 (satchmo-problem (satch~generate-satchmo-problem open-node
							  (remove open-node (pds~node-supports open-node))
							  ho-pds))
	 (res-proof (atpprb~problem-part-res-proof satchmo-problem)))
    ;; chris-test: (format t "~% delta-rel: ~A" (setq for*chris (res~proof-delta-relation res-proof)))
    ;; erzeugt directory falls es nicht existiert
    (unless (probe-file satchmo-problem-dir)
      (sys~call-system (format nil "mkdir ~A" (string-right-trim '(#\/) (namestring satchmo-problem-dir)))))

    ;; erzeuge stachmo.in file in otter*in-string, fuege otter*in-string zum satchmo-problem hinzu 
    (satch~add-in-string! satchmo-problem
			  (res~proof-clauses res-proof))

    ;; call-satchmo vot Ort -> schreibt satchmo.out file in den out-string des satchmo-problems
    (satch=call-satchmo! satchmo-problem satchmo-problem-dir ressource)
    
    ;; parsen des satchmo-beweises
    (satch~complete-satchmo-problem! satchmo-problem)))

(defun satch=call-satchmo! (satchmo-problem satchmo-problem-dir ressource)
  (declare (edited  "26-MAR-1998")
	   (authors Ameier)
	   (input   "A satchmo-problem, the directory for input, output and a time-ressource.")
	   (effect  "Writes the in-string of the satchmo-problem to the file satchmo.in in the"
		    "directory, calls satchmo on it, reads the file satchmo.out from the directory"
		    "and writes it into the out-string of the satchmo-problem.")
	   (value   "Undefined."))

  (let* ((in-file (merge-pathnames "satchmo.in" satchmo-problem-dir))
	 (temp-out-file (merge-pathnames "tmp.out" satchmo-problem-dir))
	 (out-file (merge-pathnames "satchmo.out" satchmo-problem-dir)))

    (atptop~print-string-in-file (atpprb~problem-atp-in-string satchmo-problem) in-file)
    
    (let* ((call-flag (atptop~call-with-time-ressource (format nil "~A +RTS -H32M -K4M -RTS --complement-splitting --models=1 ~A >! ~A;mv ~A ~A &"
							       (satch~program) in-file temp-out-file
							       temp-out-file out-file)
						       out-file
						       "satchmo"
						       satchmo-problem-dir
						       ressource)))

      (if (null call-flag)
	  (omega~message "~% Satchmo was able neither to find a model nor to show that there is no model in the given time ressource. ~%")
	
	;; read satchmo.out file as string ans set atp-out-string of the satchmo-problem
	(setf (atpprb~problem-atp-out-string satchmo-problem)
	      (atptop~read-file-as-string out-file))))))



(defun satch~complete-satchmo-problem! (satchmo-problem)
  (declare (edited  "23-JUL-1998")
	   (authors Ameier)
	   (input   "A satchmo problem.")
	   (effect  "None.")
	   (value   "If there is an atp-out-string that contains the line '%% No model' a message 'no model'"
		    "is printed and the 't is returned. Elsewhere the message 'model found' is printed and"
		    "nil is returned. The second return value is the out-string itself."))
  (if (null (atpprb~problem-atp-out-string satchmo-problem))
      nil
    (let* ((satchmo-out-string (atpprb~problem-atp-out-string satchmo-problem))
	   (proof-flag (satch=read satchmo-out-string)))
      
      (if proof-flag
	  (progn
	    (omega~message "~% Satchmo has shown that there is no model.~%")
	    (values 't satchmo-out-string))
	(progn
	  (omega~message "~% Satchmo has found a model.~%")
	  (omega~message "~% The Satchmo model is: ~%~A~%" satchmo-out-string)
	  (values nil satchmo-out-string))))))
  
	   
	   

(defun satch=read (out-string)
  (declare (edited  "23-JUL-1998")
	   (authors Ameier)
	   (input   "The satchmo output string.")
	   (effect  "None.")
	   (value   "T if it contains the line '%% No model', false elsewhere."))
  (let* ((line-strings (atptop~divide-string out-string #\Newline)))
    (find "%% No model" line-strings :test 'string=)))

