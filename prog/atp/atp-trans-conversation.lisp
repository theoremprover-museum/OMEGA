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

(defvar atconv*current-atp-problem nil)
;; A global variable to store an atp-problem in

(defvar atconv*current-problem nil)
;; a global variable to store a problem in

(defvar atconv*current-pds nil)
;; a global variable to store a pds in

(defvar atconv*current-node nil)
;; a global variable to store a node in

#| ----------------------------------------------------- Write/Read Problem to/from socket ------------------------------------------- |#

;; --------------------------------------> write to socket

(com~defcommand write-problem-to-socket
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats extern)
  (function atconv~write-problem-to-socket)
  (defaults )
  (log-p T) 
  (help ""))

(defun atconv~write-problem-to-socket ()
  (declare (edited  "23-JUL-1999")
	   (authors Ameier)
	   (input   "Nothing.")
	   (effect  "Writes the current problem, atconv*current-problem, to a socket.")
	   (value   "Undefined."))
  (let* ((streami (make-string-output-stream)))
    
    (post~print atconv*current-problem streami)
    
    (let* ((stringi (get-output-stream-string streami))
	   (stringii (write-to-string stringi))
	   (stringiii (format nil "solveProblem(~A node:'~A')"
			      stringii
			      (keim~name atconv*current-node))))
      
      (socket~write stringiii :inout))))

;; -----------------------------------------> read from socket

(com~defcommand read-problem-from-socket
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats extern)
  (function atconv~read-problem-from-socket)
  (defaults )
  (log-p T) 
  (help ""))

(defun atconv~read-problem-from-socket ()
  (declare (edited  "22-JUL-1999")
	   (authors Ameier)
	   (input   "Nothing.")
	   (effect  "From the socket a problem-string is read, thereby atconv*current-problem is set to the"
		    "read problem.")
	   (value   "Undefined."))
  (let* ((problem-string (socket~read))
	 (problem-as-list (read-from-string problem-string))
	 (new-problem (post~read-object problem-as-list
					(env~create)
					nil)))
    
    (setf  atconv*current-problem new-problem)))

#| ----------------------------------------------------- Write/Read PDS to/from socket ------------------------------------------- |#

;; --------------------------------------> write to socket

(com~defcommand write-pds-to-socket
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats extern)
  (function atconv~write-pds-to-socket)
  (defaults )
  (log-p T) 
  (help ""))

(defun atconv~write-pds-to-socket ()
  (declare (edited  "23-JUL-1999")
	   (authors Ameier)
	   (input   "Nothing.")
	   (effect  "Writes a string-representation of the current pds to a socket.")
	   (value   "Undefined."))
  (let* ((streami (make-string-output-stream)))
    
    (post~print omega*current-proof-plan streami)

    (let* ((stringi (get-output-stream-string streami))
	   (stringii (write-to-string stringi))
	   (stringiii (format nil "solvePds(~A)" stringii)))
      
      (socket~write stringiii :inout))))

;; -----------------------------------------> read from socket

(com~defcommand read-pds-from-socket
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats extern)
  (function atconv~read-pds-from-socket)
  (defaults )
  (log-p T) 
  (help ""))

(defun atconv~read-pds-from-socket ()
  (declare (edited  "22-JUL-1999")
	   (authors Ameier)
	   (input   "Nothing.")
	   (effect  "From the socket a pds-string is read and thereform a new pds is created."
		    "atconv*current-pds is set to the new pds.")
	   (value   "Undefined."))
  (let* ((pds-string (socket~read))
	 (pds-as-list (read-from-string pds-string))
	 (new-pds (post~read-object pds-as-list
				    (env~create)
				    nil)))

    (setf atconv*current-pds new-pds)))



#| --------------------------------------------------- Write/Read ATP-Problem to/from socket -------------------------------------- |#

;; --------------------------------------> write to socket

(com~defcommand write-atp-problem-to-socket
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats extern)
  (function atconv~write-atp-problem-to-socket)
  (defaults )
  (log-p T) 
  (help ""))

(defun atconv~write-atp-problem-to-socket ()
  (declare (edited  "23-JUL-1999")
	   (authors Ameier)
	   (input   "Nothing.")
	   (effect  "Writes a string-representation of the current atp-problem,atconv*current-atp-problem,"
		    "to a socket.")
	   (value   "Undefined."))
  (let* ((streami (make-string-output-stream)))
    
    (post~print atconv*current-atp-problem streami)
    
    (let* ((stringi (get-output-stream-string streami))
	   (stringii (write-to-string stringi))
	   (stringiii (format nil "solveAtpproblem(~A)" stringii)))
      
      (socket~write stringiii :inout))))

;; -----------------------------------------> read from socket

(com~defcommand read-atp-problem-from-socket
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats extern)
  (function atconv~read-atp-problem-from-socket)
  (defaults )
  (log-p T) 
  (help ""))

(defun atconv~read-atp-problem-from-socket ()
  (declare (edited  "22-JUL-1999")
	   (authors Ameier)
	   (input   "Nothing.")
	   (effect  "From the socket a atp-problem is read and thereform a new atp-problem is created."
		    "atconv*current-atp-problem is set to the new atp-problem.")
	   (value   "Undefined."))
  (let* ((atp-problem-string (socket~read))
	 (pds-problem-as-list (read-from-string pds-string))
	 (new-atp-problem (post~read-object pds-as-list
					    (env~create)
					    nil)))
    
    (setf atconv*current-atp-problem new-atp-problem)))


;; ------------------------------------------------> write to socket special

;; Dies ist ein spzieal kommando, dass ein ATP-Problem in   Haeppchen rueberschickt:
;;
;; 1.) alles bis hin zum ... (in-string
;; 2.) der ATP-Input-String
;; 3.) ')
;;     (out-string'
;; 4.) Der rest, d.h.
;;     ') ...
;; 
;; Damit kann auf OZ Seite ein komplettes ATP-Problem zusammengebaut werden durch zusammenhaengen der Teile und
;; einfuegen des ATP out-strings

(com~defcommand write-atp-problem-to-socket-special
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats extern)
  (function atconv~write-atp-problem-to-socket-special)
  (defaults )
  (log-p T) 
  (help ""))

(defun atconv~write-atp-problem-to-socket-special ()
  (declare (edited  "23-JUL-1999")
	   (authors Ameier)
	   (input   "Nothing.")
	   (effect  "Writes a string-representation of the current atp-problem,atconv*current-atp-problem,"
		    "to a socket in 5 strings.")
	   (value   "Undefined."))

  (multiple-value-bind
      (string1 string2 string3 string4 string5)
      (atconv~post~print-special atconv*current-atp-problem)
    
    (let* ((string1s (write-to-string string1))
	   (string2s (write-to-string string2))
	   (string3s (write-to-string string3))
	   (string4s (write-to-string string4))
	   (string5s (write-to-string string5))
	   (stringc (format nil "solveAtpproblemSpecial(~A ~A ~A ~A ~A)"
			    string1s
			    string2s
			    string3s
			    string4s
			    string5s
			    )))
      
      (socket~write stringc :inout))))


(defgeneric atconv~post~print-special (atp-problem)
  (declare (edited  "27-JUL-1999")
	   (authors Ameier)
	   (input   "An atp-problem.")
	   (effect  "None.")
	   (value   "Five strings, containing parts of the atp-problem respectively."))
  
  (:method ((atp-problem atpprb+fo-problem))  
	   
	   (let* ((streami (make-string-output-stream))
		  (id (atpprb~problem-id atp-problem))
		  (type (atpprb~problem-type atp-problem))
		  (atp-in-string (atpprb~problem-atp-in-string atp-problem))
		  (atp-out-string (atpprb~problem-atp-out-string atp-problem))
		  (part-res-proof (atpprb~problem-part-res-proof atp-problem))
		  (global-vars (atpprb~problem-global-vars atp-problem))
		  (translation-settings (atpprb~problem-translation-settings atp-problem))
		  (string1 nil)
		  (string2 nil)
		  (string3 nil)
		  (string4 nil))
	     
	     (format streami "(fo-atp-problem ~A" id)
	     (format streami "~%                 (type ~A)" type)
	     
	     ;; in-string
	     (format streami "~%                 (in-string ")

	     (setq string1 (get-output-stream-string streami))
	     
	     (setq string2 atp-in-string)

	     (format streami "~%                           )")
	     
	     ;; out-string 
	     (format streami "~%                 (out-string ")

	     (setq string3 (get-output-stream-string streami))

	     (setq string4 (if atp-out-string
			       atp-out-string
			     ""))
	     
	     (format streami "~%                            )")
	     
	     ;; partial resolution proof
	     (format streami "~%                 (res-proof ~%")
	     (post~print part-res-proof streami)
	     (format streami "~%                           )")
	     
	     ;; global-vars
	     (format streami "~%                 (global-vars ")
	     (atpprb=post-print global-vars streami)	    
	     (format streami "~%                             )")        
	     
	     ;; translation settings
	     (format streami "~%                 (translation-settings ")
	     (atpprb=post-print translation-settings streami)
	     (format streami "~%                                      ))")

	     (setq string5 (get-output-stream-string streami))

	     (values string1 string2 string3 string4 string5))))


#| ----------------------------------------------------------------------------------------------------------------------------------- |#
#|                                                                                                                                     |#
#|                                                  ZEUGS FUERS ATP-TRANS SYSTEM                                                       |#
#|                                                                                                                                     |#
#| ----------------------------------------------------------------------------------------------------------------------------------- |#





#| -------------------------------------------------------- create problem to node --------------------------------------------------- |#

(com~defcommand create-problem-to-node
  (argnames node)
  (argtypes ndline)
  (arghelps "Node to create a problem for")
  (frag-cats extern)
  (function atconv~create-problem-to-node)
  (defaults (nil
	     ))
  (log-p T) 
  (help ""))

(defun atconv~create-problem-to-node (node)
  (declare (edited  "23-JUL-1999")
	   (authors Ameier)
	   (input   "A node.")
	   (effect  "A new problem to this node is created and the atconv*current-problem is set to"
		    "this new problem. Furthermore atconv*current-node is set to the input node.")
	   (value   "Undefined."))
  (let* ((new-prob (atconv=new-problem-to-node node omega*current-proof-plan)))
    
    (setf atconv*current-problem new-prob)
    (setf atconv*current-node node)))
  
(defun atconv=new-problem-to-node (open-node pds)
  (declare (edited  "22-JUL-1999")
	   (authors Ameier)
	   (input   "An node to prove and the pds it occurs in.")
	   (effect  "None.")
	   (value   "A new problem, consisting of the open node as conclusion and its supports as assumptions."))
  
  (let* ((new-name (intern (string-upcase (format nil "~A-problem" (keim~name open-node)))))
	 (pds-env (pds~environment pds))
	 (pds-problem (prob~proof-problem pds))
	 (pds-problem-env (prob~environment pds-problem))
	 (th-env (th~env (prob~theory pds)))
	 (new-env (env~create (list th-env)))
	 (new-conclusion (node~create (keim~name open-node) (node~formula open-node) (just~create (infer~find-method 'open) nil)))
	 (new-assumptions (mapcar #'(lambda (supp)
				      (node~create (keim~name supp) (node~formula supp) (just~create (infer~find-method 'hyp) nil)))
				  (remove open-node (pds~node-supports open-node))))
	 (new-problem (prob~create new-name (prob~theory pds) new-env new-assumptions new-conclusion))
	 (all-type-vars (append (env~class-keys pds-env 'type+variable nil)
				(env~class-keys pds-problem-env 'type+variable nil)))
	 (all-type-constants (append (env~class-keys pds-env 'type+constant nil)
				     (env~class-keys pds-problem-env 'type+constant nil)))
	 (all-constants (append (env~class-keys pds-env 'term+constant nil)
				(env~class-keys pds-problem-env 'term+constant nil)))
	 (all-variables (append (env~class-keys pds-env 'term+variable nil)
				(env~class-keys pds-problem-env 'term+variable nil))))

    (mapcar #'(lambda (key)
		(let* ((obj (env~lookup-object key pds-env)))

		  (env~enter key obj new-env)))
	    (append all-type-vars
		    all-type-constants
		    all-constants
		    all-variables))
    
    new-problem))

#| -------------------------------------------------------- Create PDS from Problem ------------------------------------------------- |#

(com~defcommand create-pds-from-problem
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats extern)
  (function atconv~create-pds-from-problem)
  (defaults )
  (log-p T) 
  (help ""))

(defun atconv~create-pds-from-problem ()
  (declare (edited  "23-JUL-1999")
	   (authors Ameier)
	   (input   "Nothing.")
	   (effect  "From the problem in atconv*current-problem a pds is created and"
		    "omega*current-proof-plan is set to this new pds.")
	   (value   "Undefined."))

  (let* ((new-pds (pds~start-proof-plan atconv*current-problem (ot~new-proof-plan-name atconv*current-problem))))

    (setf omega*current-theory (prob~theory new-pds))
    (setf keim::logic*current-theory (prob~theory new-pds))
  
    (setf omega*current-proof-plan new-pds)))


#| -------------------------------------------------------- Produce ATP-PROBLEMS ---------------------------------------------------- |#

;; -----------------------> Otter

(com~defcommand create-otter-atp-problem
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats extern)
  (function atconv~create-otter-atp-problem)
  (defaults )
  (log-p T) 
  (help ""))

(defun atconv~create-otter-atp-problem ()
  (declare (edited  "22-JUL-1999")
	   (authors Ameier)
	   (input   "Nothing.")
	   (effect  "Produces the otter-atp-problem, then the atconv*current-atp-problem is set to"
		    "the new atp-problem.")
	   (value   "Undefined."))


  ;; Wir gehen davon aus, dass vorher ein Problem eingelesen wurde, und dass daher nur EIN offener Knoten da ist!!!!!!!!!
  
  (let* ((open-node (first (pds~open-nodes omega*current-proof-plan)))
	 (otter-atp-problem (otter~generate-otter-problem-default! open-node (pds~node-supports open-node) omega*current-proof-plan)))

    (setf atconv*current-atp-problem otter-atp-problem)))


;; -----------------------> EQP

(com~defcommand create-eqp-atp-problem
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats extern)
  (function atconv~create-eqp-atp-problem)
  (defaults )
  (log-p T) 
  (help ""))

(defun atconv~create-eqp-atp-problem ()
  (declare (edited  "22-JUL-1999")
	   (authors Ameier)
	   (input   "Nothing.")
	   (effect  "Produces the eqp-atp-problem, then the atconv*current-atp-problem is set to"
		    "the new atp-problem.")
	   (value   "Undefined."))


  ;; Wir gehen davon aus, dass vorher ein Problem eingelesen wurde, und dass daher nur EIN offener Knoten da ist!!!!!!!!!
  
  (let* ((open-node (first (pds~open-nodes omega*current-proof-plan)))
	 (eqp-atp-problem (eqp~generate-eqp-problem-default! open-node (pds~node-supports open-node) omega*current-proof-plan)))

    (setf atconv*current-atp-problem eqp-atp-problem)))



;; -----------------------> SPASS

(com~defcommand create-spass-atp-problem
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats extern)
  (function atconv~create-spass-atp-problem)
  (defaults )
  (log-p T) 
  (help ""))

(defun atconv~create-spass-atp-problem ()
  (declare (edited  "22-JUL-1999")
	   (authors Ameier)
	   (input   "Nothing.")
	   (effect  "Produces the spass-atp-problem, then the atconv*current-atp-problem is set to"
		    "the new atp-problem.")
	   (value   "Undefined."))


  ;; Wir gehen davon aus, dass vorher ein Problem eingelesen wurde, und dass daher nur EIN offener Knoten da ist!!!!!!!!!
  
  (let* ((open-node (first (pds~open-nodes omega*current-proof-plan)))
	 (spass-atp-problem (spass~generate-spass-problem-default! open-node (pds~node-supports open-node) omega*current-proof-plan)))

    (setf atconv*current-atp-problem spass-atp-problem)))



;; -----------------------> protein

(com~defcommand create-protein-atp-problem
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats extern)
  (function atconv~create-protein-atp-problem)
  (defaults )
  (log-p T) 
  (help ""))

(defun atconv~create-protein-atp-problem ()
  (declare (edited  "22-JUL-1999")
	   (authors Ameier)
	   (input   "Nothing.")
	   (effect  "Produces the protein-atp-problem, then the atconv*current-atp-problem is set to"
		    "the new atp-problem.")
	   (value   "Undefined."))


  ;; Wir gehen davon aus, dass vorher ein Problem eingelesen wurde, und dass daher nur EIN offener Knoten da ist!!!!!!!!!
  
  (let* ((open-node (first (pds~open-nodes omega*current-proof-plan)))
	 (protein-atp-problem (prot~generate-protein-problem-default! open-node (pds~node-supports open-node) omega*current-proof-plan)))

    (setf atconv*current-atp-problem protein-atp-problem)))


;; -----------------------> waldmeister

(com~defcommand create-waldmeister-atp-problem
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats extern)
  (function atconv~create-waldmeister-atp-problem)
  (defaults )
  (log-p T) 
  (help ""))

(defun atconv~create-waldmeister-atp-problem ()
  (declare (edited  "22-JUL-1999")
	   (authors Ameier)
	   (input   "Nothing.")
	   (effect  "Produces the waldmeister-atp-problem, then the atconv*current-atp-problem is set to"
		    "the new atp-problem.")
	   (value   "Undefined."))


  ;; Wir gehen davon aus, dass vorher ein Problem eingelesen wurde, und dass daher nur EIN offener Knoten da ist!!!!!!!!!
  
  (let* ((open-node (first (pds~open-nodes omega*current-proof-plan)))
	 (waldmeister-atp-problem (wald~generate-wald-problem-default! open-node (pds~node-supports open-node) omega*current-proof-plan)))

    (setf atconv*current-atp-problem waldmeister-atp-problem)))


;; -----------------------> Bliksem

(com~defcommand create-bliksem-atp-problem
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats extern)
  (function atconv~create-bliksem-atp-problem)
  (defaults )
  (log-p T) 
  (help ""))

(defun atconv~create-bliksem-atp-problem ()
  (declare (edited  "22-JUL-1999")
	   (authors Ameier)
	   (input   "Nothing.")
	   (effect  "Produces the bliksem-atp-problem, then the atconv*current-atp-problem is set to"
		    "the new atp-problem.")
	   (value   "Undefined."))


  ;; Wir gehen davon aus, dass vorher ein Problem eingelesen wurde, und dass daher nur EIN offener Knoten da ist!!!!!!!!!
  
  (let* ((open-node (first (pds~open-nodes omega*current-proof-plan)))
	 (bliksem-atp-problem (blik~generate-bliksem-problem-default! open-node (pds~node-supports open-node) omega*current-proof-plan)))

    (setf atconv*current-atp-problem bliksem-atp-problem)))

#| ---------------------------------------------------- parse-resolution proof of atp-problem --------------------------------------- |#

(com~defcommand parse-resolution-proof-of-atp-problem
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats extern)
  (function atconv~parse-resolution-proof-of-atp-problem)
  (defaults )
  (log-p T) 
  (help ""))

(defun atconv~parse-resolution-proof-of-atp-problem ()
  (declare (edited  "23-JUL-1999")
	   (authors Ameier)
	   (input   "Nothing.")
	   (effect  "Parses the Resolution proof of the atp-problem in atconv*current-atp-problem."
		    "Thereby the atp-problem is changed.")		    
	   (value   "Undefined."))
      
  (cond ((string-equal type 'otter)
	 (otter~complete-otter-problem! atp-prob :parse-back 't))
	((string-equal type 'eqp)
	 (eqp~complete-eqp-problem! atp-prob :parse-back 't))
	((string-equal type 'spass)
	 (spass~complete-spass-problem! atp-prob :parse-back 't))
	((string-equal type 'waldmeister)
	 (wald~complete-wald-problem! atp-prob :parse-back 't))
	((string-equal type 'protein)
	 (prot~complete-protein-problem! atp-prob :parse-back 't))
	((string-equal type 'bliksem)
	 (blik~complete-bliksem-problem! atp-prob :parse-back 't))
	(t
	 (error "~%Something wrong in function atconv\~read-atp-problem-from-socket.")))
  
  )

#| ------------------------------------------------------ ADD ATP-PROBLEM TO NODE ---------------------------------------------------- |#

(com~defcommand add-atp-problem-to-node
  (argnames node)
  (argtypes ndline)
  (arghelps "Node to add the current atp-problem")
  (frag-cats extern)
  (function atconv~add-atp-problem-to-node)
  (defaults (nil
	     ))
  (log-p T) 
  (help ""))

(defun atconv~add-atp-problem-to-node (node)
  (declare (edited  "23-JUL-1999")
	   (authors Ameier)
	   (input   "A node.")
	   (effect  "")
	   (value   "Undefined."))

  (keim~put node 'atp-problems (cons atconv*current-atp-problem (keim~get node 'atp-problems))))





#| ----------------------------------------------------------------------------------------------------------------------------------- |#
#|                                                                                                                                     |#
#|                                                  ZEUGS FUERS TRANS SYSTEM                                                           |#
#|                                                                                                                                     |#
#| ----------------------------------------------------------------------------------------------------------------------------------- |#





#| -------------------------------------------------- Trans current ATP-PROBLEM ALONE ------------------------------------------------ |#

;; diese Funktion geht davon aus, dass vorher das Commando read-atp-problem-from-socket ausgefuehrt wurde, so dass
;; atconv*current-atp-problem gesetzt ist

(com~defcommand trans-current-atp-problem-alone
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats extern)
  (function atconv~trans-current-atp-problem-alone)
  (defaults )
  (log-p T) 
  (help ""))

(defun atconv~trans-current-atp-problem-alone ()
  (declare (edited  "23-JUL-1999")
	   (authors Ameier)
	   (input   "Nothing.")
	   (effect  "The current atp-prob is translated, thereby a new pds is created and"
		    "set as the omega*current-proof-plan.")
	   (value   "Undefined."))

  (let* ((trans-seettings (atpprb~problem-translation-settings atconv*current-atp-problem))
	 (resolution-proof (atpprb~problem-part-res-proof atconv*current-atp-problem)))

    (if (not (string-equal (first trans-settings) 'pl-atp))
	(progn
	  (setq p2f*domain (second trans-settings))
	  (setq p2f*codomain (third trans-settings)))
      (progn
	(setq p2pl*domain (second trans-settings))
	(setq p2pl*codomain (third trans-settings))))
    
    (res2nd~transform resolution-proof
		      :sspu-style (case sspu-style
				    (direct 'dir)
				    (compact 'sspu)
				    (auto 'aut))
		      :indirect-proof indirect-proof
		      :maximal-depth maximal-depth
		      :integral-formulas integral-formulas
		      :reach-sspu-style (if tnd 'case 'smallest)
		      :avoid-doubeling avoid-doubeling)

    ;; F.O. -> H.O. in omega*current-proof-plan
    (if (string-equal (first trans-settings) 'pl-atp)
	(pl2p~translate omega*current-proof-plan)
      (f2p~translate omega*current-proof-plan))
    
    (atptop~construct-and-add-problem! resolution-proof omega*current-proof-plan)
    
    (setf atconv*current-pds omega*current-proof-plan)))

#| -------------------------------------------- CHOOSE ATP-PROBLEM to transform special ---------------------------------------------- |#

(com~defcommand choose-atp-problem-to-transform-special
  (argnames node)
  (argtypes ndline)
  (arghelps "Node to choose a problem from")
  (frag-cats extern)
  (function atconv~choose-atp-problem-to-transform-special)
  (defaults ((oc=default-current-plan-or-untested-or-unexpanded-otter-node)	     
	     ))
  (log-p T) 
  (help ""))

(defun atconv~choose-atp-problem-to-transform-special (node)
  (declare (edited  "28-JUL-1999")
	   (authors Ameier)
	   (input   "A node.")
	   (effect  "Write a string with all atp-problems to socket.")
	   (value   "Undefined."))
  (let* ((atp-problems (keim~get node 'atp-problems))
	 (complete-problems (remove-if-not #'atpprb~complete-p atp-problems))
	 (stringi (format nil "popupProofsSpecial(\"~A\" [" (keim~name node))))
    (mapcar #'(lambda (comp-prob)
		(let* ((type (atpprb~problem-type comp-prob)))
		  (setq stringi (format nil "~A \"~A\"" stringi (string-upcase (string type))))))
	    complete-problems)

    (setq stringi (format nil "~A])" stringi))

    (socket~write stringi :inout)))

#| ---------------------------------------------------- CHOOSE PROBLEM OF TYPE ------------------------------------------------------- |#

(com~defcommand choose-problem-of-type
  (argnames node type)
  (argtypes ndline symbol)
  (arghelps "Node to choose a problem from"
	    "Type of problem")
  (frag-cats extern)
  (function atconv~choose-atp-problem-to-transform)
  (defaults ((oc=default-current-plan-or-untested-or-unexpanded-otter-node)	     
	     'otter
	     ))
  (log-p T) 
  (help ""))

(defun atconv~choose-atp-problem-to-transform (node type)
  (declare (edited  "28-JUL-1999")
	   (authors Ameier)
	   (input   "A node and a type (symbol otter, bliksem ...)")
	   (effect  "The variable atconv*current-atp-problem is set to the"
		    "atp-problem on node of type.")
	   (value   "None."))

  (let* ((atp-problems (keim~get node 'atp-problems))
	 (complete-problems (remove-if-not #'atpprb~complete-p atp-problems))
	 (choosen-atp-problem (find type complete-problems :test #'(lambda (it1 it2)
								     (equal it1 (atpprb~problem-type it2))))))

    (setf atconv*current-atp-problem choosen-atp-problem)))


#| ---------------------------------------------------------- Insert-pds-at-node ---------------------------------------------------- |#

(com~defcommand insert-pds-at-node
  (argnames node type)
  (argtypes ndline symbol)
  (arghelps "Root node."
	    "Type of problem")
  (frag-cats extern)
  (function atconv~insert-pds-at-node)
  (defaults ((oc=default-current-plan-or-untested-or-unexpanded-otter-node)	     
	     'otter
	     ))
  (log-p T) 
  (help ""))

(defun atconv~insert-pds-at-node (node prover)
  (declare (edited  "28-JUL-1999")
	   (authors Ameier)
	   (input   "A node and a type (otter/bliksem/...)")
	   (effect  "The pds in atconv*current-pds is inserted into the omega*current-proof-plan"
		    "at node. This can also cause some changes in the environment of omega*current-proof-plan.")
	   (value   "Undefined."))

  (let* ((atp-method (infer~find-method prover))
	 (atp-just (first (remove-if-not #'(lambda (just)
					     (eq (just~method just) atp-method))
					 (pdsn~all-justs node))))
	 (atp-problems-of-node (keim~get node 'atp-problems))
	 (atp-problem-of-prover (first (remove-if-not #'(lambda (atp-prob)
							   (let* ((type (atpprb~problem-type atp-prob)))
							     (string-equal type prover)))
						       atp-problems-of-node)))
	 (res-proof (atpprb~problem-part-res-proof atp-problem-of-prover)))
    
    (when (null atp-just)
      (omega~error "Node ~A needs a justification by an atp method (otter\spass)." node))
    
    (setf (pdsj~status atp-just) "expanded")
    
;;; Hier fehlt noch ein Uebernahme neuer Konstanten in das Environment von omega*current-proof-plan    
;;;    ;; new constants may are constructed between the translation and have to be inserted into the
;;;    ;; environment of the original pds
;;;    (mapcar #'(lambda (new-const)
;;;		(let* ((key (intern (keim~name new-const) (find-package :omega))))
;;;		  (env~enter key new-const orig-env)))
;;;	    r2ntop*new-constants)

    ;; insert atconv*current-pds into omega*current-proof-plan
    (let* ((names-in-resolution-proof (mapcar #'keim~name (cons (res~proof-conclusion res-proof)
								(res~proof-assumptions res-proof))))
	   (nodes-in-original-proof (mapcar #'(lambda (name)
						(pds~label2node name omega*current-proof-plan))
					    names-in-resolution-proof))
	   (nodes-in-sub-proof (mapcar #'(lambda (name)
					   (pds~label2node name atconv*current-pds))
				       names-in-resolution-proof))
	   (node-matching-list (mapcar #'(lambda (sub-node orig-node)
					   (list sub-node orig-node))
				       nodes-in-sub-proof nodes-in-original-proof)))

      (atptop~insert-sub-proof-in-original-proof! omega*current-proof-plan
						  atconv*current-pds
						  node-matching-list)
      

;;; Hier fehlt noch ein renamen der neu eingefuegten Zeilen, da der Label-Counter nicht stimmen duerfte
;;; -> destruktives umbenennen + hochzaehlen des Label Counters

      )))


      


