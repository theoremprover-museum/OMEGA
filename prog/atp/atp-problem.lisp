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



(mod~defmod ATPPRB 
            :uses (keim res)
            :documentation "The data-structures for atp-problems."
            :exports (atpprb+fo-problem
                      atpprb+fo-problem
                      atpprb+leo-problem
                      atpprb+problem
                      atpprb+tps-problem
                      
                      atpprb~complete-p
                      atpprb~create-fo-problem
                      atpprb~create-leo-problem
                      atpprb~create-tps-problem
                      atpprb~divide-tps-string!
                      atpprb~find-problem
                      atpprb~fo-problem-p
                      atpprb~leo-flag-settings
                      atpprb~leo-formatted-proof
                      atpprb~leo-problem-p
                      atpprb~leoloui-proof
                      atpprb~problem-atp-in-string
                      atpprb~problem-atp-out-string
                      atpprb~problem-global-vars
                      atpprb~problem-id
                      atpprb~problem-p
                      atpprb~problem-part-res-proof
                      atpprb~problem-translation-settings
                      atpprb~problem-type
                      atpprb~tps-formatted-proof
                      atpprb~tps-hyps-conjunct
                      atpprb~tps-problem-file
                      atpprb~tps-problem-p
                      atpprb~tps-proof-string
                      atpprb~tps-startline-table
                      atpprb~tps-type-translation
                      atpprb~tps-typed-symbols
                      atpprb~tps-unused-types
		      atpprb~prover-succeeded-p
                      ))

;;; The following functions are internal in other modules and should not be used:
;;; (tps=planned-line)

(defclass atpprb+problem (keim+object)
  ((id :initarg :id
       :accessor atpprb~problem-id
       :documentation "The id of the atpprb+problem")
   (type :initarg :type
	 :accessor atpprb~problem-type
	 :documentation "The type of the atpprb+problem")
   (atp-in-string :initarg :atp-in-string
		  :accessor atpprb~problem-atp-in-string
		  :documentation "The in-string to send to the atp.")
   (atp-out-string :initarg :atp-out-string
		   :accessor atpprb~problem-atp-out-string
		   :documentation "The out-string that comes from the atp.")))



(defclass atpprb+fo-problem (atpprb+problem)
  ((part-res-proof :initarg :part-res-proof
		   :accessor atpprb~problem-part-res-proof
		   :documentation "The partial resolution proof of a fo-problem.")
   (global-vars :initarg :global-vars
		:accessor atpprb~problem-global-vars
		:documentation "The global vars settings of a fo-problem.")
   (translation-settings :initarg :translation-settings
			 :accessor atpprb~problem-translation-settings
			 :documentation "To store the information for p2f/f2p or p2pl/pl2p in, first 'p2f or 'p2pl then domain, then codomain.")))

   
(defclass atpprb+tps-problem (atpprb+problem)
  ((tps-problem-file :initarg :tps-problem-file
		     :accessor atpprb~tps-problem-file
		     :documentation "The problem string to be restored by TPS.")
   (tps-proof-string :initarg :tps-proof-string
		     :accessor atpprb~tps-proof-string
		     :documentation "The proof string used for translation to OMEGA.")
   (tps-formatted-proof :initarg :tps-formatted-proof
			:accessor atpprb~tps-formatted-proof
			:documentation "The formatted ascii proof used for visualization in OMEGA.")
   (tps-typed-symbols :initarg :tps-typed-symbols
		      :accessor atpprb~tps-typed-symbols
		      :documentation "Some predefined symbols, e.g. EXISTS, FORALL, =")
   (tps-unused-types  :initarg :tps-unused-types
		      :accessor atpprb~tps-unused-types
		      :documentation "Some typesymbols for tps.")
   (tps-startline-table :initarg :tps-startline-table
			:accessor atpprb~tps-startline-table
			:documentation "The already existing lines....")
   (tps-type-translation :initarg :tps-type-translation
			 :accessor atpprb~tps-type-translation
			 :documentation "A cons list, specifying the mapping from the
OMEGA-typesymbols to the ones used in TPS")
   (tps-hyps-conjunct :initarg :tps-hyps-conjunct
		      :accessor atpprb~tps-hyps-conjunct
		      :documentation "A cons list, specifying the mapping from the
OMEGA-typesymbols to the ones used in TPS"
			 )))


(defclass atpprb+leo-problem (atpprb+problem)
  ((leo-formatted-proof :initarg :leo-formatted-proof
			:accessor atpprb~leo-formatted-proof
			:documentation "The formatted ascii proof used for visualization in OMEGA.")
   (leo-flag-settings :initarg :leo-flag-settings
		      :accessor atpprb~leo-flag-settings
		      :documentation "LEO flag settings.")
   (leoloui-proof  :initarg :leoloui-proof
		   :accessor atpprb~leoloui-proof
		   :documentation "The proof as a LOUI term")))


(defun atpprb~tps-problem-p (thing)
  (declare (edited  "25-MAR-1998")
	   (authors Chris)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "T if the object is of type atpprb+tps-problem, otherwise nil."))
  (typep thing 'atpprb+tps-problem))


(defun atpprb~leo-problem-p (thing)
  (declare (edited  "25-MAR-1998")
	   (authors Chris)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "T if the object is of type atpprb+leo-problem, otherwise nil."))
  (typep thing 'atpprb+leo-problem))

(defun atpprb~problem-p (thing)
  (declare (edited  "25-MAR-1998")
	   (authors Ameier)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "T if the object is of type atpprb+problem, otherwise nil."))
  (typep thing 'atpprb+problem))

(defun atpprb~fo-problem-p (thing)
  (declare (edited  "25-MAR-1998")
	   (authors Ameier)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "T if the object is of type atpprb+fo-problem, otherwise nil."))
  (typep thing 'atpprb+fo-problem))

(defun atpprb~create-fo-problem (id type atp-in-string atp-out-string part-res-proof global-vars translation-settings)
  (declare (edited  "25-MAR-1998")
	   (authors Ameier)
	   (input   "An identification (a unique name), a type, an atp-in-string, an atp-out-string,"
		    "a partial res-proof and a global-variable setting.")
	   (effect  "None.") 
	   (value   "A new atpprb+fo-problem object."))
  (make-instance 'atpprb+fo-problem
		 :id id
		 :type type
		 :atp-in-string atp-in-string
		 :atp-out-string atp-out-string
		 :part-res-proof part-res-proof
		 :global-vars global-vars
		 :translation-settings translation-settings))


(defun atpprb~create-tps-problem (id type atp-in-string atp-out-string tps-problem-file
				     tps-proof-string
				     tps-formatted-proof tps-typed-symbols
				     tps-unused-types tps-startline-table
				     tps-type-translation tps-hyps-conjunct)
  (declare (edited  "25-MAR-1998")
	   (authors Chris)
	   (input   "An identification (a unique name), a type, an atp-in-string, an atp-out-string,"
		    "...")
	   (effect  "None.") 
	   (value   "A new atpprb+tps-problem object."))
  (make-instance 'atpprb+tps-problem
		 :id id
		 :type type
		 :atp-in-string atp-in-string
		 :atp-out-string atp-out-string
		 :tps-problem-file tps-problem-file
		 :tps-proof-string tps-proof-string
		 :tps-formatted-proof tps-formatted-proof
		 :tps-typed-symbols tps-typed-symbols
		 :tps-unused-types tps-unused-types
		 :tps-startline-table tps-startline-table
		 :tps-type-translation tps-type-translation
		 :tps-hyps-conjunct tps-hyps-conjunct))


(defun atpprb~create-leo-problem (id type atp-in-string atp-out-string 
				     leo-formatted-proof leo-flag-settings
				     leoloui-proof)
  (declare (edited  "25-MAR-1998")
	   (authors Chris)
	   (input   "An identification (a unique name), a type, an atp-in-string, an atp-out-string,"
		    "...")
	   (effect  "None.") 
	   (value   "A new atpprb+leo-problem object."))
  (make-instance 'atpprb+leo-problem
		 :id id
		 :type type
		 :atp-in-string atp-in-string
		 :atp-out-string atp-out-string
		 :leo-formatted-proof leo-formatted-proof
		 :leo-flag-settings leo-flag-settings
		 :leoloui-proof leoloui-proof))


(defun atpprb=check-tps-p (contentlist)
  (declare (edited  "16-OCT-1997")
	   (authors Gebhard)
	   (input   "TPS-proof in list format.")
	   (effect  "None")
	   (value   "True if the symbol PLAN1 occurs somewhere in the proof."))
  (cond ((not (listp (first contentlist)))
	 (atpprb=check-tps-p (rest contentlist)))
	((eq 'lines (first (first contentlist)))
	 (find-if #'(lambda (x) (and (= (first x) 100000) (tps=planned-line x)))
	       (rest (first contentlist))))
	(t
	 (atpprb=check-tps-p (rest contentlist)))))


(defgeneric atpprb~complete-p (atp-problem)
  (declare (edited  "25-MAR-1998")
	   (authors Ameier)
	   (input   "An atp-problem.")
	   (effect  "None.")
	   (value   "T if the problem is ready for translation, in f.o.-problems this is the"
		    "case, when the out-string exists and is back-parsed, that the resolution proof"
		    "is complete."))
  (:method ((atp-problem atpprb+fo-problem))
	   (if (and (atpprb~problem-atp-out-string atp-problem)
		    (res~proof-step-clauses (atpprb~problem-part-res-proof atp-problem))
		    (res~proof-empty-clause (atpprb~problem-part-res-proof atp-problem)))
	       't
	     nil))
  (:method ((tps-problem atpprb+tps-problem))
	   (and (atpprb~tps-formatted-proof tps-problem) ;; darin steht ascii beweis
		(atpprb~tps-proof-string tps-problem) ;; darin steht zu uebersetzender string
		(null (atpprb=check-tps-p (atpprb~tps-proof-string tps-problem))))))



(defgeneric atpprb~prover-succeeded-p (atp-problem)
  (declare (edited  "25-MAR-1998")
	   (authors Ameier)
	   (input   "An atp-problem.")
	   (effect  "None.")
	   (value   "T if the out-string of the problem is present and the corresponding function without parsing"
		    "returns T."
		    "In conrast to atpprb\~comlpete-p which is only t if we could parse back the proof of the ATP"
		    "this function is also t if we could not parse back the proof (why ever) but we can"
		    "determin, that the prover succeeded."))
  (:method ((atp-problem atpprb+fo-problem))
	   (or (atpprb~complete-p atp-problem)
	       (let* ((type (atpprb~problem-type atp-problem))
		      (res-proof (atpprb~problem-part-res-proof atp-problem))
		      (out-string (atpprb~problem-atp-out-string atp-problem)))
		 (if (null out-string)
		     ;; out-string not given -> return nil
		     nil
		   ;; outstring-given -> check dependend on which type of problem we have the success
		   (cond ((string-equal type 'otter)
			  (otter=read-without-parsing res-proof out-string))
			 ((string-equal type 'eqp)
			  (eqp=read res-proof out-string nil))
			 ((string-equal type 'bliksem)
			  (blik=read out-string res-proof nil))
			 ((string-equal type 'spass)
			  (spass=translate-spass-proof-from-string res-proof out-string nil))
			 ((string-equal type 'protein)
			  (prot=read-without-parsing res-proof out-string))
			 ((string-equal type 'waldmeister)
			  (wald=read res-proof out-string nil))
			 (t
			  (omega~error "~%Unknown ATP-type in function atpprb\~prover-succeeded-p."))))))))

(defun atpprb~find-problem (node type)
  (let* ((atp-problems (keim~get node 'atp-problems)))
    (first (remove-if-not #'(lambda (atp-prob)
			      (equal (atpprb~problem-type atp-prob) type))
			  atp-problems))))


(defun atpprb~divide-tps-string! (tps-problem)
  (declare (edited  "26-APR-1998")
	   (authors Ameier)
	   (input   "A TPS-Problem.")
	   (effect  "Takes the atp-out-string, seeks for the line:"
		    "%%%%% THIS WAS THE PROOF FILE, NEXT IS THE FORMATTED-PROOF FILE %%%%%"
		    "cuts the string at this line, writes the first resulting string"
		    "in the tps-proof-string slot and the second resulting string in"
		    "the tps-formatted-proof slot of the tps-problem." )
	   (value   "Undefined."))
  (let* ((devide-line "%%%%% THIS WAS THE PROOF FILE, NEXT IS THE FORMATTED-PROOF FILE %%%%%")
	 (prob-string (atpprb~problem-atp-out-string tps-problem))
	 (dev-pos (position #\% prob-string))
	 (pretty-proof (subseq prob-string 0 dev-pos))
	 (translate-proof (string-trim devide-line (subseq prob-string dev-pos))))
    (setf (atpprb~tps-proof-string  tps-problem)  (read-from-string translate-proof))
    (setf (atpprb~tps-formatted-proof tps-problem) pretty-proof)))



#| ------------------------------------------------------- POST THINGS FOR ATP-PROBLEMS --------------------------------------------- |#

;; --------------------> post~read-object fuer ATP-FO-Problems

(defmethod post~read-object (listi (env env+environment) 
				   (indicator (eql :fo-atp-problem)))
  
  (let* ((id (first listi))
	 (type (second (second listi)))
	 (atp-in-string (second (third listi)))
	 (atp-out-string (second (fourth listi)))
	 (res-proof (post~read-object (second (fifth listi))
				      env
				      nil))
	 (res-proof-env (res~proof-environment res-proof))
	 (global-var-list (second (sixth listi)))
	 (global-vars (;; was in den global vars drin steht und wie es zu lesen ist, haengt von den einzelnen ATP's ab!
		       ;; Das ist nicht ganz optimal, sollte aber vorerst mal das machen, was wir wollen ...
		       
		       cond ((or (string-equal type 'eqp)
				 (string-equal type 'bliksem)
				 (string-equal type 'protein)
				 (string-equal type 'waldmeister))
			     
			     ;; global-vars enthalten nur einen Eintrag:
			     ;; 1.) convert-liste, wobei jedes Element die form (X "X") besitzt
			     
			     (let* ((convert-list (first global-var-list)))
			       
			       (list (mapcar #'(lambda (pair)
						 (list (post~read-object (first pair) res-proof-env :existing-term)
						       (second pair)))
					     convert-list))))
			    ((string-equal type 'otter)
			     
			     ;; global-vars enthalten drei Eintraege:
			     ;; 1.) convert-liste, wobei jedes Element die form (X "X") besitzt
			     ;; 2.) reflexivity-item, d.h. eine clausel (diese Klausel muss leider mitgeschickt werden, da sie
			     ;;     in die Klauselmenge zur vollstaendigkeit hinzugefuegt werden muss!)
			     ;; 3.) flag das anzeigt, ob proof-object gesetzt ist
			     
			     
			     (let* ((convert-list (first global-var-list))
				    (refl-item (second global-var-list))
				    (flag (third global-var-list)))
			       
			       (list (mapcar #'(lambda (pair)
						 (list (post~read-object (first pair) res-proof-env :existing-term)
						       (second pair)))
					     convert-list)
				     (post~read-object refl-item res-proof-env :clause)
				     flag)))
			    ((string-equal type 'spass)
			     
			     ;; global-vars enthalten zwei Eintraege:
			     ;; 1.) relfex-clause (keine Ahnung ob man das vermeiden kann)
			     ;; 2.) convert-liste, wobei jedes Element die form ("X" X) besitzt
			     
			     (let* ((convert-list (second global-var-list))
				    (refl-item (first global-var-list)))
			       
			       (list (post~read-object refl-item res-proof-env :clause)
				     (mapcar #'(lambda (pair)
						 (list (first pair)
						       (post~read-object (second pair) res-proof-env :existing-term)))
					     convert-list))))
			    (t
			     (error "~%Something wrong in method post~read-object for fo-atp-problems!"))))
	 (translation-settings (list (first (seventh listi))
				     (post~read-object (second (seventh listi)) res-proof-env
						       :existing-term)
				     (post~read-object (third (seventh listi)) res-proof-env
						       :existing-term))))
    
    (atpprb~create-fo-problem id type atp-in-string atp-out-string res-proof global-vars translation-settings)))


;; ----------------------------------> post~print fuer ATP-PROBLEMS

(defmethod post~print ((atp-problem atpprb+fo-problem) stream)  
  (let* ((id (atpprb~problem-id atp-problem))
	 (type (atpprb~problem-type atp-problem))
	 (atp-in-string (atpprb~problem-atp-in-string atp-problem))
	 (atp-out-string (atpprb~problem-atp-out-string atp-problem))
	 (part-res-proof (atpprb~problem-part-res-proof atp-problem))
	 (global-vars (atpprb~problem-global-vars atp-problem))
	 (translation-settings (atpprb~problem-translation-settings atp-problem)))

    (format stream "(fo-atp-problem ~A" id)
    (format stream "~%                 (type ~A)" type)

    ;; in-string
    (format stream "~%                 (in-string ")
    (atpprb=post-print atp-in-string stream)
    (format stream "~%                           )")

    ;; out-string 
    (format stream "~%                 (out-string ")
    (when out-string
      (atpprb=post-print atp-out-string stream))
    (format stream "~%                            )")
    
    ;; partial resolution proof
    (format stream "~%                 (res-proof ~%")
    (post~print part-res-proof stream)
    (format stream "~%                           )")

    ;; global-vars
    (format stream "~%                 (global-vars ")
    (atpprb=post-print global-vars stream)	    
    (format stream "~%                             )")        

    ;; translation settings
    (format stream "~%                 (translation-settings ")
    (atpprb=post-print translation-settings stream)
    (format stream "~%                                      ))")))



(defgeneric atpprb=post-print (obj stream)
  (:method ((obj list) stream)

	   (format stream "(")

	   (mapcar #'(lambda (part)
		       (atpprb=post-print part stream)
		       (format stream " "))
		   obj)
	   
	   (format stream ")"))
  (:method ((obj string) stream)

	   (format stream "\"~A\"" obj)) 
  (:method ((obj t) stream)

	   (post~print obj stream)))

