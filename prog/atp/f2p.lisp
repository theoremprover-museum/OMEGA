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

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                   Module                                 ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;

(in-package :omega)



(mod~defmod F2P 
            :uses (data keim node p2f pds prob term)
            :documentation "Translating first-order problems to higher-order."
            :exports (
                      
                      f2p~translate
                      ))



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                   Main                                   ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;

(defgeneric f2p=affe-appl-p (term)
  (declare (edited  "11-OCT-1996")
	   (authors Hess)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "T if keim~name of term starts with @, else NIL"))
  (:method ((term term+constant))
	   (string= (keim~name term) "@" :start1 0 :end1 1))
  (:method ((term term+variable))
	   nil)
  (:method ((term term+appl))
	   nil)
  (:method ((term term+abstr))
	   nil))

(defgeneric f2p=translate-term (term env)
  (declare (edited  "11-OCT-1996")
	   (authors Hess)
	   (input   "A first-order term and an environment.")
	   (effect  "Removes if needed the new applications in"
		    "the environment.")
	   (value   "The retranslated higher-order term."))
  (:method ((term term+constant) env)
	   (declare (ignore env))
	   term)

  (:method ((term term+variable) env)
	   (declare (ignore env))
	   term)
	       
  (:method ((term term+appl) env)
	   (let ((func (data~appl-function term))
		 (args (data~appl-arguments term)))
	     (if (f2p=affe-appl-p func)
		 (term~appl-create (f2p=translate-term (first args) env)
				   (mapcar #'(lambda (x)
					       (f2p=translate-term x env))
					   (rest args)))
	       (term~appl-create (f2p=translate-term func env)
				 (mapcar #'(lambda (x)
					     (f2p=translate-term x env))
					 args)))))
  
  (:method ((term term+abstr) env)
	   (let ((vars (data~abstr-domain term))
		 (scope (data~abstr-range term)))
	     (term~abstr-create (mapcar #'(lambda (var)
					    (f2p=translate-term var env))
					vars)
				(f2p=translate-term scope env)))))

(defun f2p~translate (proof)
  (declare (edited  "21-OCT-1996")
	   (authors Hess)
	   (input   "A probably first-order proof.")
	   (effect  "None. na, ja")
	   (value   "The translated higher-order proof."))

  ;;(omega~message "Translating from first- to higher-order logic.")

  ;; First test whether a @-symbol is in the environment. If not it is not necessary to proceed!
  (let* ((env (pds~environment proof))
	 (constant-keys (env~class-keys env 'term+constant 't))
	 (test (some #'f2p=affe-appl-p (mapcar #'(lambda (key)
						   (env~lookup-object key env))
					       constant-keys))))
    
    (when test 
      (mapcar #'(lambda (node)
		  (setf (node~formula node) (f2p=translate-term (node~formula node) env))
		  (setf (pdsj~parameters (node~justification node))
			(mapcar #'(lambda (param)
				    (if (term~p param)
					(f2p=translate-term param env)
				      param))
				(pdsj~parameters (node~justification node)))))
	      (prob~proof-steps proof)))
    
    (fresh-line)
    
    ;; the things to restore free-variables
    
    (when (or p2f*codomain
	      p2f*domain)
      ;; replace constants by variables
      (mapcar #'(lambda (node)
		  (setf (node~formula node)
			(data~replace-structs (node~formula node)
					      p2f*codomain
					      p2f*domain
					      :downto '(data+primitive)))
		  (setf (pdsj~parameters (node~justification node))
			(mapcar #'(lambda (param)
				    (if (term~p param)
					(data~replace-structs param
							      p2f*codomain
							      p2f*domain
							      :downto '(data+primitive)
							      :replacers-downto '(data+primitive))
				      param))
				(pdsj~parameters (node~justification node)))))
	      
	      (prob~proof-steps proof)))
    proof))

