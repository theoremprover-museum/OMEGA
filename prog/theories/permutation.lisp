;;; -*- syntax: common-lisp; package: keim; base: 10; mode: keim -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     OMEGA Project                                                        ;;
;;     AG Siekmann/FB Informatik                                            ;;
;;     Universitaet des Saarlandes                                          ;;
;;     Postfach 1150                                                        ;;
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


(defconstant perm*permutation-type 'cyc)
(defconstant perm*permutation-theory 'permutation)


;; -cycles as objects of type 'cyc' that fulfill the predicate cycle,
;; as term (cons .... nil), as abstract data structure (CYC ...)
;; -permutations are objects of type 'cyc->o' that fulfill the predicate permutation,
;; as term sets of disjoint cycles, as abstract data structure (SET <cycles>),
;; or by perm-compose, perm-exp or perm-inverse of two permutations
;; -permutations become mappings with perm-apply

(defconstant perm*gap-file (concatenate 'string  (sys~getenv 'home) "/omega/omega-3/prog/sapper/permgps2.g"))
;  "/home/staff/vxs/science/eindhoven/arjeh/permgps2.g")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Translating permutations to GAP syntax
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric perm~permutation2gap (perm)
  (declare (edited  "12-JUL-2002")
	   (authors Vxs)
	   (input   "A post term representing a permutation or a composition of permutations.")
	   (effect  "None.")
	   (value   "A string containing the GAP represenation of the permutation and a boolean indicating"
		    "whether or not the permutation is a composition."))
  (:method ((perm term+term))
	   (if (perm~perm-type-p perm)
	       (perm=perm2gap  perm)
	     (omega~error "PERM~~PERMUTATION2GAP: Formula ~A is not of permutation type." perm)))
  (:method ((perm cons))
	   (perm~permutation2gap (post~read-object perm (th~env perm*permutation-theory) :existing-term)))
  (:method ((perm symbol))
	   (perm~permutation2gap (post~read-object perm (th~env perm*permutation-theory) :existing-term)))
  (:method ((perm string))
	   (perm~permutation2gap (read-from-string perm)))
  (:method (perm)
	   (omega~error "PERM~~PERMUTATION2GAP: ~A is an illegal permutation object." perm)))

(defgeneric perm~cycle2gap (cycle)
  (declare (edited  "11-SEP-2002")
	   (authors Vxs)
	   (input   "A post term representing a cycle.")
	   (effect  "None.")
	   (value   "The string with the GAP representation of the cycle."))
  (:method ((cycle term+term))
	   (if (perm~cycle-type-p cycle)
	       (perm=cycle2gap  cycle)
	     (omega~error "PERM~~CYCLE2GAP: Formula ~A is not of cycle type." cycle)))
  (:method ((cycle cons))
	   (perm~cycle2gap (post~read-object cycle (th~env perm*permutation-theory) :existing-term)))
  (:method ((cycle string))
	   (perm~cycle2gap (read-from-string cycle)))
  (:method (cycle)
	   (omega~error "PERM~~CYCLE2GAP: ~A is an illegal cycle object." cycle)))

(defun perm=perm2gap (perm)
  (declare (edited  "14-SEP-2002")
	   (authors Pollet)
	   (input   "A list representing a permutation or a composition of permutations."
		    "And an optional parameter which will cause the function to return p1*p2"
		    "as p2*p1 for the application.")
	   (effect  "None.")
	   (value   "Two values:"
		    "- A string containing a GAP representation of the permutation or the composition."
		    "- A boolean, which is T if the permutation is a composition."))
  (cond ((and (term~appl-p perm)
	      (data~equal (data~appl-function perm) perm*perm-compose))    ;;; composition of two distinct permutations 
	 (values                                               
	  (format nil "(~A * ~A)"
		  (perm=perm2gap (second (data~appl-arguments perm)))   ;; we have to swap the arguement!
		  (perm=perm2gap (first (data~appl-arguments perm))))
	  t))
	((and (term~appl-p perm)
	      (data~equal (data~appl-function perm) perm*perm-exp))    ;;; permutation contains an exponentiation of its cycles
	 (values
	  (format nil "(~A ^ ~A)"
		  (perm=perm2gap (first (data~appl-arguments perm)))
		  (second (data~appl-arguments perm)))
	  t))
	((and (term~appl-p perm)
	      (data~equal (data~appl-function perm) perm*perm-inverse))    ;;; inverse permutation 
	 (values
	  (format nil "(~A^ -1)"
		  (perm=perm2gap (first (data~appl-arguments perm)))
		  (second (data~appl-arguments perm)))
	  t))
	((term~set-p perm)                                       ;;; permutations is a (SET ...) of cycles
	 (values
	  (format nil "~{~A~}" (mapcar #'perm=perm2gap (term~normalform perm)))
	  nil))
	((data~equal perm perm*identity-perm)
	 (values
	  (format nil "()")
	  nil))
	((and (consp perm)(perm~cycle-type-p (car perm)))    ;;; permutations is a list of cycles
	 (values
	  (format nil "~{~A~}" (mapcar #'perm=perm2gap   perm))
	  nil))
	(t (values (perm=cycle2gap perm) nil))))    ;;; at last, a single cycle

(defun perm=cycle2gap (cycle &optional (gap-cycle nil))
  (declare (edited  "14-SEP-2002")
	   (authors Pollet)
	   (input   "A term representing a cycle, either a cons-term or cyc special term."
		    "The optional argument is for the tail recursion only.")
	   (effect  "None.")
	   (value   "The string with the GAP representation of the cycle."))
  (cond ((term~cyc-p cycle)
	 (let ((cy (term~normalform cycle)))
	   (format nil "(~{~A~^,~})" cy)))
	((consp cycle)
	 (format nil "(~{~A~^,~})" cycle))
	((and (term~appl-p cycle)
	      (data~schema-equal (data~appl-function cycle) perm*cons))
	 (perm=cycle2gap (second (data~appl-arguments cycle))
			 (cons (first (data~appl-arguments cycle)) gap-cycle)))
	((data~equal cycle perm*nil)
	 (let    ((result (reverse gap-cycle)))
	   (format nil "(~{~A~^,~}~A)" result)))
	(t gap-cycle)))
    
#|
(defun perm=perm2gap (perm)
  (declare (edited  "12-JUL-2002")
	   (authors Vxs)
	   (input   "A list representing a permutation or a composition of permutations.")
	   (effect  "None.")
	   (value   "Two values:"
		    "- A string containing a GAP representation of the permutation or the composition."
		    "- A boolean, which is T if the premutation is a composition."))
  (cond ((equal (car perm) 'cycle-compose)   ;;; permutations is compositions of cycles
	 (values
	  (format nil "~A~A" (perm=perm2gap (cadr perm)) (perm=perm2gap (caddr perm)))
	  nil))
	((equal (car perm) 'perm-compose)    ;;; composition of two distinct permutations
	 (values
	  (format nil "~A * ~A" (perm=perm2gap (cadr perm)) (perm=perm2gap (caddr perm)))
	  t))
	((equal (car perm) 'perm-exp)        ;;; permutation contains an exponentiation of its cycles
	 (values
	  (format nil "(~A ^ ~A)" (perm=perm2gap (cadr perm)) (caddr perm))
	  t))
	(t (values (perm=cycle2gap perm) nil))))    ;;; at last, a single cycle


(defun perm=cycle2gap (cycle &optional (gap-cycle nil))
  (cond ((equal (car cycle) 'cons)
	 (perm=cycle2gap (caddr cycle)
			(cons (cadr cycle) gap-cycle)))
	((equal (car cycle) 'head)
	 (let ((felem (cadr cycle))
	       (result (reverse gap-cycle)))
	   (unless (equal (car result) felem)
	     (omega~error "PERM=CYCLE2GAP: Malformed cycle!"))
	   (format nil "(~{~A,~}~A)" (butlast result) (car (last result)))))
	(t gap-cycle)))
|#
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Translating generating sets to GAP syntax
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric perm~generating-set2gap (set)
  (declare (edited  "22-JUL-2002")
	   (authors Sorge)
	   (input   "A post representation of a set of generators for a permutation group.")
	   (effect  "None.")
	   (value   "A string containing the GAP representation of the set of generators."))
  (:method ((set term+term))
	   (if (perm~perm-set-type-p set)
	       (perm=generating-set2gap set)
	     (omega~error "PERM~~GENERATING-SET2GAP: Formula ~A is not of permutation type." set)))
  (:method ((set term+set))  ;;special case {id} -> <id>
	   (if (data~equal
		(post~read-object '(set identity-perm) (th~env perm*permutation-theory) :existing-term)
		set)
	       (perm=generating-set2gap
		(term~appl-create perm*generated-set
				  (list set)))))
  (:method ((set cons))
	   (perm~generating-set2gap (post~read-object set (th~env perm*permutation-theory) :existing-term)))
  (:method ((set string))
	   (perm~generating-set2gap (read-from-string set)))
  (:method (set)
	   (omega~error "PERM~~GENERATING-SET2GAP: ~A is an illegal permutation object." set)))

(defun perm=generating-set2gap (set)
  (declare (edited  "22-JUL-2002")
	   (authors Sorge)
	   (input   "A term of a generating set.")
	   (effect  "None.")
	   (value   "A string containing the GAP representation of the generating set."))
  (if (and (data~appl-p set)
	   (data~equal (data~appl-function set) perm*generated-set))
      (let* ((setofperms (car (data~appl-arguments set)))
	     (elements 	 (perm=decompose-set  setofperms))
	     (gap-elems  (mapcar #'perm~permutation2gap elements)))
	(when gap-elems
	  (format nil "[~{~A~^,~}]" gap-elems)))
    (omega~error "PERM=GENERATING-SET2GAP: Malformed generating set!")))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Translating GAP Permutations to POST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun perm~permutation2post (perm)
  (declare (edited  "23-JUL-2002")
	   (authors Sorge)
	   (input   "A string containing a GAP permutation.")
	   (effect  "None.")
	   (value   "A list or atom containing a POST representation of a permutation."))
  (if (equal perm "()")
      'identity-perm
    (let ((number-lists (mapcar #'(lambda (x)
				    (remove-if #'(lambda (y) (equal y "")) (atptop~divide-string x #\,)))
				(remove-if #'(lambda (x) (equal x ""))
					   (atptop~divide-string perm #\) :ignore-char-list '(#\())))))
      (perm=compose-cycles (mapcar #'perm=build-cycle number-lists)))))


(defun perm=build-cycle (numbers)
  (declare (edited  "24-JUL-2002")
	   (authors Sorge)
	   (input   "A list of numbers and the first number.")
	   (effect  "None.")
	   (value   "A list with a POST representation of a cycle."))
  (when numbers
    (cons 'CYC
	  (mapcar #'(lambda (n)
		      (etypecase n
			(string (read-from-string  n))
			(symbol n)
			(number n)))
		  numbers))))


(defun perm=compose-cycles (cycles)
  (declare (edited  "24-JUL-2002")
	   (authors Sorge)
	   (input   "A list of cycles.")
	   (effect  "None.")
	   (value   "A list with a POST representation of a set of cycles."))
  (cons 'SET cycles))
  
(defun perm=compose-permutations (perms)
  (declare (edited  "24-JUL-2002")
	   (authors Sorge)
	   (input   "A list of permutations.")
	   (effect  "None.")
	   (value   "A list with a POST representation of the composed permutations."))
  (if (null (cdr perms))
      (car perms)
    (list 'perm-compose
	  (perm=compose-permutations (cdr perms))  ;; we have to swap the arguments of perm-compose!
	  (car perms))))

(defun perm=exponent-permutation (perm exp)
  (declare (edited  "24-JUL-2002")
	   (authors Sorge)
	   (input   "A permutation representation and a number.")
	   (effect  "None.")
	   (value   "A list with a POST representation of exponentiated permutation."))
  (if (integerp exp)
      (list 'perm-exp perm exp)
    (omega~error "PERM=EXPONENT-PERMUTATION: ~A is not a legal exponent for a permutation.")))

#|
(defun perm=build-cycle (numbers &optional (first (car numbers)))
  (declare (edited  "24-JUL-2002")
	   (authors Sorge)
	   (input   "A list of numbers and the first number.")
	   (effect  "None.")
	   (value   "A list with a POST representation of a cycle."))
  (if numbers
      (let ((felem (car numbers)))
	(list 'cons (etypecase felem
		      (string (read-from-string (car numbers)))
		      (symbol felem)
		      (number felem))
	      (perm=build-cycle (cdr numbers) first)))
    (list 'head (etypecase first
		      (string (read-from-string first))
		      (symbol first)
		      (number first)))))


(defun perm=compose-cycles (cycles)
  (declare (edited  "24-JUL-2002")
	   (authors Sorge)
	   (input   "A list of cycles.")
	   (effect  "None.")
	   (value   "A list with a POST representation of the composed cycles."))
  (if (null (cdr cycles))
      (car cycles)
    (list 'cycle-compose
	  (car cycles)
	  (perm=compose-cycles (cdr cycles)))))
  
(defun perm=compose-permutations (perms)
  (declare (edited  "24-JUL-2002")
	   (authors Sorge)
	   (input   "A list of permutations.")
	   (effect  "None.")
	   (value   "A list with a POST representation of the composed permutations."))
  (if (null (cdr perms))
      (car perms)
    (list 'perm-compose
	  (car perms)
	  (perm=compose-permutations (cdr perms)))))
  

(defun perm=exponent-permutation (perm exp)
  (declare (edited  "24-JUL-2002")
	   (authors Sorge)
	   (input   "A permutation representation and a number.")
	   (effect  "None.")
	   (value   "A list with a POST representation of exponentiated permutation."))
  (if (integerp exp)
      (list 'perm-exp perm exp)
    (omega~error "PERM=EXPONENT-PERMUTATION: ~A is not a legal exponent for a permutation.")))
|#  

(defgeneric perm~generator-combination2post (set exp-list)
    (declare (edited  "24-JUL-2002")
	   (authors Sorge)
	   (input   "A generating set and a list of pairs of indices and exponents.")
	   (effect  "None.")
	   (value   "A list with the POST representation of the composition of the generators."))
  (:method ((set term+term) (exp-list list))
	   (perm~generator-combination2post (read-from-string (post~string set)) exp-list))
  (:method ((set cons) (exp-list list))
	   (if (equal (car set) 'generated-set)
	       (let* ((elements (perm=decompose-set (cadr set))))
		 (perm=compose-permutations
		  (mapcar #'(lambda (pair)
			      (cond ((eq (first pair) 'identity)                  ;; explicit identity 
				     'identity-perm)
				    ((or (= (length pair) 1)(= (second pair) 1))  ;; if the pair has no exp or exp=1 return just perm
				     (nth (1- (first pair)) elements))
				    ((= (second pair) 0)                          ;; perm^0 is identity
				     'identity-perm)
				    (T
				     (perm=exponent-permutation
				      (nth (1- (first pair)) elements)
				      (second pair)))))
			 exp-list)))
	     (omega~error "PERM~~GENERATOR-COMBINATION2POST: Malformed generating set!")))
  (:method (set exp-list)
	   (omega~error "PERM~~GENERATOR-COMBINATION2POST: Unusable input ~A and ~A" set exp-list)))


(defun perm=gap-perm2exp-list (str)
  (if (stringp str)
      (mapcar #'(lambda (perm) (mapcar #'read-from-string (atptop~divide-string perm #\^)))
	      (atptop~divide-string (substitute #\SPACE #\<  (substitute #\SPACE #\> (substitute #\SPACE #\. str))) #\* ))
    (perm=gap-perm2exp-list (format nil "~A" str))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Query Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun perm~gap-is-cycle (cycle)
  (declare (edited  "24-DEC-2002")
	   (authors Vxs)
	   (input   "A cycle as POST term.")
	   (effect  "Calls GAP.")
	   (value   "T if CYCLE is a valid cycle."))
  (let* ((p (perm~cycle2gap cycle))
	 (result (when p (rcl~call-gap (format nil "IsPerm(~A)" p)))))
    (when (string-equal result "true") t)))

(defun perm~gap-is-perm (perm)
  (declare (edited  "24-DEC-2002")
	   (authors Vxs)
	   (input   "A permutation as POST term.")
	   (effect  "Calls GAP.")
	   (value   "T if PERM is a valid permutation."))
  (let* ((p (perm~permutation2gap perm))
	 (result (when p (rcl~call-gap (format nil "IsPerm(~A)" p)))))
    (when (string-equal result "true") t)))


(defun perm~gap-equal (perm1 perm2)
  (declare (edited  "19-JUL-2002")
	   (authors Sorge)
	   (input   "Two permutations as POST terms.")
	   (effect  "Calls GAP.")
	   (value   "T if the two permutations are equal, o/w NIL"))
  (let* ((p1 (perm~permutation2gap perm1))
	 (p2 (and p1 (perm~permutation2gap perm2)))
	 (result (and p1 p2 (rcl~call-gap (format nil "~A = ~A" p1 p2)))))
    (cond ((string-equal (subseq result (- (length result) 4) (length result))
			 "true") t)
	  ((string-equal (subseq result (- (length result) 5) (length result)) "false") nil)
	  ((null result) nil)
	  (t (omega~error "GAP could not process the input permutation ~A and ~A" perm1 perm2)))))

(defun perm~gap-simplify (perm)
  (declare (edited  "23-JUL-2002")
	   (authors Sorge)
	   (input   "A permutation as POST term.")
	   (effect  "Calls GAP.")
	   (value   "Two values:"
		    "1) A post term permutation simplified by GAP."
		    "2) T if the simplified permutation really differs from the original input. O/w NIL."))
  (let* ((p (perm~permutation2gap perm))
	 (result (and p (rcl~call-gap (format nil "~A" p))))
	 (post-term (and result (perm~permutation2post result))))
    (if result
	(values
	 (post~read-object post-term (th~env perm*permutation-theory) :existing-term)
	 (not (equal perm post-term)))
      (omega~error "GAP could not process the input permutation ~A" perm))))


(defun perm~gap-apply (perm elem)
  (declare (edited  "23-JUL-2002")
	   (authors Pollet)
	   (input   "A permutation as POST term and number.")
	   (effect  "Calls GAP.")
	   (value   "The result of the application of the permutation to the element."))
  (let* ((p (perm~permutation2gap perm))
	 (result (and p (rcl~call-gap (format nil "~A^~A" elem p)))))
    (if result
	 (post~read-object (read-from-string result) (th~env perm*permutation-theory) :existing-term)
      (omega~error "GAP could not process the input permutation ~A" perm))))

;;; Stuff to show that p is in G

(defun perm~gap-is-in-proof (perm set)
  (declare (edited  "24-JUL-2002")
	   (authors Sorge)
	   (input   "A permutation and a set of generators as POST terms.")
	   (effect  "None.")
	   (value   "A combination of generating elements comprising the permutation,"
		    "if such a combination exists. O/w NIL." ))
  (let ((id? (perm=identity-hack perm set)))
    (if id?
	id?
      (when (perm~load-gap-file perm*gap-file)
	(let* ((p (perm~permutation2gap perm))
	       (s (and p (perm~generating-set2gap set)))
	       (result (and s (rcl~call-gap (format nil "IsIn_Proof(~A,GroupByGenerators(~A),\"\")" p s)))))
	  (when result
	    (let* ((exp-list ;(mapcar #'read-from-string (atptop~divide-string result #\, :ignore-char-list '(#\[ #\]))))
		    (perm=gap-perm2exp-list result))
		   (post-term (perm~generator-combination2post set exp-list)))
	      (when post-term
		(post~read-object post-term (th~env perm*permutation-theory) :existing-term)))))))))

(defun perm=identity-hack (perm set)
  (declare (edited  "27-NOV-2002")
	   (authors Pollet)
	   (input   "A permutation and a generating set as terms.")
	   (effect  "Calls gap.")
	   (value   "If the permutation is equal to the identity, then a permutation composed"
		    "from the permutation of the generating set that is equal to id."))  
  (when (and (perm~gap-equal perm perm*identity-perm) 
	     (perm~generating-set2gap set))
    (let* ((set-of-perms (term~normalform (car (data~appl-arguments set))))
	   (minperm (car (sort set-of-perms
			       #'(lambda (x y)
				   (< (apply #'lcm (mapcar #'(lambda (l)(length (term~normalform l))) (term~normalform x)))
				      (apply #'lcm (mapcar #'(lambda (l)(length (term~normalform l))) (term~normalform y))))))))
	   (exp (apply #'lcm (mapcar #'(lambda (cyc) (length (term~normalform cyc))) (term~normalform minperm)))))
      (term~appl-create perm*perm-exp
			(list minperm
			      (post~read-object exp (th~env perm*permutation-theory) :existing-term))))))
    


;;; Compute the set of elements, that are in the orbit

(defun perm~gap-set-of-orbit (orbit)
  (declare (edited  "10-OCT-2002")
	   (authors Pollet)
	   (input   "A term (g-orbit (generated-set ...) op elem).")
	   (effect  "None.")
	   (value   "The set of elements that are the orbit of elem under op."))
  (when (and (perm~load-gap-file perm*gap-file)
	     (data~appl-p orbit)
	     (data~schema-equal (data~appl-function orbit) perm*g-orbit)
	     (= (length (data~appl-arguments orbit)) 3))
    (let* ((args (data~appl-arguments orbit))
	   (genset (perm~generating-set2gap (car args)))
	   (elem (and genset (keim~name (caddr args))))
	   (result (and (numberp elem) (rcl~call-gap (format nil "Orbit(GroupByGenerators(~A),~A)" genset elem)))))
      (when result
	(let ((number-list (mapcar #'read-from-string (atptop~divide-string result #\, :ignore-char-list '(#\[ #\])))))
	  (when number-list
	    (post~read-object number-list (th~env perm*permutation-theory) :set)))))))
	
(defun perm~gap-orbit-proof (orbit)
  (declare (edited  "10-OCT-2002")
	   (authors Pollet)
	   (input   "A term (g-orbit (generated-set ...) op elem).")
	   (effect  "None.")
	   (value   "The set of elements that are the orbit of elem under op."))
  (when (and (perm~load-gap-file perm*gap-file)
	     (data~appl-p orbit)
	     (data~schema-equal (data~appl-function orbit) perm*g-orbit)
	     (= (length (data~appl-arguments orbit)) 3))
    (let* ((args (data~appl-arguments orbit))
	   (genset (perm~generating-set2gap (car args)))
	   (elem (and genset (keim~name (caddr args))))
	   (result (and (numberp elem) (rcl~call-gap (format nil "Orbit_Proof(GroupByGenerators(~A),~A,\"\")" genset elem)))))
      (when result
	(let* ((output (read-from-string (substitute #\SPACE #\<  (substitute #\SPACE #\> (substitute #\SPACE #\.      ;;only for <identity ...>
					 (substitute #\( #\[  (substitute #\) #\] (substitute #\SPACE #\, result))))))))
	       ;(dummy  (omega~trace "~A" output))
	       (triples (mapcar #'(lambda (out)
				(destructuring-bind (elem perm &rest orb)
				    out
				    (list elem (perm~generator-combination2post (car args)  (perm=gap-perm2exp-list perm)) orb)))
				output)))
	  (when triples
	    (mapcar #'(lambda (triple) 
			(mapcar #'(lambda (tri)
				    (post~read-object tri (th~env perm*permutation-theory) :existing-term))
				(butlast triple)))
		    triples)))))))


;;; Compute the set of permutations of the stabiliser

(defun perm~gap-set-of-stabiliser (stab)
  (declare (edited  "10-OCT-2002")
	   (authors Pollet)
	   (input   "A term (stabiliser (generated-set ...) action elem).")
	   (effect  "None.")
	   (value   "The a subgroup that is the stabiliser."))
  (when (and (perm~load-gap-file perm*gap-file)
	     (data~appl-p stab)
	     (data~schema-equal (data~appl-function stab) perm*stabiliser)
	     (= (length (data~appl-arguments stab)) 3))
    (let* ((args (data~appl-arguments stab))
	   (genset (perm~generating-set2gap (car args)))
	   (elem (and genset (keim~name (caddr args))))
	   (result (and (numberp elem) (rcl~call-gap (format nil "Stabilizer(GroupByGenerators(~A),~A)" genset elem)))))
      (when result
	(if (string= (subseq result 0 5) "Group")
	    (let*
		((trimmed-result (string-left-trim "Group" result))
		 (env (th~env perm*permutation-theory))
		 (perm-list  (if (string= (subseq trimmed-result 0 4) "(())")
				 (list 'set 'identity-perm)
			     (perm=stabilizer2stabiliser trimmed-result))))
	      (post~read-object perm-list env :existing-term))
	(omega~error "Don't know how to interpret GAP result ~A" result))))))


(defun perm=stabilizer2stabiliser (str &optional elem cyc perm set)
  (cond ((or (eq (elt str 0) #\])(eq (elt str 0) #\[)(eq (elt str 0) #\SPACE)(eq (elt str 0) #\CR)(eq (elt str 0) #\())
	 (perm=stabilizer2stabiliser (subseq str 1) elem cyc perm set))
	((and elem (eq (elt str 0) #\,))
	 (perm=stabilizer2stabiliser (subseq str 1) nil (cons (read-from-string elem) cyc) perm set))
	((and elem (eq (elt str 0) #\)))
	 (perm=stabilizer2stabiliser (subseq str 1) nil nil (cons (cons 'cyc (reverse (cons (read-from-string elem) cyc))) perm) set))
	((eq (elt str 0) #\,)
	 (perm=stabilizer2stabiliser (subseq str 1) nil nil nil (cons (cons 'set (reverse  perm)) set)))
	((eq (elt str 0) #\))
	 (list 'generated-set (cons 'set (reverse (cons (cons 'set (reverse  perm)) set)))))
	(T (perm=stabilizer2stabiliser (subseq str 1) (concatenate 'string elem (subseq str 0 1)) cyc perm set))))
	  
;;; Compute a concrete g-orbit-reprentation function

(defun perm~gap-g-orbit-representation (repr)
  (declare (edited  "10-OCT-2002")
	   (authors Pollet)
	   (input   "A term (g-orbit-representation (generated-set ...) action elem).")
	   (effect  "None.")
	   (value   "The concrete function that is a g-orbit-representation."))
  (when (and (perm~load-gap-file perm*gap-file)
	     (data~appl-p repr)
	     (data~schema-equal (data~appl-function repr) perm*g-orbit-representation)
	     (= (length (data~appl-arguments repr)) 3))
    (let* ((args (data~appl-arguments repr))
	   (genset (perm~generating-set2gap (car args)))
	   (set (perm=decompose-set (car (data~appl-arguments (car args)))))
	   (elem (and genset (keim~name (caddr args))))
	   (result (and (numberp elem) (rcl~call-gap (format nil "SchreierData(GroupByGenerators(~A),~A)" genset elem)))))
      (when result
	(if (string= (subseq result 0 1) "[")
	    (destructuring-bind (orb v w)
		 (read-from-string (substitute #\SPACE #\, (substitute #\( #\[ (substitute #\) #\]  result))))
	      (post~read-object 
	       (list 'lam '(x num)
		     (reduce #'(lambda (x y) (list 'ifthen (list '= 'x y) (perm=orbit-representation-value y orb v w set) x))
			     (rest orb) :initial-value 'identity-perm))
	       (th~env perm*permutation-theory) :existing-term))
	(omega~error "Don't know how to interpret GAP result ~A" result))))))

(defun perm=orbit-representation-value (elem orb v w set &optional func)
  (declare (edited  "25-NOV-2002")
	   (authors Pollet)
	   (input   "A number, the orbit and the Schreier-vectors V and W as lists of numbers.")
	   (effect  "-")
	   (value   "Returns a POST expression representing a permutation that maps the given number to the"
		    "base element of the Schreier tree."))
  (let* ((pos (position elem orb))
	 (vvalue (nth pos v))
	 (wvalue (nth pos w)))
;    (omega~trace "~A ~A ~A" pos vvalue wvalue)
    (if (and (= 0 vvalue) (= 0 wvalue))
	func
      (let ((perm (if (posint vvalue)
		      (nth (1- vvalue) set)
		    (list 'perm-inverse (nth (1- (abs vvalue)) set)))))
	(perm=orbit-representation-value wvalue orb v w set
					 (if func
					     (list 'perm-compose  func perm)
					   perm))))))

(defun perm~gap-base-of-group (term)
  (declare (edited  "10-OCT-2002")
	   (authors Pollet)
	   (input   "A term (generated-set ...).")
	   (effect  "None.")
	   (value   "A term-list of elements that is a base."))
  (when (perm~load-gap-file perm*gap-file)
    (let* ((genset (perm~generating-set2gap term))
	   (result (and genset (rcl~call-gap (format nil "BaseOfGroup(GroupByGenerators(~A))" genset)))))
      (when result
	(let ((number-list (read-from-string (substitute #\SPACE #\, (substitute #\( #\[ (substitute #\) #\]  result))))))
	  (when number-list
	    (post~read-object number-list (th~env perm*permutation-theory) :set)))))))


(defun perm~gap-order (term)
  (declare (edited  "23-JAN-2003")
	   (authors Pollet)
	   (input   "A term (cardinality (generated-set ...)).")
	   (effect  "None.")
	   (value   "The order of the generated set."))
  (when (and (perm~load-gap-file perm*gap-file)
	     (data~appl-p term)
	     (data~schema-equal (data~appl-function term)
				(env~lookup-object 'cardinality (th~env perm*permutation-theory))))
    (let* ((args (data~appl-arguments term))
	   (genset (perm~generating-set2gap (car args)))
	   (result (rcl~call-gap (format nil "Order(GroupByGenerators(~A))" genset ))))
      (when result
	(let ((number (read-from-string result)))
	  (when number
	    (post~read-object number (th~env perm*permutation-theory) :existing-term)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other Interface Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric perm~decompose-into-permutations (comp)
  (declare (edited  "10-AUG-2002")
	   (authors Vxs)
	   (input   "A composition of permutations.")
	   (effect  "None.")
	   (value   "A list containing the single permutations contained in the composition."))
  (:method ((comp term+term))
	   (if (perm~perm-type-p comp)
	       (perm~decompose-into-permutations (read-from-string (post~string comp)))
	     (omega~error "PERM~~DECOMPOSE-INTO-PERMUTATIONS: Formula ~A is not of permutation type." comp)))
  (:method ((comp cons))
	   (let ((result (perm=decompose-into-permutations comp)))
	     (if result
		 (mapcar #'(lambda (x)
			     (post~read-object x (th~env perm*permutation-theory) :existing-term))
			 (remove-duplicates result :test #'equal))
	       (omega~warn "PERM~~DECOMPOSE-INTO-PERMUTATIONS: Permutation ~A could not be decomposed." comp))))
  (:method ((comp symbol))
	   (let ((result (perm=decompose-into-permutations comp)))
	     (if result
		 (mapcar #'(lambda (x)
			     (post~read-object x (th~env perm*permutation-theory) :existing-term))
			 (remove-duplicates result :test #'equal))
	       (omega~warn "PERM~~DECOMPOSE-INTO-PERMUTATIONS: Permutation ~A could not be decomposed." comp))))
  (:method ((comp string))
	   (perm~decompose-into-permutations (read-from-string comp)))
  (:method (comp)
	   (omega~error "PERM~~DECOMPOSE-INTO-PERMUTATIONS: ~A is an illegal permutation object." comp)))

(defun perm=decompose-into-permutations (comp)
  (declare (edited  "10-AUG-2002")
	   (authors Vxs)
	   (input   "A list containing a composition of permutations.")
	   (effect  "None.")
	   (value   "A list containing the single permutations contained in the composition."))
  (cond ((and (atom comp) (equal comp 'identity-perm))
	 (list comp))
	((equal (car comp) 'perm-compose)    ;;; composition of two distinct permutations
	 (append (perm=decompose-into-permutations (cadr comp)) (perm=decompose-into-permutations (caddr comp))))
	((equal (car comp) 'perm-exp)        ;;; permutation contains an exponentiation of its cycles
	 (perm=decompose-into-permutations (cadr comp)))
	(T    ;;; permutation is compositions of cycles or permutation is single cycle
	 (list comp))
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auxiliary Predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun perm~cycle-type-p (perm)
  (declare (edited  "22-JUL-2002")
	   (authors Sorge)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "T if term is of cycle type (i.e. type cyc), o/w NIL."))
  (keim~equal (term~type perm)
	      (post~read-object perm*permutation-type
				(th~env perm*permutation-theory)
				:existing-type)))

(defun perm~perm-type-p (perm)
  (declare (edited  "22-JUL-2002")
	   (authors Sorge)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "T if term is of permutation type, o/w NIL."))
  (keim~equal (term~type perm)
	      (post~read-object (list 'o perm*permutation-type)
				(th~env perm*permutation-theory)
				:existing-type)))

(defun perm~perm-set-type-p (set)
  (declare (edited  "22-JUL-2002")
	   (authors Sorge)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "T if term is a set of permutations, o/w NIL."))
  (keim~equal (term~type set)
	      (post~read-object (list 'o (list 'o perm*permutation-type))
				(th~env perm*permutation-theory)
				:existing-type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auxiliary Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun perm=list2pair-list (list)
  (declare (edited  "24-JUL-2002")
	   (authors Sorge)
	   (input   "A list.")
	   (effect  "None.")
	   (value   "If list has an even number of elements, a list containing pairs of elements:"
		    "((2k+1 2k+2) (2k+3 2k+4) ....) with k=0,...,n/2, where n=|list|."
		    "Returns NIL if list is odd."))
  (when (evenp (length list))
    (cond ((null list) nil)
	  ((null (cddr list)) (list list))
	  (t (cons (list (first list) (second list)) (perm=list2pair-list (cddr list)))))))

(defun perm=decompose-set (set)
  (declare (edited  "22-JUL-2002")
	   (authors Sorge)
	   (input   "A list containing a post representation of a set of permutations.")
	   (effect  "None.")
	   (value   "A list containing only the post representations of the permutations."))
  (typecase set
    (term+list (mapcar #'post~string (data~appl-arguments set)))
    (term+term (perm=decompose-set (read-from-string (post~string set))))
    (t
     (labels ((get-element (element variable)
			   (if (equal (car element) '=)
			       (let ((left (second element))
				     (right (third element)))
				 (cond ((equal left variable) (list right))
				       ((equal right variable) (list left))
				       (t (omega~error "PERM=DECOMPOSE-SET: Error in set equation ~A." element))))
			     (omega~error "PERM=DECOMPOSE-SET: Error in set representation (~A)." element)))
	   (recurse-set (elements variable)
			(if (equal (car elements) 'or)
			    (let ((ldisj (recurse-set (second elements) variable))
				  (rdisj (recurse-set (third elements) variable)))
			      (when (and ldisj rdisj)
				(append ldisj rdisj)))
			  (get-element elements variable))))
  (cond ((equal (car set) 'singleton)          ;;; (singleton (1,2,3))
	 (cdr set))
	((equal (car set) 'lam)                ;;; (lam (x perm) (or (= x (1,2,3)....)))
	 (let ((variable (car (second set)))
	       (elements (third set)))
	   (recurse-set elements variable)))
	((equal (car set) 'set)                ;;; (SET ...)
	 (rest set))
	(t                                     ;;; Any other form?
	 (omega~error "PERM=DECOMPOSE-SET: Unknown set representation of ~A." set))
	)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loading External Files into GAP
;; (Maybe this all should go somewhere into the actual GAP module.)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar perm*gap-files (make-hash-table :test #'equal)
  "A variable containing a list of external files already loaded into the current GAP.")

(defun perm~load-gap-file (file)
  (declare (edited  "24-JUL-2002")
	   (authors Sorge)
	   (input   "A filename.")
	   (effect  "Loads file into the running GAP if it was not yet loaded or"
		    "if it is newer than the same file already loaded."
		    "It then stores the file into the file-hashtable.")
	   (value   "T if file was successfully loaded or was already loaded into GAP."))
  (when (probe-file file)
    (let ((old-date (gethash file perm*gap-files))
	  (new-date (file-write-date file)))
      (when (or (null old-date) (< old-date new-date))
	(rcl~call-gap (format nil "Read(~S)" file))
	(setf (gethash file perm*gap-files) new-date))
	t)))

	 




#|
Tests
(perm~gap-is-in-proof '(set (cyc 1 2 3)) '(generated-set (set (set (cyc 1 2))(set (cyc 1 2 3 4 5)))))
(perm~gap-is-in-proof (post~read-object '(set (cyc 1 2 3)) (th~env perm*permutation-theory) :existing-term)
		      (post~read-object '(generated-set (set (set (cyc 1 2))(set (cyc 1 2 3 4 5))))
					(th~env perm*permutation-theory) :existing-term))
(perm~gap-set-of-orbit (post~read-object '(g-orbit
					    (generated-set (set (set (cyc  1 2 3))(set (cyc 1 4))))
					    perm-apply 1)
					(th~env perm*permutation-theory) :existing-term))
(perm~gap-set-of-stabiliser  (post~read-object '(stabiliser 
					    (generated-set (set (set (cyc  1 2 3))(set (cyc 1 4))))
					    perm-apply 1)
					(th~env perm*permutation-theory) :existing-term))
(perm~gap-g-orbit-representation  (post~read-object '(g-orbit-representation
					    (generated-set (set (set (cyc  1 2 3))(set (cyc 1 4))))
					    perm-apply 1)
					(th~env perm*permutation-theory) :existing-term))
(perm~gap-orbit-proof 
 (post~read-object '(g-orbit (generated-set (SET (SET (CYC 1 10) (CYC 2 8) (CYC 3 11) (CYC 5 7)) (SET (CYC 1 4 7 6) (CYC 2 11 10 9))))
			     perm-apply 1)
		   (th~env perm*permutation-theory) :existing-term))
(perm~gap-order (post~read-object '(cardinality
					    (generated-set (set (set (cyc  1 2 3))(set (cyc 1 4)))))
					(th~env perm*permutation-theory) :existing-term))
|#
