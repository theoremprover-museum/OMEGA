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

;; This is the file for the datastructures and algorithms for a refutation-graph.

(in-package :omega)



(mod~defmod REF 
            :uses (cl data keim lit pos subst term toset)
            :documentation "Datastructures and Algorithms for refutation graphs."
            :exports (
                      ref+link
                      ref+refutation-graph
                      
                      ref~add-clause-to-graph
                      ref~add-link-to-graph
                      ref~add-literal-to-graph
                      ref~append-renaming
                      ref~append-substitution
                      ref~clause-node-list-p
                      ref~clause-nodes
                      ref~combine-links
                      ref~empty-refutation-graph
                      ref~get-links-with-literal
                      ref~get-literal-with-position
                      ref~get-literals-from-link
                      ref~get-literals-with-shore-list
                      ref~get-variables-from-clause
                      ref~get-variables-from-link
                      ref~get-variables-from-list-of-literals
                      ref~get-variables-from-literal
                      ref~graph-renaming
                      ref~ground-substitution
                      ref~incompatible-substitutions-p
                      ref~instantiate-term
                      ref~inverse-substitution
                      ref~link-create
                      ref~link-empty-p
                      ref~link-is-in-graph-p
                      ref~link-list-is-in-graph-p
                      ref~link-list-p
                      ref~links
                      ref~literal-list-p
                      ref~negative-shore
                      ref~positive-shore
                      ref~print-object
                      ref~print-object-to-file
		      ref~refutation-graph-create
                      ref~refutation-graph-p
                      ref~remove-clause-from-graph
                      ref~remove-clause-list-from-graph
                      ref~remove-link-from-graph
                      ref~remove-link-list-from-graph
                      ref~remove-literal-from-graph
                      ref~remove-literal-from-negative-shore-of-link
                      ref~remove-literal-from-positive-shore-of-link
                      ref~remove-literal-list-from-graph
                      ref~remove-object
                      ref~remove-object-from-graph
                      ref~renamings
                      ref~replace-clause
                      ref~replace-literal
                      ref~replace-literals
                      ref~shore-list-p
                      ref~subst-remove-list-of-components
                      ref~substitution
                      ref~update-ground-subst-and-link-subst-after-removing-literal-from-link
                      
                      ref*graph-links))






;************************ DEFINITIONS of CLASSES ****************

(eval-when (load compile eval)
(defclass ref+refutation-graph (toset+set)
  ((clause-nodes  :initarg :clause-nodes
		  :initform NIL
		  :accessor ref~clause-nodes
		  :documentation "This is the set of clause nodes in the refutation graph.")
   (links         :initarg :links
		  :initform NIL
		  :accessor ref~links
		  :documentation "This is the set of links in the refutation graph.")
   (renamings    :initarg :renamings
		 :initform NIL
		 :accessor ref~renamings
		 :documentation "This is the set of renamings in the refutation graph.")
   (ground-substitution :initarg :ground-substitution
			:initform (subst~create nil nil)
			:accessor ref~ground-substitution
			:documentation "This is the  ground-substitution for the refutation graph."))
  (:documentation "This is the structure for a refutation graph."))


(defclass ref+link (toset+set)
  ((positive-shore :initarg :positive-shore
		   :initform NIL
		   :accessor ref~positive-shore
		   :documentation "This is a set of the positive shore of a link.")
   (negative-shore :initarg :negative-shore
		   :initform NIL
		   :accessor ref~negative-shore
		   :documentation "This is a set of the negative shore of a link.")
   (substitution :initarg :substitution
		 :initform (subst~create nil nil)
		 :accessor ref~substitution
		 :documentation "This is the substitution  of a link."))
  (:documentation "This is the structure for a link in a refutation graph.")))

;*********************** MAKE-INSTANCE OF CLASSES *********************


(defgeneric ref~refutation-graph-create (name clause-nodes links renamings ground-substitution)
  (declare (edited  "03-SEP-1992 18:00")
	   (authors ACSEHN)
	   (input "a name, a set of clause-nodes, a set of links, a renaming-sustitution and a ground-substitution"  )
	   (effect "none." )
	   (value "An instance of a refutation-graph."  ))
  (:method (name clause-nodes-list link-list renamings (ground-substitution subst+substitution))
	   (if (and (ref~clause-node-list-p clause-nodes-list)
		    (ref~link-list-p link-list))
	       (make-instance 'ref+refutation-graph
			      :clause-nodes clause-nodes-list
			      :links link-list
			      :renamings renamings
			      :ground-substitution (ref=unique-substitution ground-substitution)
			      :name name)
	       (error "one of the components has the wrong type."))))

(defun ref~empty-refutation-graph ()
  (declare (edited  "07-OCT-1992 16:14")
	   (authors ACSEHN)
	   (input "none."  )
	   (effect "none."  )
	   (value "an empty refutation-graph."  ))
  (let ((graph (make-instance 'ref+refutation-graph)))
    (ref=random-name graph "cg-")
    graph))

(defgeneric ref=random-name (object prefix)
  (declare (edited  "07-OCT-1992 16:16")
	   (authors ACSEHN)
	   (input "a keim object and a prefix."  )
	   (effect "the object is given a random name with the prefix." )
	   (value "the new name"  ))
  (:method ((object keim+object) (prefix string))
	   (keim~set-name! object (gentemp prefix))))
  

(defgeneric ref~link-create (name shore substitution)
  (declare (edited  "07-OCT-1992 16:21")
	   (authors ACSEHN)
	   (input "a name, a list of the form ((clause-1 position-1) ... (clause-n position-n)), i.e. a shore, and the substitution for this link."  )
	   (effect "none." )
	   (value "an instance of an link."  ))
  (:method (name shore (substitution subst+substitution))
	   (if (ref~shore-list-p shore)
	       (let ((positive-shore-list (remove-if-not #'lit~positive-p (ref~get-literals-with-shore-list shore)))
		     (negative-shore-list (remove-if #'lit~positive-p (ref~get-literals-with-shore-list shore))))
		 (if (and (ref~literal-list-p positive-shore-list)
			  (ref~literal-list-p negative-shore-list))
		     (make-instance 'ref+link :name (if (null name) (gentemp "link-") name)
				    :positive-shore positive-shore-list
				    :negative-shore negative-shore-list
				    :substitution (ref=unique-substitution substitution))
		     (error "one of the components has the wrong type.")))
	       (error "The shore ~A has not the form ((clause-1 position-1) ... (clause-n position-n))." shore))))

;******************************* OUTPUT FUNCTIONS ****************************

(defun ref~print-object-to-file (object filename)
  (declare (edited  "19-AUG-1992 16:33")
	   (authors ACSEHN)
	   (input "an ref-object and a filename."  )
	   (effect "the ref-object is written on the file."  )
	   (value "nil"  ))
  (with-open-file (stream filename  :direction :output
			  :if-exists :new-version)
    (ref~print-object object stream)))

(defmethod print-object ((object ref+refutation-graph) stream)
  (format stream "|Ref-Graph with Clauses: ~A|" (ref~clause-nodes object))) 

(defgeneric ref~print-object (object stream)
  (declare (edited  "03-SEP-1992 18:00")
	   (authors ACSEHN AAYARI)
	   (input  " an OBJECT ,i.e. refutation graph or link  and a STREAM" )
	   (effect  " output of this object OBJECT in the stream STREAM ")
	   (value   "none" ))
  (:method ((object ref+link) stream)
	   (format stream "~%   name: ~A" (keim~name object))
	   (format stream " ~%   positive-shore:")
	   (ref=print-literal-list (ref~positive-shore object) stream)
	   (format stream " ~%   negative-shore:")
	   (ref=print-literal-list (ref~negative-shore object) stream)
	   (format stream "~%   substitution :")
	   (print-object (ref~substitution  object) stream))
  (:method ((object ref+refutation-graph) stream)
	   (format stream "~% name: ~A" (keim~name object))
	   (format stream "~% clauses: ")
	   (ref=print-clause-list (ref~clause-nodes object) stream)
	   (format stream "~% links:")
	   (ref=print-link-list (ref~links object) stream)
	   (format stream "~% renamings :")
	   (print (ref~renamings object) stream)
	   (format stream "~% substitution :")
	   (print (ref~ground-substitution  object) stream)
	   'nil)
  (:method ((object list) stream)
	   (cond ((ref~link-list-p object) (ref=print-link-list object stream))
		 ((ref~clause-node-list-p object) (ref=print-clause-list object stream))
		 ((ref~literal-list-p object) (ref=print-literal-list object stream)))))

(defmethod print-object ((object ref+link) stream)
  (format stream "{Link:~A <+S: ~A> <-S: ~A>}"
	  (keim~name object) (ref~positive-shore object) (ref~negative-shore object)))

(defun ref=print-literal-list (literal-list  stream)
  (declare (edited  "03-SEP-1992 16:15")
	   (authors ACSEHN AAYARI)
	   (input "a list of literal LITERAL-LIST and a stream STREAM")
	   (effect "output of the list LITERAL-LIST in the stream STREAM" )
	   (value  "none" ))
  (when (ref~literal-list-p literal-list)
    (when (not (null literal-list))
      (format stream "   " )
      (format stream "~A " (ref=find-clause (first literal-list)))
      (ref=print-literal-list (rest literal-list)  stream))))





(defgeneric ref=find-clause (literal)
  (declare (edited  "01-SEP-1992 15:54")
	   (authors ACSEHN)
	   (input "a literal in a link."  )
	   (effect "none" )
	   (value "a list consisting of the clause the literal is in and the position of the literal in the clause."  ))
  (:method ((literal lit+literal))
	   (list (keim~name (lit~clause literal)) (ref=position (position literal (cl~literals (lit~clause literal)) :test #'eq)))))

(defun ref=print-history (object)
  (declare (edited  "03-SEP-1992 16:11")
	   (authors ACSEHN)
	   (input "an object."  )
	   (effect "none." )
	   (value "the history of object."  ))
  (keim~get object 'history))
  




(defun ref=print-clause-list (clause-list  stream)
  (declare (edited  "03-SEP-1992 18:00")
	   (authors AAYARI)
	   (input "a list of clauses CLAUSE-LIST and a stream STREAM")
	   (effect "output of the list CLAUSE-LIST in the stream STREAM" )
	   (value  "none" ))
  (when (ref~clause-node-list-p clause-list)
    (when (not (null clause-list))
      (format stream "~%   " )
      (print-object (first clause-list) stream)
      (format stream " <plist :~A>" (keim~plist (first clause-list)))
      (ref=print-clause-list (rest clause-list)  stream))))


(defun ref=print-link-list (link-list  stream)
  (declare (edited  "03-SEP-1992 18:06")
	   (authors ACSEHN AAYARI)
	   (input "a list of clauses LINK-LIST and a stream STREAM")
	   (effect "output of the list LINK-LIST in the stream STREAM" )
	   (value  "none" ))
  (when (ref~link-list-p link-list)
    (when (not (null link-list))
      (format stream "   " )
      (ref~print-object (first link-list) stream)
      (ref=print-link-list (rest link-list)  stream))))

 

;****************** PREDICATES ********************************

(defun ref~shore-list-p (list)
  (declare (edited  "03-SEP-1992 13:43")
	   (authors ACSEHN)
	   (input "a list."  )
	   (effect "none." )
	   (value "T, iff the list is of the form ((clause-1 position-1) ... (clause-n position-n))."  ))
  (every #'(lambda (pair) (and (cl~clause-p (first pair)) (typep (second pair) 'pos+position))) list))


(defun ref~literal-list-p (object-list)
  (declare (edited  "03-SEP-1992 12:36")
	   (authors ACSEHN)
	   (input "a list."  )
	   (effect "none." )
	   (value "T, iff the list is a list only consisting of lit+literal literals." ))
  (if (listp object-list)
      (every #'(lambda (x) (typep x 'lit+literal)) object-list)
      nil))
  


(defun ref~clause-node-list-p (object-list)
  (declare (edited  "03-SEP-1992 18:00")
	   (authors ACSEHN)
	   (input "a list."  )
	   (effect "none." )
	   (value "T, iff list is a list only consisting of keim clauses."  ))
  (if (listp object-list)
      (every #'(lambda (x) (typep x 'cl+clause)) object-list)
      nil))

(defun  ref~link-list-p (object-list)
  (declare (edited  "03-SEP-1992 18:09")
	   (authors ACSEHN)
	   (input "a list."  )
	   (effect "none." )
	   (value "T, iff the list is a list omly consisting of ref+link links." ))
  (if (listp object-list)
      (every #'(lambda (x) (typep x 'ref+link)) object-list)
      nil))



(defgeneric ref~incompatible-substitutions-p (substitution-1 substitution-2)
  (declare (edited  "16-SEP-1992 09:55")
	   (authors ACSEHN)
	   (input "two substitutions, substitution-1 and substitution-2."  )
	   (effect "none." )
	   (value "T, iff the two substitutions are incompatible, i.e. the codomain of variables occuring in both of the substitutions is  not unifiable."  ))
  (:method ((substitution-1 subst+substitution) (substitution-2 subst+substitution))
	   (let ((vars (intersection (subst~domain substitution-1) (subst~domain substitution-2) :test #'keim~equal)))
	     (notevery #'(lambda (var) (term~unify (subst~get-component var substitution-2) (subst~get-component var substitution-1))) vars))))


(defgeneric ref=link-is-positiv-polylink-p (link)
  (declare (edited  "17-AUG-1992 15:53")
	   (authors AAYARI)
	   (input  " a link ")
	   (effect "none" )
	   (value   " T falls der positive  Shore dieses Link enthaelt mehr als eine Literale" ))
  (:method ((link ref+link))
	   (eq (length (ref~positive-shore link)) 2)))

(defgeneric ref=link-is-negativ-polylink-p (link)
  (declare (edited  "17-AUG-1992 15:53")
	   (authors AAYARI)
	   (input  " a link ")
	   (effect "none" )
	   (value   " T falls der negative Shore dieses Links  enthaelt mehr als eine Literale" ))
  (:method ((link ref+link))
	   (eq (length (ref~negative-shore link)) 2)))

(defgeneric ref=link-is-polylink-p (link)
  (declare (edited  "17-AUG-1992 15:53")
	   (authors AAYARI)
	   (input  " a link ")
	   (effect "none" )
	   (value   " T falls der positive Shore dieses Link enthaelt mehr als eine Literale" ))
  (:method ((link ref+link))
	   (or (ref=link-is-negativ-polylink-p link) (ref=link-is-positiv-polylink-p link))))


(defgeneric ref=literal-is-in-positive-shore-form-link-p (literal link)
  (declare (edited  "19-AUG-1992 18:00")
	   (authors AAYARI)
	   (input  " a link and a literal")
	   (effect "none" )
	   (value   " T iff the literal LITERAL occurs in the positiv shore of the link LINK" ))
  (:method ((literal lit+literal) (link  ref+link))
	   (when (member literal (ref~positive-shore link) :test #'keim~equal ) t)))

(defgeneric ref=literal-is-in-negative-shore-form-link-p (literal link)
  (declare (edited  "19-AUG-1992 18:01")
	   (authors AAYARI)
	   (input  " a link and a literal")
	   (effect "none" )
	   (value   " T iff the literal LITERAL occurs in the negative shore of the link LINK" ))
  (:method ((literal lit+literal) (link  ref+link))
	   (when (member literal (ref~negative-shore link) :test #'keim~equal ) t)))  


(defgeneric ref~link-empty-p (link)
  (declare (edited  "18-AUG-1992 11:42")
	   (authors AAYARI)
	   (input  " a link "  )
	   (effect "none" )
	   (value   " T iff the link has no literals"))
  (:method ((link ref+link))
	   (and (null (ref~positive-shore link))
		(null (ref~negative-shore link)))))

(defgeneric ref~refutation-graph-p (graph)
  (declare (edited  "19-AUG-1992 16:35")
	   (authors ACSEHN)
	   (input "a graph."  )
	   (effect "none" )
	   (value "T,iff GRAPH is a refutation graph. (the properties are checked)"  ))
  (:method ((refutation-graph ref+refutation-graph))
	   (ref=literal-in-only-one-link-p refutation-graph)))

(defgeneric ref=literal-in-only-one-link-p (refutation-graph)
  (declare (edited  "03-SEP-1992 17:24")
	   (authors ACSEHN)
	   (input "a refutatin-graph."  )
	   (effect "none."  )
	   (value "T, iff each literal in the clauses of the refutation-graph has exactly one link."  ))
  (:method ((refutation-graph ref+refutation-graph))
	   (let* ((clauses (ref~clause-nodes refutation-graph))
		  (literal-nodes (apply #'append (mapcar #'(lambda (clause) (cl~literals clause)) clauses))))
	     (every #'(lambda (literal) (eql (length (ref~get-links-with-literal literal refutation-graph)) 1)) literal-nodes))))

(defgeneric ref~link-is-in-graph-p (refutation-graph link)
  (declare (edited  "19-AUG-1992 17:05")
	   (authors AAYARI)
	   (input  " a refutation-graph and a link " )
	   (effect "none"  )
	   (value " t iff the link is  in the graph " ))
  (:method ((refutation-graph ref+refutation-graph) (link ref+link))
	   (when (member link (ref~links refutation-graph) :test #'keim~equal) t)))

(defgeneric ref~link-list-is-in-graph-p (refutation-graph link-list)
  (declare (edited  "03-SEP-1992 18:10")
	   (authors ACSEHN AAYARI)
	   (input  " a refutation-graph and a list of links link " )
	   (effect "none"  )
	   (value " t iff the links are in the graph " ))
  (:method ((refutation-graph ref+refutation-graph) link-list)
	   (if (ref~link-list-p link-list)
	       (if (not (null link-list))
		   (and (ref~link-is-in-graph-p  refutation-graph (first link-list))
			(ref~link-list-is-in-graph-p refutation-graph (rest link-list)))
		   t)
	       (error "this  list is not list of links."))))

	     

;--------------------------------  SONSTIGES ------------------------


;(defmacro ref=position (number)
; `(pos~add-front ,number (pos~empty)))

(defun ref=position (number)
  (declare (edited  "26-JAN-1993 16:06")
	   (authors ACSEHN)
	   (input "a number."  )
	   (effect "none." )
	   (value "a position representing this number."  ))
  (pos~list-position (list number)))
  
(defgeneric ref~get-literal-with-position (clause position)
  (declare (edited  "26-JAN-1993 16:05")
	   (authors ACSEHN)
	   (input "a clause and a position."  )
	   (effect "none." )
	   (value "the literal in clause at position."  ))
  (:method ((clause cl+clause) (position pos+position))
	   (let* ((clause-length (cl~length clause))
		  (upper  clause-length)
		  (first-position (pos~first position)))
	     (if (and (>= first-position 0)
		      (<= first-position upper))
		 (nth first-position (cl~literals clause))
		 (error "~% position ~A not between ~A and ~A!" first-position 0 upper))))) 



(defun ref~get-literals-with-shore-list (shore-list)
  (declare (edited  "03-SEP-1992 13:44")
	   (authors ACSEHN)
	   (input "a shore-list, i.e. a list ((clause-1 position-1) ... (clause-n position-n))."  )
	   (effect "none." )
	   (value "the literals determined by clause and position."  ))
  (if (ref~shore-list-p shore-list)
      (mapcar #'(lambda (pair) (ref~get-literal-with-position (first pair) (second pair))) shore-list)
      (error "The list ~A has not the form ((clause-1 position-1) ... (clause-n position-n))." shore-list)))
  


(defgeneric ref=get-substitution-from-links (link-list)
  (declare (edited  "03-SEP-1992 18:10")
	   (authors ACSEHN)
	   (input "a list of links."  )
	   (effect "none."  )
	   (value "takes all substitutions of the links in LINK-LIST and composes a total substitution."  ))
  (:method ((link-list list))
	   (if (ref~link-list-p link-list)
	       (let ((substitution-list (mapcar #'(lambda (x) (ref~substitution x)) link-list))
		     (id (subst~create nil nil)))
		 (do* ((substitution-list substitution-list (rest substitution-list))
		       (first-substitution  (first substitution-list) (first substitution-list))
		       (total-substitution id total-substitution))
		      ((null substitution-list) total-substitution)
		   (setq total-substitution (subst~compose-substitution first-substitution total-substitution
									:destructive nil
									:downto '(data+primitive)
									:replacers-downto '(data+struct)
									))))
	     (error "the link-list ~A has wrong typed elements." link-list))))

(defgeneric ref=get-unique-substitution-from-links (link-list)
  (declare (edited  "03-SEP-1992 18:10")
	   (authors ACSEHN)
	   (input "a list of links."  )
	   (effect "none."  )
	   (value "takes all substitutions of the links in LINK-LIST and composes a total substitution."  ))
  (:method ((link-list list))
	   (if (ref~link-list-p link-list)
	       (let ((substitution-list (mapcar #'(lambda (x) (ref~substitution x)) link-list))
		     (id (subst~create nil nil)))
		 (do* ((substitution-list substitution-list (rest substitution-list))
		       (first-substitution  (first substitution-list) (first substitution-list))
		       (total-substitution id total-substitution))
		      ((null substitution-list) total-substitution)
		   (setq total-substitution (ref=unique-substitution
					     (subst~compose-substitution first-substitution total-substitution
									 :destructive nil
									 :downto '(data+primitive)
									 :replacers-downto '(data+struct)
									 )))))
	     (error "the link-list ~A has wrong typed elements." link-list))))


(defgeneric ref=unique-substitution (substitution)
  (declare (edited  "11-SEP-1992 13:53")
	   (authors ACSEHN)
	   (input "a substitution."  )
	   (effect "if there are equal substitutions, they will be deleted from the substitution."  )
	   (value "the altered substitution."  )
	   (example "{(X --> A) (X --> A) (Z --> B)} -> {(X --> A) (Z --> B)}" ))
					;  !!!!  Vorsicht: kompatibilitaet nicht vergessen
  (:method ((substitution subst+substitution))
	   (let* ((variable-list (remove-duplicates (subst~domain substitution) :test #'keim~equal))
		  (new-codomain (mapcar #'(lambda (var) (subst~get-component var substitution)) variable-list))
		  (new-substitution (subst~create variable-list new-codomain)))
	     (if (ref~incompatible-substitutions-p substitution new-substitution)
		 substitution
		 new-substitution))))


(defgeneric ref~get-variables-from-literal (literal)
  (declare (edited  "25-SEP-1992 18:07")
	   (authors ACSEHN)
	   (input "a literal."  )
	   (effect "none." )
	   (value "a list of all  variables contained in literal."  ))
  (:method ((literal lit+literal))
	   (ref=variables-from-term (lit~atom literal))))


(defun ref=variables-from-term (term)
  (declare (edited  "28-SEP-1992 11:15")
	   (authors ACSEHN)
	   (input "a term. (not a quantified term!)"  )
	   (effect "none." )
	   (value "all variables in term"  ))
  (if (not (term~appl-p term))
      (remove-if-not #'term~variable-p (list term))
      (apply #'append (mapcar #'ref=variables-from-term (data~appl-arguments term)))))


(defgeneric ref~get-variables-from-list-of-literals (literal-list)
  (declare (edited  "03-SEP-1992 12:37")
	   (authors AAYARI)
	   (input   " a literal-list ")
	   (effect "none" )
	   (value  " a list of all variables contained in the literal of literal-list" ))
  (:method (literal-list)
	   (if (ref~literal-list-p literal-list)
	       (remove-duplicates
		(apply #'append (mapcar #'ref~get-variables-from-literal literal-list)) :test #'keim~equal)
	       (error " This list  is not list of literals"))))

(defgeneric ref=get-polylinks-form-link-list (link-list)
  (declare (edited  "03-SEP-1992 18:10")
	   (authors ACSEHN AAYARI)
	   (input  " a link-list " )
	   (effect  " none")
	   (value   " all polylink from this link-list"))
  (:method (link-list)
	   (when (ref~link-list-p link-list)
	     (remove-if-not #'(lambda (x) (when (ref=link-is-polylink-p x) x)) link-list))))

(defgeneric ref=get-positiv-polylinks-form-link-list (link-list)
  (declare (edited  "03-SEP-1992 18:10")
	   (authors ACSEHN AAYARI)
	   (input  " a link-list " )
	   (effect  " none")
	   (value   " all positive  polylinks from this link-list"))
  (:method (link-list )
	   (when (ref~link-list-p link-list)
	     (remove-if-not #'(lambda (x) (when (ref=link-is-positiv-polylink-p x) x)) link-list))))

(defgeneric ref=get-negativ-polylinks-form-link-list (link-list)
  (declare (edited  "03-SEP-1992 18:10")
	   (authors ACSEHN AAYARI)
	   (input  " a link-list " )
	   (effect  " none")
	   (value   " all negative polylinks from this link-list"))
  (:method (link-list )
	   (when (ref~link-list-p link-list)
	     (remove-if-not #'(lambda (x) (when (ref=link-is-negativ-polylink-p x) x)) link-list))))


(defgeneric ref~get-literals-from-link (link)
  (declare (edited  "03-SEP-1992 17:08")
	   (authors ACSEHN AAYARI)
	   (input  " a link " )
	   (effect " none" )
	   (value   " a list of all literals of the link, i.e. the shore of the link." ))
  (:method ((link ref+link))
	   (union (ref~positive-shore link)  (ref~negative-shore link))))


(defgeneric ref=get-other-shore-with-literal (literal link)
  (declare (edited  "19-AUG-1992 15:29")
	   (authors ACSEHN)
	   (input "a literal and a link."  )
	   (effect "none." )
	   (value "the shore, i.e. a list of literals, of the link where the literal is in the opposite shore of this link."  ))
  (:method ((literal lit+literal) (link ref+link))
	   (cond ((member literal (ref~positive-shore link) :test #'keim~equal) (ref~negative-shore link))
		 ((member literal (ref~negative-shore link) :test #'keim~equal) (ref~positive-shore link))
		 (t NIL))))


(defgeneric ref~get-links-with-literal (literal refutation-graph)
  (declare (edited  "03-SEP-1992 17:10")
	   (authors ACSEHN)
	   (input "a literal and a refutation-graph."  )
	   (effect "none" )
	   (value "the list of links in the refutation-graph where the literal is in."  )
	   (remark "in a refutation-graph this list has only 1 link."))
  (:method ((literal lit+literal) (refutation-graph ref+refutation-graph))
	   (let ((links (ref~links refutation-graph)))
	     (remove-if-not #'(lambda (link) (if (ref=literal-is-in-link-p literal link) link)) links))))


(defgeneric ref=literal-is-in-link-p (literal link)
  (declare (edited  "03-SEP-1992 17:13")
	   (authors ACSEHN)
	   (input "a literal and a link."   )
	   (effect "none." )
	   (value "T, iff the literal is in link."  ))
  (:method ((literal lit+literal) (link ref+link))
	   (if (member literal (ref~get-literals-from-link link))
	       T
	       NIL)))


(defgeneric ref=find-linked-literals (literal refutation-graph)
  (declare (edited  "03-SEP-1992 17:25")
	   (authors ACSEHN)
	   (input "a literal and a refutation-graph."  )
	   (effect "none."  )
	   (value "the opposite shore of the shore of a link from the refutation graph where the literal is in." ))
  (:method ((literal lit+literal) (refutation-graph ref+refutation-graph))
	   (mapcar #'(lambda (literal)
		       (ref=get-other-shore-with-literal literal
							 (first (ref~get-links-with-literal literal refutation-graph))))
		   (ref~get-links-with-literal literal refutation-graph))))
	   
(defgeneric ref=find-clauses-with-linked-literal (literal graph)
  (declare (edited  "19-AUG-1992 19:00")
	   (authors ACSEHN)
	   (input "a literal and a graph."  )
	   (effect "none"  )
	   (value "a list of all clauses bound via a link with literal." ))
    (:method ((literal lit+literal) (graph ref+refutation-graph))
	     (mapcar #'(lambda (literal) (lit~clause literal)) (ref=find-linked-literals literal graph))))





(defgeneric ref~get-variables-from-clause (clause)
  (declare (edited  "20-AUG-1992 19:35")
	   (authors ACSEHN)
	   (input "a clause."  )
	   (effect "none."  )
	   (value "all variables contained in clause."  ))
  (:method ((clause cl+clause))
	   (let ((literals (cl~literals clause)))
	     (remove-duplicates (apply #'append (mapcar #'(lambda (literal) (ref~get-variables-from-literal literal)) literals))))))



;-------------------------------- ADDING OBJECTS TO GRAPH --------------------

(defgeneric ref~add-literal-to-graph (refutation-graph clause-node literal-node)
  (declare (edited  "19-AUG-1992 18:05")
	   (authors ACSEHN)
	   (input "a refutation-graph, a clause-node and a literal-node."   )
	   (effect "if the clause-node is in refutation-graph, then the clause-node is extended with literal."  )
	   (value "the altered refutation-graph."  ))
  (:method ((refutation-graph ref+refutation-graph) (clause-node cl+clause) (literal-node lit+literal))
	   (let* ((clauses (ref~clause-nodes refutation-graph))
		  (clause-in-graph (first (member clause-node clauses :test #'eq))))
	     (when clause-in-graph
	       (setf (ref~clause-nodes refutation-graph)
		     (set-exclusive-or (ref~clause-nodes refutation-graph) (list clause-in-graph)))
	       ;; this removes clause-in-graph form refutation-graph
	       (ref~add-clause-to-graph refutation-graph (cl~insert-literal! literal-node clause-in-graph)))
	     refutation-graph)))



(defgeneric ref~add-link-to-graph (refutation-graph link)
  (declare (edited  "18-AUG-1992 20:26")
	   (authors ACSEHN)
	   (input "a refutation-graph and a link."  )
	   (effect "the link is inserted into the refutation-graph." )
	   (value "the altered refutation-graph."  ))
  (:method ((refutation-graph ref+refutation-graph) (link ref+link))
	   (let ((graph-links (ref~links refutation-graph)))
	     (setf (ref~links refutation-graph) (adjoin link graph-links))
	     (setf (ref~ground-substitution refutation-graph) (ref=get-unique-substitution-from-links (ref~links refutation-graph)))
	     refutation-graph)))

(defgeneric ref~add-clause-to-graph (refutation-graph clause-node)
  (declare (edited  "12-AUG-1992 10:32")
	   (authors ACSEHN)
	   (input "a refutation-graph and a clause-node not existing in the graph."  )
	   (effect "if the clause-node isn't in the graph it is inserted otherwise the graph remains unaltered." )
	   (value "the eventually altered graph."  ))
  (:method ((refutation-graph ref+refutation-graph) (clause-node cl+clause))
	   (setf (ref~clause-nodes refutation-graph) (adjoin clause-node (ref~clause-nodes refutation-graph)))
	   refutation-graph))

;----------------------------------- REMOVE OBJECTS FROM GRAPH --------  



(defun ref~remove-object (object-1 object-2 &optional (object-3 nil))
  (declare (edited  "03-SEP-1992 18:00")
	   (authors ACSEHN AAYARI)
	   (input   " three objects ")
	   (effect  " deleting object-1 from object-2 and optional from object-3")
	   (value   " the eventually altered object-2." ))
  (cond ((and (ref~literal-list-p object-1)
	      (or (ref~clause-node-list-p object-3) (cl~clause-p object-3)))
	 (ref~remove-literal-list-from-graph object-2 object-1 object-3))
	((and (lit~literal-p object-1)
	      (cl~clause-p object-3))
	 (ref~remove-literal-from-graph object-2 object-1  object-3))
	(t (ref~remove-object-from-graph object-1 object-2)
	   object-2)))
	    



(defgeneric ref~remove-object-from-graph (object refutation-graph)
  (declare (edited  "03-SEP-1992 18:01")
	   (authors ACSEHN AAYARI)
	   (input  " an object (clause, clause-list,  link and link-list) and a refutation-graph.")
	   (effect  " deleting this object from the graph")
	   (value     "the eventually altered graph." ))
  (:method ((object cl+clause) ( refutation-graph ref+refutation-graph))
	   (ref~remove-clause-from-graph refutation-graph object))
  (:method((object ref+link) (refutation-graph ref+refutation-graph))
	  (ref~remove-link-from-graph refutation-graph object))
  (:method ((object list) (refutation-graph ref+refutation-graph))
	   (cond ((ref~clause-node-list-p object)
		  (ref~remove-clause-list-from-graph  refutation-graph object))
		 ((ref~link-list-p object)
		  (ref~remove-link-list-from-graph  refutation-graph object)))))


(defgeneric  ref~remove-literal-list-from-graph  (refutation-graph literal-list clause-or-list-of-clause)
  (declare (edited  "03-SEP-1992 18:01")
	   (authors ACSEHN AAYARI)
	   (input "a refutation-graph and a list of literals and a clause or a liste of clause"   )
	   (effect " deleting all literals from the graph and clause(s)" )
	   (value  "the eventually altered graph." ))
  (:method (( refutation-graph ref+refutation-graph) literal-list clause-or-list-of-clause)
	   (cond ((and (cl~clause-p clause-or-list-of-clause) (ref~literal-list-p literal-list))
		  (mapcar #'(lambda (x) (ref~remove-literal-from-graph refutation-graph x clause-or-list-of-clause)) literal-list)
		  refutation-graph)
		 ((and (ref~clause-node-list-p clause-or-list-of-clause)
		       (ref~literal-list-p literal-list)
		       (equal (length literal-list) (length clause-or-list-of-clause)))
		  (ref~remove-literal-from-graph refutation-graph  (first literal-list) (first clause-or-list-of-clause))
		  (when (and (not(null (rest literal-list))) (not (null (rest clause-or-list-of-clause))))
		    (ref~remove-literal-list-from-graph refutation-graph (rest literal-list) (rest clause-or-list-of-clause)))
		  refutation-graph))))
		      
		       
		
(defgeneric  ref~remove-link-list-from-graph  (refutation-graph link-list)
  (declare (edited  "03-SEP-1992 18:11")
	   (authors ACSEHN AAYARI)
	   (input "a refutation-graph and a list of links"   )
	   (effect " deleting all links from the graph" )
	   (value  "the eventually altered graph." ))
  (:method (( refutation-graph ref+refutation-graph) link-list)
	   (if (ref~link-list-p link-list)
	       (cond ((not (null link-list))
		      (ref~remove-link-from-graph refutation-graph (first link-list))
		      (ref~remove-link-list-from-graph refutation-graph (rest link-list)))
		     (t refutation-graph))
	       (error " this list is not a list of links ."))))
    



(defgeneric  ref~remove-clause-list-from-graph  (refutation-graph clause-list)
  (declare (edited  "03-SEP-1992 18:01")
	   (authors ACSEHN AAYARI)
	   (input "a refutation-graph and a list of clauses"   )
	   (effect " deleting all clauses from the graph" )
	   (value  "the eventually altered graph." ))
  (:method (( refutation-graph ref+refutation-graph) clause-list)
	   (if (ref~clause-node-list-p clause-list)
	       (cond ((not (null clause-list))
		      (ref~remove-clause-from-graph refutation-graph (first clause-list))
		      (ref~remove-clause-list-from-graph refutation-graph (rest clause-list)))
		     (t refutation-graph))
	       (error " this list is not a list of clauses ."))))

(defgeneric ref~remove-link-from-graph (refutation-graph link)
  (declare (edited  "19-AUG-1992 19:00")
	   (authors AAYARI)
	   (input   " a refutation-graph and a link existing in the graph")
	   (effect  " deleting this link from the graph")
	   (value   "the eventually altered graph."  ))
  (:method ((refutation-graph ref+refutation-graph) (link ref+link))
	   (let ((ref*graph-links (ref~links refutation-graph)))
	     (if (member link ref*graph-links :test #'keim~equal )
		 (setf (ref~ground-substitution refutation-graph)
		       (ref=get-unique-substitution-from-links
			(setf (ref~links refutation-graph)
			      (remove link ref*graph-links :test #'keim~equal))))
	       (error "This link don 't exist in this graph"))
	     
	     refutation-graph)))


(defgeneric ref=remove-link-with-positive-shore-equal-literal (literal link-list refutation-graph)
  (declare (edited  "03-SEP-1992 18:11")
	   (authors ACSEHN  AAYARI)
	   (input " a refutation-graph , a literal and link-list from the graph"  )
	   (effect "  Loeschen alle Links ,deren positive Shore gleich diese Literale und
                     Loeschen diese Literal von jedem positiven Shore eines Links ,in dem dies Literale vorkommt")
	   (value   "the eventually altered graph." ))
  (:method ((literal lit+literal)
	    link-list
	    (refutation-graph ref+refutation-graph))
	   (when (and (ref~link-list-p link-list)
		      (not (null link-list))
		      (ref~link-list-is-in-graph-p refutation-graph  link-list))
	     (let*  ((link (first link-list))
		     (positive-shore (ref~positive-shore link))
		     (link-list (rest link-list)))
	       (if (equal (list literal) positive-shore)
		   (ref~remove-link-from-graph  refutation-graph link)
		   (when (ref=literal-is-in-positive-shore-form-link-p literal link)
		     (ref~remove-literal-from-positive-shore-of-link  literal link refutation-graph)))
	       (when (ref~link-empty-p link) (ref~remove-link-from-graph refutation-graph link))
	       (when (not (null link-list))
		 (ref=remove-link-with-positive-shore-equal-literal literal link-list refutation-graph))
	       refutation-graph))))
									  


(defgeneric ref=remove-link-with-negative-shore-equal-literal (literal link-list refutation-graph)
  (declare (edited  "03-SEP-1992 18:11")
	   (authors ACSEHN AAYARI)
	   (input " a refutation-graph , a literal and link-list" )
	   (effect " Loeschen alle Links ,deren negative Shore gleich diese Literale und
                         Loeschen diese Literal von jedem negativen Shore eines Links ,in dem dies Literale vorkommt ")
	   (value   "the eventually altered graph." ))
  (:method ((literal lit+literal)
	    link-list
	    (refutation-graph ref+refutation-graph))
	   (when (and (ref~link-list-p link-list)
		      (not (null link-list))
		      (ref~link-list-is-in-graph-p refutation-graph  link-list))
	     (let*  ((link (first link-list))
		     (negative-shore (ref~negative-shore link))
		     (link-list (rest link-list)))
	       (if (equal (list literal) negative-shore)
		   (ref~remove-link-from-graph  refutation-graph link)
		   (when (ref=literal-is-in-negative-shore-form-link-p literal link )
		     (ref~remove-literal-from-negative-shore-of-link  literal link refutation-graph)))
	       (when (ref~link-empty-p link) (ref~remove-link-from-graph refutation-graph link))
	       (when (not (null link-list))
		 (ref=remove-link-with-negative-shore-equal-literal literal link-list refutation-graph))
	       refutation-graph))))




(defgeneric ref~get-variables-from-link (link)
  (declare (edited  "19-AUG-1992 17:29")
	   (authors AAYARI)
	   (input   " a link ")
	   (effect "none" )
	   (value  " a list of all variables contained in the literal of this link" ))
  (:method ((link ref+link))
	   (ref~get-variables-from-list-of-literals (ref~get-literals-from-link link))))



(defgeneric ref~subst-remove-list-of-components (variables-list substitution)
  (declare (edited  "21-AUG-1992 10:45")
	   (authors AAYARI)
	   (input   "a list of variables and a substitution " )
	   (effect  "removing all components from the substitution where the variables of variables-list are in the domain of this substituion " )
	   (value   "the eventually altered substitution."  ))
  (:method (variables-list (substitution subst+substitution) )
	   (when (not (null variables-list))
	     (ref~subst-remove-list-of-components (rest variables-list) (subst~remove-component! (first variables-list) substitution)))
	   substitution))

(defgeneric ref~update-ground-subst-and-link-subst-after-removing-literal-from-link (literal link refutation-graph)
  (declare (edited  "19-AUG-1992 10:01")
	   (authors AAYARI)
	   (input   " a literal LITERAL that occurs in link LINK and a refutation-graph REFUTATION-GRAPH that contained this link ")
	   (effect   " eventually  removing subst-component form ground-substitution of the refutation-graph and the substitution of the link"  )
	   (value   "the eventually graph"  ))
  (:method ((literal lit+literal) (link ref+link) (refutation-graph ref+refutation-graph))
	   (let* ((variables*of-literal (ref~get-variables-from-literal literal))
		  (variables*of-link-without-this-literal (ref~get-variables-from-list-of-literals (remove literal (ref~get-literals-from-link link) :test #'keim~equal)))
		  (variables*to-delete-from-subst (set-difference variables*of-literal variables*of-link-without-this-literal)))
	     (ref~subst-remove-list-of-components variables*to-delete-from-subst (ref~substitution link))
	     (setf (ref~ground-substitution refutation-graph)
		   (ref=get-unique-substitution-from-links (ref~links refutation-graph))))
	   refutation-graph))




(defgeneric ref~remove-literal-from-positive-shore-of-link (literal link refutation-graph)
  (declare (edited  "19-AUG-1992 09:53")
	   (authors AAYARI)
	   (input   " a literal and a link")
	   (effect  "deleting this literal from the positive shore of a link ")
	   (value   "the eventually altered graph."  ))
  (:method ((literal lit+literal) (link ref+link) ( refutation-graph ref+refutation-graph))
	   (cond ((ref=literal-is-in-positive-shore-form-link-p literal link)
		  (setf (ref~positive-shore link)
			(remove literal (ref~positive-shore link) :test #'keim~equal))
		  (ref~update-ground-subst-and-link-subst-after-removing-literal-from-link literal link refutation-graph))
		 (t (error "this literal doesn 't exist in the positive shore of this link")))
	   refutation-graph))

(defgeneric ref~remove-literal-from-negative-shore-of-link (literal link refutation-graph)
  (declare (edited  "19-AUG-1992 11:13")
	   (authors AAYARI)
	   (input   " a literal and a link")
	   (effect  "deleting this literal from the negative  shore of a link ")
	   (value   "the eventually altered link."  ))
  (:method ((literal lit+literal) (link ref+link) (refutation-graph ref+refutation-graph))
	   (cond ((ref=literal-is-in-negative-shore-form-link-p literal link)
		  (setf (ref~negative-shore link) (remove literal (ref~negative-shore link) :test #'keim~equal))
		  (ref~update-ground-subst-and-link-subst-after-removing-literal-from-link literal link refutation-graph))
		 (t (error "this literal doesn 't exist in the negative shoore of this link")))
	   refutation-graph))


(defgeneric ref~remove-clause-from-graph ( refutation-graph clause)
  (declare (edited  "19-AUG-1992 15:02")
	   (authors AAYARI)
	   (input  " a clause and a refutation-graph" )
	   (effect  "deleting the clause from the refutation-graph" )
	   (value  "the eventually altered graph."  ))
  (:method (( refutation-graph ref+refutation-graph) (clause cl+clause))
	   (cond ((member clause (ref~clause-nodes refutation-graph) :test  #'eq)
		  (when (not (cl~empty-p clause))
		    (mapcar #'(lambda (x) (ref~remove-literal-from-graph refutation-graph x clause)) (cl~literals clause)))
		  (setf (ref~clause-nodes refutation-graph)
			(remove clause (ref~clause-nodes refutation-graph) :test #'eq)))
		 (t (error " this clause doesn 't exist in this graph .")))
	   refutation-graph))


(defgeneric ref~remove-literal-from-graph (refutation-graph literal clause)
  (declare (edited  "19-AUG-1992 11:02")
	   (authors AAYARI)
	   (input   " a refutation-graph , a literal and a clause" )
	   (effect  " deleting this literal from the clause in the graph" )
	   (value   "the eventually altered graph."  ))
  (:method ((refutation-graph ref+refutation-graph)
	    (literal lit+literal)
	    (clause cl+clause))
	   (cond ((member literal (cl~literals clause) :test #'keim~equal)
		  (if (lit~positive-p literal)
		      (ref=remove-link-with-positive-shore-equal-literal literal (ref~links refutation-graph) refutation-graph)
		      (ref=remove-link-with-negative-shore-equal-literal literal (ref~links refutation-graph) refutation-graph))
		  (setf (ref~clause-nodes refutation-graph)
			(adjoin (cl~remove-literal!  literal clause)
				(remove clause (ref~clause-nodes refutation-graph) :test #'eq))))
		 (t (error "this  literal doesn 't exist in this clause")))
	   refutation-graph))



(defun ref=make-substitution (substitution-list)
  (declare (edited  "09-SEP-1992 11:43")
	   (authors ACSEHN)
	   (input "a list of substitutions, i. e. (subst-1 ... subst-n)."  )
	   (effect "none."  )
	   (value "the composition of subst-1 to subst-n."  ))
  (let ((id (subst~create nil nil)))
    (if (null substitution-list)
	id
	(subst~compose-substitution (first substitution-list) (ref=make-substitution (rest substitution-list))
				    :replacers-downto '(data+struct)
				    :downto '(data+primitive)
				    :destructive nil))))


(defgeneric ref=substitution-domains (substitution)
  (declare (edited  "02-SEP-1992 17:31")
	   (authors ACSEHN)
	   (input "a substitution."   )
	   (effect "none."  )
	   (value "1. the domain of substitution"
		  "2. the codomain of substitution"))
  (:method ((substitution subst+substitution))
	   (let ((domain (subst~domain substitution))
		 (codomain (subst~codomain substitution)))
	     (values domain codomain))))


(defgeneric ref=create-substitution-components (substitution)
  (declare (edited  "09-SEP-1992 17:29")
	   (authors ACSEHN)
	   (input "a substitution."  )
	   (effect "none." )
	   (value "a list of all pairs of substitution."  ))
  (:method ((substitution subst+substitution))
	   (multiple-value-bind (domain codomain) (ref=substitution-domains substitution)
	     (do* ((dom domain (rest dom))
		   (first-dom (first dom) (first dom))
		   (codom codomain (rest codom))
		   (first-codom (first codom) (first codom))
		   (subst (subst~create nil nil))
		   (new-subst nil))
		  ((null dom) (reverse subst))
	       (setq new-subst (subst~create (list first-dom) (list first-codom)))
	       (setq subst (cons new-subst subst))))))



(defun ref=apply-function-to-all-permutations (afn list)
  (declare (edited  "09-SEP-1992 17:28")
	   (authors ACSEHN)
	   (input   "afn is a one place function."
		    "list is an arbitrary list.")
	   (effect  "the function is applied to all permutations of the list."
		    "the loop terminates as soon as the first application of afn returns something not nil."
		    "the list is destructively modified.")
	   (value   "the value of the last application of afn."))
  (catch 'end
    (when list
      (let (help help1)
	(labels ((srv=ploop (start1 end1 start2)
		   (if start2
		       (let ((help (cdr start2)) help1)
			 (rplacd end1 start2) (rplacd start2 nil)
			 (srv=ploop start1 start2 help)
			 (rplacd end1 help) (rplacd start2 help)
			 (do ((tail start2 help))
			     ((endp (setq help (cdr tail))))
			   (setq help1 (cdr help))
			   (rplacd end1 help) (rplacd tail help1) (rplacd help nil)
			   (srv=ploop start1 help start2)
			   (rplacd tail help) (rplacd help help1))
			 (rplacd end1 nil))
		       (when (setq start2 (funcall afn start1)) (throw 'end start2))))) 
	  (setq help (cdr list))
	  (rplacd list nil)
	  (srv=ploop list list help)
	  (rplacd list help)
	  (do ((tail list help))
	      ((endp (setq help (cdr tail))))
	    (setq help1 (cdr help))
	    (rplacd help nil) (rplacd tail help1)
	    (srv=ploop help help list)
	    (rplacd tail help) (rplacd help help1)))))))

(defgeneric ref~inverse-substitution (substitution)
  (declare (edited  "02-SEP-1992 17:39")
	   (authors ACSEHN)
	   (input "a substitution"  )
	   (effect "none." )
	   (value "the inverse substitution of SUBSTITUTION, i.e. the domain becomes codomain and vice versa."  ))
  (:method ((substitution subst+substitution))
	   (multiple-value-bind (domain codomain) (ref=substitution-domains substitution)
	     (subst~create codomain domain))))


(defgeneric ref~replace-literal (literal new-literal object)
  (declare (edited  "03-SEP-1992 18:11")
	   (authors ACSEHN)
	   (input "a  LITERAL, a NEW-LITERAL and an OBJECT,i.e.a link or a literal-list or a link-list."  )
	   (effect "the LITERAL in OBJECT is replaced by NEW-LITERAL."  )
	   (value "the altered OBJECT."  )
	   (remark "LITERAL must belong to a clause,i.e. must have an entry for lit~clause."))
  (:method ((literal lit+literal) (new-literal lit+literal) (link ref+link))
	   (let ((positive-shore-list (remove-if-not #'lit~positive-p (ref~positive-shore link)))
		 (negative-shore-list (remove-if #'lit~positive-p (ref~negative-shore link))))
	     (when (member literal positive-shore-list :test #'keim~equal)
	       (setf (ref~positive-shore link) (ref~replace-literal literal new-literal positive-shore-list)))
	     (when (member literal negative-shore-list :test #'keim~equal)
	       (setf (ref~negative-shore link) (ref~replace-literal literal new-literal negative-shore-list)))
	     link))
  (:method ((literal lit+literal) (new-literal lit+literal) (list list))
	   (cond ((ref~literal-list-p list)
		  (if (member literal list :test #'keim~equal)
		      (adjoin new-literal (remove literal list :test #'keim~equal) :test #'keim~equal)
		    list))
		 ((ref~link-list-p list)
		  (mapcar #'(lambda (link) (ref~replace-literal literal new-literal link)) list))
		 (t list))))




(defgeneric ref~replace-literals (literals new-literals object)
  (declare (edited  "30-SEP-1992 15:14")
	   (authors ACSEHN)
	   (input "a  a list of LITERALS, a list of NEW-LITERALS and an OBJECT,i.e.a link or a literal-list or a link-list."  )
	   (effect "the LITERALS in OBJECT is replaced by NEW-LITERALS."  )
	   (value "the altered OBJECT."  )
	   (remark "LITERALS must belong to a clause,i.e. must have an entry for lit~clause."))
  (:method ((literals list) (new-literals list) (link ref+link))
	   (if (and (ref~literal-list-p literals)
		    (ref~literal-list-p new-literals))
	       (let ((positive-shore-list (remove-if-not #'lit~positive-p (ref~positive-shore link)))
		     (negative-shore-list (remove-if #'lit~positive-p (ref~negative-shore link))))
		 (when (subsetp literals positive-shore-list :test #'keim~equal)
		   (setf (ref~positive-shore link) (ref~replace-literals literals new-literals positive-shore-list)))
		 (when (subsetp literals negative-shore-list :test #'keim~equal)
		   (setf (ref~negative-shore link) (ref~replace-literals literals new-literals negative-shore-list)))
		 link)
	     (error "one of the parameters have the wrong type")))
  (:method ((literals list) (new-literals list) (list list))
	   (cond ((ref~literal-list-p list)
		    (if (subsetp literals list :test #'keim~equal)
			(union new-literals (set-difference list literals :test #'keim~equal) :test #'keim~equal)
			list))
		   ((ref~link-list-p list)
		    (mapcar #'(lambda (link) (ref~replace-literals literals new-literals link)) list))
		   (t list))))



(defgeneric ref~replace-clause (clause new-clause object)
  (declare (edited  "03-SEP-1992 18:02")
	   (authors ACSEHN)
	   (input "two clauses and an object, i.e. a clause-node list."  )
	   (effect "none."  )
	   (value "the clause-node list, where clause is replaced by new-clause."  ))
  (:method ((clause cl+clause) (new-clause cl+clause) (list list))
	   (cond ((ref~clause-node-list-p list)
		  (if (member clause list :test #'eq)
		      (adjoin new-clause (remove clause list :test #'eq) :test #'eq)
		      list))
		 (t list))))
	       

(defgeneric ref~instantiate-term (term refutation-graph)
  (declare (edited  "03-SEP-1992 18:27")
	   (authors ACSEHN)
	   (input "a term and a refutation-graph."  )
	   (effect "none" )
	   (value "the term where the ground-substitution is applied on." ))
  (:method ((term term+term) (refutation-graph ref+refutation-graph))
	   (let ((ground-substitution (ref~ground-substitution refutation-graph)))
	     (subst~apply ground-substitution term
			  :destructive nil
			  :downto '(data+primitive)
			  :replacers-downto '(data+struct)
			  ))))


(defgeneric ref~append-substitution (substitution refutation-graph)
  (declare (edited  "22-SEP-1992 17:44")
	   (authors ACSEHN)
	   (input "a substitution and a refutation-graph."  )
	   (effect "the substitution is composed with the ground-substitution."  )
	   (value "the refutation-graph where the ground-substitution is the composition of substitution and the old ground-substitution."))
  (:method ((substitution subst+substitution) (refutation-graph ref+refutation-graph))
	   (let ((graph-substitution (ref~ground-substitution refutation-graph)))
	     (setf (ref~ground-substitution refutation-graph)
		   (ref=unique-substitution
		    (subst~disjoint-compose-substitution substitution graph-substitution
							 :destructive nil
							 :downto '(data+primitive)
							 :replacers-downto '(data+struct))))
	     refutation-graph)))



(defgeneric ref~append-renaming (renaming refutation-graph)
  (declare (edited  "11-SEP-1992 14:13")
	   (authors ACSEHN)
	   (input "a renaming and a refutation-graph."  )
	   (effect "the renaming of the refutation-graph is composed with the renaming."  )
	   (value "the refutation-graph where the renaming is appended to the list of the old renaming."))
  (:method ((renaming subst+substitution) (refutation-graph ref+refutation-graph))
	   (let ((graph-renaming (ref~renamings refutation-graph)))
	     (setf (ref~renamings refutation-graph) (cons renaming graph-renaming))
	     refutation-graph)))

(defgeneric ref~graph-renaming (refutation-graph)
  (declare (edited  "07-OCT-1993 15:43")
	   (authors ACSEHN)
	   (input "a refutation graph, REFUTATION-GRAPH."  )
	   (effect "none." )
	   (value "the list of renamings of REFUTATION-GRAPH."  ))
  (:method ((refutation-graph ref+refutation-graph))
	   (ref~renamings refutation-graph)))



(defun ref~combine-links (link-list)
  (declare (edited  "28-SEP-1992 17:28")
	   (authors ACSEHN)
	   (input "a list of links."  )
	   (effect "none." )
	   (value "a poly-link constructed from the list of links."  ))
  (if (ref~link-list-p link-list)
      (let* ((literals (remove-duplicates (apply #'append (mapcar #'ref~get-literals-from-link link-list))))
	     (shore (mapcar #'ref=find-clause-and-position literals))
	     (substitution (ref=get-substitution-from-links link-list)))
	(ref~link-create NIL shore substitution))
      (error "the parameters have the wrong type")))
  




(defgeneric ref=find-clause-and-position (literal)
  (declare (edited  "28-JAN-1993 18:41")
	   (authors ACSEHN)
	   (input "a literal in a link."  )
	   (effect "none" )
	   (value "a list consisting of the clause the literal is in and the position of the literal in the clause."  ))
  (:method ((literal lit+literal))
	   (list (lit~clause literal) (ref=position (position literal (cl~literals (lit~clause literal)) :test #'keim~equal)))))



(defgeneric ref=copy-clause (clause)
  (declare (edited  "29-SEP-1992 15:38")
	   (authors ACSEHN)
	   (input "a clause."  )
	   (effect "none."  )
	   (value "the copied clause."  )
	   (remark "keim~copy is not enough,because the literals are not copied!"
		   "the literals in the copied clause are keim~equal and not eql to the original literals in clause."))
  (:method ((clause cl+clause))
	   (cl~create (ref=copy-literal-list (cl~literals clause)))))


(defun ref=copy-literal-list (literal-list)
  (declare (edited  "29-SEP-1992 15:38")
	   (authors ACSEHN)
	   (input "a list of literals."  )
	   (effect "none."  )
	   (value "the list of the copied literals."  ))
  (mapcar #'keim~copy literal-list))

(defun ref=copy-clause-set (clauses)
  (declare (edited  "29-SEP-1992 15:40")
	   (authors ACSEHN)
	   (input "a list of clauses."  )
	   (effect "none." )
	   (value "a list of copied clauses."  ))
  (mapcar #'ref=copy-clause clauses))

(defun ref==copy-link (link)
  (let ((shore (mapcar #'ref=find-clause-and-position (ref~get-literals-from-link link)))
	(substitution (ref~substitution link))
	(name (keim~name link)))
    (ref~link-create name shore substitution)))
