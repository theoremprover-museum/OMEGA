;;; -*- Syntax: Common-lisp; package: OMEGA; base: 10; mode: keim -*-
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; ana-variabelize.lisp; This file is part of the OMEGA system
;;
;; major updates: 4.10.1999
;; 
;;
;; Authors: Carsten Ullrich
;; email: cullrich@ags.uni-sb.de 
;;
;; For information about this program, write to:                          
;;   OMEGA Project                                                        
;;   AG Siekmann/FB Informatik                                            
;;   Universitaet des Saarlandes                                          
;;   Bau 36, 4. Stock                                                     
;;   D-66041 Saarbruecken                                                 
;;   Germany    
;;
;; For information about the newest version of Omega, see 
;;   http://www.ags.uni-sb.de/~omega/
;;
;; This program is free software; it can be used under the terms of the GNU General
;; Public License as published by the Free Software Foundation; either version 2 of
;; the License, or any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; merchantibility or fitness for a particular purpose. 
;; See the GNU General Public License (http://www.fsf.org/copyleft/gpl.html)
;; for more details.
;;
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;; variabelizing nodes and terms, creating new names

(in-package "OMEGA")

(mod~defmod ANAVAR
            :uses (data env keim logic node omega pds pdsn prob term)
            :documentation "Variabelizing of nodes and terms."
            :exports (
                      
                      ana~create-name
                      ana~term-equal-p
                      ana~variabelize
                      ana~variabelize-reset
                      
                      ana*const-to-var-hashtable
                      ana*node-counter
                      ana*var-to-const-hashtable
                      ana*variabelize-verbose))

(defvar ana*variabelize-verbose nil "Verbose mode for the variabelizer.")

(defvar ana*var-to-const-hashtable (make-hash-table)
  "The hashtable that maps the new created variables to the constants they originated from.")

(defvar ana*const-to-var-hashtable (make-hash-table)
  "The hashtable that maps the constants to the new created variables.")


(defun ana=create-variable-for-constant (const)
  (let* ((var-name (ana~create-name const))
	 ;; create the object
	 (var (make-instance 'term+variable :name var-name
			     :annotation (data~annotation const))))
    ; enter it in the hashtables
    (setf (gethash (keim~name var) ana*var-to-const-hashtable) const)
    ;; why keim-name? because of problems with term+number where every number is different
    (setf (gethash (keim~name const) ana*const-to-var-hashtable) var)
    ; and enter it in the environment
    (env~enter var-name var (pds~environment omega*current-proof-plan))
    var))

(defun ana~term-equal-p (source-term target-term)
  (let ((corresponding-constant (gethash (keim~name source-term) ana*var-to-const-hashtable)))
    (and corresponding-constant
	 (data~equal corresponding-constant target-term))))

(defun ana=get-corresponding-variable (term)
  (let* ((already-replaced (gethash (keim~name term) ana*const-to-var-hashtable)))
    (if already-replaced already-replaced
      (ana=create-variable-for-constant term))))

(defgeneric ana~variabelize (object)
  (declare (edited  "08-DEC-1998")
	   (authors Cullrich)
	   (input   "an object")
	   (effect  "")
	   (value   "an object with the constants replaced by variables"))
  (:method ((object node+node))
	   (when ana*variabelize-verbose (omega~trace "Now variabelizing node ~S." (keim~name object)))
	   (ana~variabelize (node~formula object))) ;;we return the formula
  (:method ((object term+variable))
	   (ana=get-corresponding-variable object)) ;; this is needed for metavariables, as
					    ;; metavariables from different environment
					    ;; may be data~equal!
  (:method ((object term+constant))
	   (if (logic~connective-p object)
	       object
	     (ana=get-corresponding-variable object)))
  (:method ((object term+appl))
	   (let ((function (data~appl-n-function object ))
		 (args (data~appl-n-arguments object)))
	     (data~appl-create (ana~variabelize function)
			       (ana~variabelize args))))
  (:method ((object term+abstr))
	   (let ((binder (data~abstr-n-domain object))
		 (scope (data~abstr-n-range object)))
	     (data~abstr-create (ana~variabelize binder)
				(ana~variabelize scope))))
				
  (:method ((object list))
	   (mapcar #'ana~variabelize object))
  (:method ((object pds+proof-plan))
	   (omega~trace "=====================================")
	   (omega~trace "Now variabelizing the source problem.")
	   (ana~variabelize (append (list (prob~proof-root object))
				    (pds~open-nodes object)
				    (pds~support-nodes object)
				    (prob~proof-steps object)))
	   (omega~trace "Variabelizing of ~S successfull." object)
	   object
	   )
  (:method (object)
	   (omega~warn "Variabelizing not defined for ~S of type ~S." object (type-of
										object))
	   ))

  

(defun ana~variabelize-reset()
  (setf ana*var-to-const-hashtable (make-hash-table)
	ana*const-to-var-hashtable (make-hash-table)))

;;creating names for variables and nodes

(defvar ana*node-counter 0 "A counter for the proposed nodes.")

(defgeneric ana~create-name (thing)
  (:method ((constant term+constant))
	    (format nil "~s-VAR"
		    (keim~name constant))
	    )
  (:method ((constant term+variable))
	    (format nil "~s-VAR"
		    (keim~name constant))
	    )
  (:method ((constant fixnum))
	    (format nil "~s-VAR"
		    (keim~name constant))
	    )
  (:method ((node pdsn+node))
	   (make-symbol
	    (format nil "~s-PROP~s"
		    (keim~name node)
		    (incf ana*node-counter)))))

