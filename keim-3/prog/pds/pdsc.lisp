;;; -*- syntax: common-lisp; package: keim; base: 10; mode: keim -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1993 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     KEIM Project                                                         ;;
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

(in-package "KEIM")

(mod~defmod pdsc
	    :uses (mod node just keim th)
	    :documentation "Basic datastructures and functionality of pds control structures."
	    :exports (
		      pdsc~an-node
		      pdsc~an-just
		      pdsc~an-create
		      pdsc+control 
		      pdsc~reasons
		      pdsc~sponsors
		      pdsc~unsponsors
		      pdsc~successor
		      pdsc~predecessor
		      pdsc~control-p
		      pdsc~substitute-sponsor!
		      pdsc~substitute-unsponsor!
		      pdsc~alternatives
		      pdsc~applied-critics
		      pdsc~alternative-methods
		      pdsc~alternative-mmatchings
		      pdsc~already-applied-strategy-ks
		      pdsc~failed-methods
		      pdsc~excluded-methods
		      pdsc~create
		      pdsc~copy
		      pdsc~why-method
		      )
	    )

#{\section{Control Data Structure and Functions}\label{mod:pdsc}
#}

#{\subsection{Actual Nodes}

An actual node represents the node on some abstract level (the node with its justification on this level).
#}

(eval-when (load compile eval)
(defclass pdsc=actual-node (keim+object)
  ((node :accessor pdsc~an-node
	 :initarg :an-node
	 :documentation "The node.")
   (just :accessor pdsc~an-just
	 :initarg :an-just
	 :documentation "One of the node justifications."))
  (:documentation "An actual node is an object that contains a node and one of its justifications."
		  "The data structure is used to return to the justification that was actually"
		  "used during the planning process."))
)

(defun pdsc~an-create (node &optional just)
  (declare (edited   "25-SEP-1996" "07-MAY-1996 11:07")
	   (authors Lassaad  SORGE)
	   (input   "A node, and optioanl a justification.")
	   (effect  "Creates an actual node.")
	   (value   "The new instance."))
  (let ((curr-just (if just
		       just
		     (node~justification node))))
    (make-instance 'pdsc=actual-node
		   :an-node node
		   :an-just curr-just)))


(defmethod keim~equal ((an1 pdsc=actual-node) (an2 pdsc=actual-node))
  (and (keim~equal (pdsc~an-node an1) (pdsc~an-node an2))
       (keim~equal (pdsc~an-just an1) (pdsc~an-just an2))))


(defmethod print-object ((an pdsc=actual-node) stream)
  (format stream 
	  "#<ANode ~A ~A>"
	  (keim~name (pdsc~an-node an)) (pdsc~an-just an)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data structure and functions for control 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-when (load compile eval)
(defclass pdsc+control (keim+object)
  ((reasons :accessor pdsc~reasons
	    :initform nil
	    :initarg :reasons
	    :documentation "The reasons why the associated node exists in the proof plan.")
   (sponsors :accessor pdsc~sponsors
	     :initarg :sponsors
	     :initform nil
	     :documentation "Proof nodes can be used in justifying the associated node.")
   (unsponsors :accessor pdsc~unsponsors
	       :initarg :unsponsors
	       :initform nil
	       :documentation "Proof nodes may not be used in justifying the associated node.")
   (alternatives :accessor pdsc=alternatives
		 :initform nil
		 :documentation "All the alternatives for choosing that node at this point in the planning state.")
   (applied-critics :accessor pdsc=applied-critics
		    :initform nil
		    :documentation "All the critic applications which were applied in planning situations involving this node.")
   (alternative-methods :accessor pdsc=alternative-methods
			:initform nil
			:documentation "All the alternative methods which could be applicable to this node and are at this point not tested.")
   (alternative-mmatchings :accessor pdsc=alternative-mmatchings
			  :initform nil
			  :documentation "All the alternative matchings of the method justifying that node at this point.")
   (failed-methods :accessor pdsc=failed-methods
		   :initform nil
		   :documentation "All methods applied to this node and its application leaded to a failure and therefore backtracked.")
   (excluded-methods :accessor pdsc=excluded-methods
 		     :initform nil
		     :initarg :excluded-methods
 		     :documentation "All methods applied to this node and its application leaded to a failure and therefore backtracked.")
   (why-sequent :accessor pdsc=why-sequent
		:initform nil
		:initarg :why-sequent
		:documentation "Left to somebody else.")
   (why-method :accessor pdsc=why-method
	       :initform nil
	       :initarg :why-method
	       :documentation "Left to somebody else as well.")
   (successor :accessor pdsc~successor
	      :initform nil
	      :type pdsc=actual-node
	      :documentation "The next justified actual node in the proof.")
   (predecessor :accessor pdsc~predecessor
		:initform nil
		:type pdsc=actual-node
		:documentation "The previous justified actual node in the proof.")
;   (excluded-methods :accessor pdsc~excluded-methods
;		     :initform nil
;		     :initarg :excluded-methods
;		     :documentation "The methods we already tried and are now exluded.")
   (already-applied-mmatchings :accessor pdsc~already-applied-mmatchings
			       :initform nil
			       :initarg :already-applied-mmatchings
			       :documentation "The mmatchings we already applied.")
   (broken-matchings :accessor pdsc~broken-matchings
		     :initform nil
		     :initarg :broken-matchings
		     :documentation "The broken matchings.")
   (already-applied-strategy-ks :accessor pdsc~already-applied-strategy-ks
				:initform nil
				:initarg :already-applied-strategy-ks
				:documentation "The strategies we already applied on the node.")
   )))



(defmethod keim~copy ((cunit pdsc+control) &key (explode :all-classes) share preserve downto)
  (declare (edited  "29-AUG-2002 13:49")
	   (authors AMEIER)
	   (input   "A control unit")
	   (effect  "None.")
	   (value   "An (exact) copy of control unit."))

  (let ((new-instance (make-instance 'pdsc+control
				     :reasons (pdsc~reasons cunit)
				     :sponsors (pdsc~sponsors cunit)
				     :unsponsors (pdsc~unsponsors cunit)
				     :excluded-methods (pdsc=excluded-methods cunit)
				     :why-sequent (pdsc=why-sequent cunit)
				     :why-method (pdsc=why-method cunit)
				     :excluded-methods (pdsc~excluded-methods cunit)
				     :already-applied-mmatchings (pdsc~already-applied-mmatchings cunit)
				     :broken-matchings (pdsc~broken-matchings cunit)
				     :already-applied-strategy-ks (pdsc~already-applied-strategy-ks cunit))))
    
    (setf (pdsc~successor new-instance)
	  (pdsc~successor cunit))
    
    (setf (pdsc~predecessor new-instance)
	  (pdsc~predecessor cunit))
    
    (setf (pdsc=failed-methods new-instance)
	  (pdsc=failed-methods cunit))
    
    (setf (pdsc=alternative-mmatchings new-instance)
	  (pdsc=alternative-mmatchings cunit))
    
    (setf (pdsc=alternative-methods new-instance)
	  (pdsc=alternative-methods cunit))
    
    (setf (pdsc=alternatives new-instance)
	  (pdsc=alternatives cunit))
    
    (setf (pdsc=applied-critics new-instance)
	  (pdsc=applied-critics cunit))
    
    new-instance)) 


#{\subsection{Functions for the Control Structure}#}

(defun pdsc~control-p (thing)
  (declare (edited  "12-APR-1996")
	   (authors Lassaad)
	   (input   "Any THING.")
	   (effect  "None")
	   (value   "T if THING is of type PDSC+CONTROL else NIL."))
  (typep thing 'pdsc+control))


(defun pdsc~substitute-sponsor! (ctrl from to)
  (declare (edited  "23-MAY-1997")
	   (authors Lassaad)
	   (input   "A control, and two nodes.")
	   (effect  "Replaces in sponsors of CTRL FROM with TO.")
	   (value   "Unspecified."))
  (nsubstitute to from (pdsc~sponsors ctrl)))


(defun pdsc~substitute-unsponsor! (ctrl from to)
  (declare (edited  "23-MAY-1997")
	   (authors Lassaad)
	   (input   "A control, and two nodes.")
	   (effect  "Replaces in unsponsors of CTRL FROM with TO.")
	   (value   "Unspecified."))
  (nsubstitute to from (pdsc~unsponsors ctrl)))


(defgeneric pdsc~alternatives (control)
  (declare (edited  "17-JUN-1996 17:33")
	   (authors SORGE)
	   (input   "A control structure.")
	   (value   "A list of pairs consisting of a node and a justification."))
  (:method ((control pdsc+control))
	   (pdsc=alternatives control))
  (:method (control)
	   (error ";;;PDSC~~ALTERNATIVES: ~A is not of type PDSC+CONTROL." control))
  )

(defgeneric pdsc~applied-critics (control)
  (declare (edited  "12-FEB-1999")
 	   (authors Lassaad)
 	   (input   "A control structure.")
 	   (value   "A list of critic applications."))
  (:method ((control pdsc+control))
 	   (pdsc=applied-critics control))
  (:method (control)
 	   (error ";;;pdsc~~applied-critics: ~A is not of type PDSC+CONTROL." control))
  )

(defgeneric pdsc~alternative-methods (control)
  (declare (edited  "03-APR-1998")
	   (authors Lassaad)
	   (input   "A control structure.")
	   (value   "A list of methods."))
  (:method ((control pdsc+control))
	   (pdsc=alternative-methods control))
  (:method (control)
	   (error ";;;PDSC~~ALTERNATIVES: ~A is not of type PDSC+CONTROL." control))
  )

(defgeneric pdsc~alternative-mmatchings (control)
  (declare (edited  "03-APR-1998")
	   (authors Lassaad)
	   (input   "A control structure.")
	   (value   "A list of method matchings."))
  (:method ((control pdsc+control))
	   (pdsc=alternative-mmatchings control))
  (:method (control)
	   (error ";;;PDSC~~ALTERNATIVES: ~A is not of type PDSC+CONTROL." control))
  )

(defgeneric pdsc~failed-methods (control)
  (declare (edited  "03-APR-1998")
	   (authors Lassaad)
	   (input   "A control structure.")
	   (value   "A list of methods."))
  (:method ((control pdsc+control))
	   (pdsc=failed-methods control))
  (:method (control)
	   (error ";;;PDSC~~ALTERNATIVES: ~A is not of type PDSC+CONTROL." control))
  )

(defgeneric pdsc~excluded-methods (control)
  (declare (edited  "03-APR-1998")
 	   (authors Lassaad)
 	   (input   "A control structure.")
 	   (value   "A list of methods."))
  (:method ((control pdsc+control))
	   (pdsc=excluded-methods control))
  (:method (control)
	   (error ";;;PDSC~~ALTERNATIVES: ~A is not of type PDSC+CONTROL." control))
  )


;(defsetf pdsc~alternatives (control) (nodelist)
;  (declare (edited  "17-JUN-1996 17:44")
;           (authors SORGE)
;           (input   "A control-structure and a node-list.")
;           (effect  "Sets the alternatives of control to the node-list.")
;           (value   "Undefined."))
;  `(if (typep ,control 'pdsc+control)
;       (let ((anlist (remove-if #'null
;                                (mapcar #'(lambda (x)
;                                            (if (node~p x)
;                                                (pdsc~an-create x)
;                                              (warn ";;;PDSC~~ALTERNATIVES ~A is not a node." x)))
;                                        ,nodelist))))
;         (setf (pdsc=alternatives ,control) anlist))
;     (error ";;;PDSC~~ALTERNATIVES ~A must be of type PDSC+CONTROL." ,control)))
;;LC: The alternatives in the control of a node contains the method (matchings) that
;; can be used to close the node.
(defsetf pdsc~alternatives (control) (alist)
  (declare (edited  "03-FEB-1998")
	   (authors Lassaad)
	   (input   "A control-structure and a list of methods and/or"
		    "method matchings.")
	   (effect  "Sets the alternatives of control to ALIST.")
	   (value   "Undefined."))
  `(if (typep ,control 'pdsc+control)
       (setf (pdsc=alternatives ,control) ,alist)
     (error ";;;PDSC~~ALTERNATIVES ~A must be of type PDSC+CONTROL." ,control)))

(defsetf pdsc~applied-critics (control) (alist)
  (declare (edited  "12-FEB-1999")
 	   (authors Lassaad)
 	   (input   "A control-structure and a list of critic applications.")
 	   (effect  "Sets the applied critics of control to ALIST.")
 	   (value   "Undefined."))
  `(if (typep ,control 'pdsc+control)
       (setf (pdsc=applied-critics ,control) ,alist)
     (error ";;;pdsc~~applied-critics ~A must be of type PDSC+CONTROL." ,control)))

(defsetf pdsc~alternative-methods (control) (alist)
  (declare (edited  "03-APR-1998")
	   (authors Lassaad)
	   (input   "A control-structure and a list of methods.")
	   (effect  "Sets the alternative methods of control to ALIST.")
	   (value   "Undefined."))
  `(if (typep ,control 'pdsc+control)
       (setf (pdsc=alternative-methods ,control) ,alist)
     (error ";;;PDSC~~ALTERNATIVES ~A must be of type PDSC+CONTROL." ,control)))

(defsetf pdsc~alternative-mmatchings (control) (alist)
  (declare (edited  "03-APR-1998")
	   (authors Lassaad)
	   (input   "A control-structure and a list of method matchings.")
	   (effect  "Sets the alternative method matchings of control to ALIST.")
	   (value   "Undefined."))
  `(if (typep ,control 'pdsc+control)
       (setf (pdsc=alternative-mmatchings ,control) ,alist)
     (error ";;;PDSC~~ALTERNATIVES ~A must be of type PDSC+CONTROL." ,control)))

(defsetf pdsc~failed-methods (control) (alist)
  (declare (edited  "03-APR-1998")
	   (authors Lassaad)
	   (input   "A control-structure and a list of methods.")
	   (effect  "Sets the failed methods of control to ALIST.")
	   (value   "Undefined."))
  `(if (typep ,control 'pdsc+control)
       (setf (pdsc=failed-methods ,control) ,alist)
     (error ";;;PDSC~~ALTERNATIVES ~A must be of type PDSC+CONTROL." ,control)))

(defsetf pdsc~excluded-methods (control) (alist)
  (declare (edited  "03-APR-1998")
 	   (authors Lassaad)
 	   (input   "A control-structure and a list of methods.")
 	   (effect  "Sets the failed methods of control to ALIST.")
 	   (value   "Undefined."))
  `(if (typep ,control 'pdsc+control)
       (setf (pdsc=excluded-methods ,control) ,alist)
     (error ";;;PDSC~~ALTERNATIVES ~A must be of type PDSC+CONTROL." ,control)))


(defgeneric pdsc~why-method (control)
  (declare (edited  "27-MAY-1997" "17-JUN-1996 17:33")
	   (authors Lassaad SORGE)
	   (input   "A control structure.")
	   (value   "..."))
  (:method ((control pdsc+control))
	   (pdsc=why-method control))
  (:method (control)
	   (error ";;;PDSC~~WHY-METHOD: ~A is not of type PDSC+CONTROL." control))
  )





(defun pdsc~create (reasons sponsors unsponsors &optional alternatives why-sequent why-method successor predecessor)
  (declare (edited  "23-MAY-1997"  "17-JUN-1996 18:04")
	   (authors Lassaad SORGE)
	   (input   "A reason list, two node lists, and optionally a node list,"
		    "two control informations, and two actual nodes.")
	   (effect  "Creates an instant of PDSC+CONTROL.")
	   (value   "The newly created instance."))
  (let ((new-instance  (make-instance 'pdsc+control
				      :reasons reasons
				      :sponsors sponsors
				      :unsponsors unsponsors
				      :why-sequent why-sequent
				      :why-method why-method)))
    (setf (pdsc~alternatives new-instance) alternatives)
    (setf (pdsc~successor new-instance) successor)
    (setf (pdsc~predecessor new-instance) predecessor)
    new-instance
    ))


(defun pdsc~copy (control)
  (declare (edited  "10-OCT-1996")
	   (authors Lassaad)
	   (input   "A control object.")
	   (effect  "None.")
	   (value   "A copy of the control object CONTROL."))
  (with-slots (reasons sponsors unsponsors alternatives why-sequent why-method successor predecessor) control
	      (pdsc~create (copy-list reasons)
			   (copy-list sponsors)
			   (copy-list unsponsors)
			   (copy-list alternatives)
			   why-sequent
			   why-method
			   successor
			   predecessor)))







