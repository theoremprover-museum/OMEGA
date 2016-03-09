;;; -*- syntax: common-lisp; package: keim; base: 10; mode: lisp -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
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



(mod~defmod TREE 
            :uses (mod)
            :documentation "Macros for tree traversal."
            :exports (tree~clearhash
                      tree~fresh-label
                      tree~hash
                      tree~hashed
                      tree~label!
                      tree~labelled-p
                      tree~newhash
                      tree~pop-label
                      tree~pophash
                      tree~pushhash
                      tree~unlabel!
                      tree~with-fresh-hash
                      tree~with-fresh-label
                      tree~with-old-hash
		      )
	    )


;;; Label Handling:
;;; ===============

(defvar tree*labels nil)

#{\section{Labels}

Sometimes it is necessary to leave marks (so called labels) at data structures when traversing
trees. There are different types of labellings, the simplest being just a discrimination between
``labelled'' and ``unlabelled''. This occurs very often, so procedures exist in \keim\ that
support this for all data structures.

In order not to have to traverse each tree twice (first for unlabelling everything and then for
the real thing), it is convenient to ``invent'' a completely new label every time and then
test for equality. Exactly this is done in \keim\ .

The actual labels are completely hidden from the user, but nevertheless the respective procedures
can be used recursively, for the labels are kept on a stack, so that there is something like a
{\em momentarily active label}, which can change by push and pop operations.

The usual procedure for using labels is to encapsulate the respective  procedures in a
{\tt tree$\sim$with-fresh-label}. Inside this one can use the operations {\tt tree$\sim$label!},
{\tt tree$\sim$unlabel!}
and {\tt tree$\sim$labelled-p} to (un)label a datum or check for the state of being labelled
with the active label (note that unlike the active labels the labels belonging to structures
are {\em not} remembered in stacks, i.e. {\tt tree$\sim$label!} and {\tt tree$\sim$unlabel!}
are destructive).#}

(defmacro tree~with-fresh-label (corpus)
  (declare (edited  "09-FEB-1995")
	   (authors Fehrer)
	   (input   "a body")
	   (effect  "a fresh label is generated and pushed on the label stack for the time body is executed")
	   (value   "what body returns"))
  `(prog2 (tree~fresh-label)
       ,corpus
     (tree~pop-label)))

(defun tree~labelled-p (datum)
  (declare (edited  "09-FEB-1995")
	   (authors Fehrer)
	   (input   "a datum")
	   (effect  "none")
	   (value   "T iff the datum is labelled with the momentarily active label"))
  (eq (tree=label datum) (car tree*labels)))

(defun tree~label! (datum)
  (declare (edited  "09-FEB-1995")
	   (authors Fehrer)
	   (input   "a datum")
	   (effect  "the datum is labelled with the currently active label")
	   (value   "the label"))
  (setf (tree=label datum) (car tree*labels)))

(defun tree~unlabel! (datum)
  (declare (edited  "09-FEB-1995")
	   (authors Fehrer)
	   (input   "a datum")
	   (effect  "the datum is made unlabelled")
	   (value   "nil"))
  (setf (tree=label datum) nil))

#{

In case one wants to explicitly perform push and pop operations to alter the currently active
label the functions {\tt tree$\sim$fresh-label} and {\tt tree$\sim$pop-label} can be used.
If called when the label stack is empty, {\tt tree$\sim$label!} and {\tt tree$\sim$labelled-p}
yield unexpected results (this case is not checked, so the programmer takes every responsibility
when using {\tt tree$\sim$fresh-label} and {\tt tree$\sim$pop-label}!). This, however,cannot occur if everything is
correctly encapsulated in a {\tt tree$\sim$with-fresh-label} construct.

#}

(defun tree~fresh-label ()
  (declare (edited  "09-FEB-1995")
	   (authors Fehrer)
	   (effect  "a new label is generated and pushed on the label stack"))
  (setq tree*labels (cons (gensym) tree*labels)))

(defun tree~pop-label ()
  (declare (edited  "09-FEB-1995")
	   (authors Fehrer)
	   (effect  "the previously active label becomes the currently active label"))
  (setq tree*labels (cdr tree*labels)))

;;; Hashtable Handling:
;;; ===================

(defvar tree*hashtabs nil)

#{\section{Hashtables}

For example in order to apply structure sharing it is sometimes convenient to use hashtables to store
data structures in order to find out if the same structures already occured in a given context.
To simplify this a mechanism analog to the labelling has been incorporated into \keim\ .

The usual procedure here is to enclose the context into a {\tt tree$\sim$with-fresh-hash} and
then use the functions {\tt tree$\sim$hash} and {\tt tree$\sim$hashed} to enter a structure
into the current hashtable or check for its presence. Instead of {\tt tree$\sim$with-fresh-hash}
{\tt tree$\sim$with-old-hash} can be used. This creates a new context, but pushes a copy of the
previous hashtable instead of an empty one. Thus the context can be enlarged, but a backtrack
point exists.

#}

(defmacro tree~with-fresh-hash (corpus)
  (declare (edited  "09-FEB-1995")
	   (authors Fehrer)
	   (input   "a body")
	   (effect  "executes corpus with a fresh hash table pushed on the stack")
	   (value   "what body returns"))
  `(prog2 (tree~newhash)
       ,corpus
     (tree~pophash)))

(defmacro tree~with-old-hash (corpus)
  (declare (edited  "09-FEB-1995")
	   (authors Fehrer)
	   (input   "a body")
	   (effect  "executes body with a copy of the active hash table pushed on the stack")
	   (value   "what body returns"))
  `(prog2 (tree~pushhash)
       ,corpus
     (tree~pophash)))

(defun tree~hash (datum entry)
  (declare (edited  "09-FEB-1995")
	   (authors Fehrer)
	   (input   "a datum")
	   (effect  "enters entry under index datum into the current hashtable")
	   (value   "entry"))
  (setf (gethash datum (car tree*hashtabs)) entry))

(defun tree~hashed (datum)
  (declare (edited  "09-FEB-1995")
	   (authors Fehrer)
	   (input   "a datum")
	   (effect  "none")
	   (value   "the entry value, if datum is present in the current hashtable, nil else"))
  (gethash datum (car tree*hashtabs)))

#{

Explicit control can be exhibited by use of {\tt tree$\sim$newhash}, {\tt tree$\sim$pophash},
{\tt tree$\sim$pushhash} and {\tt tree$\sim$clearhash}

#}

(defun tree~clearhash ()
  (declare (edited  "09-FEB-1995")
	   (authors Fehrer)
	   (effect  "makes the active hashtable empty (destructive!)"))
  (clrhash (car tree*hashtabs)))

(defun tree~newhash ()
  (declare (edited  "09-FEB-1995")
	   (authors Fehrer)
	   (effect  "pushes an empty hashtable on the stack"))
  (setq tree*hashtabs (cons (make-hash-table :test #'equal :size 50 :rehash-size 2.0 :rehash-threshold 0.8) tree*hashtabs)))

(defun tree~pushhash ()
  (declare (edited  "09-FEB-1995")
	   (authors Fehrer)
	   (effect  "pushes a copy of the active hashtable on the stack"))
  (if (null tree*hashtabs)
      (error "there is no hashtable in use")
    (setq treehashtabs (cons (car tree*hashtabs) tree*hashtabs))))

(defun tree~pophash ()
  (declare (edited  "09-FEB-1995")
	   (authors Fehrer)
	   (effect  "makes the previously active hashtable the active one"))
         (setq tree*hashtabs (cdr tree*hashtabs)))
               

