;;; -*- syntax: common-lisp; package: keim; base: 10; mode: lisp -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1994 by AG Siekmann, Fachbereich Informatik,             ;;
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


(in-package :KEIM)

(mod~defmod cb :uses (mod )
            :documentation "Continue Objects."
            :exports ()
            )


#{
  
  This module provides some functions for a simple search mechanism in
  and-or-trees with a Prolog-like backtracking mechanism and the possibility to
  return a solution and continue the search later.  The alternatives occuring at
  the or-branches can be sorted like a stack (for depth-search), like a queue
  (for breadth-first-search), or with heuristic values.

  The key point of the continuation mechanism is the {\em continue object} that
  holds the alternatives and the last result. For this purpose the class {\tt
    cb+continue} is defined. It is intended to be used as a mixin class.
  After the creation and initializaton of a continue object with {\tt
    cb~create-cont-object}, the continue mechanism is started with {\tt
    cb~next-solution}.  At an and-fork {\tt cb~and} sets up goals which
  have to be solved together.  At an or-fork you have to call {\tt (cb~or
    ({\it val-1} {\it form-1}) \ldots ({\it val-n} {\it form-n}))}.  This
  expression evaluates the values and adds the forms associated with these
  values to the current list of alternatives. The values can be arbitrary
  objects; there meaning is determined by the call to the given {\em test function}
  (although {\tt t} and {\tt nil} have a predefined meaning).  This test
  function can be initialized when {\tt cb~create-cont-object} is called and
  it can be change each time {\tt cb~next-solution} is called.

  When an algorithm started or continued with {\tt cb~next-solution} returns
  a result, this result is stored in the continue object where it can be
  accessed with {\tt (cb~result cont-object)}. Furthermore all alternatives
  that have been set with {\tt cb~or} or {\tt cb~or-fns} are stored in
  the continue object.  The algorithm can be continued with {\tt
    (cb~next-solution {\it continue-object} \&optional {\it test})}.

  Since it is difficult to return a meaningfull result if a solution tree is
  found in an and-or-tree, a continue object can be initialized with a {\em
    result function}.  This result function is called each time a solution is
  found (with the value returned from the search as argument).  It is intended
  to compute the final result from some destructively changed state of the
  search algorithm.

  The macro {\tt (cb~or ({\it val-1} {\it form-1}) \ldots ({\it val-n} {\it
      form-n}))} is only some syntactic sugar for the function {\tt
    (cb~or-fns (list (cons {\it val-1} \#'(lambda () {\it form-1})) \ldots
    (cons {\it val-n} \#'(lambda () {\it form-n}))))}.  The same holds for {\tt
    cb~and}, {\tt cb~create-cont-object}, and {\tt cb~search}.

  If you do not want to use the continuation mechanism but the backtracking
  mechanism, you can call {\tt cb~search}.

  IMPORTANT: The search algorithm {\bf must not include any conjunctions like
    {\tt and}, {\tt every} etc}.  (They are only allowed if they do not
  call---directly or indirectly---any cb functions.)  

  This has to do with the fact that use of this module is sensible only if the
  solvability of a subtree of an and-fork depends on the choices that have been
  made in the other subtrees.  An example where this is not the case is a Prolog
  program where the goal clause is ground: If the the first literal of the goal
  has been solved and and the second literal turns out to be not solvable, it is
  useless to track back to the first literal in order to try to solve it in
  another way since the solvability of the second literal does not depend on
  choices made during solving the first literal. Of course this is not the case
  if the goal clause contains variables.

#}


#|
(cb~or
 (0 (list 0))
 ((1+ 0) ((wuerg)))
 ((+ 7 9) (format t "hallo")))

|#

      
(eval-when (load compile eval)
(defclass cb+continue ()
  ((result          :initform nil             :reader cb~result   :accessor cb=result)
   (alternatives    :initarg :alternatives    :accessor cb=alternatives)
   (result-function :initarg :result-function :reader cb=result-function)
   (test-function   :initarg :test-function   :accessor cb=test-function))
  (:documentation "This is class is intended to be a mixin for objects which are
                   associated with a computation that must be able to be stopped
                   and continued later.")
  ))

(defun cb~create-cont-object-fn (fn &key result-function test-function) 
  (declare (edited  "01-AUG-1994 11:35")
           (authors GKLEIN RICHTS)
           (input   "A nullary function and optional a unary function and binary function.")
           (effect  "None.")
           (value   "A new continue object.  FN is"
                    "the initial alternative; RESULT-FUNCTION is the"
                    "function that will be called when a solution is"
                    "found (the default is the identity); TEST-FUNCTION is"
                    "the function that determines the order in which the"
                    "alternatives are executed (default is <)."))
  (make-instance 'cb+continue
                 :alternatives (list (list t (bind=current-context-list) fn))
                 :result-function result-function
                 :test-function (or test-function #'<)))

(defmacro cb~create-cont-object (form &key result-function test-function)
  (declare (edited  "09-FEB-1994 1013")
           (authors RICHTS)
           (input   "A nullary function and optional a unary function and binary function.")
           (effect  "None.")
           (value   "A new continue object.  FORM is"
                    "the initial alternative; RESULT-FUNCTION is the"
                    "function that will be called when a solution is"
                    "found (the default is the identity); TEST-FUNCTION is"
                    "the function that determines the order in which the"
                    "alternatives are executed (default is <)."))
  `(cb~create-cont-object-fn (function (lambda () ,form))
			     :result-function ,result-function
			     :test-function ,test-function))

(defun cb~next-solution (cont-object &key (new-test-function nil supplied-p))
  (declare (edited  "06-APR-1995")
           (authors Richts)
           (input   "A continue object and optional a new test function.")
           (effect  "If NEW-TEST-FUNCTION is supplied the test function of"
                    "CONT-OBJECT (that determines the order in which the"
                    "alternatives are executed) is changed."
                    ""
                    "The alternatives of CONT-OBJECT are executed"
                    "consecutivly until a solution is found.  The executed"
                    "alternatives are removed from CONT-OBJECT and new"
                    "alternatives are added to it.  If an alternative returns"
                    "successfully the result function of CONT-OBJECT is called"
                    "with the result of the alternative and the returned final"
                    "result is stored in CONT-OBJECT.  If all alternatives"
                    "return unsuccessfully (i.e. there are no more"
                    "alternatives), NIL is returned.")
           (value   "The final result."))
  (when supplied-p
      (setf (cb=test-function cont-object) new-test-function))
  (multiple-value-bind (result alternatives)
            (cb=search-fns (cb=alternatives cont-object)
                               (cb=test-function cont-object))
      (setf (cb=alternatives cont-object) alternatives)
      (if (and result (cb=result-function cont-object))
          (setf (cb=result cont-object)
                (funcall (cb=result-function cont-object) result))
        (setf (cb=result cont-object) result))
      (cb=result cont-object)))


(defun cb~search-fn (fn &key result-function test-function)
  (declare (edited  "06-APR-1995")
           (authors Richts)
           (input   "A nullary function and optional"
                    "a unary function and a binary function.")
           (effect  "None.")
           (value   "FN is called with the backtracking mechanism enabled."
                    "TEST-FUNCTION determines the order in which the"
                    "alternatives are executed (the default is #'<)."
                    "If a solution is found, RESULT-FUNCTION (the default is"
                    "the identity) is called with the result and this final"
                    "result is returned."))
  (let ((result (cb=search-fns (list (list t fn)) (or test-function #'<))))
    (if (and result result-function)
        (funcall result-function result)
      result)))

(defmacro cb~search (form &key result-function test-function)
  (declare (edited  "09-FEB-1994 1013")
           (authors RICHTS)
           (input   "A form and optional a unary function.")
           (effect  "None.")
           (value   "FORM is called with the backtracking mechanism enabled."
                    "TEST-FUNCTION determines the order in which the"
                    "alternatives are executed (the default is #'<)."
                    "If a solution is found, RESULT-FUNCTION (the default is"
                    "the identity) is called with the result and this final"
                    "result is returned."))
  `(cb~search-fn (function (lambda () ,form))
                     :result-function ,result-function
                     :test-function ,test-function))


(defun cb=search-fns (alternatives test)
  (declare (edited  "06-APR-1995")
           (authors Richts)
           (input   "A list of alternatives, i.e. a list of pairs of a value"
                    "and list of nullary functions; a binary function.")
           (effect  "None.")
           (value   "The alternatives are executed consecutively until a"
                    "solution is found (i.e. a non-NIL value is returned) or until"
                    "there are no more alternatives.  A multiple value is"
                    "returned:"
                    "1. The result of the successfull alternative or NIL."
                    "2. The list of remaining and new alternatives."))
  (do ((new-alternatives alternatives)
       result)
      ((or (null new-alternatives) result)
       (values result new-alternatives))
    (multiple-value-setq (result new-alternatives)
        (cb=next new-alternatives test))))

(defun cb=next (alternatives test)
  (declare (edited  "06-APR-1995")
           (authors GKLEIN Richts)
           (input   "A list of alternatives, i.e. a list of pairs of a value"
                    "and list of nullary functions; a binary function.")
           (effect  "None.")
           (value   "The next alternative is seleceted: If there is one with"
                    "value T, that alternative that has been added last, is"
                    "selected (LIFO, stack); else if there is a non-nil"
                    "value, the alternative with the minimal value (according"
                    "to TEST) is selected; else if there is a alternative"
                    "with value NIL, that alternative that has been added"
                    "first, is selected (FIFO, queue). The selected"
                    "alternative is then executed."
                    "A multiple value is returned:"
                    "1. The result of the selected alternative."
                    "2. The list of remaining and new alternatives."))
  (let (cb*conjuncts cb*disjuncts)
    (declare (special cb*conjuncts cb*disjuncts))
    (let ((result (cond ((null alternatives)
                         nil)
                        ((or (null (car (first alternatives)))
                             (eq t (car (first alternatives))))
                         (bind~with-new-stack
			  ((cb~and-fns (cdr (cdr (pop alternatives)))))
			  :parent-context-list (car (cdr (first alternatives)))))
                        (t (let ((minimum (cb=min (first alternatives)
						  (rest alternatives) test)))
                           (setf alternatives (delete minimum alternatives))
			   (bind~with-new-stack
			    ((cb~and-fns (cdr (cdr minimum))))
			    :parent-context-list (car (cdr minimum))))))))
      (when cb*disjuncts
        (let* ((new-alts (mapcar #'(lambda (disjunct)
                                     (cons (car disjunct)
					   (cons (cadr disjunct)
						 (cons (cddr disjunct)
						       cb*conjuncts))))
                                 cb*disjuncts))
               (t-list   (remove-if-not #'(lambda (x) (eq x t))
                                        new-alts :key #'car))
               (nil-list (remove-if-not #'null
                                        new-alts :key #'car))
               (val-list (remove-if     #'(lambda (x) (or (null x) (eq x t)))
                                        new-alts :key #'car))
               (pos (or (position-if-not #'(lambda (x) (eq x t))
                                         alternatives :key #'car)
                        0)))
          (setf alternatives (nconc t-list
                                    (subseq alternatives 0 pos)
                                    val-list
                                    (subseq alternatives pos)
                                    nil-list))))
      (values result alternatives))))

(defun cb=min (minimum list test)
  (cond ((or (null list) (null (car (first list))))
         minimum)
        ((funcall test (car minimum) (car (first list)))
         (cb=min minimum (rest list) test))
        (t (cb=min (first list) (rest list) test))))


(defun cb~and-fns (conjuncts)
  (declare (edited  "06-APR-1995")
           (authors Richts)
           (input   "A list of nullary functions.")
           (effect  "The CONJUNCTS are ececuted consecutively.  If a"
                    "disjunction is encountered during this execution (and"
                    "therefore the execution is interrupted), the remaining"
                    "list of CONJUNCTS is returned via a dynamic bound"
                    "variable.")
           (value   "NIL, if one of the CONJUNCTS is not solvable or if a"
                    "disjunction is encountered."
                    "The value of the last cunjunction else."
                    "T if CONJUNCTS is empty.")
           (special cb*conjuncts cb*disjuncts))
  (if conjuncts
      (let ((result (funcall (first conjuncts))))
        (cond (result (cb~and-fns (rest conjuncts)))
              ((null cb*disjuncts) nil)
              (t (setf cb*conjuncts (append cb*conjuncts (rest conjuncts)))
                 nil)))
    t))

(defmacro cb~and (&body forms)
  (declare (edited  "09-FEB-1994 1013")
           (authors RICHTS)
           (input   "A list of forms.")
           (effect  "The same as cb~and-fns.")
           (value   "The same as cb~and-fns."))
  `(cb~and-fns ,(mapcar #'(lambda (form) 
                                 `(function (lambda () ,form)))
                              forms)))

(defun cb~or-fns (disjuncts)
  (declare (edited  "06-APR-1995")
           (authors Richts)
           (input   "A list of disjuncts, i.e. a list of pairs of a value"
                    "and a nullary function.")
           (effect  "The DISJUNCTS are returned via a dynamic bound variable.")
           (value   "Since the execution of the current alternative is"
                    "stopped and the new alternatives in DISJUNCTS are"
                    "returned to the central search loop where a new"
                    "alternative will be selected, the value of"
                    "cb~or-fns is always NIL.")
           (special cb*disjuncts))
  (let ((disjuncts-with-contexts (mapcar #'(lambda (disjunct)
					     (cons (car disjunct) (cons (bind=current-context-list) (cdr disjunct))))
					 disjuncts)))
    (setf cb*disjuncts disjuncts-with-contexts)
    nil))

(defmacro cb~or (&body clauses)
  (declare (edited  "09-FEB-1994 1013")
           (authors RICHTS)
           (input   "A list of clauses of the form (VALUE EXPRESSION).")
           (effect  "The same as cb~or-fns.")
           (value   "The same as cb~or-fns."))
  `(cb~or-fns ,(mapcar #'(lambda (clause) 
                                 `(cons ,(first clause)
                                        (function (lambda () ,(second clause)))))
                             clauses)))


